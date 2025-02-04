module MMC_EXT
  class Thread
  end

  class Mutex<BasicObject
  end

  class MutexEmptyLock<BasicObject
  end
end

module MTypeInf
  class ThreadType<BasicType
    @@id = 0
    def initialize(co, proc, *rest)
      super(co, *rest)
      @proc = proc
      @id = @@id
      @@id = @@id + 1
    end

    def inspect_aux(hist, level)
      "Thread #{@proc.inspect} e=#{is_escape?}"
    end

    def ==(other)
      self.class == other.class &&
        @proc == other.proc #&&
        # @name == other.name
    end

    attr_accessor :proc
  end
end

module MTypeInf
  class TypeInferencer
    define_inf_rule_class_method :new, MMC_EXT::Thread do |infer, inst, node, tup|
      proc = inst.inreg[-1].flush_type(tup)[tup]
      proc[0].irep.thread_top = true
      thtype = ThreadType.new(MMC_EXT::Thread, proc[0])
      # proc[0].slf = [thtype]
      inst.inreg[1..-2].each do |ele|
        ele.type[tup].each do |type|
          if !type.is_a?(ThreadType) then
            type.place[:thread] = thtype
          end
        end
      end
      curth = infer.thread
      infer.thread = thtype
      inreg = inst.inreg.clone
      inst.outreg[0].add_type(thtype, tup)
      rule_yield_passed_block(infer, inst, node, tup, proc, nil, inreg)
      infer.thread = curth
      nil
    end

    define_inf_rule_method :join, MMC_EXT::Thread do |infer, inst, node, tup|
      type = inst.inreg[0].get_type(tup)[0]
      proc = type.proc
      inst.outreg[0].add_type ThreadType.new(MMC_EXT::Thread, proc), tup
      nil
    end
  end
end

module CodeGenC
  class CodeGen
    define_ccgen_rule_class_method :new, MMC_EXT::Thread do |ccgen, inst, node, infer, history, tup|
      procty = get_ctype(ccgen, inst.inreg[-1], tup, infer)
      oreg = inst.outreg[0]
      ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
      MTypeInf::TypeInferencer::make_intype(infer, inst.inreg, node, tup, inst.para[1]) do |intype, argc|
        ptype = intype[-1][0]
        slf = inst.inreg[0]
        intype[0] = ptype.slf

        if intype[0].size == 1 then
          # store self object only 1 kind.
          utup = infer.typetupletab.get_tupple_id(intype, ptype, tup)
          codeno = ptype.using_tup[utup]
          if codeno then
            bfunc = gen_block_func("p#{ptype.id}", ptype.slf[0].class_object, codeno, utup)
          else
            # create main thread proc
            codeno = ptype.using_tup.size
            ptype.using_tup[utup] = codeno
            mtab = ccgen.proctab[ptype.irep]
            nminf = mtab[0].dup
            bfunc = gen_block_func("p#{ptype.id}", ptype.slf[0].class_object, codeno, utup)
            dstt = get_ctype(ccgen, inst.inreg[0], tup, infer)
            nminf[0] = bfunc
            nminf[1] = ptype
            nminf[2] = utup
            nminf[3] = dstt
            mtab[codeno] = nminf
          end

          if inst.para[1] == 127 then
            i = 0
            inreg = inst.inreg[1]
            base = "v#{inreg.id}"
            argsv = []
            while argr = inreg.type[tup][0].element[i]
              argsv.push "#{base}[#{i}]"
              i = i + 1
            end
            args = [(reg_real_value_noconv(ccgen, inst.inreg[-1], node, tup, infer, history))[0]]
            args += argsv
          else
            procreg = inst.inreg[-1]
            args = []
            args << ["mrb", "mrb"]
            if procreg.is_escape?(tup) then
              args << ["mrbproc",
              reg_real_value_noconv(ccgen, procreg, node, tup, infer, history)[0]]
            else
              args << ["cgproc",
                "#{(reg_real_value_noconv(ccgen, procreg, node, tup, infer, history))[0]}"]
            end
            inst.inreg[1..-2].each {|reg|
              args << ["v#{reg.id}",
                (reg_real_value_noconv(ccgen, reg, node, tup, infer, history))[0]]
            }
          end
          reg = inst.inreg[-1]
          tys = reg.get_type(tup)
          if tys and tys.size == 1 and tys[0].class_object != NilClass then
            args << ["v#{reg.id}", (reg_real_value_noconv(ccgen, reg, node, tup, infer, history))[0]]
          end
          args << ["prevgctab", "gctab"]

          outtype0 = get_ctype(ccgen, ptype.irep.retreg, utup, infer)
          outtype = get_ctype(ccgen, oreg, tup, infer)
          argt = "mrb_state *mrb, "
          if procreg.is_escape?(tup) then
            argt << "mrb_value mrbproc"
          else
            argt << "gproc cgproc"
          end
          argt << ", "
          argt << inst.inreg[1..-1].map {|reg|
            gen_declare(ccgen, reg, tup, infer, false, true)
          }.join(", ")
          argt << ", struct gctab *prevgctab"

          argst = "struct thprocarg#{oreg.id} {\n"
          argst << "mrb_state *mrb;\n"
          if procreg.is_escape?(tup) then
            argst << "mrb_value mrbproc;\n"
          else
            argst << "gproc cgproc;\n"
          end
          argst << inst.inreg[1..-1].map {|reg|
            gen_declare(ccgen, reg, tup, infer, false, true)
          }.join(";\n")
          argst <<  ";\nstruct gctab *prevgctab;\n};\n"

          if ccgen.proctab[ptype.irep] then
            minf = ccgen.proctab[ptype.irep][codeno]
            if minf then
              fname = minf[0]
            else
              minf = ccgen.proctab[ptype.irep][0]
              fname = ccgen.proctab[ptype.irep][0][0]
            end
            ccgen.using_block.push minf
          else
            p ptype.irep.class
            p ptype.irep.irep
            p ccgen.proctab.keys
            p inst.line
            fname = "((#{outtype0} (*)(mrb_state *, #{argt}))((struct proc#{ptype.id} *)(#{procvar}))->code[#{codeno}])"
            raise "Unnown proc"
          end

          ccgen.scode << argst
          ccgen.pcode << "{\n"
          ccgen.pcode << "struct thprocarg#{oreg.id} *tpargv = malloc(sizeof(struct thprocarg#{oreg.id}));\n"
          ccgen.pcode << args.map {|mem, val|
            "tpargv->#{mem} = #{val}"
          }.join(";\n")
          ccgen.pcode << ";\n"
          ccgen.lfcode << "void *apply_thproc_#{oreg.id}(void *arg) {\n"
          ccgen.lfcode << "struct thprocarg#{oreg.id} *tpargv = (struct thprocarg#{oreg.id} *)arg;\n"
          ccgen.lfcode << "#{fname}("
          ccgen.lfcode << args.map {|mem, val|
            "tpargv->#{mem}"
          }.join(", ")
          ccgen.lfcode << ");\n"
          ccgen.lfcode << "return NULL;\n"
          ccgen.lfcode << "}\n"

          oreg_nb = oreg.clone
          oreg_nb.type = {}
          ntypes = []
          oreg.type[tup].each do |ty|
            nty = MTypeInf::ThreadType.new(MMC_EXT::Thread, ty.proc)
            ntypes.push nty
          end
          oreg_nb.type[tup] = ntypes

          ccgen.dcode << CodeGen::gen_declare_core(ccgen, oreg_nb, tup, infer, false, "ubv#{oreg.id}")
          ccgen.dcode << ";\n"
          ccgen.pcode << "ubv#{oreg.id} = malloc(sizeof(*ubv#{oreg.id}));\n"
          ccgen.pcode << "ubv#{oreg.id}->argv = tpargv;\n"
          ccgen.pcode << "pthread_create(&ubv#{oreg.id}->thread, NULL, apply_thproc_#{oreg.id}, tpargv);\n"
          src2 = gen_type_conversion(ccgen, outtype, [:thread, nil, nil], "ubv#{oreg.id}", tup, node, infer, history, oreg, oreg_nb)
          ccgen.dcode << CodeGen::gen_declare(ccgen, oreg, tup, infer)
          ccgen.dcode << ";\n"
          ccgen.pcode << "v#{oreg.id} = #{src2};\n"
          ccgen.pcode << "}\n"
        end
      end
    end

    define_ccgen_rule_method :join, MMC_EXT::Thread do |ccgen, inst, node, infer, history, tup|
      slfreg = inst.inreg[0]
      oreg = inst.outreg[0]
      val = reg_real_value(ccgen, slfreg, oreg, node, tup,
                     infer, history)
      ccgen.dcode << gen_declare(ccgen, slfreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "{\n"
      ccgen.pcode << "void *ret;\n"
      ccgen.pcode << "pthread_join(#{val}->thread, &ret);\n"
      ccgen.pcode << "}\n"
    end
  end
end

module CodeGenC
  class CodeGen
    @@ruletab ||= {}

    def self.define_ccgen_rule_op(name, &block)
      @@ruletab[:CCGEN] ||= {}
      if @@ruletab[:CCGEN][name] then
        raise "Already defined #{name}"
      end
      @@ruletab[:CCGEN][name] = block
    end

    define_ccgen_rule_op :MOVE do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        reg_real_value(ccgen, inst.inreg[0], inst.outreg[0], node, tup, infer, history)
      }
      olddreg = inst.para[0]
      while olddreg.is_a?(RiteSSA::Reg) do
        if node.root.export_regs.include?(olddreg) then
          src = reg_real_value(ccgen, inst.inreg[0], olddreg,
                         node, tup, infer, history)
          ccgen.pcode << "v#{olddreg.id} = #{src};\n"
        end
        ins = olddreg.genpoint
        if ins.is_a?(RiteSSA::Inst) then
          olddreg = ins.para[0]
        else
          break
        end
      end
      nil
    end

    define_ccgen_rule_op :LOADI do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      if node.root.export_regs.include?(oreg) then
        ccgen.dcode << "#{gen_declare(self, oreg, tup)};\n"
        src = inst.para[0]
        if src.is_a?(Fixnum) then
          srct = :mrb_int
        elsif src.is_a?(Float) then
          srct = :mrb_float
        else
          srct = :mrb_value
        end
        dstt = get_ctype(ccgen, oreg, tup)
        gen_type_conversion(dstt, srct, src)
        ccgen.pcode << "v#{oreg.id} = #{src};\n"
      end
    end

    define_ccgen_rule_op :LOADL do |ccgen, inst, node, infer, history, tup|
      @@ruletab[:CCGEN][:LOADI].call(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :LOADSYM do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LOADNIL do |ccgen, inst, node, infer, history, tup|
      nil
    end


    define_ccgen_rule_op :LOADSELF do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LOADT do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LOADF do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :GETCONST do |ccgen, inst, node, infer, history, tup|
      val = inst.para[1]
      if !(val.is_a?(Fixnum) or val.is_a?(Float)) then
        ccgen.clstab[val] = [inst.para[0], "const#{ccgen.clstab.size}"]
      end
      nil
    end

    define_ccgen_rule_op :GETIV do |ccgen, inst, node, infer, history, tup|
      dst = inst.outreg[0]
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = inst.inreg[0]
      srct = get_ctype(ccgen, inst.inreg[0], tup)
      slf = inst.inreg[1]
      ccgen.dcode << "#{gen_declare(self, dst, tup)};\n"

      if is_escape?(slf) then
        idx = src.genpoint
        src = "mrb_ary_ref(mrb, self, #{idx});"
        src = gen_type_conversion(dstt, :mrb_value, src)
        ccgen.pcode << "v#{dst.id} = #{src}\n"
      else
        ccgen.pcode << " v#{dst.id} = self->v#{src.id};\n"
      end
      nil
    end

    define_ccgen_rule_op :SETIV do |ccgen, inst, node, infer, history, tup|
      dst = inst.outreg[0]
      dstt = get_ctype(ccgen, dst, tup)
      slf = inst.inreg[1]
      valr = inst.inreg[0]
      val = reg_real_value(ccgen, valr, dst, node, tup, infer, history)
      if is_escape?(slf) then
        val = gen_type_conversion(:mrb_value, dstt, val)
        ccgen.pcode << "mrb_ary_set(mrb, self, #{dst.genpoint}, #{val});\n"
      else
        ccgen.pcode << "self->v#{dst.id} = #{val};\n"
      end
      nil
    end

    define_ccgen_rule_op :GETUPVAR do |ccgen, inst, node, infer, history, tup|
      up = inst.para[1]
      pos = inst.para[2]
      proc = ccgen.callstack[-1][0]
      ireg = inst.inreg[0]
      oreg = inst.outreg[0]

      ccgen.dcode << "#{gen_declare(self, oreg, tup)};\n"
      ccgen.pcode << "v#{oreg.id} = proc->env#{"->prev" * up}->v#{ireg.id};\n"
      nil
    end

    define_ccgen_rule_op :SETUPVAR do |ccgen, inst, node, infer, history, tup|
      up = inst.para[1]
      pos = inst.para[2]
      proc = ccgen.callstack[-1][0]
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]

      ccgen.dcode << "#{gen_declare(self, oreg, tup)};\n"
      val = reg_real_value(ccgen, ireg, oreg, node, tup, infer, history)
      ccgen.pcode << "proc->env#{"->prev" * up}->v#{oreg.id} = #{val};\n"
      nil
    end

    define_ccgen_rule_op :ENTER do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :JMP do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :JMPIF do |ccgen, inst, node, infer, history, tup|
      cond = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))[0]
      r = 0
      inst.inreg[0].type[tup].each do |ty|
        if ty.class_object == NilClass or
            ty.class_object == FalseClass then
          r |= 1
        else
          r |= 2
        end
      end
      if cond == true or r == 2 then
        [node.exit_link[1]]
      elsif cond == false or r == 1 then
        [node.exit_link[0]]
      else
        ccgen.pcode << "if (#{cond}) goto L#{node.exit_link[1].id}; else goto L#{node.exit_link[0].id};\n"
        [node.exit_link[1], node.exit_link[0]]
      end
    end

    define_ccgen_rule_op :JMPNOT do |ccgen, inst, node, infer, history, tup|
      cond = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))[0]
      r = 0
      inst.inreg[0].type[tup].each do |ty|
        if ty.class_object == NilClass or
            ty.class_object == FalseClass then
          r |= 1
        else
          r |= 2
        end
      end
      if cond == true or r == 2 then
        [node.exit_link[0]]
      elsif cond == false or r == 1 then
        [node.exit_link[1]]
      else
        ccgen.pcode << "if (#{cond}) goto L#{node.exit_link[0].id}; else goto L#{node.exit_link[1].id};\n"
        [node.exit_link[0], node.exit_link[1]]
      end
    end

    define_ccgen_rule_op :SEND do |ccgen, inst, node, infer, history, tup|
      op_send(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :SENDB do |ccgen, inst, node, infer, history, tup|
      op_send(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :RETURN do |ccgen, inst, node, infer, history, tup|
      if ccgen.callstack[-1][1] then
        ccgen.pcode << "mrb_gc_arena_restore(mrb, ai);\n"
      end
      retval = reg_real_value(ccgen, inst.inreg[0], inst.outreg[0], node, tup, infer, history)
      if retval then
        ccgen.pcode << "return #{retval};\n"
      else
        ccgen.pcode << "return mrb_nil_value();\n"
      end
      nil
    end

    define_ccgen_rule_op :EQ do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :==)
      }
      nil
    end

    define_ccgen_rule_op :LT do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :<)
      }
      nil
    end

    define_ccgen_rule_op :LE do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :<=)
      }
      nil
    end

    define_ccgen_rule_op :GT do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :>)
      }
      nil
    end

    define_ccgen_rule_op :GE do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :>=)
      }
      nil
    end

    define_ccgen_rule_op :ADD do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :+)
      }
      nil
    end

    define_ccgen_rule_op :SUB do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :-)
      }
      nil
    end

    define_ccgen_rule_op :MUL do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :*)
      }
      nil
    end

      define_ccgen_rule_op :DIV do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup)  {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :/)
      }
      nil
    end

    define_ccgen_rule_op :ADDI do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :+)
      }
      nil
    end

    define_ccgen_rule_op :SUBI do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :-)
      }
      nil
    end

    define_ccgen_rule_op :ARRAY do |ccgen, inst, node, infer, history, tup|
      reg = inst.outreg[0]
      aescape = is_escape?(reg)
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      ereg = inst.outreg[0].type[tup][0].element[uv]
      etype = get_ctype_aux(ccgen, ereg, tup)

      if aescape then
        vals = inst.inreg.map {|ireg|
          val, convp = reg_real_value_noconv(ccgen, ireg, ereg, node, tup, infer, history)
          srct = get_ctype(ccgen, ireg, tup)
          gen_type_conversion(:mrb_value, srct, val)
        }

        ccgen.dcode << "mrb_value v#{reg.id};\n"
        ccgen.pcode << "{\n"
        ccgen.pcode << "mrb_value tmpele[] = {\n"
        ccgen.pcode << vals.join(', ')
        ccgen.pcode << "\n};\n"
        ccgen.pcode << "v#{reg.id} = mrb_ary_new_from_values(mrb, #{vals.size}, tmpele);\n"
        ccgen.callstack[-1][1] = true
        ccgen.pcode << "}\n"
      else
        type = get_ctype_aux(ccgen, reg, tup)
        if type == :array then
          vals = inst.inreg.map {|ireg|
            reg_real_value(ccgen, ireg, ereg, node, tup, infer, history)
          }

          ccgen.pcode << "#{etype} v#{reg.id}[] = {\n"
          ccgen.pcode << vals.join(', ')
          ccgen.pcode << "\n};\n"
        end
      end
      nil
    end

    define_ccgen_rule_op :LAMBDA do |ccgen, inst, node, infer, history, tup|
      # make env struct
      envreg = inst.para[1]
      proc = inst.outreg[0].type[tup][0]
      cproc = ccgen.callstack[-1][0]
      pproc = cproc.parent
      tupsize = proc.using_tup.size
      if envreg.size > 0 or
          (pproc and pproc.irep.export_regs.size > 0) then
        ccgen.hcode << "struct proc#{proc.id} {\n"
        ccgen.hcode << "int id;\n"
        ccgen.hcode << "void *code[#{tupsize}];\n"
        ccgen.hcode << "struct env#{cproc.id} *env;\n"
      else
        ccgen.hcode << "struct proc#{proc.id} {\n"
        ccgen.hcode << "int id;\n"
        ccgen.hcode << "void *code[#{tupsize}];\n"
      end

      slfdecl = gen_declare(ccgen, proc.slfreg, tup)
      ccgen.hcode << "#{slfdecl};\n"
      ccgen.hcode << "};\n"

      regno = inst.outreg[0].id
      ccgen.pcode << "struct proc#{proc.id} v#{regno};\n"
      ccgen.pcode << "v#{regno}.id = #{proc.id};\n"
      ccgen.pcode << "v#{regno}.self = self;\n"
      if envreg.size > 0 then
        ccgen.pcode << "v#{regno}.env = &env;\n"
      end
      proc.using_tup.each do |tp, i|
        bfunc = gen_block_func("p#{proc.id}", proc.slf.class_object, inst.para[3], tp)
        ccgen.pcode << "v#{regno}.code[#{i}] = (void *)#{bfunc};\n"
        minf = [bfunc, proc, tp]
        ccgen.using_block.push minf
      end
    end
  end
end


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
        reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :LOADI do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      if node.root.export_regs.include?(oreg) then
        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup)};\n"
        src = inst.para[0]
        srct = get_ctype_from_robj(src)
        dstt = get_ctype(ccgen, oreg, tup)
        src = gen_type_conversion(ccgen, dstt, srct, src, tup, node, infer, history)
        ccgen.pcode << "v#{oreg.id} = #{src};\n"
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :LOADL do |ccgen, inst, node, infer, history, tup|
      @@ruletab[:CCGEN][:LOADI].call(ccgen, inst, node, infer, history, tup)
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :LOADSYM do |ccgen, inst, node, infer, history, tup|
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :LOADNIL do |ccgen, inst, node, infer, history, tup|
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end


    define_ccgen_rule_op :LOADSELF do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LOADT do |ccgen, inst, node, infer, history, tup|
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :LOADF do |ccgen, inst, node, infer, history, tup|
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :GETCONST do |ccgen, inst, node, infer, history, tup|
      val = inst.outreg[0].type[tup][0].val

      if !(val.is_a?(Fixnum) or val.is_a?(Float)) then
        ccgen.clstab[val] = [inst.para[0], "const#{ccgen.clstab.size}"]
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :GETIV do |ccgen, inst, node, infer, history, tup|
      dst = inst.outreg[0]
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = inst.inreg[0]
      srct = get_ctype(ccgen, inst.inreg[0], tup)
      slf = inst.inreg[1]
      ccgen.dcode << "#{gen_declare(ccgen, dst, tup)};\n"

      if slf.is_escape?(tup) then
        idx = src.genpoint
        src = "ARY_PTR(mrb_ary_ptr(self))[#{idx}]"
        src = gen_type_conversion(ccgen, dstt, :mrb_value, src, tup, node, infer, history)
        ccgen.pcode << "v#{dst.id} = #{src};\n"
      else
        ccgen.pcode << " v#{dst.id} = self->v#{src.id};\n"
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :SETIV do |ccgen, inst, node, infer, history, tup|
      dst = inst.outreg[0]
      dstt = get_ctype(ccgen, dst, tup)
      slf = inst.inreg[1]
      valr = inst.inreg[0]
      val = reg_real_value(ccgen, valr, dst, node, tup, infer, history)
      if slf.is_escape?(tup) then
        val = gen_type_conversion(ccgen, :mrb_value, dstt, val, tup, node, infer, history)
#        ccgen.pcode << "mrb_ary_set(mrb, self, #{dst.genpoint}, #{val});\n"
        ccgen.pcode << "ARY_PTR(mrb_ary_ptr(self))[#{dst.genpoint}] = #{val};\n"
        ccgen.pcode << "mrb_field_write_barrier_value(mrb, (struct RBasic*)mrb_ary_ptr(self), #{val});\n"
      else
        ccgen.pcode << "self->v#{dst.id} = #{val};\n"
      end
      nil
    end

    define_ccgen_rule_op :GETUPVAR do |ccgen, inst, node, infer, history, tup|
      up = inst.para[1]
      ireg = inst.inreg[0]
      oreg = inst.outreg[0]
      proc = ccgen.callstack[-1][0]
      pty = ccgen.callstack[-1][2]
      up.times do
        proc = proc.parent
      end
      dstt = get_ctype(ccgen, oreg, tup)

      ccgen.dcode << "#{gen_declare(ccgen, oreg, tup)};\n"
      if pty == :mrb_value then
        pos = proc.env.index(ireg)
        val = "(mrb_proc_ptr(mrbproc))->e.env->stack[#{pos + 1}]"
        val = gen_type_conversion(ccgen, dstt, :mrb_value, val, tup, node, infer, history)
        ccgen.pcode << "v#{oreg.id} = #{val};\n"
      else
        val = "proc->env#{"->prev" * up}->v#{ireg.id}"
        #val = gen_type_conversion(ccgen, pty, dstt, val, tup, node, infer, history)
        ccgen.pcode << "v#{oreg.id} = #{val};\n"
      end
      nil
    end

    define_ccgen_rule_op :SETUPVAR do |ccgen, inst, node, infer, history, tup|
      up = inst.para[1]
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      pty = ccgen.callstack[-1][2]
      proc = ccgen.callstack[-1][0]
      frame = inst.para[0]
      ptup = 0
      proc.tups.each do |f, t|
        if frame == f then
          ptup = t
          break
        end
      end
      dstt = get_ctype(ccgen, oreg, ptup)

      if pty == :mrb_value then
        pos = proc.env.index(oreg)
        dst = "(mrb_proc_ptr(mrbproc))->e.env->stack[#{pos + 1}]"
        val = reg_real_value2(ccgen, ireg, oreg, node, tup, pttup, infer, history)
        val = gen_type_conversion(ccgen, :mrb_value, dstt, val, tup, node, infer, history)
        ccgen.pcode << "#{dst} = #{val};\n"
      else
        ccgen.dcode << "#{gen_declare(ccgen, oreg, ptup)};\n"
        val = reg_real_value2(ccgen, ireg, oreg, node, tup, ptup, infer, history)
        ccgen.pcode << "proc->env#{"->prev" * up}->v#{oreg.id} = #{val};\n"
      end
      nil
    end

    define_ccgen_rule_op :ENTER do |ccgen, inst, node, infer, history, tup|
      inst.inreg.each_with_index {|ireg, i|
        oreg = inst.outreg[i]

        if node.root.export_regs.include?(oreg) then
          src = reg_real_value(ccgen, oreg, oreg,
                         node, tup, infer, history)
          ccgen.pcode << "env.v#{oreg.id} = #{src};\n"
        end
      }
      nil
    end

    define_ccgen_rule_op :JMP do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :JMPIF do |ccgen, inst, node, infer, history, tup|
      cond, srct = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      cond = gen_type_conversion(ccgen, :mrb_bool, srct, cond, node, tup, infer, history)
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
        ["", [node.exit_link[1]]]
      elsif cond == false or r == 1 then
        ["", [node.exit_link[0]]]
      else
        src = "if (#{cond}) goto L#{node.exit_link[1].id}; else goto L#{node.exit_link[0].id};\n"
        [src, [node.exit_link[1], node.exit_link[0]]]
      end
    end

    define_ccgen_rule_op :JMPNOT do |ccgen, inst, node, infer, history, tup|
      cond, srct = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      cond = gen_type_conversion(ccgen, :mrb_bool, srct, cond, node, tup, infer, history)
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
        ["", [node.exit_link[0]]]
      elsif cond == false or r == 1 then
        ["", [node.exit_link[1]]]
      else
        src = "if (#{cond}) goto L#{node.exit_link[0].id}; else goto L#{node.exit_link[1].id};\n"
        [src, [node.exit_link[0], node.exit_link[1]]]
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
      retval = reg_real_value(ccgen, inst.inreg[0], inst.outreg[0], node, tup, infer, history)
      rettys = inst.outreg[0].type[tup]
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
      aescape = reg.is_escape?(tup)
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      ereg = inst.outreg[0].type[tup][0].element[uv]
      etype = get_ctype_aux(ccgen, ereg, tup)

      if aescape then
        vals = inst.inreg.map {|ireg|
          val, convp = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
          srct = get_ctype(ccgen, ireg, tup)
          gen_type_conversion(ccgen, :mrb_value, srct, val, tup, node, infer, history)
        }

        ccgen.dcode << "mrb_value v#{reg.id};\n"
        ccgen.pcode << "{\n"
        ccgen.pcode << "mrb_value tmpele[] = {\n"
        ccgen.pcode << vals.join(', ')
        ccgen.pcode << "\n};\n"
        ccgen.pcode << "mrb->ud = (void *)gctab;\n"
        ccgen.pcode << "v#{reg.id} = mrb_ary_new_from_values(mrb, #{vals.size}, tmpele);\n"
        ccgen.pcode << "for (int i = 0;i < #{vals.size}; i++) ARY_PTR(mrb_ary_ptr(v#{reg.id}))[i] = mrb_nil_value();\n"
        ccgen.pcode << "ARY_SET_LEN(mrb_ary_ptr(v#{reg.id}), #{vals.size});\n"
        ccgen.pcode << "mrb_gc_arena_restore(mrb, ai);\n"
        ccgen.callstack[-1][1] = true
        ccgen.pcode << "}\n"
      else
        type = get_ctype_aux(ccgen, reg, tup)
        if type == :array then
          vals = inst.inreg.map {|ireg|
            reg_real_value(ccgen, ireg, ereg, node, tup, infer, history)
          }

          ccgen.pcode << "#{etype} v#{reg.id}[#{vals.size + 1}] = {\n"
          ccgen.pcode << vals.join(', ')
          ccgen.pcode << "};\n"
          if etype == :mrb_value then
            ccgen.pcode << "v#{reg.id}[#{vals.size}].value.ttt = MRB_TT_FREE;\n"
            csize = ccgen.gccomplex_size
            ccgen.gccomplex_size += 1
            ccgen.pcode << "gctab->complex[#{csize}] = v#{reg.id};\n"
            ccgen.pcode << "gctab->csize = #{ccgen.gccomplex_size};\n"
          end
        end
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :ARYCAT do |ccgen, inst, node, infer, history, tup|
    end

    define_ccgen_rule_op :STRING do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      if oreg.is_escape?(tup) then
        ccgen.dcode << "mrb_value v#{reg.id};\n"
        ccgen.pcode << "v#{reg.id} = mrb_str_new(mrb, \"#{inst.para[0]}\", #{inst.para[0].size});"
      end
    end

    define_ccgen_rule_op :LAMBDA do |ccgen, inst, node, infer, history, tup|
      # make env struct
      envreg = inst.para[1]
      proc = inst.outreg[0].type[tup][0]
      cproc = ccgen.callstack[-1][0]
      pproc = cproc.parent
      if !ccgen.using_proc.include?(proc) then
        tupnum = proc.using_tup.size
        if envreg.size > 0 or pproc then
          ccgen.hcode << "struct proc#{proc.id} {\n"
          ccgen.hcode << "int id;\n"
          ccgen.hcode << "void *code[#{tupnum}];\n"
          ccgen.hcode << "struct env#{cproc.id} *env;\n"
        else
          ccgen.hcode << "struct proc#{proc.id} {\n"
          ccgen.hcode << "int id;\n"
          ccgen.hcode << "void *code[#{tupnum}];\n"
        end

        slfdecl = gen_declare(ccgen, proc.slfreg, tup)
        ccgen.hcode << "#{slfdecl};\n"
        ccgen.hcode << "};\n"

        ccgen.using_proc.push proc
      end

      regno = inst.outreg[0].id
      ccgen.pcode << "struct proc#{proc.id} v#{regno};\n"
      ccgen.pcode << "v#{regno}.id = #{proc.id};\n"
      ccgen.pcode << "v#{regno}.self = self;\n"
      if envreg.size > 0 or pproc then
        ccgen.pcode << "v#{regno}.env = &env;\n"
      end
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      proc.using_tup.each do |tp, i|
        bfunc = gen_block_func("p#{proc.id}", proc.slf.class_object, inst.para[3], tp)
#        ccgen.pcode << "v#{regno}.code[#{i}] = (void *)#{bfunc};\n"
        minf = [bfunc, proc, tp, dstt]
        ccgen.proctab[proc] ||= []
        ccgen.proctab[proc][i] = bfunc;
        ccgen.using_block.push minf
      end

      if node.root.is_export_env then
        val = gen_type_conversion(ccgen, dstt, [:gproc, proc.id], "(gproc)&v#{regno}", tup, node, infer, history)
        ccgen.dcode << "mrb_value vv#{regno};\n"
        ccgen.pcode << "vv#{regno} = #{val};\n"
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end
  end
end


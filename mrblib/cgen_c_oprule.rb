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

    define_ccgen_rule_op :START do |ccgen, inst, node, infer, history, tup|
      argc = infer.typetupletab.rev_table[tup].size - 4;
      ireg = inst.inreg[0]
      oreg = inst.outreg[0]

      if !oreg.type[tup] then
        tup = oreg.type.keys[0]
      end

      if node.root.effects[:iv_write] #or node.root.effects[:iv_read] then
        oreg.type[tup].each_with_index do |type|
          type.threads = []
        end
      end

      src, srct = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
      dstt = get_ctype(ccgen, oreg, tup, infer)
      src = gen_type_conversion(ccgen, dstt, srct, src, tup, node, infer, history, nil, ireg)
      ccgen.dcode << "#{gen_declare_core(ccgen, oreg, tup, infer, false, "self")};\n"
      ccgen.dcode << "#{gen_declare_core(ccgen, ireg, tup, infer, false, "mutexself")};\n"
      ccgen.pcode << "mutexself = v#{ireg.id};\n"
      ccgen.pcode << "self = #{src};\n"
      if argc >= 1 then
        inst.outreg[1..argc].each_with_index do |oreg, i|
          if oreg.type[tup] then
            ireg = inst.inreg[i + 1]
            ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
            ccgen.pcode << "v#{oreg.id} = v#{ireg.id};\n"
          end
        end
      end

      if argc >= 0 then
        oreg = inst.outreg[argc + 1]
        ireg = inst.inreg[argc + 1]
        if ireg and
            ireg.type[tup] and
            ireg.type[tup][0].class_object != NilClass then

          ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
          ccgen.pcode << "v#{oreg.id} = v#{ireg.id};\n"
        end
      end
      ccgen.pcode << "/* START END */\n"
      nil
    end

    define_ccgen_rule_op :NOP do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :MOVE do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        outr = inst.outreg[0]
        ccgen.dcode << "#{gen_declare(ccgen, outr, tup, infer)};/*snd*/\n"
        reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :LOADI do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      if node.root.export_regs.include?(oreg) and false then
        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
        src = inst.para[0]
        srct = get_ctype_from_robj(src)
        dstt = get_ctype(ccgen, oreg, tup, infer)
        src = gen_type_conversion(ccgen, dstt, srct, src, tup, node, infer, history, oreg)
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
      set_closure_env(ccgen, inst, node, infer, history, tup)
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

    define_ccgen_rule_op :GETGLOBAL do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      case inst.para[0]
      when :$/
        ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
        ccgen.dcode << ";\n"
        ccgen.pcode << "v#{oreg.id} = mrb_gv_get(mrb, mrb_intern_lit(mrb, \"#{inst.para[0]}\"));\n"
      else

      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :SETGLOBAL do |ccgen, inst, node, infer, history, tup|
      p "FOO"
      p tup
      p infer.typetupletab.rev_table[tup]
      p inst.inreg[0].id
     p inst.inreg[0].get_type(tup).map {|ty| ty} if inst.inreg[0].type[tup]
      inst.inreg[0].type.each do |tp, tys|
        p "#{tp} #{tys.map {|ty| ty.place}}"
      end
      nil
    end

    define_ccgen_rule_op :GETCONST do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      if oreg.type.values and
          oreg.type.values[0][0].is_a?(MTypeInf::LiteralType) then
        val = oreg.type.values[0][0].val
      else
        p "Unknown Constant value"
        p inst
        p inst.para[0]
        p oreg.type
      end

      if !(val.is_a?(Fixnum) or val.is_a?(Float) or ccgen.clstab[val]) then
        cno = ccgen.clstab.size
        ccgen.hcode << gen_declare_core(ccgen, oreg, tup, infer, false, "const#{cno}")
        ccgen.hcode << ";\n"
        ccgen.clstab[val] = [inst.para[0], "const#{cno}"]
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :SETCONST do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :GETMCNST do |ccgen, inst, node, infer, history, tup|
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :GETIV do |ccgen, inst, node, infer, history, tup|
      dst = inst.outreg[0]
      ivreg = inst.inreg[0]
      slf = inst.inreg[1]
      gen_get_iv(ccgen, inst, node, infer, history, tup, slf, ivreg, dst)
      nil
    end

    define_ccgen_rule_op :SETIV do |ccgen, inst, node, infer, history, tup|
      dst = inst.outreg[0]
      valr = inst.inreg[0]
      slf = inst.inreg[1]
      ivreg = inst.outreg[0]
      gen_set_iv(ccgen, inst, node, infer, history, tup, slf, ivreg, valr, dst)
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
      dstt = get_ctype(ccgen, oreg, tup, infer)
      srct = get_ctype(ccgen, ireg, tup, infer)

      ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
      if pty == :mrb_value then
        pos = proc.env.index(ireg)
        val = "(mrb_proc_ptr(mrbproc))->e.env->stack[#{pos + 1}]"
        val = gen_type_conversion(ccgen, pty, dstt, val, tup, node, infer, history, oreg)
        ccgen.pcode << "v#{oreg.id} = #{val};\n"
      else
        val = "proc->env#{"->prev" * up}->v#{ireg.id}"
        val = gen_type_conversion(ccgen, dstt, srct, val, tup, node, infer, history, oreg)
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
      dstt = get_ctype(ccgen, oreg, ptup, infer)

      if pty == :mrb_value then
        pos = proc.env.index(oreg)
        dst = "(mrb_proc_ptr(mrbproc))->e.env->stack[#{pos + 1}]"
        val = reg_real_value2(ccgen, ireg, oreg, node, tup, ptup, infer, history)
        val = gen_type_conversion(ccgen, :mrb_value, dstt, val, tup, node, infer, history, oreg)
        ccgen.pcode << "#{dst} = #{val};\n"
      else
        val = reg_real_value2(ccgen, ireg, oreg, node, tup, ptup, infer, history)
        ccgen.pcode << "proc->env#{"->prev" * up}->v#{oreg.id} = #{val};\n"
      end
      nil
    end

    define_ccgen_rule_op :ENTER do |ccgen, inst, node, infer, history, tup|
      ax = inst.para[0]
      m1 = (ax >> 18) & 0x1f
      o = (ax >> 13) & 0x1f
      r = (ax >> 12) & 0x1
      m2 = (ax >> 7) & 0x1f
      argc = infer.typetupletab.rev_table[tup].size - 4
      len = m1 + o + r + m2
      argreg = inst.inreg[0]
      argtype = argreg.get_type(tup)
      if len > 1 and argc == 1 and
          argtype and argtype[0].class_object_core == Array then

        argty = get_ctype(ccgen, argreg, tup, infer)
        ccgen.pcode << "{\n"
        ary = "v#{argreg.id}"
        if argtype[0].is_escape? then
          ary = gen_type_conversion(ccgen, :mrb_value, argty, ary, tup, node, infer, history, nil, argreg)
          ccgen.pcode << "mrb_value *array = ARY_PTR2(mrb_ary_ptr(#{ary}));\n"
        else
          ccgen.pcode << "#{argty} *array = #{ary};\n"
        end
        m1.times do |i|
          oreg = inst.outreg[i]
          oregtype = oreg.get_type(tup)[0]
          oregty = get_ctype(ccgen, oreg, tup, infer)

          argelereg = argtype[0].element[i]
          argelety = get_ctype(ccgen, argelereg, tup, infer)

          ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
          valsrc = "array[#{i}]"
          if argelety != :mrb_value_mutex and argelety != :mrb_value_mutex_emptylock and
              argelety != :mrb_value then
            valsrc = gen_type_conversion(ccgen, oregty, :mrb_value, valsrc, tup, node, infer, history, nil)
          end
          ccgen.pcode << "v#{oreg.id} = #{valsrc};\n"
          inst.para[2][i] = "v#{oreg.id}"
        end
        ccgen.pcode << "}\n"
      else
        if r == 1 then
          asize = argc - m1
          oreg = inst.outreg[m1]
          ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
          if oreg.is_escape?(tup) then
            gen_global_lock(ccgen, node)
            ccgen.pcode << "v#{oreg.id} = mrb_ary_new_from_values(mrb, #{asize}, &v#{inst.inreg[m1].id});\n"
            gen_global_unlock(ccgen, node)
          else
            dstt = get_ctype(ccgen, oreg, tup, infer)
            dstt2 = dstt
            case dstt[1]
            when "**"
              dstt2 = "#{get_ctype_to_c(ccgen, oreg, tup, infer, dstt[0])} *"
              dstt = [dstt[0], "*", dstt[2]]

            when "*"
              dstt2 = get_ctype_to_c(ccgen, oreg, tup, infer, dstt[0])
              dstt = get_ctype_to_c(ccgen, oreg, tup, infer, dstt[0])

            else
              raise "Not support yet #{dstt}"
            end
            ccgen.pcode << "v#{oreg.id} = alloca(sizeof(#{dstt2}) * #{asize}); // xxx\n"
            asize.times do |i|
              iregs = node.enter_reg[1..-1]
              val, srct = reg_real_value_noconv(ccgen, iregs[m1 + i], node, tup, infer, history)
              val = gen_type_conversion(ccgen, dstt, srct, val, tup, node, infer, history, nil)
              ccgen.pcode << "v#{oreg.id}[#{i}] = #{val};\n"
            end
            inst.para[2][m1] = "v#{oreg.id}"
          end
        end

        inst.inreg.each_with_index do |ireg, i|
          oreg = inst.outreg[i]

          if node.root.export_regs.include?(oreg) then
            cirep = node.root.irep
            reggen =  oreg.genpoint
            if (!oreg.is_a?(RiteSSA::ParmReg) or oreg.gen_declare > argc) and
                (!oreg.is_a?(RiteSSA::ParmReg) and
                (oreg.refpoint.any? {|ins| ins.node.root.irep == cirep} or
                  (reggen.op == :ENTER and
                    reggen.outreg.index(oreg) > argc))) then
              ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer, true)};\n"
            end

            if ireg.refpoint.size == 0 and
                (!ireg.is_a?(RiteSSA::ParmReg) or ireg.genpoint > argc) then
              ccgen.dcode << "#{gen_declare(ccgen, ireg, tup, infer, true)};\n"
            end

            if ireg.type[tup] == nil and
                get_ctype(ccgen, oreg, tup, infer) != :mrb_value then
              ccgen.pcode << "env.v#{oreg.id} = 0;/*enter */\n"
            else
              src = reg_real_value(ccgen, ireg, oreg,
                             node, tup, infer, history)
              if src then
                ccgen.pcode << "env.v#{oreg.id} = #{src};/*enter */\n"
              end
            end
          end
        end
      end

      if o != 0 and argc > m1 + m2 then
        if r == 1 then
          pos = argc - m1 - m2
          if pos >= inst.para[3].size then
            pos = inst.para[3].size - 1
          end

        else
          pos = argc - m1 - m2
        end

        nnode = inst.para[3][pos].exit_link[0]
        ["", [nnode]]
      else
        nil
      end
    end

    define_ccgen_rule_op :JMP do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :JMPIF do |ccgen, inst, node, infer, history, tup|
      cond, srct = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      cond = gen_type_conversion(ccgen, :mrb_bool, srct, cond, node, tup, infer, history, nil)
      r = 0
      inst.inreg[0].get_type(tup).each do |ty|
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
      cond = gen_type_conversion(ccgen, :mrb_bool, srct, cond, node, tup, infer, history, nil)
      r = 0
      inst.inreg[0].get_type(tup).each do |ty|
        if ty.class_object == NilClass or
            ty.class_object == FalseClass then
          r |= 1
        else
          r |= 2
        end
      end
      if cond == true or r == 2 then
        nd = MTypeInf::TypeInferencer.get_jmp_target(node, 0, inst)
        ["", [nd]]
      elsif cond == false or r == 1 then
        nd = MTypeInf::TypeInferencer.get_jmp_target(node, 1, inst)
        ["", [nd]]
      else
        src = "if (#{cond}) goto L#{node.exit_link[0].id}; else goto L#{node.exit_link[1].id};\n"
        nd0 = MTypeInf::TypeInferencer.get_jmp_target(node, 0, inst)
        nd1 = MTypeInf::TypeInferencer.get_jmp_target(node, 1, inst)
        [src, [nd0, nd1]]
      end
    end

    define_ccgen_rule_op :ONERR do |ccgen, inst, node, infer, history, tup|
      ccgen.pcode << "{\n"
      ccgen.pcode << "struct mrb_jmpbuf *oldjmp = mrb->jmp;\n"
      ccgen.pcode << "struct mrb_jmpbuf newjmp;\n"
      ccgen.pcode << "mrb->jmp = &newjmp;\n"
      ccgen.pcode << "if (MRB_SETJMP(mrb->jmp->impl)) {\n"
      ccgen.pcode << "mrb->jmp = oldjmp;\n"
      ccgen.code_gen_node(node.exit_link[1], infer, :onerr, history, tup)
      ccgen.pcode << "}\n"
      nil
    end

    define_ccgen_rule_op :RESCUE do |ccgen, inst, node, infer, history, tup|
      if inst.para[0] == 0 then
        oreg = inst.outreg[0]
        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer, true)};\n"

        ccgen.pcode << "v#{oreg.id} = mrb_obj_value(mrb->exc);\n"
        ccgen.pcode << "mrb->exc = 0;\n"
      else
        oreg = inst.outreg[0]
        if oreg.type[tup].size > 1 then
          ireg0 = inst.inreg[0]
          ireg1 = inst.inreg[1]
          ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer, true)};\n"
          ccgen.pcode << "v#{oreg.id} = mrb_bool_value(mrb_obj_is_kind_of(mrb, v#{ireg0.id}, mrb_class_ptr(v#{ireg1.id})));\n"
        end
      end
      nil
    end

    define_ccgen_rule_op :POPERR do |ccgen, inst, node, infer, history, tup|
      ccgen.pcode << "mrb->jmp = oldjmp;\n"
      ccgen.pcode << "}\n"
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :RAISE do |ccgen, inst, node, infer, history, tup|
      arg, argt = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      arg = gen_type_conversion(ccgen, :mrb_value, argt, arg, tup, node, infer, history, nil)
      ccgen.pcode << "mrb_exc_raise(mrb, mrb_make_exception(mrb, 1, &#{arg}));\n"
      nil
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

      # Skip unnessery boxing
      otype = get_ctype(ccgen, inst.outreg[0], tup, infer)
      itype = get_ctype(ccgen, inst.inreg[0], tup, infer)
      if inst.para[-1] == 0 then
        retval = "self"
      elsif ccgen.callstack[-1][3] or
          otype != :mrb_value or
          itype == :mrb_value then
        retval = reg_real_value(ccgen, inst.inreg[0], node.root.retreg, node, tup, infer, history)
      else
        retval = nil
      end

      aregs = node.root.allocate_reg[tup]
      useheap = nil
      if aregs then
        useheap = aregs.any? {|reg|
          !CodeGen::gen_typesize(ccgen, reg, tup, infer)
        }
      end
      if useheap then
        ccgen.pcode << "mrb_gc_arena_restore(mrb, ai);\n"
      end
      if inst.para[1] != node.root then  # This is not method
        if inst.para[0] == 1 then
          ccgen.have_ret_handler = true
          ccgen.pcode << "prevgctab->ret_status = 1;\n"
        end
        if inst.para[0] == 2 then
          ccgen.have_ret_handler = true
          ccgen.pcode << "prevgctab->ret_status = 2;\n"
        end
      end

      if retval then
        ccgen.pcode << "return #{retval};\n"
      else
        ccgen.pcode << "return mrb_nil_value();\n"
      end
      nil
    end

    define_ccgen_rule_op :BLKPUSH do |ccgen, inst, node, infer, history, tup|
      up = inst.para[1]
      ireg = inst.inreg[0]
      oreg = inst.outreg[0]
      pty = ccgen.callstack[-1][2]
      up.times do
        proc = proc.parent
      end
      dstt = get_ctype(ccgen, oreg, tup, infer)
      srct = get_ctype(ccgen, ireg, tup, infer)

      ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
      if up == 0 then
        val, proct = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
        ccgen.pcode << "v#{oreg.id} = (struct proc#{proct[1]} *)#{val};\n"
      else
        if pty == :mrb_value then
          pos = proc.env.index(ireg)
          val = "(mrb_proc_ptr(mrbproc))->e.env->stack[#{pos + 1}]"
          val = gen_type_conversion(ccgen, pty, dstt, val, tup, node, infer, history, oreg)
          ccgen.pcode << "v#{oreg.id} = #{val};\n"
        else
          val = "proc->env#{"->prev" * up}->v#{ireg.id}"
          val = gen_type_conversion(ccgen, dstt, srct, val, tup, node, infer, history, oreg)
          ccgen.pcode << "v#{oreg.id} = #{val};\n"
        end
      end
      nil
    end

    define_ccgen_rule_op :EQ do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :==)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :LT do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :<)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :LE do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :<=)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :GT do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :>)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :GE do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :>=)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :ADD do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :+)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :SUB do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :-)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :MUL do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :*)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :DIV do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup)  {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :/)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :ADDI do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :+)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :SUBI do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :-)
      }
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :ARRAY do |ccgen, inst, node, infer, history, tup|
      reg = inst.outreg[0]
      if reg.get_type_or_nil(tup) == nil then
        tup = reg.type.keys[0]
      end
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      arytype = reg.get_type(tup)[0]
      eareg = arytype.element
      aescape = reg.is_escape?(tup) #or (!arytype.immidiate_only)
      etype = get_ctype(ccgen, eareg[uv], tup, infer)
      if etype.is_a?(Array) then
        etype = etype[0..1].join(' ')
      end

      if aescape then
        vals = inst.inreg.map {|ireg|
          val, convp = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
          aryt = get_ctype(ccgen, reg, tup, infer)
          dst = get_mutex_dst(convp, ireg.type.values[0], aryt, arytype)
          gen_type_conversion(ccgen, dst, convp, val, tup, node, infer, history, reg)
        }

        ccgen.dcode << "mrb_value v#{reg.id};\n"
        ccgen.pcode << "{\n"
        ccgen.pcode << "mrb_value tmpele[] = {\n"
        ccgen.pcode << vals.join(', ')
        ccgen.pcode << "\n};\n"
        gen_gc_table2(ccgen, node, reg)
        gen_global_lock(ccgen, node)
        ccgen.pcode << "mrb_value array = mrb_ary_new_from_values(mrb, #{vals.size}, tmpele);\n"
        gen_global_unlock(ccgen, node)
        src = "array"
        regt = get_ctype(ccgen, reg, tup, infer)
        src = gen_type_conversion(ccgen, regt, :mrb_value, src, tup, node, infer, history, reg)
        ccgen.pcode << "v#{reg.id} = #{src};\n"
        ccgen.pcode << "ARY_SET_LEN(mrb_ary_ptr(array), #{vals.size});\n"
        ccgen.callstack[-1][1] = true
        ccgen.pcode << "}\n"
      else
        type = get_ctype_aux(ccgen, reg, tup, infer)
        if type == :array then
          i = -1
          ereg = eareg[uv]
          vals = inst.inreg.map {|ireg|
            i = i + 1
            reg_real_value(ccgen, ireg, ereg, node, tup, infer, history)
          }
          valnum = vals.size
          asize = eareg.size
          asize = (asize < valnum) ? valnum : asize

          rc = can_use_caller_area(reg.get_type(tup)[0])
          if rc == 2 or rc == 3 then
            ccgen.dcode << "#{etype} *v#{reg.id};\n"

            if rc == 2 then
              ccgen.pcode << "v#{reg.id} = prevgctab->caller_alloc;\n"
              ccgen.pcode << "prevgctab->caller_alloc += (sizeof(#{etype}) * #{asize + 1});\n"

            elsif rc == 3 then
              ccgen.pcode << "v#{reg.id} = prevgctab->prev->caller_alloc;\n"
              ccgen.pcode << "prevgctab->prev->caller_alloc += (sizeof(#{etype}) * #{asize + 1});\n"
            end

            vals.each_with_index do |src, i|
              ccgen.pcode << "v#{reg.id}[#{i}] = #{src};\n"
            end
          else
            ccgen.pcode << "#{etype} v#{reg.id}[#{asize + 1}] = {\n"
            ccgen.pcode << vals.join(', ')
            ccgen.pcode << "};\n"
          end
          if etype == :mrb_value then
            ccgen.pcode << "v#{reg.id}[#{valnum}].value.ttt = MRB_TT_FREE;\n"
            (asize - valnum).times do |i|
              ccgen.pcode << "v#{reg.id}[#{valnum + i + 1}].value.ttt = MRB_TT_FREE;\n"
            end
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
      posbase = inst.para[0]
      basereg = inst.inreg[0]
      oreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      valreg = inst.inreg[1]
      valtype = valreg.type[tup][0]

      if valtype.is_a?(MTypeInf::ContainerType) then
        ccgen.pcode << "{\n"
        ary, valty = reg_real_value_noconv(ccgen, valreg, node, tup, infer, history)
        if valtype.is_escape? then
          ary = gen_type_conversion(ccgen, :mrb_value, valty, ary, tup,
                         node, infer, history, inst.outreg[0], valreg)
          ccgen.pcode << "mrb_value *array = ARY_PTR2(mrb_ary_ptr(#{ary}));\n"
        else
          valtyp = valty
          if valtyp.is_a?(Array) then
            valtyp[0] = get_ctype_to_c(ccgen, valreg, tup, infer, valtyp[0])
            valtyp = valtyp[0..1].join(' ')
          end
          ccgen.pcode << "#{valtyp} array = #{ary};\n"
        end
        valtype.element.each do |key, reg|
          if key.is_a?(Fixnum) then
            elereg = oreg.type[tup][0].element[key]
            if elereg == nil then
              elereg = oreg.type[tup][0].element[MTypeInf::ContainerType::UNDEF_VALUE]
            end
            eletype = elereg.type[tup][0]
            elety = get_ctype(ccgen, elereg, tup, infer)
            valereg = valtype.element[key]
            valety = get_ctype(ccgen, valereg, tup, infer)
            valsrc = "array[#{key}]"
            valsrc = gen_type_conversion(ccgen, elety, valety, valsrc, tup, node, infer, history, oreg)
            ccgen.pcode << "v#{basereg.id}[#{posbase + key}] = #{valsrc};\n"
          end
        end
        ccgen.pcode << "}\n"
      else
        elereg = oreg.type[tup][0].element[0]
        eletype = elereg.type[tup][0]
        if valreg then
          elety = get_ctype(ccgen, elereg, tup, infer)
          valsrc, valety = reg_real_value_noconv(ccgen, valreg, node, tup, infer, history)
          valsrc = gen_type_conversion(ccgen, elety, valety, valsrc, tup,
                            node, infer, history, oreg, oreg)
        else
          valsrc = "mrb_nil_value()"
        end
        ccgen.pcode << "v#{basereg.id}[#{posbase}] = #{valsrc};\n"
      end
      ccgen.pcode << "v#{oreg.id} = v#{basereg.id};\n"
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :AREF do |ccgen, inst, node, infer, history, tup|
      idx = inst.para[0]
      gen_array_aref(ccgen, inst, node, infer, history, tup, idx)
      nil
    end

    define_ccgen_rule_op :STRING do |ccgen, inst, node, infer, history, tup|
      strlit = unescape_string(inst.para[0])
      oreg = inst.outreg[0]
      if oreg.is_escape?(tup) then
        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
        gen_gc_table_core(ccgen, node, infer, history, tup, inst.para[1], inst.para[2], 0) {|code| ccgen.pcode << code}
        gen_global_lock(ccgen, node)
        ccgen.pcode << "mrb->allocf_ud = (void *)gctab;\n"
        ccgen.pcode << "v#{oreg.id} = mrb_str_new(mrb, #{strlit}, #{inst.para[0].size});\n"
        gen_global_unlock(ccgen, node)
      else
        ccgen.dcode << "char *v#{oreg.id} = #{strlit};\n"
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)

      nil
    end

    define_ccgen_rule_op :STRCAT do |ccgen, inst, node, infer, history, tup|
      ireg0 = inst.inreg[0]
      ireg0.flush_type(tup)
      ireg1 = inst.inreg[1]
      ireg1.flush_type(tup)
      oreg = inst.outreg[0]
      ccgen.dcode << "mrb_value v#{oreg.id};\n"
      val0, val0t = reg_real_value_noconv(ccgen, ireg0, node, tup, infer, history)
      val1, val1t = reg_real_value_noconv(ccgen, ireg1, node, tup, infer, history)
      gen_global_lock(ccgen, node)
      if val0t == :mrb_value then
        if val1t == :mrb_value then
          ccgen.pcode << "mrb->allocf_ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_str_cat_str(mrb, #{val0}, #{val1});\n"
        else
          val1 = gen_type_conversion(ccgen, [:char, "*"], val1t, val1, node, tup, infer, history, nil)
          ccgen.pcode << "mrb->allocf_ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_str_cat_cstr(mrb, #{val0}, #{val1});\n"
        end
      else
        val0 = gen_type_conversion(ccgen, :mrb_value, val0t, val0, node, tup, infer, history, oreg)
        p0var = "v#{oreg.id}"
        ccgen.pcode << "#{p0var} = #{val0};\n"
        if val1t == :mrb_value then
#          val1, dmy = reg_real_value_noconv(ccgen, ireg1, node, tup, infer, history)
          p1var = "v#{ireg1.id}"
          if p1var != val1 then
            ccgen.dcode << "mrb_value #{p1var};\n"
            ccgen.pcode << "#{p1var} = #{val1};\n"
          end
          ccgen.pcode << "mrb->allocf_ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_str_cat_str(mrb, #{p0var}, #{p1var});\n"
        else
          val1 = gen_type_conversion(ccgen, [:char, "*"], val1t, val1, node, tup, infer, history, nil)
          ccgen.pcode << "mrb->allocf_ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_str_cat_cstr(mrb, #{p0var}, #{val1});\n"
        end
      end
      gen_global_unlock(ccgen, node)
      set_closure_env(ccgen, inst, node, infer, history, tup)

      nil
    end

    define_ccgen_rule_op :LAMBDA do |ccgen, inst, node, infer, history, tup|
      # make env struct
      envreg = inst.para[1]
      proc = inst.outreg[0].get_type(tup)[0]
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

        slfdecl = gen_declare_core(ccgen, proc.slfreg, tup, infer, false, "self")
        ccgen.hcode << "#{slfdecl};\n"
        ccgen.hcode << "};\n"

        ccgen.using_proc.push proc
      end

      regno = inst.outreg[0].id
      ccgen.dcode << "struct proc#{proc.id} v#{regno};\n"
      ccgen.pcode << "v#{regno}.id = #{proc.id};\n"
      ccgen.pcode << "v#{regno}.self = self;\n"
      if envreg.size > 0 or pproc then
        ccgen.pcode << "v#{regno}.env = &env;\n"
      end
      dstt = get_ctype(ccgen, inst.outreg[0], tup, infer)
      proc.using_tup.each do |tp, i|
        bfunc = gen_block_func("p#{proc.id}", proc.slf[0].class_object, inst.para[3], tp)
#        ccgen.pcode << "v#{regno}.code[#{i}] = (void *)#{bfunc};\n"
        pproc = ccgen.callstack[-1][0]
        minf = [bfunc, proc, tp, dstt, pproc]
#        @foo = minf.inspect # for gc bug
        ccgen.proctab[proc.irep] ||= []
        ccgen.proctab[proc.irep][i] = minf
      end

      if node.root.is_export_env then
        val = gen_type_conversion(ccgen, dstt, [:gproc, proc.id], "(gproc)&v#{regno}", tup, node, infer, history, inst.outreg[0])
        ccgen.dcode << "mrb_value vv#{regno};\n"
        ccgen.pcode << "vv#{regno} = #{val};\n"
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :HASH do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      hasht = oreg.get_type(tup)[0]
      eareg = hasht.element
      etype = get_ctype(ccgen, eareg[uv], tup, infer)
      if oreg.is_escape?(tup) or true then
        ccgen.dcode << "mrb_value v#{oreg.id};\n"
        ccgen.pcode << "{\n"
        gen_gc_table2(ccgen, node, oreg)
        gen_global_lock(ccgen, node)
        ccgen.pcode << "mrb_value hash = mrb_hash_new(mrb);\n"
        gen_global_unlock(ccgen, node)
        src = "hash"
        regt = get_ctype(ccgen, oreg, tup, infer)
        src = gen_type_conversion(ccgen, regt, :mrb_value, src, tup, node, infer, history, oreg)
        ccgen.pcode << "v#{oreg.id} = #{src};\n"
        ccgen.pcode << "}\n"
      else
        ccgen.dcode << "struct hash_#{etype} *v#{oreg.id};\n"
        ccgen.pcode << "v#{oreg.id} = alloca(sizeof(struct hash_#{etype}));\n"
        ccgen.pcode << "v#{oreg.id}->first = #{bval};\n"
        ccgen.pcode << "v#{oreg.id}->last = #{eval};\n"
        ccgen.pcode << "v#{oreg.id}->exclude_end = #{inst.para[0]};\n"
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)

      nil
    end

    define_ccgen_rule_op :CLASS do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :MODULE do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :EXEC do |ccgen, inst, node, infer, history, tup|
      tclass = inst.inreg[0].flush_type_alltup(tup)[tup]
      root = node.root
      co = tclass[0].val
      irepssa = inst.objcache[co]
      intype = [tclass]
      ntup = infer.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), tup)
      fname = "f#{inst.inreg[0].id}"
      otype = get_ctype(ccgen, inst.outreg[0], tup, infer)

      ccgen.pcode << "#{otype} #{fname}(){\n"
      ccgen.pcode << gen_declare_core(ccgen, inst.inreg[0], tup, infer, false,
"self")
      ccgen.pcode << ";\n"
      ccgen.code_gen_node(irepssa.nodes[0], infer, nil, history, ntup)

      ccgen.pcode << "}\n"
      ccgen.pcode << "#{fname}();\n"
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :METHOD do |ccgen, inst, node, infer, history, tup|
      if ccgen.tmp_attribute.size != 0 then
        rt = inst.inreg[0].type(tup)[0][0].val
        name = inst.para[0]
        ccgen.method_attribute[[name, rt]] = ccgen.tmp_attribute.dup
        ccgen.tmp_attribute.clear
      end
      nil
    end

    define_ccgen_rule_op :TCLASS do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :RANGE do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      et = oreg.get_type(tup)[0]
      ereg = et.element[0]
      bval, bt = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      eval, et = reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history)
      etype = get_ctype(ccgen, ereg, tup, infer)
      if oreg.is_escape?(tup) then
        bval = gen_type_conversion(ccgen, :mrb_value, bt, bval, tup, node, infer, history, oreg)
        eval = gen_type_conversion(ccgen, :mrb_value, et, eval, tup, node, infer, history, oreg)
        # TODO BOXING Range
        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
        gen_global_lock(ccgen, node)
        ccgen.pcode << "v#{oreg.id} = mrb_range_new(mrb, #{bval}, #{eval}, #{inst.para[0]});\n"
        gen_global_unlock(ccgen, node)
      else
        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
        ccgen.pcode << "v#{oreg.id} = alloca(sizeof(struct range_#{etype}));\n"
        ccgen.pcode << "v#{oreg.id}->first = #{bval};\n"
        ccgen.pcode << "v#{oreg.id}->last = #{eval};\n"
        ccgen.pcode << "v#{oreg.id}->exclude_end = #{inst.para[0]};\n"
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)

      nil
    end
  end
end


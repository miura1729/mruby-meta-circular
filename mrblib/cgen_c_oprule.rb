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

    define_ccgen_rule_op :SETGLOBAL do |ccgen, inst, node, infer, history, tup|
      p "FOO"
      p tup
      p infer.typetupletab.rev_table[tup]
      p inst.inreg[0].get_type(tup).map {|ty| ty} if inst.inreg[0].type[tup]
      inst.inreg[0].type.each do |tp, tys|
        p "#{tp} #{tys.map {|ty| ty.place}}"
      end
      nil
    end

    define_ccgen_rule_op :GETCONST do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      val = oreg.type[tup][0].val

      if !(val.is_a?(Fixnum) or val.is_a?(Float) or ccgen.clstab[val]) then
        cno = ccgen.clstab.size
        ccgen.hcode << "#{get_ctype(ccgen, oreg, tup, infer)} const#{cno};\n"
        ccgen.clstab[val] = [inst.para[0], "const#{cno}"]
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :GETMCNST do |ccgen, inst, node, infer, history, tup|
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

      ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
      if pty == :mrb_value then
        pos = proc.env.index(ireg)
        val = "(mrb_proc_ptr(mrbproc))->e.env->stack[#{pos + 1}]"
        val = gen_type_conversion(ccgen, dstt, :mrb_value, val, tup, node, infer, history, oreg)
        ccgen.pcode << "v#{oreg.id} = #{val};\n"
      else
        val = "proc->env#{"->prev" * up}->v#{ireg.id}"
        #val = gen_type_conversion(ccgen, pty, dstt, val, tup, node, infer, history, oreg)
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
        val = reg_real_value2(ccgen, ireg, oreg, node, tup, pttup, infer, history)
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

      if r == 1 then
        if argc - m1 == 1 then
          oreg = inst.outreg[m1]
          # argument num is 1
          ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
          if oreg.is_escape?(tup) then
            ccgen.pcode << "v#{oreg.id} = mrb_ary_new_from_values(mrb, 1, &v#{inst.inreg[m1].id});\n"
          else
            ccgen.pcode << "v#{oreg.id} = &v#{inst.inreg[m1].id};\n"
          end
          inst.para[2][m1] = "v#{oreg.id}"
        else
          # TODO multiple variale argument
        end
      end

      inst.inreg.each_with_index {|ireg, i|
        oreg = inst.outreg[i]

        if node.root.export_regs.include?(oreg) then
          if !oreg.is_a?(RiteSSA::ParmReg) and oreg.refpoint.size == 0 then
            ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
          end

          if ireg.refpoint.size == 0 and
              (!ireg.is_a?(RiteSSA::ParmReg) or ireg.genpoint > argc) then
            ccgen.dcode << "#{gen_declare(ccgen, ireg, tup, infer)};\n"
          end

          src = reg_real_value(ccgen, ireg, oreg,
                         node, tup, infer, history)
          ccgen.pcode << "env.v#{oreg.id} = #{src};/*enter */\n"
        end
      }
      nil
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

      # Skip unnessery boxing
      otype = get_ctype(ccgen, inst.outreg[0], tup, infer)
      itype = get_ctype(ccgen, inst.inreg[0], tup, infer)
      if ccgen.callstack[-1][3] or
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

    define_ccgen_rule_op :EQ do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :==)
      }
      nil
    end

    define_ccgen_rule_op :LT do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :<)
      }
      nil
    end

    define_ccgen_rule_op :LE do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :<=)
      }
      nil
    end

    define_ccgen_rule_op :GT do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :>)
      }
      nil
    end

    define_ccgen_rule_op :GE do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :>=)
      }
      nil
    end

    define_ccgen_rule_op :ADD do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :+)
      }
      nil
    end

    define_ccgen_rule_op :SUB do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :-)
      }
      nil
    end

    define_ccgen_rule_op :MUL do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :*)
      }
      nil
    end

    define_ccgen_rule_op :DIV do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup)  {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :/)
      }
      nil
    end

    define_ccgen_rule_op :ADDI do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :+)
      }
      nil
    end

    define_ccgen_rule_op :SUBI do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :-)
      }
      nil
    end

    define_ccgen_rule_op :ARRAY do |ccgen, inst, node, infer, history, tup|
      reg = inst.outreg[0]
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      aryt = reg.get_type(tup)[0]
      eareg = aryt.element
      aescape = reg.is_escape?(tup) #or (!aryt.immidiate_only)
      etype = get_ctype(ccgen, eareg[uv], tup, infer)
      if etype.is_a?(Array) then
        etype = etype[0..1].join(' ')
      end

      if aescape then
        vals = inst.inreg.map {|ireg|
          val, convp = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
          srct = get_ctype(ccgen, ireg, tup, infer)
          gen_type_conversion(ccgen, :mrb_value, srct, val, tup, node, infer, history, reg)
        }

        ccgen.dcode << "mrb_value v#{reg.id};\n"
        ccgen.pcode << "{\n"
        ccgen.pcode << "mrb_value tmpele[] = {\n"
        ccgen.pcode << vals.join(', ')
        ccgen.pcode << "\n};\n"
        gen_gc_table2(ccgen, node, reg)
        ccgen.pcode << "v#{reg.id} = mrb_ary_new_from_values(mrb, #{vals.size}, tmpele);\n"
        ccgen.pcode << "ARY_SET_LEN(mrb_ary_ptr(v#{reg.id}), #{vals.size});\n"
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

          ccgen.pcode << "#{etype} v#{reg.id}[#{asize + 1}] = {\n"
          ccgen.pcode << vals.join(', ')
          ccgen.pcode << "};\n"
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
    end

    define_ccgen_rule_op :STRING do |ccgen, inst, node, infer, history, tup|
      strlit = unescape_string(inst.para[0])
      oreg = inst.outreg[0]
      if oreg.is_escape?(tup) then
        ccgen.dcode << "mrb_value v#{oreg.id};\n"
        gen_gc_table_core(ccgen, inst, node, infer, history, tup, inst.para[1], inst.para[2], 0)
        ccgen.pcode << "mrb->ud = (void *)gctab;\n"
        ccgen.pcode << "v#{oreg.id} = mrb_str_new(mrb, #{strlit}, #{inst.para[0].size});"
      else
        ccgen.dcode << "char *v#{oreg.id} = #{strlit};\n"
      end
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
      if val0t == :mrb_value then
        if val1t == :mrb_value then
          ccgen.pcode << "mrb->ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_str_cat_str(mrb, #{val0}, #{val1});\n"
        else
          val1 = gen_type_conversion(ccgen, [:char, "*"], val1t, val1, node, tup, infer, history, nil)
          ccgen.pcode << "mrb->ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_str_cat_cstr(mrb, #{val0}, #{val1});\n"
        end
      else
        val0 = gen_type_conversion(ccgen, :mrb_value, val0t, val0, node, tup, infer, history, oreg)
        p0var = "v#{oreg.id}"
        ccgen.pcode << "#{p0var} = #{val0};\n"
        if val1t == :mrb_value then
#          val1, dmy = reg_real_value_noconv(ccgen, ireg1, node, tup, infer, history)
          p1var = "v#{ireg1.id}"
          ccgen.dcode << "mrb_value #{p1var};\n"
          ccgen.pcode << "#{p1var} = #{val1};\n"
          ccgen.pcode << "mrb->ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_str_cat_str(mrb, #{p0var}, #{p1var});\n"
        else
          val1 = gen_type_conversion(ccgen, [:char, "*"], val1t, val1, node, tup, infer, history, nil)
          ccgen.pcode << "mrb->ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_str_cat_cstr(mrb, #{p0var}, #{val1});\n"
        end
      end
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

        slfdecl = gen_declare(ccgen, proc.slfreg, tup, infer)
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
        bfunc = gen_block_func("p#{proc.id}", proc.slf.class_object, inst.para[3], tp)
#        ccgen.pcode << "v#{regno}.code[#{i}] = (void *)#{bfunc};\n"
        pproc = ccgen.callstack[-1][0]
        minf = [bfunc, proc, tp, dstt, pproc]
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
      if oreg.is_escape?(tup) then
        ccgen.dcode << "mrb_value v#{oreg.id};\n"
        gen_gc_table2(ccgen, node, oreg)
        ccgen.pcode << "v#{oreg.id} = mrb_hash_new(mrb);\n"
      else
        ccgen.dcode << "struct hash_#{etype} *v#{oreg.id};\n"
        ccgen.pcode << "v#{oreg.id} = alloca(sizeof(struct hash_#{etype}));\n"
        ccgen.pcode << "v#{oreg.id}->first = #{bval};\n"
        ccgen.pcode << "v#{oreg.id}->last = #{eval};\n"
        ccgen.pcode << "v#{oreg.id}->exclude_end = #{inst.para[0]};\n"
      end
      nil
    end

    define_ccgen_rule_op :CLASS do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :EXEC do |ccgen, inst, node, infer, history, tup|
      tclass = inst.inreg[0].flush_type(tup)[tup]
      root = node.root
      co = tclass[0].val
      irepssa = inst.objcache[co]
      intype = [tclass]
      ntup = infer.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), tup)
      ccgen.code_gen_node(irepssa.nodes[0], infer, nil, history, ntup)
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
      bval = reg_real_value(ccgen, inst.inreg[0], ereg, node, tup, infer, history)
      eval = reg_real_value(ccgen, inst.inreg[1], ereg, node, tup, infer, history)
      etype = get_ctype(ccgen, ereg, tup, infer)
      if oreg.is_escape?(tup) then
        # TODO BOXING Range
        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
        ccgen.pcode << "v#{oreg.id} = mrb_range_new(mrb, #{bval}, #{eval}, #{inst.para[0]});\n"
      else
        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
        ccgen.pcode << "v#{oreg.id} = alloca(sizeof(struct range_#{etype}));\n"
        ccgen.pcode << "v#{oreg.id}->first = #{bval};\n"
        ccgen.pcode << "v#{oreg.id}->last = #{eval};\n"
        ccgen.pcode << "v#{oreg.id}->exclude_end = #{inst.para[0]};\n"
      end
      nil
    end
  end
end


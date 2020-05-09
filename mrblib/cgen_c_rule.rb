module CodeGenC
  class CodeGen
    def self.set_closure_env(ccgen, inst, node, infer, history, tup)
      clsreg = inst.outreg[0]
      proc = ccgen.callstack[-1][0]
      if clsreg.setpoint.size != 0 and inst.op != :LAMBDA then
        src, inty = reg_real_value_noconv(ccgen, clsreg, node, tup, infer, history, true)
        ccgen.dcode << "#{gen_declare(ccgen, clsreg, tup, infer)}; /* fst */\n"
        ccgen.pcode << "v#{clsreg.id} = #{src};\n"
      end

      while clsreg.is_a?(RiteSSA::Reg) do
        if node.root.export_regs.include?(clsreg) then
          src, inty = reg_real_value_noconv(ccgen, inst.outreg[0], node, tup, infer, history)
          outty = get_ctype(ccgen, clsreg, tup, infer)
          if node.root.is_export_env then
            ccgen.pcode << "if (venv)\n"
            src2 = gen_type_conversion(ccgen, :mrb_value, inty, src, tup, node, infer, history, clsreg)
            pos = proc.irep.export_regs.index(clsreg) + 1
            ccgen.pcode << "venv->stack[#{pos}] = #{src2};\n"
          else
            src2 = gen_type_conversion(ccgen, outty, inty, src, tup, node, infer, history, clsreg)
            ccgen.pcode << "env.v#{clsreg.id} = #{src2};/*foo*/\n"
          end
        end
        ins = clsreg.genpoint
        if ins.is_a?(RiteSSA::Inst) then
          clsreg = ins.para[0]
        else
          break
        end
      end
    end

    def self.is_not_assign_emit(outr)
      outr.setpoint.size != 0 or outr.refpoint.size < 3 or
        ((!outr.genpoint.is_a?(RiteSSA::Inst)) or
        [
          :SENDB, :SEND, :ARRAY, :MOVE, :GETIV, :STRCAT
        ].include?(outr.genpoint.op))
    end

    def self.do_if_multi_use(ccgen, inst, node, infer, history, tup)
      outr = inst.outreg[0]
      if !is_not_assign_emit(outr) then
        val, srct = yield
        dstt = get_ctype(ccgen, outr, tup, infer)
        val = gen_type_conversion(ccgen, dstt, srct, val, tup, node, infer, history, outr)
        ccgen.pcode << "v#{outr.id} = #{val};\n"
      end
    end

    def self.do_ifnot_multi_use(ccgen, inst, node, ti, history, tup)
      outr = inst.outreg[0]
      if is_not_assign_emit(outr) then
        yield
      else
        srct = get_ctype(ccgen, outr, tup, ti)
        ["v#{inst.outreg[0].id}", srct]
      end
    end

    def self.gen_gc_table2(ccgen, node, oreg)
      cpsize = ccgen.gcsingle_psize
      if cpsize == 0 then
        ccgen.pcode << "gctab->size = 0;\n"
        ccgen.gcsingle_size = 1 # for making gctab
      end
      ccgen.pcode << "mrb->ud = (void *)gctab;\n"
      if oreg and
          ccgen.is_live_reg?(node, oreg) then
        name = "v#{oreg.id}"
        ccgen.pcode << "#{name} = mrb_nil_value();\n"
        ccgen.pcode << "gctab->single[#{cpsize}] = &#{name}; /* conversion */\n"
        ccgen.pcode << "gctab->size++;\n"
        ccgen.prev_gcsingle[cpsize] = name
        ccgen.gcsingle_psize += 1
        if ccgen.gcsingle_psize > ccgen.gcsingle_size then
          ccgen.gcsingle_size = ccgen.gcsingle_psize
        end
      end
    end

    def self.gen_gc_table(ccgen, inst, node, infer, history, tup)
      regs = inst.para[2]
      pos = inst.para[4]
      num = inst.para[1]
      gen_gc_table_core(ccgen, node, infer, history, tup, regs, pos, num)
    end

    def self.gen_gc_table_core(ccgen, node, infer, history, tup, regs, pos, num)
      tabpos = 0
      prevsize = ccgen.prev_gcsingle.size
      (pos + num).times do |i|
        r = regs[i]
        if r == nil or
            (r.get_type(tup) and r.get_type(tup)[0].is_gcobject? and
            !r.is_escape?(tup)) then

        elsif !(r.get_type(tup) and r.get_type(tup).any? {|ty| ty.is_gcobject?} and
            r.is_escape?(tup)) or
            (r.genpoint.is_a?(Fixnum) and
            !ccgen.is_live_reg?(node, r) and
            i != 0) then

          # Do nothing

        else
          name = reg_real_value_noconv(ccgen, r, node, tup, infer, history)[0]
          if ccgen.prev_gcsingle[tabpos] != name then
            ccgen.pcode << "gctab->single[#{tabpos}] = &#{name};/* normal */\n"
            ccgen.prev_gcsingle[tabpos] = name
          end
          tabpos += 1
        end
      end

      if tabpos != ccgen.gcsingle_psize then
        ccgen.pcode << "gctab->size = #{tabpos};\n"
        ccgen.gcsingle_psize = tabpos
      end

      if tabpos > ccgen.gcsingle_size then
        ccgen.gcsingle_size = tabpos
      end
    end

    def self.op_send(ccgen, inst, node, infer, history, tup)
      name = inst.para[0]
      op_send_aux(ccgen, inst, inst.inreg, inst.outreg, node, infer, history, tup, name)
    end

    def self.op_send_selmet(ccgen, inst, node, infer, history, tup, name, rectype, intype)
      mtab = MTypeInf::TypeInferencer.get_ruby_methodtab
      fname = nil
      utup = nil
      proc = nil
      rectype.class_object.ancestors.each do |rt|
        if @@ruletab[:CCGEN_METHOD][name] and mproc = @@ruletab[:CCGEN_METHOD][name][rt] then
          orgrec = inst.inreg[0].get_type(tup)
          if orgrec.size > 1 then
            rectype2 = rectype.dup
            rectype2.place = {true => true}
            inst.inreg[0].type[tup] = [rectype2]
          end
          if mproc == :writer then
            clsobj = RiteSSA::ClassSSA.get_instance(rectype.class_object)
            ivreg = clsobj.get_iv("@#{name.to_s.chop}".to_sym)
            gen_set_iv(ccgen, inst, node, infer, history, tup, inst.inreg[0], ivreg, inst.inreg[1], inst.outreg[0])

          elsif mproc == :reader then
            clsobj = RiteSSA::ClassSSA.get_instance(rectype.class_object)
            ivreg = clsobj.get_iv("@#{name.to_s}".to_sym)
            gen_get_iv(ccgen, inst, node, infer, history, tup, inst.inreg[0], ivreg, inst.outreg[0])

          else
            mproc.call(ccgen, inst, node, infer, history, tup)
          end
          inst.inreg[0].type[tup] = orgrec
          return [:ccall, 0, nil]
        else
          if mtab[name] and mtab[name][rt] then
            proc = mtab[name][rt]
          else
            next
          end

          utup = infer.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), tup)

          pptup = nil
          if ccgen.compiled_method[proc.irep]
            pptup = ccgen.compiled_method[proc.irep][utup]
          end
          if pptup.is_a?(Fixnum) then
            utup = pptup
          end
          fname = gen_method_func(name, rt, utup)
          break
        end
      end

      [fname, utup, proc]
    end

    def self.op_send_genarg(ccgen, inst, inreg, outreg, node, infer, history, tup, name, utup, fname, proc)
      regs =  proc.irep.allocate_reg[utup]
      if regs
#        regs = regs.uniq
        rets = regs.inject([]) {|res, reg|
          rsize = gen_typesize(ccgen, reg, utup, infer)
          if rsize then
            res << rsize
          end
          res
        }
        if rets.size > 0 then
          ccgen.caller_alloc_size += 1
          ccgen.pcode << "gctab->caller_alloc = alloca(#{rets.join(' + ')});\n"
        end
      end
      procexport = false
      i = 0
      topnode = node.root.nodes[0]
      args = inreg[0..-2].map {|reg|
        rs, srct = reg_real_value_noconv(ccgen, reg, node, tup, infer, history)
        if srct.is_a?(Array) and srct[0] == :gproc then
          procexport = true
        end
        dstt = get_ctype(ccgen, reg, tup, infer)
        i = i + 1
        gen_type_conversion(ccgen, dstt, srct, rs, tup, node, infer, history, nil)
      }.join(", ")

      reg = inreg[-1]
      tys = reg.get_type(tup)
      if tys and (tys.size == 1 and tys[0].class_object != NilClass) then
        args << ", "
        rs, srct = reg_real_value_noconv(ccgen, reg, node, tup, infer, history)
        if srct.is_a?(Array) and srct[0] == :gproc then
          procexport = true
        end
        dstt = get_ctype(ccgen, reg, tup, infer)
        args << gen_type_conversion(ccgen, dstt, srct, rs, tup, node, infer, history, nil)
      end

      args << ", gctab"
      [args, procexport]
    end

    def self.op_send_aux(ccgen, inst, inreg, outreg, node, infer, history, tup, name)
      MTypeInf::TypeInferencer::make_intype(infer, inreg, node, tup, inst.para[1]) do |intype, argc|
        #      intype[0] = [intype[0][0]]
        if !intype[0][0]
          p name
          p tup
          p inreg[0].type
          p intype
          p inst.line
        end
        rectypes = intype[0]
        if rectypes.size == 1 then
          # Not polymorphism
          rectype = rectypes[0]
          fname, utup, proc = op_send_selmet(ccgen, inst, node, infer, history, tup, name, rectype, intype)

        elsif rectypes.size == 2 and
            (rectypes[0].class_object == NilClass or rectypes[1].class_object == NilClass) then
          # nilable
          gen_gc_table(ccgen, inst, node, infer, history, tup)
          if rectypes[0].class_object == NilClass then
            ccgen.pcode << "if (mrb_nil_p(v#{inreg[0].id})) {\n"
          else
            ccgen.pcode << "if (!mrb_nil_p(v#{inreg[0].id})) {\n"
          end

          rectype = rectypes[0]
          fname, utup, proc = op_send_selmet(ccgen, inst, node, infer, history, tup, name, rectype, intype)

          if fname == :ccall and proc.nil? then

          elsif fname then
            args, procexport = op_send_genarg(ccgen, inst, inreg, outreg, node, infer, history, tup, name, utup, fname, proc)

            if outreg then
              nreg = outreg[0]
              ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
              ccgen.dcode << ";\n"
              ccgen.pcode << "v#{nreg.id} = #{fname}(mrb, #{args});\n"
            else
              ccgen.pcode << "#{fname}(mrb, #{args});\n"
            end

          elsif name != :initialize
            p inst.filename
            p inst.line
            p name
            p intype[0][0]
            ccgen.pcode << "mrb_no_method_error1(mrb, mrb_intern_lit(mrb, \"#{name}\"), mrb_nil_value(), \"undefined method #{name}\");\n"
          end

          ccgen.pcode << "}\n"
          ccgen.pcode << "else {\n"

          rectype = rectypes[1]
          fname, utup, proc = op_send_selmet(ccgen, inst, node, infer, history, tup, name, rectype, intype)

          if fname == :ccall and proc.nil? then

          elsif fname then
            args, procexport = op_send_genarg(ccgen, inst, inreg, outreg, node, infer, history, tup, name, utup, fname, proc)

            if outreg then
              nreg = outreg[0]
              ccgen.pcode << "v#{nreg.id} = #{fname}(mrb, #{args});\n"
            else
              ccgen.pcode << "#{fname}(mrb, #{args});\n"
            end

          elsif name != :initialize
            p inst.filename
            p inst.line
            p name
            p intype[0][1]
            ccgen.pcode << "mrb_no_method_error2(mrb, mrb_intern_lit(mrb, \"#{name}\"), mrb_nil_value(), \"undefined method #{name}\");\n"
          end
          ccgen.pcode << "}\n"
          return

        else
          # polymorphism
        end

        if fname == :ccall and proc.nil? then
          return

        elsif fname then
          args, procexport = op_send_genarg(ccgen, inst, inreg, outreg, node, infer, history, tup, name, utup, fname, proc)

          gen_gc_table(ccgen, inst, node, infer, history, tup)

          if outreg then
            nreg = outreg[0]
            ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
            ccgen.dcode << ";\n"
            ccgen.pcode << "v#{nreg.id} = #{fname}(mrb, #{args});\n"
          else
            ccgen.pcode << "#{fname}(mrb, #{args});\n"
          end

          if proc.irep.have_return then
            ccgen.have_ret_handler = true
            val = reg_real_value(ccgen, nreg, node.root.retreg2, node, tup, infer, history)
            ccgen.pcode << "if (gctab->ret_status) return #{val};\n"
          end

          if procexport then
            node.root.import_regs.each do |reg|
              if ccgen.is_live_reg_local?(node, reg) then
                ccgen.pcode << "v#{reg.id} = env.v#{reg.id};\n"
              end
            end
          end

          pproc = ccgen.callstack[-1][0]

          usereturn = true
          if outreg then
            if inst.op == :SENDB
              usereturn = (outreg[0].refpoint.size != 0)
            end
          else
            # initialize method
            usereturn = false
          end

          minf = [fname, proc, utup, pproc, name, usereturn]
          if ccgen.using_method.index(minf) == nil then
            ccgen.using_method.push minf
          end

        elsif name != :initialize then
          p inst.filename
          p inst.line
          p name
          p intype[0][0]
          ccgen.pcode << "mrb_no_method_error(mrb, mrb_intern_lit(mrb, \"#{name}\"), mrb_nil_value(), \"undefined method #{name}\");\n"
        end
      end
      nil
    end

    def self.op_send_initialize(ccgen, inst, inreg, outreg, node, infer, history, tup, name)
      intype = inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      intype[0] = [intype[0][0]]
      rectype = intype[0][0].class_object
      mtab = MTypeInf::TypeInferencer.get_ruby_methodtab
      rectype.ancestors.each do |rt|
        if @@ruletab[:CCGEN_METHOD][name] and mproc = @@ruletab[:CCGEN_METHOD][name][rt] then
          return mproc.call(ccgen, inst, node, infer, history, tup)
        else
          if mtab[name] and mtab[name][rt] then
            proc = mtab[name][rt]
            block = proc.irep
            node = block.nodes[0]
          else
            next
          end

          utup = infer.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), tup)

          procexport = false
          topnode = node.root.nodes[0]
          inreg.each_with_index do |reg, i|
            rs, srct = reg_real_value_noconv(ccgen, reg, node, tup, infer, history)
            if srct.is_a?(Array) and srct[0] == :gproc then
              procexport = true
            end
            dstt = get_ctype(ccgen, reg, tup, infer)
            src = gen_type_conversion(ccgen, dstt, srct, rs, tup, node, infer, history, dreg)
            dreg = node.enter_reg[i]
            ccgen.dcode << gen_declare(ccgen, dreg, utup, infer)
            ccgen.dcode << ";\n"
            ccgen.pcode << "v#{dreg.id} = #{src};\n"
          end
          ccgen.code_gen_node(node, infer, :initialize, {}, utup)
          if procexport then
            node.root.import_regs.each do |reg|
              if ccgen.is_live_reg_local?(node, reg) then
                ccgen.pcode << "v#{reg.id} = env.v#{reg.id};\n"
              end
            end
          end

          return
        end
      end
      nil
    end

    def self.gen_term_top(ccgen, gins, node, tup, ti, history, reg0, reg1, op)
      src, srct, needdecl = gen_term(ccgen, gins, node, tup, ti, history, reg0, reg1, op)
      if needdecl then
        outr = gins.outreg[0]
        ccgen.dcode << "#{gen_declare(ccgen, outr, tup, ti)};/*snd*/\n"
      end
      [src, srct]
    end

    def self.gen_term(ccgen, gins, node, tup, ti, history, reg0, reg1, op)
      valuep = 0
      needdecl = true
      if reg0.is_a?(RiteSSA::Reg) then
        reg0.flush_type(tup)
        reg0.rearrange_type(tup)
        arg0, srcs0 = reg_real_value_noconv(ccgen, reg0, node, tup, ti, history)
        if reg0.get_type(tup) then
          case reg0.get_type(tup).size
          when 1
            srcd0 = get_ctype(ccgen, reg0, tup, ti, false)
            if reg0.get_type(tup)[0].is_a?(MTypeInf::LiteralType) and false then
              srcs0 = srcd0
              arg0 = reg0.get_type(tup)[0].val
              valuep |= 1
            end
          else
            srcs0 = :mrb_value
            srcd0 = :mrb_value
          end
        else
          srcs0 = :mrb_value
          srcd0 = :mrb_value
        end
      else
        valuep |= 1
        if !reg0.is_a?(RiteSSA::Reg) then
          arg0 = reg0
        else
          arg0 = "v#{reg0.id}"
""
        end
        srcd0 = get_ctype(ccgen, gins.inreg[0], tup, ti, false)
        srcs0 = srcd0
      end

      if reg1.is_a?(RiteSSA::Reg) then
        reg1.flush_type(tup)
        reg1.rearrange_type(tup)
        arg1, srcs1 = reg_real_value_noconv(ccgen, reg1, node, tup, ti, history)
        if reg1.get_type(tup) then
          case reg1.get_type(tup).size
          when 1
            srcd1 = get_ctype(ccgen, reg1, tup, ti, false)
            if reg1.get_type(tup)[0].is_a?(MTypeInf::LiteralType) and false then
              srcs1 = srcd1
              arg1 = reg1.get_type(tup)[0].val
              valuep |= 2
            end
          else
            srcs1 = :mrb_value
            srcd1 = :mrb_value
          end
        else
          srcs1 = :mrb_value
          srcd1 = :mrb_value
        end
      else
        valuep |= 2
        if !reg1.is_a?(RiteSSA::Reg) then
          arg1 = reg1
        else
          arg1 = "v#{reg1.id}"
        end
        srcd1 = get_ctype(ccgen, gins.inreg[0], tup, ti, false)
        srcs1 = srcd1
      end
      outr = gins.outreg[0]
      if outr.refpoint[0].is_a?(RiteSSA::Inst) and
          [:ADD, :SUB, :MUL, :DIV, :EQ, :GT, :GE, :LT, :LE, :ADDI, :SUBI].include?(outr.refpoint[0].op) then
        dstd = get_ctype(ccgen, gins.outreg[0], tup, ti, false)
      else
        dstd = get_ctype(ccgen, gins.outreg[0], tup, ti)
      end
      src = ""

      if [:+, :-, :*, :/, :<<, :>>, :&, :|].include?(op) then
        if (srcd0 == :mrb_int or srcd0 == :mrb_float2) and
            (srcd1 == :mrb_int or srcd1 == :mrb_float2) then
          if (srcd0 == :mrb_float2 or srcd1 == :mrb_float2) then
            dsts = :mrb_float2
            if valuep == 3 then
              #          [eval("(#{arg0} #{op} #{arg1})"), srcd0]
              src = "(#{arg0} #{op} #{arg1})"
            else
              term0 = gen_type_conversion(ccgen, :mrb_float2, srcs0, arg0, tup, node, ti, history, nil)
              term1 = gen_type_conversion(ccgen, :mrb_float2, srcs1, arg1, tup, node, ti, history, nil)
              src = "(#{term0} #{op} #{term1})"
            end
          else
            dsts = :mrb_int
            if valuep == 3 then
              #          [eval("(#{arg0} #{op} #{arg1})"), srcd0]
              src = "(#{arg0} #{op} #{arg1})"
            else
              term0 = gen_type_conversion(ccgen, :mrb_int, srcs0, arg0, tup, node, ti, history, nil)
              term1 = gen_type_conversion(ccgen, :mrb_int, srcs1, arg1, tup, node, ti, history, nil)
              src = "(#{term0} #{op} #{term1})"
            end
          end
        else
          #p reg0.get_type(tup)
          op_send(ccgen, gins, node, ti, history, tup)
          needdecl = false
          src = "v#{gins.outreg[0].id}"
        end
      elsif [:>, :>=, :<, :<=].include?(op) then
        if (srcd0 == :mrb_int or srcd0 == :mrb_float2) and
            (srcd1 == :mrb_int or srcd1 == :mrb_float2) then
          dsts = :mrb_bool
          if (srcd0 == :mrb_float2 or srcd1 == :mrb_float2) then
            if valuep == 3 then
              #          [eval("(#{arg0} #{op} #{arg1})"), srcd0]
              src = "(#{arg0} #{op} #{arg1})"
            else
              term0 = gen_type_conversion(ccgen, :mrb_float2, srcs0, arg0, tup, node, ti, history, nil)
              term1 = gen_type_conversion(ccgen, :mrb_float2, srcs1, arg1, tup, node, ti, history, nil)
              src = "(#{term0} #{op} #{term1})"
            end
          else
            if valuep == 3 then
              src = eval("(#{arg0} #{op} #{arg1})")
            else
              term0 = gen_type_conversion(ccgen, :mrb_int, srcs0, arg0, tup, node, ti, history, nil)
              term1 = gen_type_conversion(ccgen, :mrb_int, srcs1, arg1, tup, node, ti, history, nil)
              src = "(#{term0} #{op} #{term1})"
            end
          end
        else
          #p reg0.get_type(tup)
          op_send(ccgen, gins, node, ti, history, tup)
          needdecl = false
          src = "v#{gins.outreg[0].id}"
        end

      elsif op == :== then
        dsts = :mrb_bool
        if srcd0 == :mrb_float2 or srcd0 == :mrb_int then
          if valuep == 3 then
            src = eval("(#{arg0} #{op} #{arg1})")
          else
            arg0 = gen_type_conversion(ccgen, srcs1, srcs0, arg0, tup, node, ti, history, nil)
            src = "(#{arg0} #{op} #{arg1})"
          end
        else
          op_send_aux(ccgen, gins, gins.inreg, gins.outreg, node, ti, history, tup, :==)
          needdecl = false
          src = "v#{gins.outreg[0].id}"
        end
      else
        raise "No suche opcode #{op}"
      end

      src = gen_type_conversion(ccgen, dstd, dsts, src, tup, node, ti, history, gins.outreg[0])
      [src, dstd, needdecl]
    end

    def self.reg_real_value(ccgen, ireg, oreg, node, tup, ti, history, escheck = true)
      val, srct = reg_real_value_noconv(ccgen, ireg, node, tup, ti, history)
      dstt = get_ctype(ccgen, oreg, tup, ti, escheck)
      a = gen_type_conversion(ccgen, dstt, srct, val, tup, node, ti, history, oreg)
      a
    end

    def self.reg_real_value2(ccgen, ireg, oreg, node, tup, ptup, ti, history, escheck = true)
      val, srct = reg_real_value_noconv(ccgen, ireg, node, tup, ti, history)
      dstt = get_ctype(ccgen, oreg, ptup, ti, escheck)
      gen_type_conversion(ccgen, dstt, srct, val, ptup, node, ti, history, oreg)
    end

    def self.get_ctype_from_robj(src)
      srct = :mrb_value
      if src.is_a?(Fixnum) then
        srct = :mrb_int
      elsif src.is_a?(Float) then
        srct = :mrb_float2
      elsif src.is_a?(String) then
        srct = [:char , "*", nil]
        src = unescape_string(src)
      elsif src.is_a?(NilClass) then
        srct = :mrb_value
        src = "mrb_nil_value()"
      else
        srct = :mrb_value
        src = "mrb_nil_value()"
#        raise "Not support #{src.inspect}"
      end

      [src, srct]
    end

    def self.unescape_string(str)
      str.dump
    end

    def self.reg_real_value_noconv(ccgen, reg, node, tup, ti, history, fstp = false)
      srct = get_ctype(ccgen, reg, tup, ti)
      if reg.setpoint.size != 0 and !fstp then
        return ["v#{reg.id}", srct]
      end

      if reg.is_a?(RiteSSA::ParmReg) then
        if node.enter_link.size == 1 then
          pnode = node.enter_link[0]
          preg = pnode.exit_reg[reg.genpoint]
          return reg_real_value_noconv(ccgen, preg, pnode, tup, ti, history)
        end

        if node.enter_link.size == 0 then # TOP of block
          ptype = ti.typetupletab.rev_table[tup][reg.genpoint]
          if ptype.is_a?(Array) and ptype.size == 1 and
              !ptype[0].is_escape? and
              ptype[0].class == MTypeInf::LiteralType then
            src = ptype[0].val
            if src == true then
              return [1, :mrb_bool]
            elsif src == false then
              return [0, :mrb_bool]
            else
              return get_ctype_from_robj(src)
            end
          end
        end

        if reg.genpoint == 0 then
          return ["self", srct]
        else
          return ["v#{reg.id}", srct]
        end
      end

      gins = reg.genpoint
      if !gins then
        return ["mrb_nil_value()", :mrb_value]
      end
      case gins.op
      when :MOVE
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          reg_real_value_noconv(ccgen, gins.inreg[0], node, tup, ti, history)
        }

      when :LOADL, :LOADI
        if node.root.export_regs.include?(reg) and false then
          ["v#{reg.id}", srct]
        else
          src = gins.para[0]
          get_ctype_from_robj(src)
        end

      when :LOADT
        [1, :mrb_bool]

      when :LOADF
        [0, :mrb_bool]

      when :LOADSYM
        ["mrb_intern_lit(mrb, \"#{gins.para[0]}\")", "mrb_sym"]

      when :LOADNIL
        ["mrb_nil_value()", :mrb_value]

      when :LOADSELF
        ["self", srct]

      when :GETCONST
        val = gins.outreg[0].get_type(tup)[0].val
        vid =  ccgen.clstab[val]
        if vid then
          [vid[1], srct]
        else
          [val, srct]
        end

      when :ENTER
        i = gins.outreg.index(reg)
        src = gins.para[2][i]
        if src then
          [src, srct]
        else
          [reg_real_value(ccgen, gins.inreg[i], gins.outreg[i], node, tup, ti, history), srct]
        end

      when :EQ
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :==)
        }

      when :SEND
        case gins.para[0]
        when :&, :|, :<<, :>>
          do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
            gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], gins.para[0])
          }

        else
          ["v#{reg.id}", srct]
        end

      when :LT
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :<)
        }

      when :LE
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :<=)
        }

      when :GT
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :>)
        }

      when :GE
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :>=)
        }

      when :ADD
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :+)
        }

      when :SUB
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :-)
        }

      when :MUL
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :*)
        }

      when :DIV
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :/)
        }

      when :ADDI
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.para[1], :+)
        }

      when :SUBI
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.para[1], :-)
        }

      when :LAMBDA
        #envreg = gins.para[1]
        #envreg.each do |reg|
        #  val = reg_real_value(ccgen, reg, reg, node, tup, ti, history)
        #  res += "(v#{gins.outreg[0].id}.env->v#{reg.id} = #{val}),"
        #end
        if srct == :mrb_value then
          ["vv#{reg.id}", :mrb_value]
        else
          res = "((gproc)&v#{reg.id})"
          [res, [:gproc, reg.get_type(tup)[0].id]]
        end

      when :STRING
        oreg = gins.outreg[0]
        if oreg.is_escape?(tup) then
          ["v#{reg.id}", :mrb_value]
        else
          strlit = unescape_string(gins.para[0])
          [strlit, [:char, "*", gins.para[0]]]
        end

      else
        ["v#{reg.id}", srct]
      end
    end

    def self.gen_name_marshal(name)
      name = name.to_s
      name.gsub!("#", "_s")
      name.gsub!("<", "_l")
      name.gsub!(">", "_g")
      name.gsub!(":", "_c")
      name.gsub!("_", "_u")
      name.gsub!("=", "_e")
      name.gsub!("!", "_E")
      name.gsub!("@", "_a")
      name.gsub!("$", "_d")
      name.gsub!("%", "_p")
      name.gsub!("+", "_P")
      name.gsub!("-", "_m")
      name.gsub!("*", "_A")
      name.gsub!("?", "_q")
      name
    end

    def self.gen_method_func(name, rectype, tup)
      name = gen_name_marshal(name)
      rect = gen_name_marshal(rectype.inspect)
      "#{name}__#{rect}__#{tup}"
    end

    def self.gen_block_func(name, rectype, blkno, tup)
      name = gen_name_marshal(name)
      "#{name}_#{rectype}_#{blkno}_#{tup}"
    end

    TTABLE = {
      Fixnum => :mrb_int,
      Float => :mrb_float2,
      Array => :array,
      Range => :range,
      Proc => :gproc,
      NilClass => :mrb_value,
      String => :string,
      Symbol => :symbol
    }

    def self.get_ctype_aux_aux(ccgen, reg, tup, infer)
      rtype = reg.get_type(tup)
      if !rtype then
#        p caller
#        p "#{tup} #{infer.typetupletab.rev_table[tup]}"
#        p reg
#        p reg.genpoint
#        reg.type.keys.uniq.each {|tp|
#          p "  #{tp} #{infer.typetupletab.rev_table[tp]}"
#        }
        # for element of array
        rtype = reg.type[reg.type.keys[0]]
        if rtype.nil? then
          return :mrb_value
        end
      end

      if rtype.any? {|r| r.is_escape?} then
        return :mrb_value
      end

      rtypesize = rtype.size
      cls0 = rtype[0].class_object

      # not escape and pointer type is nullable.
      if rtypesize == 2 then
        if cls0 == NilClass then
          cls0 = rtype[1].class_object
        end
        rtypesize = 1
        res = TTABLE[cls0]
        if res and cls0 != Array and cls0 != String then
          return :mrb_value
        end
      end

      if rtype.all? {|e| e.class_object == cls0} then
        res = TTABLE[cls0]
        rtypesize = 1
        if res then
          return res
        end
      end

      if rtype.all? {|e|
          cls = e.class_object
          cls == TrueClass || cls == FalseClass
        } and rtype.size > 0 then
        return :mrb_bool
      end

      nilobj = MTypeInf::PrimitiveType.new(NilClass)
      if rtypesize == 1 then
        clsssa =  RiteSSA::ClassSSA.get_instance(cls0)
        ccgen.using_class[clsssa] ||= {}
        ivtypes = [nilobj]
        clsssa.iv.each do |nm, reg|
          val = reg.flush_type(tup)[tup]
          if !val then
            val = reg.flush_type_alltup(tup)[tup]
          end
          ivtypes.push val
        end
        ivtup = infer.typetupletab.get_tupple_id(ivtypes, nilobj, tup, true)
        if !ccgen.using_class[clsssa][ivtup] then
          clsssa.iv.each do |nm, reg|
            if reg.get_type(tup) then
              reg.type[ivtup] = reg.get_type(tup).dup
            end
          end
          ccgen.using_class[clsssa][ivtup] = ["cls#{clsssa.id}_#{ivtup}", rtype[0].hometown]
        end
        if clsssa and clsssa.id != 0 then
          ["struct cls#{clsssa.id}_#{ivtup} ", "*"]
        else
          :mrb_value
        end
      else
        rtype.each do |ety|
          clsssa =  RiteSSA::ClassSSA.get_instance(ety.class_object)
          ivtypes = [nilobj]
          clsssa.iv.each do |nm, reg|
            ivtypes.push reg.flush_type(tup)[tup]
          end
          ivtup = infer.typetupletab.get_tupple_id(ivtypes, nilobj, tup, true)
          rtype.each do |e|
            cls = e.class_object
            clsssa =  RiteSSA::ClassSSA.get_instance(cls)
            ccgen.using_class[clsssa] ||= {}
            ccgen.using_class[clsssa][ivtup] ||= ["cls#{clsssa.id}_#{ivtup}", ety.hometown]
          end
        end
        :mrb_value
      end
    end

    def self.get_ctype_aux(ccgen, reg, tup, infer)
      type = get_ctype_aux_aux(ccgen, reg, tup, infer)
      if type.is_a?(Array) then
        type.join
      else
        type
      end
    end

    def self.get_ctype(ccgen, reg, tup, infer, strobj = true)
      type = get_ctype_aux(ccgen, reg, tup, infer)
      case type
      when :array
        if strobj and !reg.is_escape?(tup) then
          tys = reg.get_type(tup)
          if !tys then
            tys = reg.type[reg.type.keys[0]]
            if !tys then
              return :mrb_value
            end
          end
          uv = MTypeInf::ContainerType::UNDEF_VALUE
          ereg = tys[0].element[uv]
          size = tys[0].element.size
          rc = nil
          etup = tup
          if ereg.type[etup] == nil then
            etup = ereg.type.keys[0]
          end

          rc = get_ctype_aux(ccgen, ereg, etup, infer)
          if rc == :array or rc == :mrb_value then
            [:mrb_value, "*", size]
          elsif rc == :string
            [:char, "**", size]
          else
            [rc, "*", size]
          end
        else
          :mrb_value
        end

      when :string
        if strobj and !reg.is_escape?(tup) then
          [:char , "*", nil]
        else
          :mrb_value
        end

      when :symbol
        "mrb_sym"

      when :range
        ereg = reg.get_type(tup)[0].element[0]
        rc = get_ctype_aux(ccgen, ereg, tup, infer)
        tname = "struct range_#{rc}"
        if rc == :array then
          :mrb_value
        else
          [tname , "*"]
          #rc
        end

      when :gproc
        [:gproc, reg.get_type(tup)[0].id]

      else
#        if reg.get_type(tup)
#          p reg.get_type(tup)[0].class_object
#        else
#          p "Unnown #{type}"
#        end
        type
      end
    end


    def self.gen_declare_core(ccgen, reg, tup, infer, initp, regnm)
      type = get_ctype_aux(ccgen, reg, tup, infer)
      case type
      when :array
        uv = MTypeInf::ContainerType::UNDEF_VALUE
        ereg = reg.get_type(tup)[0].element[uv]
        etup = tup
        if ereg.get_type(tup) == nil then
          etup = ereg.type.keys[0]
        end
        etype = get_ctype(ccgen, ereg, etup, infer)
        if etype.is_a?(Array) then
          etype = etype[0..1].join(' ')
        end
        "#{etype} *#{regnm}"

      when :range
        ereg = reg.get_type(tup)[0].element[0]
        etup = tup
        if ereg.get_type(tup) == nil then
          etup = ereg.type.keys[0]
        end
        etype = get_ctype_aux(ccgen, ereg, etup, infer)
        typestr = "struct range_#{etype}"
        if !ccgen.range_types.include?(etype) then
          ccgen.range_types.push etype
          ccgen.scode << <<"EOS"
#{typestr} {
  #{etype} first;
  #{etype} last;
  mrb_bool exclude_end;
};
EOS
        end

        "#{typestr} *#{regnm}"

      when :nil
        "mrb_value #{regnm} = mrb_nil_value()"

      when :string
        "char *#{regnm}"

      when :symbol
        "mrb_sym #{regnm}"

      else
        if type == :mrb_value and initp then
            "mrb_value #{regnm} = mrb_nil_value()"
        else
            "#{type} #{regnm}"
        end
      end
    end

    def self.gen_declare(ccgen, reg, tup, infer, initp = false)
      if reg.is_a?(RiteSSA::ParmReg) and reg.genpoint == 0 then
        regnm = "self"
      else
        regnm = "v#{reg.id}"
      end
      gen_declare_core(ccgen, reg, tup, infer, initp, regnm)
    end

    def self.can_use_caller_area(otype)
      rc = otype.place.keys.any? {|e|
        e.is_a?(MTypeInf::UserDefinedType) or
        e.is_a?(MTypeInf::ContainerType) or
        e == :return_fst
      }
      if rc then
        rc2 = nil
        if otype.place.keys.all? {|e1|
            if (e1.is_a?(MTypeInf::UserDefinedType) or
                e1.is_a?(MTypeInf::ContainerType)) and
                otype.level == e1.level and rc2 == nil then
              rc2 = can_use_caller_area(e1)
            end
            e1 != :return_fst and
            (!(e1.is_a?(MTypeInf::UserDefinedType) or
              e1.is_a?(MTypeInf::ContainerType)) or
            otype.level <= e1.level)
          } then

          # same level store object and store object is caller alloc
          return rc2
        end

        if otype.place.keys.all? {|e1|
            !(e1.is_a?(MTypeInf::UserDefinedType) or
            e1.is_a?(MTypeInf::ContainerType)) or
            otype.level <= e1.level + 1
          } then
          # 1 level caller alloc
          return 2
        end

        if otype.place.keys.all? {|e1|
            !(e1.is_a?(MTypeInf::UserDefinedType) or
            e1.is_a?(MTypeInf::ContainerType)) or
            otype.level <= e1.level + 2
          } then
          # 2 level caller alloc
          return 3
        end
      end
      return nil
    end

    def self.gen_typesize(ccgen, reg, tup, infer)
      otype = reg.get_type(tup)[0]
      if can_use_caller_area(otype) then

        type = get_ctype_aux_aux(ccgen, reg, tup, infer)

        case type
        when :array
          uv = MTypeInf::ContainerType::UNDEF_VALUE
          eele = reg.get_type(tup)[0].element
          ereg = eele[uv]
          etup = tup
          if ereg.get_type(tup) == nil then
            etup = ereg.type.keys[0]
          end
          etype = get_ctype_aux(ccgen, ereg, etup, infer)
          if etype != :rvalue then
            return "(sizeof(#{etype}) * #{eele.size + 1})"
          else
            return nil
          end

        when :nil
          return nil

        when :mrb_value
          return nil

        when :string
          if otype.size then
            return "sizeof(char) * #{((otype.size / 4).to_i + 1) * 4}"
          end

        when :range
          ereg = otype.element[0]
          etup = tup
          if ereg.get_type(tup) == nil then
            etup = ereg.type.keys[0]
          end
          etype = get_ctype_aux(ccgen, ereg, etup, infer)
          return "sizeof(struct range_#{etype})"

        else
          if type.is_a?(Array) then
            type = type[0]
          end
          return "(sizeof(#{type}))"
        end
      end
      nil
    end

    def self.gen_type_conversion(ccgen, dstt, srct, src, tup, node, ti, history, oreg)
      if dstt == srct then
        return src
      end

      if dstt.is_a?(Array) and srct.is_a?(Array) and
          dstt[0] == srct[0] and dstt[0] == :char then
        return src
      end

      if !srct then
        return src
      end

      if dstt.is_a?(Array) then
        case dstt[0]
        when :gproc
          p MTypeInf::ProcType.gettab[dstt[1]]
          p "proc"
          p caller
          p srct
          raise "Not support yet #{dstt} #{srct}"

        when :char
          case srct
          when :mrb_int
            "({char *_buf = alloca(255);sprintf(_buf, \"%d\", #{src});_buf;})"

          when :mrb_float2
            "({char *_buf = alloca(255);sprintf(_buf, \"%f\", #{src});_buf;})"

          when :mrb_value
            "RSTRING_PTR(#{src})"

          else
            raise "Not support yet #{dstt} #{srct}"
          end

        when :mrb_value
          case srct
          when :mrb_value
            "RARRAY_PTR(#{src})"

          else
            raise "Not support yet #{dstt} #{srct}"
          end

        else
          p src
          p dstt
          p srct
          raise "Not support yet #{dstt} #{srct}"
        end

      else
        case dstt
        when :mrb_value
          if srct.is_a?(Array) then
            case srct[0]
            when :gproc
              proc = MTypeInf::ProcType.gettab[srct[1]]
              nval = proc.env.size + 1

              res =  "({\n mrb_value tmpval[#{nval}];\n"
              res << "struct RProc *tproc;\n"

              if node.root.repsreg.size > 1 then
                res << "if (venv) {\n"
                res << "tproc = mrb_proc_new_cfunc(mrb, ((struct proc#{proc.id} *)#{src})->code);\n"
                res << "tproc->flags |= MRB_PROC_ENVSET;\n"
                res << "tproc->e.env = venv;\n"
                res << "}\n"
                res << "else "
                ccgen.callstack[-1][1] = true
              end

              res << "{\n"
              val = "((struct proc#{proc.id} *)(#{src}))->self"
              slfty = get_ctype(ccgen, proc.slfreg, tup, ti)
              val = gen_type_conversion(ccgen, :mrb_value, slfty, val, tup, node, ti, history, )
              res << "tmpval[0] = #{val};\n"
              proc.env.each_with_index do |srcreg, i|
                val = reg_real_value_noconv(ccgen, srcreg, node, tup, ti, history)[0]
                stype = get_ctype(ccgen, srcreg, tup, ti)
                val = gen_type_conversion(ccgen, :mrb_value, stype, val, tup, node, ti, history, nil)
                res << "tmpval[#{i + 1}] = #{val};\n"
              end
              res << "tproc = mrb_proc_new_cfunc_with_env(mrb, ((struct proc#{proc.id} *)#{src})->code, #{nval}, tmpval);\n"
              res << "venv = tproc->e.env;\n"

              res << "}\n"

              res << "mrb_obj_value(tproc);\n"
              res <<  "})"
              ccgen.callstack[-1][1] = true
              res

            when :char
              gen_gc_table2(ccgen, node, oreg)
              if srct[1] == "*" then
               "(mrb_str_new_cstr(mrb, #{src}))"

              elsif srct[1] == "**" then
                #gen_gc_table_core(ccgen, node, ti, history, tup, [], 0, 0)
                gen_gc_table2(ccgen, node, oreg)
                "mmc_boxing_array(#{src}, #{srct[2]}, mrb_str_new_cstr)"

              else
                raise "Unkown type #{srct}"
              end

            when "struct range_mrb_int"
              "(mrb_range_new(mrb, mrb_fixnum_value(#{src}->first), mrb_fixnum_value(#{src}->last), #{src}->exclude_end))"

            else
              raise "Not support yet #{dstt} #{srct}"
            end

          elsif srct.is_a?(String)
            case srct
            when "mrb_sym"
              "(mrb_symbol_value(#{src}))"

            else
              raise "Not support yet #{dstt} #{srct}"
            end

          else
            case srct
            when :mrb_int
              "(mrb_fixnum_value(#{src}))"

            when :mrb_float2
              "(mrb_float_value2(#{src}))"

            when :mrb_bool
              "((#{src}) ? mrb_true_value() : mrb_false_value())"

            when :nil
              "#{src}"

            when String
              p src
              p srct
              p caller
              "conv_to_rvalue(#{src})"

            else
              p node.root.irep.disasm
              p srct
              p src
              p "Not support yet #{dstt} #{srct}"
              #raise "Not support yet #{dstt} #{srct}"
            end
          end

        when :mrb_int
          if srct.is_a?(Array) then
            case srct[0]
            when :char
              if srct[1] == "*" then
                "({mrb_int res; sscanf(#{src}, \"%d\", &res); res;})"

              else
                raise "Unkown type #{srct}"
              end

            else
              raise "Not support yet #{dstt} #{srct}"
            end

          else
            case srct
            when :mrb_value
              "(mrb_fixnum(#{src}))"

            when :mrb_float2
              "((mrb_int)#{src})"

            else
              raise "Not support yet #{dstt} #{srct}"
            end
          end

        when :mrb_float2
          if srct.is_a?(Array) then
            case srct[0]
            when :char
              if srct[1] == "*" then
                "({mrb_float2 res; sscanf(#{src}, \"%g\", &res); res;})"

              else
                raise "Unkown type #{srct}"
              end

            else
              raise "Not support yet #{dstt} #{srct}"
            end

          else
            case srct
            when :mrb_value
              "(mrb_float(#{src}))"

            when :mrb_int
              "((mrb_float)#{src})"

            else
              raise "Not support yet #{dstt} #{srct}"
            end
          end

        when :mrb_bool
          "(mrb_test(#{src}))"

        when :nil
          "mrb_nil_value()"

        else
#          p src
#          raise "Not support yet #{dstt} #{srct}"
        end
      end
    end

    def self.gen_get_iv(ccgen, inst, node, infer, history, tup, slf, ivreg, dst)
      ccgen.dcode << "#{gen_declare(ccgen, dst, tup, infer)};\n"
      ivt = get_ctype(ccgen, ivreg, tup, infer)
      slft = get_ctype(ccgen, slf, tup, infer)
      dstt = get_ctype(ccgen, dst, tup, infer)
      slfsrc = reg_real_value_noconv(ccgen, slf, node, tup, infer, history)[0]

      if slf.is_escape?(tup) then
        idx = ivreg.genpoint
        src = "ARY_PTR(mrb_ary_ptr(#{slfsrc}))[#{idx}]"
        src = gen_type_conversion(ccgen, dstt, :mrb_value, src, tup, node, infer, history, dst)
        ccgen.pcode << "v#{dst.id} = #{src};\n"
      else
        src = "#{slfsrc}->v#{ivreg.id}"
        src = gen_type_conversion(ccgen, dstt, ivt, src, tup, node, infer, history, dst)
        ccgen.pcode << " v#{dst.id} = #{src};\n"
      end
      set_closure_env(ccgen, inst, node, infer, history, tup)
    end

    def self.gen_set_iv(ccgen, inst, node, infer, history, tup, slf, ivreg, valr, dst)
      ivt = get_ctype(ccgen, ivreg, tup, infer)
      valt = get_ctype(ccgen, valr, tup, infer)
      val = reg_real_value_noconv(ccgen, valr, node, tup, infer, history)[0]
      slfsrc = reg_real_value_noconv(ccgen, slf, node, tup, infer, history)[0]
      if slf.is_escape?(tup) then
        val = gen_type_conversion(ccgen, :mrb_value, valt, val, tup, node, infer, history, dst)
    # ccgen.pcode << "mrb_ary_set(mrb, #{slfsrc}, #{ivreg.genpoint}, #{val});\n"
        ccgen.pcode << "ARY_PTR(mrb_ary_ptr(#{slfsrc}))[#{ivreg.genpoint}] = #{val};\n"
        ccgen.pcode << "mrb_field_write_barrier_value(mrb, (struct RBasic*)mrb_ary_ptr(#{slfsrc}), #{val});\n"
      else
        val = gen_type_conversion(ccgen, ivt, valt, val, tup, node, infer, history, dst)
        ccgen.pcode << "#{slfsrc}->v#{ivreg.id} = #{val}; /* #{valt} -> #{ivt} */\n"
      end
    end

    def self.gen_array_range_check(ccgen, inst, tup, idx)
      idxty = inst.inreg[1].get_type(tup)[0]
      aryty = inst.inreg[0].get_type(tup)[0]
      rc = idx
      case idxty
      when  MTypeInf::LiteralType
        idxval = idxty.val
        if idxval < 0 then
          rc = "#{aryty.val.size - idxval}"
        end

      when  MTypeInf::NumericType
        posp = idxty.positive
        if !posp then
          rc = "((#{idx} < 0) ? #{aryty.element.size} - #{idx}  : #{idx})"
        end
      end
      rc
    end
  end
end

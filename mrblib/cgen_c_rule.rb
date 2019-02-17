module CodeGenC
  class CodeGen
    def self.set_closure_env(ccgen, inst, node, infer, history, tup)
      clsreg = inst.outreg[0]
      proc = ccgen.callstack[-1][0]
      while clsreg.is_a?(RiteSSA::Reg) do
        if node.root.export_regs.include?(clsreg) then
          src, inty = reg_real_value_noconv(ccgen, inst.outreg[0], node, tup, infer, history)
          outty = get_ctype(ccgen, clsreg, tup)
          if node.root.is_export_env then
            ccgen.pcode << "if (venv)\n"
            src2 = gen_type_conversion(ccgen, :mrb_value, inty, src, tup, node, infer, history)
            pos = proc.irep.export_regs.index(clsreg) + 1
            ccgen.pcode << "venv->stack[#{pos}] = #{src2};\n"
          else
            src2 = gen_type_conversion(ccgen, outty, inty, src, tup, node, infer, history)
            ccgen.pcode << "env.v#{clsreg.id} = #{src2};\n"
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
      outr.refpoint.size < 3 or
        ((!outr.genpoint.is_a?(RiteSSA::Inst)) or
        [
          :SENDB, :SEND, :ARRAY, :MOVE, :GETIV
        ].include?(outr.genpoint.op))
    end

    def self.do_if_multi_use(ccgen, inst, node, infer, history, tup)
      outr = inst.outreg[0]
      if !is_not_assign_emit(outr) then
        val, srct = yield
        dstt = get_ctype(ccgen, outr, tup)
        val = gen_type_conversion(ccgen, dstt, srct, val, tup, node, infer, history)
        ccgen.dcode << "#{gen_declare(ccgen, outr, tup)};\n"
        ccgen.pcode << "v#{outr.id} = #{val};\n"
      end
    end

    def self.do_ifnot_multi_use(ccgen, inst, node, ti, history, tup)
      outr = inst.outreg[0]
      if is_not_assign_emit(outr) then
        yield
      else
        srct = get_ctype(ccgen, outr, tup)
        ["v#{inst.outreg[0].id}", srct]
      end
    end

    def self.gen_gc_table(ccgen, inst, node, infer, history, tup)
      regs = inst.para[2]
      pos = inst.para[4]
      num = inst.para[1]
      tabpos = 0
      prevsize = ccgen.prev_gcsingle.size
      (pos + num).times do |i|
        r = regs[i]
        if !(r.type[tup] and r.type[tup].any? {|ty| is_gcobject?(ty)} and
            is_escape?(r)) or
            (r.genpoint.is_a?(Fixnum) and
            !ccgen.is_live_reg?(node, r, r.genpoint) and
            i != 0) then
            next
        end

        name = reg_real_value_noconv(ccgen, r, node, tup, infer, history)[0]
        if ccgen.prev_gcsingle[tabpos] != name then
          ccgen.pcode << "gctab->single[#{tabpos}] = &#{name};\n"
          ccgen.prev_gcsingle[tabpos] = name
        end
        tabpos += 1
      end

      if tabpos > 0 and tabpos != prevsize then
        ccgen.pcode << "gctab->size = #{tabpos};\n"
      end

      if tabpos > ccgen.gcsingle_size then
        ccgen.gcsingle_size = tabpos
      end
    end

    def self.op_send(ccgen, inst, node, infer, history, tup)
      name = inst.para[0]
      op_send_aux(ccgen, inst, inst.inreg, inst.outreg, node, infer, history, tup, name)
    end

    def self.op_send_aux(ccgen, inst, inreg, outreg, node, infer, history, tup, name)
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
          else
            next
          end

          utup = infer.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), tup)
          fname = gen_method_func(name, rt, utup)

          procexport = false
          i = 0
          topnode = node.root.nodes[0]
          args = inreg.map {|reg|
            rs, srct = reg_real_value_noconv(ccgen, reg, node, tup, infer, history)
            if srct == :gproc then
              procexport = true
            end
            dstt = get_ctype(ccgen, reg, tup)
            i = i + 1
            gen_type_conversion(ccgen, dstt, srct, rs, tup, node, infer, history)
          }.join(", ")
          args << ", gctab"
          gen_gc_table(ccgen, inst, node, infer, history, tup)

          if outreg then
            nreg = outreg[0]
            ccgen.dcode << gen_declare(ccgen, nreg, tup)
            ccgen.dcode << ";\n"
            ccgen.pcode << "v#{nreg.id} = #{fname}(mrb, #{args});\n"
          else
            ccgen.pcode << "#{fname}(mrb, #{args});\n"
          end

          if procexport then
            node.root.import_regs.each do |reg|
              ccgen.pcode << "v#{reg.id} = env.v#{reg.id};\n"
            end
          end
          minf = [fname, proc, utup]
          if ccgen.using_method.index(minf) == nil then
            ccgen.using_method.push minf
          end

          return
        end
      end
      if name != :initialize then
        p name
        ccgen.pcode << "mrb_no_method_error(mrb, mrb_intern_lit(mrb, \"#{name}\"), mrb_nil_value(), \"undefined method #{name}\");\n"
      end
      nil
    end

    def self.gen_term(ccgen, gins, node, tup, ti, history, reg0, reg1, op)
      valuep = 0
      if reg0.is_a?(RiteSSA::Reg) then
        reg0.flush_type(tup)
        reg0.rearrange_type(tup)
        arg0, srcs0 = reg_real_value_noconv(ccgen, reg0, node, tup, ti, history)
        case reg0.type[tup].size
        when 1
          srcd0 = get_ctype(ccgen, reg0, tup, false)
          if reg0.type[tup][0].is_a?(MTypeInf::LiteralType) then
            srcs0 = srcd0
            arg0 = reg0.type[tup][0].val
            valuep |= 1
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
        srcd0 = get_ctype(ccgen, gins.inreg[0], tup, false)
        srcs0 = srcd0
      end

      if reg1.is_a?(RiteSSA::Reg) then
        reg1.flush_type(tup)
        reg1.rearrange_type(tup)
        arg1, srcs1 = reg_real_value_noconv(ccgen, reg1, node, tup, ti, history)
        case reg1.type[tup].size
        when 1
          srcd1 = get_ctype(ccgen, reg1, tup, false)
          if reg1.type[tup][0].is_a?(MTypeInf::LiteralType) then
            srcs1 = srcd1
            arg1 = reg1.type[tup][0].val
            valuep |= 2
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
        srcd1 = get_ctype(ccgen, gins.inreg[0], tup, false)
        srcs1 = srcd1
      end
      dstd = get_ctype(ccgen, gins.outreg[0], tup)
      src = ""

      if [:+, :-, :*, :/].include?(op) then
        if (srcd0 == :mrb_int or srcd0 == :mrb_float2) and
            (srcd1 == :mrb_int or srcd1 == :mrb_float2) then
          if (srcd0 == :mrb_float2 or srcd1 == :mrb_float2) then
            dsts = :mrb_float2
            if valuep == 3 then
              #          [eval("(#{arg0} #{op} #{arg1})"), srcd0]
              src = "(#{arg0} #{op} #{arg1})"
            else
              term0 = gen_type_conversion(ccgen, :mrb_float2, srcs0, arg0, tup, node, ti, history)
              term1 = gen_type_conversion(ccgen, :mrb_float2, srcs1, arg1, tup, node, ti, history)
              src = "(#{term0} #{op} #{term1})"
            end
          else
            dsts = :mrb_int
            if valuep == 3 then
              #          [eval("(#{arg0} #{op} #{arg1})"), srcd0]
              src = "(#{arg0} #{op} #{arg1})"
            else
              term0 = gen_type_conversion(ccgen, :mrb_int, srcs0, arg0, tup, node, ti, history)
              term1 = gen_type_conversion(ccgen, :mrb_int, srcs1, arg1, tup, node, ti, history)
              src = "(#{term0} #{op} #{term1})"
            end
          end
        else
          #p reg0.type[tup]
          op_send(ccgen, gins, node, ti, history, tup)
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
              term0 = gen_type_conversion(ccgen, :mrb_float2, srcs0, arg0, tup, node, ti, history)
              term1 = gen_type_conversion(ccgen, :mrb_float2, srcs1, arg1, tup, node, ti, history)
              src = "(#{term0} #{op} #{term1})"
            end
          else
            if valuep == 3 then
              src = eval("(#{arg0} #{op} #{arg1})")
            else
              term0 = gen_type_conversion(ccgen, :mrb_int, srcs0, arg0, tup, node, ti, history)
              term1 = gen_type_conversion(ccgen, :mrb_int, srcs1, arg1, tup, node, ti, history)
              src = "(#{term0} #{op} #{term1})"
            end
          end
        else
          #p reg0.type[tup]
          op_send(ccgen, gins, node, ti, history, tup)
          src = "v#{gins.outreg[0].id}"
        end

      elsif op == :== then
        dsts = :mrb_bool
        if valuep == 3 then
          src = eval("(#{arg0} #{op} #{arg1})")
        else
        arg0 = gen_type_conversion(ccgen, srcs1, srcs0, arg0, tup, node, ti, history)
          src = "(#{arg0} #{op} #{arg1})"
        end

      else
        raise "No suche opcode #{op}"
      end

      src = gen_type_conversion(ccgen, dstd, dsts, src, tup, node, ti, history)
      [src, dstd]
    end

    def self.reg_real_value(ccgen, ireg, oreg, node, tup, ti, history)
      val, srct = reg_real_value_noconv(ccgen, ireg, node, tup, ti, history)
      dstt = get_ctype(ccgen, oreg, tup)
      gen_type_conversion(ccgen, dstt, srct, val, tup, node, ti, history)
    end

    def self.get_ctype_from_robj(src)
      srct = :mrb_value
      if src.is_a?(Fixnum) then
        srct = :mrb_int
      elsif src.is_a?(Float) then
        srct = :mrb_float2
      else
      end

      srct
    end

    def self.reg_real_value_noconv(ccgen, reg, node, tup, ti, history)
      srct = get_ctype(ccgen, reg, tup)
      if reg.is_a?(RiteSSA::ParmReg) then
        if node.enter_link.size == 1 then
          pnode = node.enter_link[0]
          preg = pnode.exit_reg[reg.genpoint]
          return reg_real_value_noconv(ccgen, preg, pnode, tup, ti, history)
        end

        if node.enter_link.size == 0 then # TOP of block
          ptype = ti.typetupletab.rev_table[tup][reg.genpoint]
          if ptype.is_a?(Array) and ptype.size == 1 and
              ptype[0].class == MTypeInf::LiteralType then
            src = ptype[0].val
            if src == true then
              return [1, :mrb_bool]
            elsif src == false then
              return [0, :mrb_bool]
            else
              return [src, get_ctype_from_robj(src)]
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
          otype = get_ctype(ccgen, gins.outreg[0], tup)
          [reg_real_value(ccgen, gins.inreg[0], gins.outreg[0], node, tup, ti, history), otype]
        }

      when :LOADL, :LOADI
        if node.root.export_regs.include?(reg) then
          ["v#{reg.id}", srct]
        else
          src = gins.para[0]
          [src, get_ctype_from_robj(src)]
        end

      when :LOADT
        [1, :mrb_bool]

      when :LOADF
        [0, :mrb_bool]

      when :LOADSYM
        ["mrb_intern_lit(mrb, \"#{gins.para[0]}\")", :mrb_value]

      when :LOADNIL
        ["mrb_nil_value()", :mrb_value]

      when :LOADSELF
        ["self", srct]

      when :GETCONST
        val = gins.para[1]
        vid =  ccgen.clstab[val]
        if vid then
          [vid[1], srct]
        else
          [val, srct]
        end

      when :ENTER
        i = gins.outreg.index(reg)
        [reg_real_value(ccgen, gins.inreg[i], gins.outreg[i], node, tup, ti, history), srct]

      when :EQ
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :==)
        }

      when :SEND
        ["v#{reg.id}", srct]

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
          [res, [:gproc, reg.type[tup][0].id]]
        end

      else
        ["v#{reg.id}", srct]
      end
    end

    def self.gen_name_marshal(name)
      name = name.to_s
      name.gsub!("_", "_u")
      name.gsub!("=", "_e")
      name.gsub!("!", "_E")
      name.gsub!("@", "_a")
      name.gsub!("$", "_d")
      name.gsub!("%", "_p")
      name.gsub!("+", "_P")
      name.gsub!("-", "_m")
      name.gsub!("*", "_A")
      name
    end

    def self.gen_method_func(name, rectype, tup)
      name = gen_name_marshal(name)
      "#{name}__#{rectype}__#{tup}"
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
      NilClass => :mrb_value
    }

    def self.get_ctype_aux(ccgen, reg, tup, escheck = true)
      if escheck and is_escape?(reg) then
        return :mrb_value
      end

      rtype = reg.type[tup]
      if !rtype then
        # for element of array
        rtype = reg.type[reg.type.keys[0]]
        if rtype.nil? then
          return :mrb_value
        end
      end

      cls0 = rtype[0].class_object
      if rtype.all? {|e| e.class_object == cls0} then
        res = TTABLE[cls0]
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

      if rtype.size == 1 then
        clsssa =  RiteSSA::ClassSSA.get_instance(cls0)
        ccgen.using_class[clsssa] ||= "cls#{clsssa.id}"
        if clsssa and clsssa.id != 0 then
          "struct cls#{clsssa.id} *"
        else
          :mrb_value
        end
      else
        rtype.each do |e|
          cls = e.class_object
          clsssa =  RiteSSA::ClassSSA.get_instance(cls)
          ccgen.using_class[clsssa] ||= "cls#{clsssa.id}"
        end

        :mrb_value
      end
    end

    def self.get_ctype(ccgen, reg, tup, escheck = true)
      type = get_ctype_aux(ccgen, reg, tup, escheck)
      case type
      when :array
        if escheck and !is_escape?(reg) then
          uv = MTypeInf::ContainerType::UNDEF_VALUE
          ereg = reg.type[tup][0].element[uv]
          rc = get_ctype_aux(ccgen, ereg, tup, escheck)
          if rc == :array then
            :mrb_value
          else
            "#{rc} *"
          end
        else
          :mrb_value
        end

      when :range
        ereg = reg.type[tup][0].element[0]
        rc = get_ctype_aux(ccgen, ereg, tup)
        if rc == :array then
          :mrb_value
        else
          rc
        end

      when :gproc
        [:gproc, reg.type[tup][0].id]

      else
        type
      end
    end

    def self.gen_declare(ccgen, reg, tup)
      type = get_ctype_aux(ccgen, reg, tup)
      if reg.is_a?(RiteSSA::ParmReg) and reg.genpoint == 0 then
        regnm = "self"
      else
        regnm = "v#{reg.id}"
      end

      case type
      when :array
        uv = MTypeInf::ContainerType::UNDEF_VALUE
        ereg = reg.type[tup][0].element[uv]
        etype = get_ctype_aux(ccgen, ereg, tup)
        "#{etype} *#{regnm}"

      when :nil
        "mrb_value #{regnm}"

      else
        "#{type} #{regnm}"
      end
    end

    def self.is_escape?(reg, cache = {})
      res = @@escape_cache[reg]
      if res then
        res
      end

      if cache[reg] then
        @@escape_cache[reg] = false
        return false
      end
      cache[reg] = true

      reg.type.each do |tup, tys|
        tys.each do |ty|
          if is_escape_aux(ty, ty.place, cache) then
            @@escape_cache[reg] = true
            return true
          end
        end
      end

      @@escape_cache[reg] = false
      false
    end

    def self.is_escape_aux(ty, plist, cache)
      if plist.size == 0 then
        return false
      end
      if cache[plist] then
        return false
      end

      plist.any? {|e, val|
        case e
        when :return
          is_gcobject?(ty)

        when MTypeInf::ProcType
          cache[e.place] = true
          is_escape_aux(ty, e.place, cache)

        when RiteSSA::Reg
          is_escape?(e, cache)

        when TrueClass
          true

        else
          true
        end
      }
    end

    def self.is_gcobject?(ty)
      ty.is_a?(MTypeInf::ContainerType) or
        ty.is_a?(MTypeInf::UserDefinedType) or
        ty.is_a?(MTypeInf::ProcType)
    end

    def self.gen_type_conversion(ccgen, dstt, srct, src, tup, node, ti, history)
      if dstt == srct then
        return src
      end

      if !srct then
        return src
      end

      if dstt.is_a?(Array) then
        p MTypeInf::ProcType.gettab[dstt[1]]
        raise "Not support yet #{dstt} #{srct}"
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
              slfty = get_ctype(ccgen, proc.slfreg, tup)
              val = gen_type_conversion(ccgen, :mrb_value, slfty, val, tup, node, ti, history)
              res << "tmpval[0] = #{val};\n"
              proc.env.each_with_index do |srcreg, i|
                val = reg_real_value_noconv(ccgen, srcreg, node, tup, ti, history)[0]
                stype = get_ctype(ccgen, srcreg, tup)
                val = gen_type_conversion(ccgen, :mrb_value, stype, val, tup, node, ti, history)
                res << "tmpval[#{i + 1}] = #{val};\n"
              end
              res << "tproc = mrb_proc_new_cfunc_with_env(mrb, ((struct proc#{proc.id} *)#{src})->code, #{nval}, tmpval);\n"
              res << "venv = tproc->e.env;\n"

              res << "}\n"

              res << "mrb_obj_value(tproc);\n"
              res <<  "})"
              ccgen.callstack[-1][1] = true
              res
            else
              raise "Not support yet #{dstt} #{srct}"
            end
          else
            case srct
            when :mrb_int
              "(mrb_fixnum_value(#{src}))"

            when :mrb_float2
              "(mrb_float_value(mrb, #{src}))"

            when :mrb_bool
              "((#{src}) ? mrb_true_value() : mrb_false_value())"

            when :nil
              "#{src}"

            else
              raise "Not support yet #{dstt} #{srct}"
            end
          end

        when :mrb_int
          case srct
          when :mrb_value
            "(mrb_fixnum(#{src}))"

          when :mrb_float2
            "((mrb_int)#{src})"

          else
            raise "Not support yet #{dstt} #{srct}"
          end

        when :mrb_float2
          case srct
          when :mrb_value
            "(mrb_float(#{src}))"

          when :mrb_int
            "((mrb_float)#{src})"

          else
            raise "Not support yet #{dstt} #{srct}"
          end

        when :mrb_bool
          "(mrb_test(#{src}))"

        when :nil
          "mrb_nil_value()"

        else
          raise "Not support yet #{dstt} #{srct}"
        end
      end
    end
  end
end

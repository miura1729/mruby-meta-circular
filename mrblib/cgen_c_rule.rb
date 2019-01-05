module CodeGenC
  class CodeGen
    def self.do_if_multi_use(ccgen, inst, node, infer, history, tup)
      if inst.outreg[0].refpoint.size > 1 then
        dst = inst.outreg[0]
        ccgen.dcode << "#{gen_declare(ccgen, dst, tup)};\n"
        dstt = get_ctype(ccgen, dst, tup)
        src = yield
        ccgen.pcode << "v#{dst.id} = #{src};\n"
      end
    end

    def self.do_ifnot_multi_use(ccgen, inst, node, ti, history, tup)
      if inst.outreg[0].refpoint.size < 2 then
        yield
      else
        "v#{inst.outreg[0].id}"
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
          args = inreg.map {|reg|
            rs = reg_real_value(ccgen, reg, node, tup, infer, history)
            if get_ctype_aux(ccgen, reg, tup) == :gproc then
              procexport = true
            end
            rs
          }.join(", ")

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
      p name
      ccgen.pcode << "mrb_no_method_error(mrb, mrb_intern_lit(mrb, \"#{name}\"), mrb_nil_value(), \"undefined method #{name}\");\n"
      nil
    end

    def self.gen_term(ccgen, gins, node, tup, ti, history, reg0, reg1, op)
      if reg0.is_a?(RiteSSA::Reg) and reg0.type[tup] then
        reg0.rearrange_type(tup)
        arg0 = reg_real_value(ccgen, reg0, node, tup, ti, history)
        case reg0.type[tup].size
        when 1
          srcs0 = get_ctype(ccgen, reg0, tup)
          srcd0 = get_ctype(ccgen, reg0, tup, false)
        else
          srcs0 = :mrb_value
          srcd0 = :mrb_value
        end
      else
        arg0 = reg0
        srcd0 = get_ctype(ccgen, gins.inreg[0], tup, false)
      end

      if reg1.is_a?(RiteSSA::Reg) and reg1.type[tup] then
        reg1.rearrange_type(tup)
        arg1 = reg_real_value(ccgen, reg1, node, tup, ti, history)
        case reg1.type[tup].size
        when 1
          srcs1 = get_ctype(ccgen, reg1, tup)
          srcd1 = get_ctype(ccgen, reg1, tup, false)
        else
          srcs1 = :mrb_value
          srcd1 = :mrb_value
        end
      else
        arg1 = reg1
        if gins.inreg[1] then
          srcd1 = get_ctype(ccgen, gins.inreg[1], tup, false)
        else
          if gins.para[1].is_a?(Fixnum) then
            srcd1 = :mrb_int
            srcs1 = srcd1
          elsif  gins.para[1].is_a?(Float) then
            srcd1 = :mrb_float
            srcs1 = srcd1
          else
            srcd1 = :mrb_value
            srcs1 = srcd1
          end
        end
      end

      if (srcd0 == :mrb_int or srcd0 == :mrb_float) and
          (srcd1 == :mrb_int or srcd1 == :mrb_float) then
        if arg0.class != String and arg1.class != String then
#            eval "(#{arg0} #{op} #{arg1})"
          "(#{arg0} #{op} #{arg1})"
        else
          "(#{gen_type_conversion(srcd0, srcs0, arg0)} #{op} #{gen_type_conversion(srcd1, srcs1, arg1)})"
        end
      elsif op == :== and
          srcd0 == srcd1 then
        if arg0.class != String and arg1.class != String then
          "(#{arg0} #{op} #{arg1})"
        else
          "(#{gen_type_conversion(srcd0, srcs0, arg0)} #{op} #{gen_type_conversion(srcd1, srcs1, arg1)})"
        end

      else
        #p reg0.type[tup]
        op_send(ccgen, gins, node, ti, history, tup)
        nil
      end
    end

    def self.reg_real_value(ccgen, reg, node, tup, ti, history)
      rc = reg_real_value_noconv(ccgen, reg, node, tup, ti, history)
    end

    def self.reg_real_value_noconv(ccgen, reg, node, tup, ti, history)
      if reg.is_a?(RiteSSA::ParmReg) then
        if node.enter_link.size == 1 then
          pnode = node.enter_link[0]
          preg = pnode.exit_reg[reg.genpoint]
          return reg_real_value(ccgen, preg, pnode, tup, ti, history)
        end

        if node.enter_link.size == 0 then # TOP of block
          ptype = ti.typetupletab.rev_table[tup][reg.genpoint]
          if ptype.is_a?(Array) and ptype.size == 1 and
              ptype[0].class == MTypeInf::LiteralType then
            return ptype[0].val
          end
        end

        if reg.genpoint == 0 then
          return "self"
        else
          return "v#{reg.id}"
        end
      end

      gins = reg.genpoint
      if !gins then
        return "mrb_nil_value()"
      end
      case gins.op
      when :MOVE
        do_ifnot_multi_use(ccgen, gins, node, ti, history, tup) {
          srct = get_ctype(ccgen, gins.inreg[0], tup)
          dstt = get_ctype(ccgen, gins.outreg[0], tup)
          src = reg_real_value(ccgen, gins.inreg[0], node, tup, ti, history)
          gen_type_conversion(dstt, srct, src)
        }

      when :LOADL, :LOADI
        if node.root.export_regs.include?(reg) then
          "v#{reg.id}"
        else
          src = gins.para[0]
          if src.is_a?(Fixnum) then
            srct = :mrb_int
          elsif src.is_a?(Float) then
            srct = :mrb_float
          else
            srct = :mrb_value
          end
          dstt = get_ctype(ccgen, gins.outreg[0], tup)
          gen_type_conversion(dstt, srct, src)
        end

      when :LOADSYM
        "mrb_intern_lit(mrb, \"#{gins.para[0]}\")"

      when :LOADNIL
        "mrb_nil_value()"

      when :LOADSELF
        "self"

      when :GETCONST
        val = gins.para[1]
        vid =  ccgen.clstab[val]
        if vid then
          vid[1]
        else
          val
        end

      when :ENTER
        i = gins.outreg.index(reg)
        reg_real_value(ccgen, gins.inreg[i], node, tup, ti, history)

      when :EQ
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :==)

      when :LT
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :<)

      when :LE
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :<=)

      when :GT
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :>)

      when :GE
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :>=)

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
        res = "("
        envreg = gins.para[1]
        envreg.each do |reg|
          val = reg_real_value(ccgen, reg, node, tup, ti, history)
          res += "(v#{gins.outreg[0].id}.env->v#{reg.id} = #{val}),"
        end
        res += "(gproc)&v#{reg.id})"
        res

      else
        "v#{reg.id}"

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
      Float => :mrb_float,
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

      clsssa =  RiteSSA::ClassSSA.get_instance(cls0)

      if clsssa and clsssa.id != 0 then
        "struct cls#{clsssa.id} *"
      else
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
            rc
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

      plist.any? {|e, val|
        case e
        when :return
          if ty.is_a?(MTypeInf::ContainerType) or
              ty.is_a?(MTypeInf::UserDefinedType) then
            true
          else
            false
          end

        when MTypeInf::ProcType
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

    def self.gen_type_conversion(dstt, srct, src)
      if dstt == srct then
        return src
      end

      case dstt
      when :mrb_value
        case srct
        when :mrb_int
          "(mrb_fixnum_value(#{src}))"

        when :mrb_float
          "(mrb_float_value(mrb, #{src}))"

        when :mrb_bool
          "((#{src}) ? mrb_true_value() : mrb_false_value())"

        when :nil
          "#{src}"

        else
          raise "Not support yet #{dstt} #{srct}"
        end

      when :mrb_int
        "(mrb_fixnum(#{src}))"

      when :mrb_float
        "(mrb_float(#{src}))"

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

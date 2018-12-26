module CodeGenC
  class CodeGen
    def self.op_send(ccgen, inst, node, infer, history, tup)
      name = inst.para[0]
      op_send_aux(ccgen, inst, node, infer, history, tup, name)
    end

    def self.op_send_aux(ccgen, inst, node, infer, history, tup, name)
      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      intype[0] = [intype[0][0]]
      rectype = intype[0][0].class_object
      mtab = MTypeInf::TypeInferencer.get_ruby_methodtab
      rectype.ancestors.each do |rt|
        if @@ruletab[:CCGEN_METHOD][name] and mproc = @@ruletab[:CCGEN_METHOD][name][rt] then
          return mproc.call(ccgen, inst, node, infer, history, tup)
        else
          if mtab[inst.para[0]] and mtab[inst.para[0]][rt] then
            proc = mtab[inst.para[0]][rt]
          else
            next
          end
          nreg = inst.outreg[0]
          ccgen.dcode << gen_declare(ccgen, inst, nreg, tup)
          ccgen.dcode << ";\n"

          utup = infer.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), tup)
          fname = gen_method_func(name, rt, utup)

          procexport = false
          args = inst.inreg.map {|reg|
            rs = reg_real_value(ccgen, reg, node, tup, infer, history)
            if get_ctype_aux(ccgen, inst, reg, tup) == :gproc then
              procexport = true
            end
            rs
          }.join(", ")
          ccgen.pcode << "v#{nreg.id} = #{fname}(mrb, #{args});\n"
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
      ccgen.pcode << "mrb_no_method_error(mrb, mrb_intern_lit(mrb, \"#{name}\"), mrb_nil_value(), \"undefined method #{name}\");\n"
      nil
    end

    def self.gen_term(ccgen, gins, node, tup, ti, history, reg0, reg1, op)
      if reg0.is_a?(RiteSSA::Reg) then
        reg0.rearrange_type(tup)
        arg0 = reg_real_value(ccgen, reg0, node, tup, ti, history)
        case reg0.type[tup].size
        when 1
          type0 = reg0.type[tup][0].class_object
        else
          type0 = Object
        end
      else
        arg0 = reg0
        type0 = arg0.class
      end

      if reg1.is_a?(RiteSSA::Reg) then
        reg1.rearrange_type(tup)
        arg1 = reg_real_value(ccgen, reg1, node, tup, ti, history)
        case reg1.type[tup].size
        when 1
          type1 = reg1.type[tup][0].class_object
        else
          type1 = Object
        end
      else
        arg1 = reg1
        type1 = arg1.class
      end

      if (type0 == Fixnum or type0 == Float) and
          (type1 == Fixnum or type1 == Float) then
        if arg0.class != String and arg1.class != String then
          eval "(#{arg0} #{op} #{arg1})"
        else
          "(#{arg0} #{op} #{arg1})"
        end
      elsif op == :== and
          type0 == type1 then
        if arg0.class != String and arg1.class != String then
          eval "(#{arg0} #{op} #{arg1})"
        else
          "(#{arg0} #{op} #{arg1})"
        end

      else
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
          if ptype and ptype.size == 1 and
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
        reg_real_value(ccgen, gins.inreg[0], node, tup, ti, history)

      when :LOADL, :LOADI
        if node.root.export_regs.include?(reg) then
          "v#{reg.id}"
        else
          gins.para[0]
        end

      when :LOADSYM
        "mrb_intern_lit(mrb, \"#{gins.para[0]}\")"

      when :LOADNIL
        "mrb_nil_value()"

      when :LOADSELF
        "self"

      when :GETCONST
        gins.para[1]

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
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :+)

      when :SUB
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :-)

      when :MUL
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :*)

      when :DIV
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.inreg[1], :/)

      when :ADDI
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.para[1], :+)

      when :SUBI
        gen_term(ccgen, gins, node, tup, ti, history, gins.inreg[0], gins.para[1], :-)

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

    def self.gen_method_func(name, rectype, tup)
      "#{name}_#{rectype}_#{tup}"
    end

    def self.gen_block_func(name, rectype, blkno, tup)
      "#{name}_#{rectype}_#{blkno}_#{tup}"
    end

    TTABLE = {
      Fixnum => :mrb_int,
      Float => :mrb_float,
      Array => :array,
      Range => :range,
      Proc => :gproc,
      NilClass => :nil
    }

    def self.get_ctype_aux(ccgen, inst, reg, tup)
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

      :mrb_value
    end

    def self.get_ctype(ccgen, inst, reg, tup)
      type = get_ctype_aux(ccgen, inst, reg, tup)
      case type
      when :array
        if !is_escape?(reg) then
          uv = MTypeInf::ContainerType::UNDEF_VALUE
          ereg = reg.type[tup][0].element[uv]
          rc = get_ctype_aux(ccgen, inst, ereg, tup)
          if rc == :array then
            :mrb_value
          else
            rc
          end
        else
          :mrb_value
        end

      when :range
        if !is_escape?(reg) then
          ereg = reg.type[tup][0].element[0]
          rc = get_ctype_aux(ccgen, inst, ereg, tup)
          if rc == :array then
            :mrb_value
          else
            rc
          end
        else
          :mrb_value
        end
      else
        type
      end
    end

    def self.gen_declare(ccgen, inst, reg, tup)
      type = get_ctype_aux(ccgen, inst, reg, tup)
      if reg.is_a?(RiteSSA::ParmReg) and reg.genpoint == 0 then
        regnm = "self"
      else
        regnm = "v#{reg.id}"
      end

      case type
      when :array
        uv = MTypeInf::ContainerType::UNDEF_VALUE
        ereg = reg.type[tup][0].element[uv]
        etype = get_ctype_aux(ccgen, inst, ereg, tup)
        if !is_escape?(reg) then
          "#{etype} *#{regnm}"
        else
          "mrb_value #{regnm}"
        end

      when :nil
        "mrb_value #{regnm}"

      else
        "#{type} #{regnm}"
      end
    end

    def self.is_escape?(reg)
      plist = reg.type.keys.map { |tup|
        reg.type[tup].map {|ty| ty.place.keys}.flatten.uniq
      }.flatten.uniq
      is_escape_aux(plist)
    end

    def self.is_escape_aux(plist)
      plist.any? {|e|
        case e
        when TrueClass
          true

        when RiteSSA::Reg
          e.is_escape?

        when MTypeInf::ProcType
          is_escape_aux(e.place.keys)

        else
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

module CodeGenC
  class CodeGen
    def self.reg_real_value(ccgen, reg, node, tup, ti)
      if reg.is_a?(RiteSSA::ParmReg) then
        if node.enter_link.size == 1 then
          pnode = node.enter_link[0]
          preg = pnode.exit_reg[reg.genpoint]
          return reg_real_value(ccgen, preg, pnode, tup, ti)
        end

        if node.enter_link.size == 0 then # TOP of block
          ptype = ti.typetupletab.rev_table[tup][reg.genpoint]
          if ptype and ptype.size == 1 and
              ptype[0].class == MTypeInf::LiteralType then
            return ptype[0].val
          end
        end

        return "v#{reg.id}"
      end

      gins = reg.genpoint
      if !gins then
        return "mrb_nil_value(mrb)"
      end
      case gins.op
      when :MOVE
        reg_real_value(ccgen, gins.inreg[0], node, tup, ti)

      when :LOADL, :LOADI
        gins.para[0]

      when :LOADSYM
        "mrb_intern_lit(mrb, \"#{gins.para[0]}\")"

      when :LOADNIL
        "mrb_nil_value()"

      when :LOADSELF
        "self"

      when :ENTER
        i = gins.outreg.index(reg)
        reg_real_value(ccgen, gins.inreg[i], node, tup, ti)

      when :EQ
        arg0 = reg_real_value(ccgen, gins.inreg[0], node, tup, ti)
        arg1 = reg_real_value(ccgen, gins.inreg[1], node, tup, ti)
        "(#{arg0} == #{arg1})"

      when :SUBI
        arg0 = reg_real_value(ccgen, gins.inreg[0], node, tup, ti)
        "(#{arg0} - #{gins.para[1]})"

      when :ADD
        arg0 = reg_real_value(ccgen, gins.inreg[0], node, tup, ti)
        arg1 = reg_real_value(ccgen, gins.inreg[1], node, tup, ti)
        "(#{arg0} + #{arg1})"

      else
        "v#{reg.id}"

      end
    end

    def self.gen_method_func(name, rectype, tup)
      "#{name}_#{rectype}_#{tup}"
    end

    TTABLE = {
      Fixnum => :mrb_int,
      Float => :mrb_float
    }
    def self.get_type(ccgen, inst, reg, tup)
      rtype = reg.type[tup]
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
        } then
        return :mrb_bool
      end

      :mrb_value
    end

    def self.gen_declare(ccgen, inst, reg, tup)
      type = get_type(ccgen, inst, reg, tup)
      "#{type} v#{reg.id}"
    end
  end
end

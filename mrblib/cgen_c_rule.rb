module CodeGenC
  class CodeGen
    def self.reg_real_value(ccgen, reg, node)
      if reg.is_a?(RiteSSA::ParmReg) then
        if node.enter_link.size == 1 then
          pnode = node.enter_link[0]
          preg = pnode.exit_reg[reg.genpoint]
          return reg_real_value(ccgen, preg, pnode)
        end

        return "v#{reg.id}"
      end

      gins = reg.genpoint
      if !gins then
        return "mrb_nil_value(mrb)"
      end
      case gins.op
      when :MOVE
        reg_real_value(ccgen, gins.inreg[0], node)

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
        "v#{gins.inreg[i].id}"

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

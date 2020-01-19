module HAL
  class RawWord
  end

  class RawInt8
  end

  class RawInt16
  end

  class RawIn32
  end

  class RawInt64
  end

  class RawAddress
  end

  class RawUword
  end

  class RawUint8
  end

  class RawUint16
  end

  class RawUin32
  end

  class RawUint64
  end

  class CPU
  end

  class Regs
  end

  class Reg
  end

  class Mem
  end

  class BinExp
  end

  class UniExp
  end
end

module MMC
end

module MTypeInf
  class RegClassType<BasicType
    def initialize(co, id, *rest)
      super(co, *rest)
      @regid = id
    end

    def to_s
      "%#{regid}"
    end

    attr :regid
  end

  class MemClassType<BasicType
    def initialize(co, addr, *rest)
      super(co, *rest)
      @address = addr
    end

    attr :address
  end

  class ExpType<BasicType
    def initialize(co, op, term0, term1, *rest)
      super(co, *rest)
      @opcode = op
      @term0 = term0
      @term1 = term1
    end

    attr :opcode
    attr :term0
    attr :term1
  end

  class TypeInferencer
    define_inf_rule_class_method :new, HAL::CPU do |infer, inst, node, tup|
      type = UserDefinedType.new(HAL::CPU, inst, infer.callstack[-2][0].irep)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :jmp, HAL::CPU do |infer, inst, node, tup|
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :label, HAL::CPU do |infer, inst, node, tup|
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :byte, HAL::CPU do |infer, inst, node, tup|
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :short, HAL::CPU do |infer, inst, node, tup|
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :long, HAL::CPU do |infer, inst, node, tup|
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :regs, HAL::CPU do |infer, inst, node, tup|
      type = UserDefinedType.new(HAL::Regs, inst, infer.callstack[-2][0].irep)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :[], HAL::Regs do |infer, inst, node, tup|
      idx = inst.inreg[1].flush_type(tup)[tup][0]
      type = RegClassType.new(HAL::Reg, idx.val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :[]=, HAL::Regs do |infer, inst, node, tup|
      idx = inst.inreg[1].flush_type(tup)[tup][0]
      type = RegClassType.new(HAL::Reg, idx)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :+, HAL::Reg do |infer, inst, node, tup|
      term0 = inst.inreg[0].flush_type(tup)[tup][0]
      if inst.inreg[1] then
        term1 = inst.inreg[1].flush_type(tup)[tup][0]
      else
        term1 = LiteralType.new(Fixnum, inst.para[1])
      end
      type = ExpType.new(HAL::BinExp, :+, term0, term1)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :mem, HAL::CPU do |infer, inst, node, tup|
      type = UserDefinedType.new(HAL::Mem, inst, infer.callstack[-2][0].irep)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    def self.mem_type(inst, tup)
      idx = inst.inreg[1].flush_type(tup)[tup][0]
      type = nil
      if idx.class_object == Fixnum then
        siztype = inst.inreg[2].get_type(tup)[0]
        ctype = nil
        case siztype.val
        when 1
          ctype = RawUint8

        when 2
          ctype = RawUint16

        when 4
          ctype = RawUint32

        when 8
          ctype = RawUint64

        when nil
          ctype = RawUword
        end
        type = NumericType.new(ctype, false)

      elsif idx.class_object == HAL::Reg or
          idx.class_object == HAL::BinExp then

        type = MemClassType.new(HAL::Mem, idx)
      end

      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :[], HAL::Mem do |infer, inst, node, tup|
      mem_type(inst, tup)
      nil
    end

    define_inf_rule_method :[]=, HAL::Mem do |infer, inst, node, tup|
      mem_type(inst, tup)
      nil
    end

    define_inf_rule_method :static_cast, HAL::Mem do |infer, inst, node, tup|
      oty = inst.inreg[2].flush_type(tup)[tup][0]
      type = UserDefinedStaticType.new(oty.val, inst, infer.callstack[-2][0].irep)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :static_allocate, HAL::Mem do |infer, inst, node, tup|
      oty = inst.inreg[1].flush_type(tup)[tup][0]
      type = UserDefinedStaticType.new(oty.val, inst, infer.callstack[-2][0].irep)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_class_method :attribute, MMC do |infer, inst, node, tup|
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_class_method :class_sizeof, MMC do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_class_method :instance_sizeof, MMC do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_class_method :offsetof, MMC do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end
  end
end

module CodeGenC
  class CodeGen
    define_ccgen_rule_class_method :new, HAL::CPU do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_method :regs, HAL::CPU do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_method :jmp, HAL::CPU do |ccgen, inst, node, infer, history, tup|
      label = inst.inreg[1].flush_type(tup)[tup][0].val
      ccgen.pcode << "asm volatile (\"jmp #{label.to_s}\");\n"
      nil
    end

    define_ccgen_rule_method :label, HAL::CPU do |ccgen, inst, node, infer, history, tup|
      label = inst.inreg[1].flush_type(tup)[tup][0].val
      ccgen.pcode << "asm volatile (\"#{label.to_s}:\");\n"
      nil
    end

    define_ccgen_rule_method :byte, HAL::CPU do |ccgen, inst, node, infer, history, tup|
      val = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0].to_s
      val = eval(val)
      ccgen.pcode << "asm volatile (\".byte #{val.to_s}\");\n"
      nil
    end

    define_ccgen_rule_method :short, HAL::CPU do |ccgen, inst, node, infer, history, tup|
      val = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0].to_s
      val = eval(val)
      ccgen.pcode << "asm volatile (\".short #{val}\");\n"
      nil
    end

    define_ccgen_rule_method :long, HAL::CPU do |ccgen, inst, node, infer, history, tup|
      val = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0].to_s
      val = eval(val)
      ccgen.pcode << "asm volatile (\".long #{val}\");\n"
      nil
    end

    define_ccgen_rule_method :mem, HAL::CPU do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_method :[], HAL::Regs do |ccgen, inst, node, infer, history, tup|
      nil
    end

    def self.op2mnemonic(op)
      ({:+ => "add",
          :- => "sub",
          :& => "and",
          :| => "or",
          :- => "xor",
          :! => "not",
          :-@=> "neg"}[op])
    end

    define_ccgen_rule_method :[]=, HAL::Regs do |ccgen, inst, node, infer, history, tup|
      idxtype = inst.inreg[1].flush_type(tup)[tup][0]
      valtype = inst.inreg[2].flush_type(tup)[tup][0]

      mnemonic = "mov"
      dst = ""
      src = ""
      if idxtype.class_object == Symbol then
        case idxtype.val
        when :rax, :rbx, :rcx, :rdx, :rsp, :rbp, :rdi, :rsi
          dst = "%" + idxtype.val.to_s
          mnemonic = "mov"

        when :eax, :ebx, :ecx, :edx, :esp, :ebp, :edi, :esi
          dst = "%" + idxtype.val.to_s
          mnemonic = "mov"

        when :ax, :bx, :cx, :dx, :sp, :bp, :di, :si
          dst = "%" + idxtype.val.to_s
          mnemonic = "mov"

        else
          raise "Not support reg #{idxtype.val}"
        end

      else
        raise "Regname must be Symbol (ex. :rax)"
      end

      if valtype.class_object == Fixnum then
        exp = valtype.val
        src = exp
        ccgen.pcode << "asm volatile (\"#{mnemonic} #{src}, #{dst}\");\n"

      elsif valtype.class_object == HAL::Reg then
        src = valtype.to_s
        ccgen.pcode << "asm volatile (\"#{mnemonic} #{src}, #{dst}\");\n"

      elsif valtype.class_object == HAL::Mem then
        src = deref_mem(valtype.address)
        ccgen.pcode << "asm volatile (\"#{mnemonic} #{src}, #{dst}\");\n"

      elsif valtype.class_object == HAL::BinExp then
        if valtype.term0.regid == idxtype.val then
          mnemonic = op2mnemonic(valtype.opcode)
          if valtype.term1.class_object == HAL::Reg then
            src = valtype.term1.to_s
          else
            src = valtype.term1.val.to_s
          end
          ccgen.pcode << "asm volatile (\"#{mnemonic} #{src}, #{dst}\");\n"

        else
          # This condition not support normally
        end

      elsif valtype.class_object == HAL::UniExp then
        if valtype.term1.regid == idxtype.val then
          mnemonic = op2mnemonic(valtype.opcode)
          ccgen.pcode << "asm volatile (\"#{mnemonic} #{dst}\");\n"

        else
          # This condition not support normally
        end
      end

      nil
    end

    define_ccgen_rule_method :+, HAL::Reg do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_method :[], HAL::Mem do |ccgen, inst, node, infer, history, tup|
      nil
    end

    def self.deref_mem(idxtype)
      dst = ""
      idxcls = idxtype.class_object
      if idxcls == HAL::Reg then
        dst = idxtype.to_s

      elsif idxtype.class_object == HAL::BinExp then
        dst = idxtype.term0.to_s
        off = idxtype.term1.val.to_s
        dst = "#{off}(#{dst})"

      else
        raise "Must be reg and regoffset #{idxtype.class_object}"
      end

      dst
    end

    define_ccgen_rule_method :[]=, HAL::Mem do |ccgen, inst, node, infer, history, tup|
      idxtype = inst.inreg[1].flush_type(tup)[tup][0]
      valtype = inst.inreg[2].flush_type(tup)[tup][0]

      dst = deref_mem(idxtype)

      mnemonic = "mov"
      src = ""
      if valtype.class_object == HAL::Reg then
        src = valtype.to_s
        ccgen.pcode << "asm volatile (\"#{mnemonic} #{src}, #{dst}\");\n"

      elsif valtype.class_object == HAL::BinExp then
        if idxcls == HAL::Reg then
          mnemonic = op2mnemonic(valtype.opcode)
          if valtype.term1.class_object == HAL::Reg then
            src = valtype.term1.to_s
          else
            src = valtype.term1.val.to_s
          end
          ccgen.pcode << "asm volatile (\"#{mnemonic} #{src}, #{dst}\");\n"

        else
          # This condition not support normally
        end

      else

        raise "Invalid value"
      end

      nil
    end

    define_ccgen_rule_method :static_cast, HAL::Mem do |ccgen, inst, node, infer, history, tup|
      idx = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0].to_s
      target = get_ctype(ccgen, inst.outreg[0], tup, infer, false)
      oreg = inst.outreg[0]
      ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
      ccgen.pcode << "v#{oreg.id} = ((#{target})(#{idx}));\n"

      nil
    end

    define_ccgen_rule_method :static_allocate, HAL::Mem do |ccgen, inst, node, infer, history, tup|

      oreg = inst.outreg[0]
      type = get_ctype_aux_aux(ccgen, inst.outreg[0], tup, infer)
      if type.is_a?(Array) then
        dtype = type[0]
        type = type.join
      end

      regnm = "v#{oreg.id}"

      case dtype
      when :array
        uv = MTypeInf::ContainerType::UNDEF_VALUE
        ereg = reg.type[tup][0].element[uv]
        etup = tup
        if ereg.type[tup] == nil then
          etup = ereg.type.keys[0]
        end
        etype = get_ctype_aux(ccgen, ereg, etup, infer)
        ccgen.dcode << "#{type} v#{oreg.id};\n"
        ccgen.hcode << "#{etype} #{regnm}_ent[];\n"
        ccgen.pcode << "v#{oreg.id} = ((#{etype})(#{regnm}_ent));\n"

      when :nil
        ccgen.dcode << "#{type} v#{oreg.id};\n"
        ccgen.hcode << "mrb_value #{regnm}_ent;\n"
        ccgen.pcode << "v#{oreg.id} = ((#{type})(&#{regnm}_ent));\n"

      else
        ccgen.dcode << "#{type} v#{oreg.id};\n"
        ccgen.hcode << "#{dtype} #{regnm}_ent;\n"
        ccgen.pcode << "v#{oreg.id} = ((#{type})(&#{regnm}_ent));\n"
      end

      nil
    end

    define_ccgen_rule_class_method :attribute, MMC do |ccgen, inst, node, infer, history, tup|
      attrname = inst.inreg[1].flush_type(tup)[tup][0]
      attrval = inst.inreg[2].flush_type(tup)[tup][0]
      ccgen.tmp_attribute[attrname.val] = attrval.val

      nil
    end

    define_ccgen_rule_class_method :class_sizeof, MMC do |ccgen, inst, node, infer, history, tup|
      klassobj = inst.inreg[1].flush_type(tup)[tup][0].val

      nil
    end

    define_ccgen_rule_class_method :instance_sizeof, MMC do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      type = get_ctype_aux_aux(ccgen, inst.inreg[1], tup, infer)
      ccgen.pcode << "v#{oreg.id} = sizeof(#{trype});"
    end

    define_ccgen_rule_class_method :offsetof, MMC do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      name = inst.inreg[2].flush_type(tup)[tup][0].val
      cty = inst.inreg[1].type[tup][0]
      cls =ClassSSA.get_instance(cty.class_object)
      ctype = get_ctype_aux_aux(ccgen, inst.inreg[1], tup, infer)
      ivval = cls.iv[name]
      ccgen.pcode << "v#{oreg.id} = (((#{ctype} *)0)->v#{ivval.id});"
    end
  end
end

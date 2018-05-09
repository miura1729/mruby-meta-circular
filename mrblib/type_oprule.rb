module MTypeInf
  class TypeInferencer
    @@ruletab ||= {}
    @@ruby_methodtab ||= {}

    def self.define_inf_rule_op(name, &block)
      @@ruletab[:OP] ||= {}
      if @@ruletab[:OP][name] then
        raise "Already defined #{name}"
      end
      @@ruletab[:OP][name] = block
    end

    define_inf_rule_op :MOVE do |infer, inst, node, tup|
      inst.outreg[0].add_same(inst.inreg[0])
      nil
    end

    define_inf_rule_op :LOADL do |infer, inst, node, tup|
      val = inst.para[0]
      type = LiteralType.new(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADI do |infer, inst, node, tup|
      val = inst.para[0]
      type = LiteralType.new(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADSYM do |infer, inst, node, tup|
      val = inst.para[0]
      type = LiteralType.new(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADNIL do |infer, inst, node, tup|
      val = inst.para[0]
      type = LiteralType.new(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADSELF do |infer, inst, node, tup|
      inst.inreg[0].flush_type(tup)
      inst.outreg[0].add_type(inst.inreg[0].type[tup][0], tup)
      nil
    end

    define_inf_rule_op :LOADT do |infer, inst, node, tup|
      val = inst.para[0]
      type = LiteralType.new(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADF do |infer, inst, node, tup|
      val = inst.para[0]
      type = LiteralType.new(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    #  define_inf_rule_op :GETGLOBAL do |infer, inst, node, tup|
    #  end

    #  define_inf_rule_op :SETGLOBAL do |infer, inst, node, tup|
    #  end

    #  define_inf_rule_op :GETSPECIAL do |infer, inst, node, tup|
    #  end

    #  define_inf_rule_op :SETSPECIAL do |infer, inst, node, tup|
    #  end

    define_inf_rule_op :GETIV do |infer, inst, node, tup|
      inst.outreg[0].add_same(inst.inreg[0])
      nil
    end

    define_inf_rule_op :SETIV do |infer, inst, node, tup|
      inst.outreg[0].add_same(inst.inreg[0])
      nil
    end

    define_inf_rule_op :GETUPVAR do |infer, inst, node, tup|
      up = inst.para[1]
      frame = inst.para[0]
      stpos = infer.callstack.index {|item| item[0] == frame}
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type(tup, infer.callstack[stpos][1])

      nil
    end

    define_inf_rule_op :SETUPVAR do |infer, inst, node, tup|
      inst.outreg[0].add_same(inst.inreg[0])
      nil
    end
    
    define_inf_rule_op :JMP do |infer, inst, code, tup|
    end

    define_inf_rule_op :JMPIF do |infer, inst, code, tup|
    end

    define_inf_rule_op :JMPNOT do |infer, inst, code, tup|
        @@ruletab[:OP][:JMPIF].call(self, inst, code, tup)
    end

    define_inf_rule_op :ENTER do |infer, inst, node, tup|
      nil
    end

    define_inf_rule_op :SEND do |infer, inst, node, tup|
      inst.inreg[0..-2].each do |reg|
        reg.flush_type(tup)
      end
      inst.inreg[-1].add_type(LiteralType.new(NilClass, nil), tup)

      rule_send_common(infer, inst, node, tup)
    end

    define_inf_rule_op :SENDB do |infer, inst, node, tup|
      inst.inreg[0..-1].each do |reg|
        reg.flush_type(tup)
      end

      rule_send_common(infer, inst, node, tup)
    end

    define_inf_rule_op :RETURN do |infer, inst, node, tup|
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type_alltup(tup)
      nil
    end

    define_inf_rule_op :EQ do |infer, inst, node, tup|
      inst.inreg[0].flush_type(tup)
      inst.inreg[1].flush_type(tup)

      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LT do |infer, inst, code, tup|
        @@ruletab[:OP][:EQ].call(self, inst, code, tup)
    end

    define_inf_rule_op :LE do |infer, inst, code, tup|
        @@ruletab[:OP][:EQ].call(self, inst, code, tup)
    end

    define_inf_rule_op :GT do |infer, inst, code, tup|
        @@ruletab[:OP][:EQ].call(self, inst, code, tup)
    end

    define_inf_rule_op :GE do |infer, inst, code, tup|
        @@ruletab[:OP][:EQ].call(self, inst, code, tup)
    end

    define_inf_rule_op :ADD do |infer, inst, node, tup|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = inst.inreg[0].flush_type(tup)[tup]
      if arg0type
      end
      restype = arg0type
      restype.each do |ty|
        inst.outreg[0].add_type ty, tup
      end
      nil
    end

    define_inf_rule_op :SUB do |infer, inst, code, tup|
        @@ruletab[:OP][:ADD].call(self, inst, code, tup)
    end

    define_inf_rule_op :MUL do |infer, inst, code, tup|
        @@ruletab[:OP][:ADD].call(self, inst, code, tup)
    end

    define_inf_rule_op :ADDI do |infer, inst, node, tup|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = LiteralType.new(Fixnum, inst.para[1])

      arg0type.each do |ty|
        inst.outreg[0].add_type ty, tup
      end
      nil
    end

    define_inf_rule_op :SUBI do |infer, inst, code, tup|
        @@ruletab[:OP][:ADDI].call(self, inst, code, tup)
    end

    define_inf_rule_op :ARRAY do |infer, inst, code, tup|
      type = ContainerType.new(Array)
      nilreg = RiteSSA::Reg.new(nil)
      type.element[nil] = nilreg
      inst.para[0].times do |i|
        nreg = RiteSSA::Reg.new(nil)
        nreg.add_same inst.inreg[i]
        nreg.flush_type(tup)
        type.element[i] = nreg
        nilreg.add_same inst.inreg[i]
        nilreg.flush_type(tup)
      end

      inst.outreg[0].add_type type, tup
    end

    define_inf_rule_op :LAMBDA do |infer, inst, code, tup|
      pty = ProcType.new(Proc, inst.para[0])
      inst.outreg[0].add_type pty, tup
      nil
    end
  end
end

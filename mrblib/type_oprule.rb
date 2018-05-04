module MTypeInf
  class TypeInferencer
    @@ruletab ||= {}
    @@methodtab ||= {}
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
      name = inst.para[0]
      inst.inreg[0..-2].each do |reg|
        reg.flush_type(tup)
      end
      inst.inreg[-1].add_type(LiteralType.new(NilClass, nil), tup)

      # if inst.inreg[0].type[tup].size > 1 then   # Polymorphism

      intype = inst.inreg.map {|reg| reg.type[tup]}
      ntup = infer.typetupletab.get_tupple_id(intype)

      inst.inreg[0].type[tup].each do |ty|
        slf = ty.class_object
        @@methodtab[slf] ||= {}
        irepssa = @@methodtab[slf][name]
        if irepssa.nil? then
          irep = Irep::get_irep(slf, name)
          irepssa =  RiteSSA::Block.new(irep, nil, slf)
          @@methodtab[slf][name] = irepssa
        end

        if irepssa.retreg.flush_type(ntup)[ntup].nil? then
          infer.inference_block(irepssa, intype)
        end
        inst.outreg[0].add_same irepssa.retreg
      end
      inst.outreg[0].flush_type(tup, ntup)
      nil
    end

    define_inf_rule_op :RETURN do |infer, inst, node, tup|
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type(tup)
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

      arg0type.each do |ty|
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

    define_inf_rule_op :LAMBDA do |infer, inst, code, tup|
      pty = ProcType.new(Proc, inst.para)
      inst.outreg[0].add_type pty, tup
      nil
    end
  end
end

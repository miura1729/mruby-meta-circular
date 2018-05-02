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

    define_inf_rule_op :MOVE do |infer, ins, node, tup|
      ins.outreg[0].add_same(ins.inreg[0])
      nil
    end

    define_inf_rule_op :LOADL do |infer, ins, node, tup|
      val = ins.para[0]
      type = LiteralType.new(val.class, val)
      ins.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADI do |infer, ins, node, tup|
      val = ins.para[0]
      type = LiteralType.new(val.class, val)
      ins.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADSYM do |infer, ins, node, tup|
      val = ins.para[0]
      type = LiteralType.new(val.class, val)
      ins.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADNIL do |infer, ins, node, tup|
      val = ins.para[0]
      type = LiteralType.new(val.class, val)
      ins.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADSELF do |infer, ins, node, tup|
      ins.inreg[0].flush_type(tup)
      ins.outreg[0].add_type(ins.inreg[0].type[tup][0], tup)
      nil
    end

    define_inf_rule_op :LOADT do |infer, ins, node, tup|
      val = ins.para[0]
      type = LiteralType.new(val.class, val)
      ins.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADF do |infer, ins, node, tup|
      val = ins.para[0]
      type = LiteralType.new(val.class, val)
      ins.outreg[0].add_type(type, tup)
      nil
    end

    #  define_inf_rule_op :GETGLOBAL do |infer, ins, node, tup|
    #  end

    #  define_inf_rule_op :SETGLOBAL do |infer, ins, node, tup|
    #  end

    #  define_inf_rule_op :GETSPECIAL do |infer, ins, node, tup|
    #  end

    #  define_inf_rule_op :SETSPECIAL do |infer, ins, node, tup|
    #  end

    define_inf_rule_op :GETIV do |infer, ins, node, tup|
      ins.outreg[0].add_same(ins.inreg[0])
      nil
    end

    define_inf_rule_op :SETIV do |infer, ins, node, tup|
      ins.outreg[0].add_same(ins.inreg[0])
      nil
    end

    
    define_inf_rule_op :JMP do |infer, ins, code, tup|
    end

    define_inf_rule_op :JMPIF do |infer, ins, code, tup|
    end

    define_inf_rule_op :JMPNOT do |infer, ins, code, tup|
        @@ruletab[:OP][:JMPIF].call(self, ins, code, tup)
    end

    define_inf_rule_op :ENTER do |infer, ins, node, tup|
      nil
    end

    define_inf_rule_op :SEND do |infer, ins, node, tup|
      name = ins.para[0]
      ins.inreg[0..-2].each do |reg|
        reg.flush_type(tup)
      end
      ins.inreg[-1].add_type(LiteralType.new(NilClass, nil), tup)

      # if ins.inreg[0].type[tup].size > 1 then   # Polymorphism

      intype = ins.inreg.map {|reg| reg.type[tup]}
      ntup = infer.typetupletab.get_tupple_id(intype)

      ins.inreg[0].type[tup].each do |ty|
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
        ins.outreg[0].add_same irepssa.retreg
      end

      p intype
      p ntup
      p ins.outreg[0].flush_type(ntup)
      nil
    end

    define_inf_rule_op :RETURN do |infer, ins, node, tup|
      ins.outreg[0].add_same(ins.inreg[0])
      p tup
      p ins.inreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_op :EQ do |infer, ins, node, tup|
      ins.inreg[0].flush_type(tup)
      p ins.inreg[0].type
      ins.inreg[1].flush_type(tup)
      p ins.inreg[1].type
      nil
    end

    define_inf_rule_op :ADD do |infer, ins, node, tup|
      arg0type = ins.inreg[0].flush_type(tup)[tup]
      arg1type = ins.inreg[0].flush_type(tup)[tup]

      arg0type.each do |ty|
        ins.outreg[0].add_type ty, tup
      end
      nil
    end

    define_inf_rule_op :SUB do |infer, ins, code, tup|
        @@ruletab[:OP][:ADD].call(self, ins, code, tup)
    end

    define_inf_rule_op :ADDI do |infer, ins, node, tup|
      arg0type = ins.inreg[0].flush_type(tup)[tup]
      arg1type = LiteralType.new(Fixnum, ins.para[1])

      arg0type.each do |ty|
        ins.outreg[0].add_type ty, tup
      end
      nil
    end

    define_inf_rule_op :SUBI do |infer, ins, code, tup|
        @@ruletab[:OP][:ADDI].call(self, ins, code, tup)
    end
  end
end

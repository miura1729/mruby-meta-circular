module MTypeInf
  class TypeInferencer
    @@ruletab ||= {}
    def self.define_inf_rule_op(name, &block)
      @@ruletab[:OP] ||= {}
      if @@ruletab[:OP][name] then
        raise "Already defined #{name}"
      end
      @@ruletab[:OP][name] = block
    end
  end

  TypeInferencer::define_inf_rule_op :MOVE do |ins, node, tup|
    ins.outreg[0].add_same(ins.inreg[0])
  end

  TypeInferencer::define_inf_rule_op :LOADL do |ins, node, tup|
    val = ins.para[0]
    type = LiteralType.new(val.class, val)
    ins.outreg[0].add_type(ins.inreg[0], tup)
  end

  TypeInferencer::define_inf_rule_op :LOADI do |ins, node, tup|
    val = ins.para[0]
    type = LiteralType.new(val.class, val)
    ins.outreg[0].add_type(ins.inreg[0], tup)
  end

  TypeInferencer::define_inf_rule_op :LOADSYM do |ins, node, tup|
    val = ins.para[0]
    type = LiteralType.new(val.class, val)
    ins.outreg[0].add_type(ins.inreg[0], tup)
  end


  TypeInferencer::define_inf_rule_op :LOADNIL do |ins, node, tup|
    val = ins.para[0]
    type = LiteralType.new(val.class, val)
    ins.outreg[0].add_type(ins.inreg[0], tup)
  end

  TypeInferencer::define_inf_rule_op :LOADSELF do |ins, node, tup|
    ins.outreg[0].add_same(ins.inreg[0])
  end

  TypeInferencer::define_inf_rule_op :LOADT do |ins, node, tup|
    val = ins.para[0]
    type = LiteralType.new(val.class, val)
    ins.outreg[0].add_type(ins.inreg[0], tup)
  end

  TypeInferencer::define_inf_rule_op :LOADF do |ins, node, tup|
    val = ins.para[0]
    type = LiteralType.new(val.class, val)
    ins.outreg[0].add_type(ins.inreg[0], tup)
  end

#  TypeInferencer::define_inf_rule_op :GETGLOBAL do |ins, node, tup|
#  end

#  TypeInferencer::define_inf_rule_op :SETGLOBAL do |ins, node, tup|
#  end

#  TypeInferencer::define_inf_rule_op :GETSPECIAL do |ins, node, tup|
#  end

#  TypeInferencer::define_inf_rule_op :SETSPECIAL do |ins, node, tup|
#  end

  TypeInferencer::define_inf_rule_op :GETIV do |ins, node, tup|
    ins.outreg[0].add_same(ins.inreg[0])
  end

  TypeInferencer::define_inf_rule_op :SETIV do |ins, node, tup|
    ins.outreg[0].add_same(ins.inreg[0])
  end

  TypeInferencer::define_inf_rule_op :ENTER do |ins, node, tup|
  end
end

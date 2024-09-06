module MMC_EXT
  class Vector<Array
  end
end

module MTypeInf
  class TypeInferencer
    define_inf_rule_method :map_with_index!, MMC_EXT::Vector do |infer, inst, node, tup|
      p inst.inreg[1].type
      p inst.inreg[1].get_type(tup)[0].irep.nodes[0].enter_reg
      inst.outreg[0].add_same inst.inreg[0]
      nil
    end
  end
end

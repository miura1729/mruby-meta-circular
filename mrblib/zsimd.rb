class StringView
end

module MMC_EXT
  module SIMD
  end
end

module MTypeInf
  class TypeInferencer
    define_inf_rule_method :_simd_check, StringView do |infer, inst, node, tup|
      block = inst.inreg[1].get_type(tup)[0].irep
      effects = block.effects
      inst.outreg[0].type[tup] =  [LiteralType.new(NilClass, nil)]
      effects[:return].values.each do |reteff|
        genvalins = reteff[0].genpoint
        if genvalins.is_a?(RiteSSA::Inst) and genvalins.op == :SEND then
          slfreg = genvalins.inreg[0]
          positive = reteff[1]
          if genvalins.para[0] == :st and  positive and positive.size == 1 then
            refinement = positive[0]
            predicate = refinement.predicate
            arg0 = refinement.args[0][0]
            arg1 = refinement.args[1][0]
            if predicate == :start_with and arg1.is_a?(LiteralType) and arg1.val.is_a?(String) and arg1.val.size < 8 and effects[:return].size == 1 then
              inst.outreg[0].type[tup] =  [SymbolType.instance(Symbol, :find)]

            elsif predicate == :include? and arg0.is_a?(RangeType)  and
                arg0.element[0].type.values[0][0].is_a?(LiteralType) and
                arg0.element[1].type.values[0][0].is_a?(LiteralType) then
              inst.outreg[0].type[tup] =  [SymbolType.instance(Symbol, :select)]

            else
              inst.outreg[0].type[tup] =  [LiteralType.new(NilClass, nil)]
              break
            end
          end
        end
      end
      nil
    end
  end
end

module CodeGenC
  class CodeGen
    define_ccgen_rule_method :_simd_check, StringView do |ccgen, inst, node, infer, history, tup|
      # No code generate this methed only for type
      nil
    end
  end
end

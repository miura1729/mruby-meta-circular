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
      if effects[:return].size == 1 then
        genvalins = effects[:return].values[0][0].genpoint
        if genvalins.is_a?(RiteSSA::Inst) and genvalins.op == :SEND then
          slfreg = genvalins.inreg[0]
          positive = effects[:return].values[0][1]
          if genvalins.para[0] == :st and  positive and positive.size == 1 then
            refinement = positive[0]
            if refinement.is_a?(RefinementType) then
              predicate = refinement.predicate
              arg1 = refinement.args[1][0]
              if predicate == :start_with and arg1.is_a?(LiteralType) and arg1.val.is_a?(String) and arg1.val.size < 8 then
                inst.outreg[0].type[tup] =  [SymbolType.instance(Symbol, :foo)]
              end
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

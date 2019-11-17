class TypeVariable
  include Enumerable
  def method_missing(*arg, &blk)
    missing(arg[1], &blk)
  end
end

module MTypeInf
  class TypeVarType<BasicType
    @@id = 0
    def initialize(co, *rest)
      super(co, *rest)
      @using_method = {}
      @sub_type_var = {}
      @name = @@id
      @@id = @@id + 1
    end

    def inspect_aux(hist, level)
      "TV#{@name}"
    end

    attr :using_method
    attr :sub_type_var
  end
end

module MTypeInf
  class TypeInferencer
    define_inf_rule_class_method :new, TypeVariable do |infer, inst, node, tup|
      type = TypeVarType.new(TypeVariable)
      inst.outreg[0].add_type type, tup
      nil
    end

    define_inf_rule_method :missing, TypeVariable do |infer, inst, node, tup|
      block = inst.inreg[2].type[tup][0]
      if block.is_a?(ProcType) then
        make_intype(infer, inst, node, tup) do |intype, argc|
          #      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
          intype[0] = [block.slf]
          intype[1] = [TypeVarType.new(TypeVariable)]
          ntup = infer.typetupletab.get_tupple_id(intype, block, tup)
          irepssa = block.irep
          infer.inference_block(irepssa, intype, ntup, argc, block)
          inst.outreg[0].add_same irepssa.retreg
          inst.outreg[0].flush_type(tup, ntup)
        end
      end

      inst.outreg[0].add_type TypeVarType.new(TypeVariable), tup
      nil
    end
  end
end

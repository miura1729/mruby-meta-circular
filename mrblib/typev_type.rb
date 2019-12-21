class TypeVariable
  def method_missing(name, *arg, &blk)
    missing(name, arg, &blk)
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

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object #&&
        # @name == other.name
    end

    attr :using_method
    attr :sub_type_var
    attr :name
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
      rec = inst.inreg[0].type[tup][0]
      name = inst.inreg[1].type[tup][0]
      block = inst.inreg[3].type[tup][0]
      if block.is_a?(ProcType) then
        make_intype(infer, inst, node, tup) do |intype, argc|
          #      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
          intype[0] = [block.slf]
          ele = rec.sub_type_var[:ele] ? rec.sub_type_var[:ele] : TypeVarType.new(TypeVariable)
          rec.sub_type_var[:ele] = ele
          intype[1] = [ele]
          intype[2] = inst.inreg[3].type[tup]
          ntup = infer.typetupletab.get_tupple_id(intype, block, tup)
          irepssa = block.irep
          infer.inference_block(irepssa, intype, ntup, argc, block)
          inst.outreg[0].add_same irepssa.retreg
          inst.outreg[0].flush_type(tup, ntup)
        end
      end

      ret = rec.sub_type_var[:ret] || TypeVarType.new(TypeVariable)
      rec.using_method[name] = [inst.inreg.map {|r| r.type[tup]}, ret]
      rec.sub_type_var[:ret] = ret
      inst.outreg[0].add_type ret, tup
      nil
    end
  end
end

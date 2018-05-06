module MTypeInf
  class TypeInferencer

    @@ruletab ||= {}
    @@ruby_methodtab ||= {}

    def self.define_inf_rule_method(name, rec, &block)
      @@ruletab[:METHOD] ||= {}
      @@ruletab[:METHOD][name] ||= {}
      if @@ruletab[:METHOD][name][rec] then
        raise "Already defined #{name}"
      end
      @@ruletab[:METHOD][name][rec] = block
    end

    define_inf_rule_method :lambda, Object do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[1]
      nil
    end

    define_inf_rule_method :call, Proc do |infer, inst, node, tup|
      intype = inst.inreg.map {|reg| reg.type[tup]}
      ptype = intype[0][0]
      intype[0] = node.regtab[0].type[tup]
      ntup = infer.typetupletab.get_tupple_id(intype)
      irepssa = ptype.irep
      if irepssa.retreg.flush_type(ntup)[ntup].nil? then
        infer.inference_block(irepssa, intype, ntup)
      end
      inst.outreg[0].add_same irepssa.retreg
      inst.outreg[0].flush_type(tup, ntup)
      nil
    end
  end
end



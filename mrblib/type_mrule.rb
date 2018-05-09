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

    define_inf_rule_method :[], Array do |infer, inst, node, tup|
      if inst.inreg.size != 3 then
        raise "multiple argument not support yet in Array::[]"
      end

      argtypes = inst.inreg[1].type[tup]
      arrtypes = inst.inreg[0].type[tup]
      arrele = arrtypes[0].element
      if argtypes.size == 1 and
          (argtype = argtypes[0]).class_object == Fixnum then
        case argtype
        when MTypeInf::LiteralType
          no = argtype.val
          inst.outreg[0].add_same arrele[no]
          inst.outreg[0].flush_type(tup)

        when MTypeInf::BasicType
          inst.outreg[0].add_same arrele[nil]
          inst.outreg[0].flush_type(tup)

        else
          raise "Not supported in Array::[]"
        end
      end
      inst.outreg[0].flush_type_alltup(tup)
      nil
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



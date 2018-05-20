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

    define_inf_rule_method :to_f, Fixnum do |infer, inst, node, tup|
      type = LiteralType.new(Float, nil)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :[], Array do |infer, inst, node, tup|
      if inst.inreg.size != 3 then
        raise "multiple argument not support yet in Array::[]"
      end

      idxtypes = inst.inreg[1].flush_type(tup)[tup]
      arrtypes = inst.inreg[0].type[tup]

      arrtypes.each do |arrt|
        if arrt.class_object. == Array then
          arrele = arrt.element
          if idxtypes.size == 1 and
              (idxtype = idxtypes[0]).class_object == Fixnum then
            case idxtype
            when MTypeInf::LiteralType
              no = idxtype.val
              if arrele[no].nil? then
                no = nil
              end

              inst.outreg[0].add_same arrele[no]

            when MTypeInf::BasicType
              inst.outreg[0].add_same arrele[nil]

            else
              raise "Not supported in Array::[]"
            end
          end
        end
      end

      inst.outreg[0].flush_type_alltup(tup)
      nil
    end

    define_inf_rule_method :[]=, Array do |infer, inst, node, tup|
      if inst.inreg.size != 4 then
        raise "multiple argument not support yet in Array::[]="
      end

      idxtypes = inst.inreg[1].flush_type(tup)[tup]
      arrtypes = inst.inreg[0].type[tup]
      valreg = inst.inreg[2]

      arrtypes.each do |arrt|
        if arrt.class_object. == Array then
          arrele = arrt.element
          if idxtypes.size == 1 and
              (idxtype = idxtypes[0]).class_object == Fixnum then
            case idxtype
            when MTypeInf::LiteralType
              no = idxtype.val
              if arrele[no].nil? then
                no = nil
              end

              arrele[no].add_same valreg
              inst.outreg[0].add_same valreg

            when MTypeInf::BasicType
              arrele[nil].add_same valreg
              inst.outreg[0].add_same valreg

            else
              raise "Not supported in Array::[]="
            end
          end
        end
      end

      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :lambda, Object do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[1]
      nil
    end


    define_inf_rule_method :nil?, Object do |infer, inst, node, tup|
      inst.inreg[1].flush_type(tup)

      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :!, BasicObject do |infer, inst, node, tup|
      inst.inreg[1].flush_type(tup)

      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :new, Class do |infer, inst, node, tup|
      recvtypes = inst.inreg[0].flush_type_alltup(tup)[tup]
      recvtypes.each do |rtype|
        ntype = rtype.val
        cls = TypeSource[ntype]
        type = nil
        if  cls then
          type = cls.new(ntype)
        else
          type = UserDefinedType.new(ntype)
        end

        intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup]}
        intype[0] = [type]
        dmyreg = RiteSSA::Reg.new(nil)
        dmyreg.add_type type, tup

        if !cls then
          rule_send_common_aux(infer, inst, node, tup, :initialize, intype, dmyreg, dmyreg)
        end

        inst.outreg[0].add_type type, tup
      end
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

    define_inf_rule_method :sqrt, Module do |infer, inst, node, tup|
      type = LiteralType.new(Float, nil)
      inst.outreg[0].add_type(type, tup)
    end
  end
end

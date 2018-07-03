module MTypeInf
  class TypeInferencer

    @@ruletab ||= {}

    def self.define_inf_rule_method(name, rec, &block)
      @@ruletab[:METHOD] ||= {}
      @@ruletab[:METHOD][name] ||= {}
      if @@ruletab[:METHOD][name][rec] then
        raise "Already defined #{name}"
      end
      @@ruletab[:METHOD][name][rec] = block
    end

    define_inf_rule_method :to_f, Fixnum do |infer, inst, node, tup|
      type = PrimitiveType.new(Float)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :to_i, Fixnum do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :>>, Fixnum do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end


    define_inf_rule_method :&, Fixnum do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :|, Fixnum do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum, nil)
      inst.outreg[0].add_type(type, tup)
      nil
    end


    define_inf_rule_method :chr, Fixnum do |infer, inst, node, tup|
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :%, Fixnum do |infer, inst, node, tup|
      type = LiteralType.new(Fixnum, nil)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :to_f, Float do |infer, inst, node, tup|
      type = LiteralType.new(Float, nil)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :to_i, Float do |infer, inst, node, tup|
      type = LiteralType.new(Fixnum, nil)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :<=>, Object do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :[], Array do |infer, inst, node, tup|
      if inst.inreg.size != 3 then
        raise "multiple argument not support yet in Array::[]"
      end

      arrtypes = inst.inreg[0].flush_type(tup)[tup] || []
      idxtypes = inst.inreg[1].flush_type(tup)[tup] || []

      arrtypes.each do |arrt|
        if arrt.class_object. == Array then
          arrele = arrt.element
          if idxtypes.size == 1 then
            idxtype = idxtypes[0]
            if idxtype.class_object == Fixnum then
              case idxtype
              when MTypeInf::LiteralType
                no = idxtype.val
                if arrele[no].nil? then
                  arrele[no] = RiteSSA::Reg.new(nil)
                  arrele[no].add_same arrele[ContainerType::UNDEF_VALUE]
                end
                arrele[no].flush_type_alltup(tup)
                inst.outreg[0].add_same arrele[no]

              when MTypeInf::PrimitiveType
                inst.outreg[0].add_same arrele[ContainerType::UNDEF_VALUE]
                arrele[ContainerType::UNDEF_VALUE].flush_type_alltup(tup)

              else
                raise "Not supported in Array::[]"
              end

            elsif idxtype.class_object == Range then
              inst.outreg[0].add_same inst.inreg[0]
            end
          end
        end
      end

      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :first, Array do |infer, inst, node, tup|
      arrtypes = inst.inreg[0].flush_type(tup)[tup] || []

      arrtypes.each do |arrt|
        if arrt.class_object. == Array then
          arrele = arrt.element
          if arrele[0].nil? then
            arrele[0] = RiteSSA::Reg.new(nil)
            arrele[0].add_same arrele[ContainerType::UNDEF_VALUE]
          end
          inst.outreg[0].add_same arrele[0]
          arrele[0].flush_type_alltup(tup)
        end
      end

      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :last, Array do |infer, inst, node, tup|
      arrtypes = inst.inreg[0].flush_type(tup)[tup] || []

      arrtypes.each do |arrt|
        if arrt.class_object. == Array then
          arrele = arrt.element
          inst.outreg[0].add_same arrele[ContainerType::UNDEF_VALUE]
          arrele[ContainerType::UNDEF_VALUE].flush_type_alltup(tup)
        end
      end

      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :delete_at, Array do |infer, inst, node, tup|
      arrtypes = inst.inreg[0].flush_type(tup)[tup] || []

      arrtypes.each do |arrt|
        if arrt.class_object. == Array then
          arrele = arrt.element
          if arrele[0].nil? then
            arrele[0] = RiteSSA::Reg.new(nil)
            arrele[0].add_same arrele[ContainerType::UNDEF_VALUE]
          end
          inst.outreg[0].add_same arrele[0]
          type = PrimitiveType.new(NilClass)
          inst.outreg[0].add_type type, tup
          arrele[0].flush_type_alltup(tup)
        end
      end

      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :[]=, Array do |infer, inst, node, tup|
      if inst.inreg.size != 4 then
        raise "multiple argument not support yet in Array::[]="
      end

      idxtypes = inst.inreg[1].flush_type(tup)[tup] || []
      arrtypes = inst.inreg[0].flush_type(tup)[tup] || []
      valreg = inst.inreg[2]

      arrtypes.each do |arrt|
        if arrt.class_object. == Array then
          arrele = arrt.element
          idxtypes.each do |idxtype|
            if idxtype.class_object == Fixnum then
              case idxtype
              when MTypeInf::LiteralType
                no = idxtype.val
                if arrele[no].nil? then
                  arrele[no] = RiteSSA::Reg.new(nil)
                end
                arrele[no].add_same valreg
                arrele[no].flush_type(tup)
                arrele[ContainerType::UNDEF_VALUE].add_same valreg
                arrele[ContainerType::UNDEF_VALUE].flush_type(tup)
                inst.outreg[0].add_same valreg

              when MTypeInf::PrimitiveType
                arrele.each do |idx, reg|
                  reg.add_same valreg
                  reg.flush_type_alltup(tup)
                end
                inst.outreg[0].add_same valreg

              else
                raise "Not supported in Array::[]="
              end
            end
          end
        end
      end

      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :dup, Array do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[0]
      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :<<, Array do |infer, inst, node, tup|
      rule_ary_push_common(infer, inst, node, tup)
    end

    define_inf_rule_method :push, Array do |infer, inst, node, tup|
      rule_ary_push_common(infer, inst, node, tup)
    end

    define_inf_rule_method :pop, Array do |infer, inst, node, tup|
      arrtypes = inst.inreg[0].flush_type(tup)[tup] || []

      arrtypes.each do |arrt|
        if arrt.class_object. == Array then
          arrele = arrt.element
          inst.outreg[0].add_same arrele[ContainerType::UNDEF_VALUE]
          arrele[ContainerType::UNDEF_VALUE].flush_type_alltup(tup)
        end
      end

      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :replace, Array do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[0]
      inst.outreg[0].flush_type(tup)
      nil
    end


    define_inf_rule_method :shift, Array do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[0]
      inst.outreg[0].flush_type(tup)
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type type, tup
      nil
    end

    define_inf_rule_method :length, Array do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :size, Array do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :__ary_index, Array do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :empty?, Array do |infer, inst, node, tup|
      inst.inreg[0].flush_type(tup)

      if inst.inreg[0].type[tup].size == 1 and
          inst.inreg[0].type[tup][0].element[ContainerType::UNDEF_VALUE].flush_type(tup)[tup] and
          inst.inreg[0].type[tup][0].element[ContainerType::UNDEF_VALUE].type[tup][0].class_object != NilClass then
        type = LiteralType.new(FalseClass, false)
        inst.outreg[0].add_type(type, tup)
      end

      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      nil
    end


    define_inf_rule_method :__ary_cmp, Array do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[1]
      inst.outreg[0].flush_type(tup)

      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)

      type = LiteralType.new(Fixnum, 0)
      inst.outreg[0].add_type(type, tup)

      nil
    end

    define_inf_rule_method :include?, Array do |infer, inst, node, tup|
      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :lambda, Object do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[1]
      nil
    end

    define_inf_rule_method :raise, Object do |infer, inst, node, tup|
      nil
    end

    define_inf_rule_method :respond_to?, Object do |infer, inst, node, tup|
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :__svalue, Array do |infer, inst, node, tup|
      arrtypes = inst.inreg[0].flush_type(tup)[tup]
      arrtypes.each do |arrt|
        inst.outreg[0].add_same arrt.element[0]
      end
      inst.outreg[0].flush_type(tup)
      nil
    end


    define_inf_rule_method :nil?, Object do |infer, inst, node, tup|
      slf = inst.inreg[0].flush_type(tup)[tup]

      if slf.size != 1 || slf[0].class_object == NilClass then
        type = LiteralType.new(TrueClass, true)
        inst.outreg[0].add_type(type, tup)
      end
      if slf.size != 1 || slf[0].class_object != NilClass then
        type = LiteralType.new(FalseClass, false)
        inst.outreg[0].add_type(type, tup)
      end
      nil
    end

    define_inf_rule_method :===, Object do |infer, inst, node, tup|
      slf = inst.inreg[0].flush_type(tup)[tup]
      other = inst.inreg[1].flush_type(tup)[tup]

      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :!, BasicObject do |infer, inst, node, tup|
      slf = inst.inreg[0].flush_type(tup)[tup]

      if slf.size != 1 ||
          slf[0].class_object == NilClass ||
          slf[0].class_object == FalseClass then
        type = LiteralType.new(TrueClass, true)
        inst.outreg[0].add_type(type, tup)
      end
      if slf.size != 1 ||
          slf[0].class_object != NilClass ||
          slf[0].class_object != FalseClass then
        type = LiteralType.new(FalseClass, false)
        inst.outreg[0].add_type(type, tup)
      end
      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :!=, BasicObject do |infer, inst, node, tup|
      inst.inreg[1].flush_type(tup)

      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :kind_of?, Object do |infer, inst, node, tup|
      slf = inst.inreg[0].flush_type(tup)[tup]
      arg = inst.inreg[1].flush_type(tup)[tup]

      if slf.size != 1 || slf[0].class_object == arg[0].val then
        type = LiteralType.new(TrueClass, true)
        inst.outreg[0].add_type(type, tup)
      end
      if slf.size != 1 || slf[0].class_object != arg[0].val then
        type = LiteralType.new(FalseClass, false)
        inst.outreg[0].add_type(type, tup)
      end
      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :class, Object do |infer, inst, node, tup|
      type = inst.inreg[0].flush_type(tup)[tup][0].class_object.class

      type = PrimitiveType.new(type)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :new, Class do |infer, inst, node, tup|
      recvtypes = inst.inreg[0].flush_type_alltup(tup)[tup]
      intype = nil
      argc = inst.para[1]
      if argc == 127 then
        intype = []
        type = ContainerType.new(Array)
        intype[0] = inst.inreg[0].flush_type(tup)[tup] || []
        inst.inreg[1..-2].each_with_index do|ar, i|
          reg = RiteSSA::Reg.new(nil)
          reg.add_same ar
          reg.flush_type(tup)
          type.element[i] = reg
        end
        intype[1] = [type]
        intype[2] = inst.inreg[-1].flush_type(tup)[tup] || []
      else
        intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      end

      recvtypes.each do |rtype|
        ntype = rtype.val
        cls = TypeSource[ntype]
        type = nil
        if  cls then
          type = cls.new(ntype)
        else
          type = UserDefinedType.new(ntype)
        end
        intype[0] = [type]

        if !cls then
          ntup = infer.typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), tup)
          dmyreg = RiteSSA::Reg.new(nil)
          dmyreg.add_type type, ntup
          rule_send_common_aux(infer, inst, node, ntup, :initialize, intype, dmyreg, dmyreg, inst.para[1])
        end

        inst.outreg[0].add_type type, tup
      end
      nil
    end

    define_inf_rule_method :call, Proc do |infer, inst, node, tup|
      intype = inst.inreg.map {|reg| reg.type[tup] || []}
      ptype = intype[0][0]
      intype[0] = [ptype.slf]
      ntup = infer.typetupletab.get_tupple_id(intype, ptype, tup)
      irepssa = ptype.irep
      infer.inference_block(irepssa, intype, ntup, inst.para[1], ptype)
      inst.outreg[0].add_same irepssa.retreg
      inst.outreg[0].flush_type(tup, ntup)
      nil
    end

    define_inf_rule_method :to_s, String do |infer, inst, node, tup|
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :to_f, String do |infer, inst, node, tup|
      type = PrimitiveType.new(Float)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :to_i, String do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :downcase, String do |infer, inst, node, tup|
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :size, String do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :[], String do |infer, inst, node, tup|
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :include?, String do |infer, inst, node, tup|
      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :index, String do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :sprintf, Object do |infer, inst, node, tup|
      type = LiteralType.new(String, nil)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :__printstr__, Object do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[0]
    end

    define_inf_rule_method :sqrt, Module do |infer, inst, node, tup|
      type = LiteralType.new(Float, nil)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :rand, Math.class do |infer, inst, node, tup|
      type = LiteralType.new(Float, nil)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :cos, Math.class do |infer, inst, node, tup|
      type = LiteralType.new(Float, nil)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :sin, Math.class do |infer, inst, node, tup|
      type = LiteralType.new(Float, nil)
      inst.outreg[0].add_type(type, tup)
    end

    define_inf_rule_method :begin, Range do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[0].type[tup][0].element[0]
      inst.outreg[0].flush_type_alltup(tup)
      nil
    end

    define_inf_rule_method :last, Range do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[0].type[tup][0].element[1]
      inst.outreg[0].flush_type_alltup(tup)
      nil
    end

    define_inf_rule_method :exclude_end?, Range do |infer, inst, node, tup|
      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :[], Hash do |infer, inst, node, tup|
      if inst.inreg.size != 3 then
        raise "multiple argument not support yet in Hash::[]"
      end

      hashtypes = inst.inreg[0].flush_type(tup)[tup] || []
      idxtypes = inst.inreg[1].flush_type(tup)[tup] || []

      hashtypes.each do |hasht|
        if hasht.class_object. == Hash then
          hashele = hasht.element
          idxtypes.each do |idxtype|
            case idxtype
            when MTypeInf::LiteralType
              idx = idxtype.val
              if hashele[idx].nil? then
                hashele[idx] = RiteSSA::Reg.new(nil)
                hashele[idx].add_same hashele[ContainerType::UNDEF_VALUE]
              end
              hashele[idx].flush_type_alltup(tup)
              inst.outreg[0].add_same hashele[idx]

            when MTypeInf::PrimitiveType
              inst.outreg[0].add_same hashele[ContainerType::UNDEF_VALUE]
              hashele[ContainerType::UNDEF_VALUE].flush_type_alltup(tup)

            else
              raise "Not supported in Hash::[]"
            end
          end
        end
      end

      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :[]=, Hash do |infer, inst, node, tup|
      if inst.inreg.size != 4 then
        raise "multiple argument not support yet in Hash::[]="
      end

      idxtypes = inst.inreg[1].flush_type(tup)[tup] || []
      hashtypes = inst.inreg[0].flush_type(tup)[tup] || []
      valreg = inst.inreg[2]

      hashtypes.each do |hasht|
        if hasht.class_object. == Hash then
          hashele = hasht.element
          idxtypes.each do |idxtype|
            hasht.key.add_type idxtype, 0
            case idxtype
            when MTypeInf::LiteralType
              idx = idxtype.val
              hashele[idx] ||= RiteSSA::Reg.new(nil)
              hashele[idx].add_same valreg
              hashele[idx].flush_type(tup)
              hashele[ContainerType::UNDEF_VALUE].add_same valreg
              hashele[ContainerType::UNDEF_VALUE].flush_type(tup)
              inst.outreg[0].add_same valreg

            when MTypeInf::PrimitiveType
              hashele.each do |idx, reg|
                reg.add_same valreg
                reg.flush_type(tup)
              end
              inst.outreg[0].add_same valreg

            else
              raise "Not supported in Hash::[]="
            end
          end
        end
      end

      inst.outreg[0].flush_type(tup)
      nil
    end
  end
end

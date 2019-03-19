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

    def self.define_inf_rule_class_method(name, rec, &block)
      rec = class << rec
              self
            end
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
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end


    define_inf_rule_method :chr, Fixnum do |infer, inst, node, tup|
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :%, Fixnum do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :to_f, Float do |infer, inst, node, tup|
      type = PrimitiveType.new(Float)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :to_i, Float do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
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

      inst.inreg[0].flush_type(tup)
      inst.inreg[1].flush_type(tup)
      arrtypes = inst.inreg[0].get_type(tup)
      idxtypes = inst.inreg[1].get_type(tup)
      nilobj = PrimitiveType.new(NilClass)

      arrtypes.each do |arrt|
        if arrt.class_object. == Array then
          arrele = arrt.element
          altele = arrele[ContainerType::UNDEF_VALUE]
          if idxtypes.size == 1 then
            idxtype = idxtypes[0]
            if idxtype.class_object == Fixnum then
              case idxtype
              when MTypeInf::LiteralType
                no = idxtype.val
                if arrele[no].nil? then
                  if no != 0 then
                    inst.outreg[0].add_type nilobj, tup
                    altele.add_type nilobj, tup
                  end
                  inst.outreg[0].add_same altele
                  altele.flush_type_alltup(tup)
                else
                  arrele[no].flush_type_alltup(tup)
                  inst.outreg[0].add_same arrele[no]
                end

              when MTypeInf::PrimitiveType
                inst.outreg[0].add_same altele
                altele.flush_type_alltup(tup)

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
      if valreg.type[tup] then
        valreg.type[tup].each do |ty|
          ty.place[inst.inreg[0]] = true
        end
      end
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
      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :respond_to?, Object do |infer, inst, node, tup|
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :index, Array do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      type = PrimitiveType.new(NilClass)
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

    define_inf_rule_method :inspect, Object do |infer, inst, node, tup|
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
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
      type = inst.inreg[0].flush_type(tup)[tup][0].class_object

      type = LiteralType.new(type.class, type)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :attr_reader, Module do |infer, inst, node, tup|
      recvtypes = inst.inreg[0].flush_type(tup)[tup]
      nameary = inst.inreg[1].flush_type(tup)[tup][0]
      nameary.element.each do |k, symty|
        if symty.type[tup] then
          symcls = symty.type[tup][0]
          name = symcls.val
          recvtypes.each do |rtype|
            rcls = rtype.val
            @@ruletab[:METHOD][name] = {}
            @@ruletab[:METHOD][name][rcls] = :reader
          end
        end
      end
      nil
    end

    define_inf_rule_method :attr_writer, Module do |infer, inst, node, tup|
      recvtypes = inst.inreg[0].flush_type(tup)[tup]
      nameary = inst.inreg[1].flush_type(tup)[tup][0]
      nameary.element.each do |k, symty|
        if symty.type[tup] then
          symcls = symty.type[tup][0]
          name = symcls.val
          recvtypes.each do |rtype|
            rcls = rtype.val
            name2 = "#{name.to_s}=".to_sym
            @@ruletab[:METHOD][name2] = {}
            @@ruletab[:METHOD][name2][rcls] = :writer
          end
        end
      end
      nil
    end

    define_inf_rule_method :new, Class do |infer, inst, node, tup|
      recvtypes = inst.inreg[0].flush_type_alltup(tup)[tup]
      intype = nil
      argc = inst.para[1]
      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}

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
          dmyreg = RiteSSA::Reg.new(nil)
          dmyreg.add_type type, tup
          dmyoreg = RiteSSA::Reg.new(nil)
          rule_send_common_aux(infer, inst, node, tup, :initialize, intype, dmyreg, dmyoreg, inst.para[1], nil)
#          rule_send_common_aux(infer, inst, node, tup, :initialize, intype, dmyreg, dmyreg, inst.para[1], nil)
        end

        inst.outreg[0].add_type type, tup
      end
      nil
    end

    define_inf_rule_method :call, Proc do |infer, inst, node, tup|
      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      ptype = intype[0][0]
      intype[0] = [ptype.slf]
      ntup = infer.typetupletab.get_tupple_id(intype, ptype, tup)
      if !ptype.using_tup[ntup] then
        curpos = ptype.using_tup.size
        ptype.using_tup[ntup] = curpos
      end
      irepssa = ptype.irep
      infer.inference_block(irepssa, intype, ntup, inst.para[1], ptype)
      inst.outreg[0].add_same irepssa.retreg
      inst.outreg[0].flush_type(tup, ntup)
      nil
    end

    define_inf_rule_method :to_s, String do |infer, inst, node, tup|
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :to_f, String do |infer, inst, node, tup|
      type = PrimitiveType.new(Float)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :to_i, String do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :downcase, String do |infer, inst, node, tup|
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :size, String do |infer, inst, node, tup|
      type = PrimitiveType.new(Fixnum)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :[], String do |infer, inst, node, tup|
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
      type = PrimitiveType.new(NilClass)
      inst.outreg[0].add_type(type, tup)
      nil
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
      type = PrimitiveType.new(String)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :__printstr__, Object do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[0]
      nil
    end

    define_inf_rule_method :raise, Kernel do |infer, inst, node, tup|
      argc = infer.callstack[-1][2]
      reg = RiteSSA::Reg.new(nil)
      infer.exception.push reg

      case argc
      when 0
        type = ExceptionType.new(RuntimeError)
        reg.add_type(type, tup)

      else
        reg.add_same inst.inreg[1]
        reg.flush_type(tup)
      end

#      p inst.line
#      p inst.filename

      nil
    end

    define_inf_rule_method :send, Kernel do |infer, inst, node, tup|
      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      mname = intype[1]
      intype = intype[0] + intype[2..-1]
      outr = inst.outreg[0]
      mname.each do |sym|
        mn = sym.val
        rule_send_common_aux(infer, inst, node, tup, mn, intype, inst.inreg[0], outr, intype.size, nil)
      end
      nil
    end

    define_inf_rule_class_method :sqrt, Math do |infer, inst, node, tup|
      type = PrimitiveType.new(Float)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :rand, Kernel do |infer, inst, node, tup|
      type = PrimitiveType.new(Float)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :cos, Math.class do |infer, inst, node, tup|
      type = PrimitiveType.new(Float)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_method :sin, Math.class do |infer, inst, node, tup|
      type = PrimitiveType.new(Float)
      inst.outreg[0].add_type(type, tup)
      nil
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

            when MTypeInf::PrimitiveType, MTypeInf::ContainerType
              inst.outreg[0].add_same hashele[ContainerType::UNDEF_VALUE]
              hashele[ContainerType::UNDEF_VALUE].flush_type_alltup(tup)

            else
              raise "Not supported in Hash::[] #{idxtype.class}"
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

            when MTypeInf::PrimitiveType,
                 MTypeInf::ContainerType,
                 MTypeInf::ProcType
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

    define_inf_rule_class_method :current, Fiber do |infer, inst, node, tup|
      proc = infer.callstack[-1][3]
      type = FiberType.new(Fiber, proc)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_class_method :new, Fiber do |infer, inst, node, tup|
      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      proc = intype.pop
      type = FiberType.new(Fiber, proc[0])

      argc = inst.para[1]

      intype[0] = [type]
      intype = [proc] + intype + [[]]
      ninst = RiteSSA::Inst.new(33, proc[0].irep, 0, node) #33 is :send maybe
      intype.each {|tys|
        nreg = RiteSSA::Reg.new(nil)
        tys.each do |ty|
          nreg.add_type ty, tup
        end
        ninst.inreg.push nreg
      }

      dmyreg = RiteSSA::Reg.new(nil)
      dmyreg.add_type proc[0], tup
      ninst.outreg.push dmyreg

      rule_send_common_aux(infer, ninst, node, tup, :call, intype, dmyreg, dmyreg, inst.para[1], nil)

      inst.outreg[0].add_type type, tup
      nil
    end

    define_inf_rule_method :alive?, Fiber do |infer, inst, node, tup|
      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      nil
    end
  end
end

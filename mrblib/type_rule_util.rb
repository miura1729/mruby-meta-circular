module MTypeInf
  class TypeInferencer
    @@ruby_methodtab ||= {}

    def self.get_ruby_methodtab
      @@ruby_methodtab
    end

    def self.rule_literal_commin(infer, inst, node, tup)
      val = inst.para[0]
      type = LiteralType.new(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    def self.rule_jmpif_common(infer, inst, node, tup, history, bidx)
      notp = false
      typemethodp = false
      genp = inst.inreg[0].genpoint
      if genp.is_a?(RiteSSA::Inst) then
        if genp.op == :SEND and genp.para[0] == :! then
          notp = true
          genp = genp.inreg[0].genpoint
        end

        if genp.op == :SEND then
          addtional_type_spec = nil
          case genp.para[0]
          when :kind_of?
            typemethodp = true
            tcls = genp.inreg[1].flush_type(tup)[tup]
            cls = nil
            if tcls.size == 1 and tcls[0].class_object == Class then
              cls = tcls[0].val
            end
            type = PrimitiveType.new(cls)

            addtional_type_spec = [type]
            genp = genp.inreg[0].genpoint

          when :nil?
            typemethodp = true
            type = PrimitiveType.new(NilClass)

            addtional_type_spec = [type]
            genp = genp.inreg[0].genpoint
          end

        elsif genp.op == :MOVE then
          notp = !notp
          typemethodp = true
          type0 = PrimitiveType.new(NilClass)
          type1 = LiteralType.new(false.class, false)

          addtional_type_spec = [type0, type1]
        end
      end

      type = inst.inreg[0].flush_type(tup)[tup]
      condtype = type && type.size == 1 && type[0].class_object
      if condtype == NilClass or condtype == FalseClass then
        enode = node.exit_link[bidx]
        while enode.ext_iseq.size == 1 and
            (enode.ext_iseq[0].op == :JMPIF or enode.ext_iseq[0].op == :JMPNOT)
          enode = enode.exit_link[bidx]
        end
        infer.inference_node(enode, tup, node.exit_reg, history)
        true

      elsif type and type.size == 1 then
        infer.inference_node(node.exit_link[1 - bidx], tup, node.exit_reg, history)
        true

      elsif typemethodp then
        idx = notp ? bidx : 1 - bidx
        nd = node.exit_link[idx]
        greg = genp.inreg[0]
        greg.positive_list.push addtional_type_spec
        greg.refpoint.each do |reg|
          reg.outreg[0].positive_list.push  addtional_type_spec
        end
        infer.inference_node(nd, tup, node.exit_reg, history)
        greg.positive_list.pop
        greg.refpoint.each do |reg|
          reg.outreg[0].positive_list.pop
        end

        idx = 1 - idx
        nd = node.exit_link[idx]
        greg.negative_list.push addtional_type_spec
        greg.refpoint.each do |reg|
          reg.outreg[0].negative_list.push addtional_type_spec
        end
        infer.inference_node(nd, tup, node.exit_reg, history)
        greg.negative_list.pop
        greg.refpoint.each do |reg|
          reg.outreg[0].negative_list.pop
        end

        true
      else
        nil
      end
    end

    def self.rule_send_common_aux(infer, inst, node, tup, name, intype, recreg, outreg, argc)
      ntup = 0
      recvtypes = recreg.get_type(tup)

      @@ruby_methodtab[name] ||= {}
      misirep = nil
      recvtypes.each do |ty|
        existf = false
        slf = ty.class_object
        if !slf.is_a?(Module) then
          next
        end
        slf.ancestors.each do |slfcls|
          irep = nil

          # p name
          # p slfcls
          # GC bug?
          a = name
          b = slfcls
          irepssa = @@ruby_methodtab[name][slfcls]
          if irepssa.nil? then
            irep = Irep::get_irep_instance(slf, name)
            name2 = name
            if irep and Irep::OPTABLE_SYM[Irep::get_opcode(irep.iseq[0])] == :CALL then
              name2 = :call
              irep = nil
            end

            if irep.nil? then
              if @@ruletab[:METHOD][name2] and @@ruletab[:METHOD][name2][slfcls] then
                # written in C or method mmsing  or no method error
                existf = true
                @@ruletab[:METHOD][name2][slfcls].call(infer, inst, node, tup)

              elsif @@ruby_methodtab[:method_missing] and
                  @@ruby_methodtab[:method_missing][cls] then
                name2 = :method_missing
                irepssa = @@ruby_methodtab[:method_missing][cls]

              else
                # No method found
              end
            else
              irepssa =  RiteSSA::Block.new(irep, nil, slfcls)
              @@ruby_methodtab[name][slfcls] = irepssa
              clsobj = RiteSSA::ClassSSA.get_instance(slfcls)
              clsobj.method[name] = irepssa
            end
          end

          if irepssa then
            intype[0] = [ty]
            ntup = infer.typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), tup)
            infer.inference_block(irepssa, intype, ntup, argc, nil)
            if outreg then
              outreg.add_same irepssa.retreg
              existf = true
            end
          end

          if existf then
            break
          end
        end
        if !existf then
          # No method fuound
          irep = Irep::get_irep(slf, :method_missing)
          if irep then
            irepssa =  RiteSSA::Block.new(irep, nil, slfcls)
            @@ruby_methodtab[name][slfcls] = irepssa
            clsobj = ClassSSA.get_instance(slfcls)
            clsobj.method[name] = irepssa
            intype[0] = [ty]
            ntup = infer.typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), tup)
            infer.inference_block(irepssa, intype, ntup, argc, nil)
            outreg.add_same irepssa.retreg
          else
            mess = "Method missing able to call #{slf}##{name} in #{inst.line}:#{inst.filename}\n"
            # print mess #for debug
            infer.messages[mess] ||= 0
            infer.messages[mess] += 1
          end
        end
      end

      outreg.flush_type(tup, ntup)
      nil
    end

    def self.rule_send_common(infer, inst, node, tup)
      name = inst.para[0]
      argc = inst.para[1]
      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      recreg = inst.inreg[0]

      rule_send_common_aux(infer, inst, node, tup, name, intype, recreg, inst.outreg[0], argc)
    end

    def self.rule_send_cfimc(infer, inst, node, tup)
    end

    def self.rule_ary_push_common(infer, inst, node, tup)
      arrtypes = inst.inreg[0].type[tup]
      valreg = inst.inreg[1]
      valreg.flush_type(tup)

      arrtypes.each do |arrt|
        if arrt.class_object.== Array then
          arrele = arrt.element
          arrele[ContainerType::UNDEF_VALUE].add_same valreg
          arrele[ContainerType::UNDEF_VALUE].flush_type(tup)
        end
      end

      inst.inreg[0].genpoint.inreg[0].add_same inst.inreg[0]
      inst.inreg[0].genpoint.inreg[0].flush_type(tup)
      inst.outreg[0].add_same inst.inreg[0]
      inst.outreg[0].flush_type(tup)
      nil
    end
  end
end


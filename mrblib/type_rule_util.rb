module MTypeInf
  class TypeInferencer
    @@ruby_methodtab ||= {}

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
            p "foo"
            p type

            addtional_type_spec = [type]
            genp = genp.inreg[0].genpoint

          when :nil?
            typemethodp = true
            type = LiteralType.new(nil.class, nil)

            addtional_type_spec = [type]
            genp = genp.inreg[0].genpoint
          end
        end
      end
      type = inst.inreg[0].flush_type(tup)[tup]
      condtype = type && type.size == 1 && type[0].class_object
      if condtype == NilClass or condtype == FalseClass then
        infer.inference_node(node.exit_link[bidx], tup, node.exit_reg, history)
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
      ntup = infer.typetupletab.get_tupple_id(intype, tup)
      recvtypes = recreg.get_type(tup)

      @@ruby_methodtab[name] ||= {}
      misirep = nil
      #p name
      recvtypes.each do |ty|
        existf = false
        slf = ty.class_object
        if !slf.is_a?(Module) then
          next
        end
        slf.ancestors.each do |slfcls|
          irep = nil

          irepssa = @@ruby_methodtab[name][slfcls]
          if irepssa.nil? then
            irep = Irep::get_irep_instance(slf, name)
            if irep == nil or name == :call then
              if @@ruletab[:METHOD][name] and @@ruletab[:METHOD][name][slfcls] then
                # written in C or method mmsing  or no method error
                existf = true
                @@ruletab[:METHOD][name][slfcls].call(infer, inst, node, tup)

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
            infer.inference_block(irepssa, intype, ntup, argc)
            outreg.add_same irepssa.retreg
            existf = true
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
            infer.inference_block(irepssa, intype, ntup, argc)
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
      intype = nil
      if argc == 127 then
        intype = []
        type = ContainerType.new(Array)
        intype[0] = inst.inreg[0].flush_type(tup)[tup] || []
        inst.inreg[1..-2].each_with_index do|ar, i|
          reg = RiteSSA::Reg.new(inst)
          reg.add_same ar
          reg.flush_type(tup)
          type.element[i] = reg
        end
        intype[1] = [type]
        intype[2] = inst.inreg[-1].flush_type(tup)[tup] || []
      else
        intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      end
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
          arrele.each do |idx, reg|
            reg.add_same valreg
            reg.flush_type_alltup(tup)
          end
        end
      end

      inst.inreg[0].genpoint.inreg[0].add_same inst.inreg[0]
      inst.outreg[0].add_same inst.inreg[0]
      nil
    end
  end
end


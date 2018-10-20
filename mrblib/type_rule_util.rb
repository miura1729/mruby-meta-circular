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

    def self.get_original_reg(infer, inst, tup)
      case inst.op
      when :MOVE
        return inst.inreg[0]

      when :SEND
        case inst.para[0]
        when :[]
          rectype = inst.inreg[0].get_type(tup)
          idxtype = inst.inreg[1].get_type(tup)
          if rectype.size == 1 then
            if rectype[0].class_object == Array then
              if idxtype.size == 1 and
                  idxtype[0].is_a?(MTypeInf::LiteralType) then
                return rectype[0].element[idxtype[0].val]
              else
                return rectype[0].element[ContainerType::UNDEF_VALUE]
              end

            else
              return nil
            end
          end
          return nil

        else
          return nil
        end
      else
        return nil
      end
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
        #  genp = genp.inreg[0].genpoint
        end
      end

      type = inst.inreg[0].flush_type(tup)[tup]
      if type && type.size == 1 then
        condtype = type[0].class_object
      else
        condtype = nil
      end
      if condtype == NilClass or condtype == FalseClass then
        enode = node.exit_link[bidx]
        while enode.ext_iseq.size == 1 and
            (enode.ext_iseq[0].op == :JMPIF or enode.ext_iseq[0].op == :JMPNOT)
          enode = enode.exit_link[bidx]
        end
        infer.inference_node(enode, tup, node.exit_reg, history)
        true

      elsif type and type.size == 1 then
        enode = node.exit_link[1 - bidx]
        while enode.ext_iseq.size == 1 and
            (enode.ext_iseq[0].op == :JMPIF or enode.ext_iseq[0].op == :JMPNOT)
          enode = enode.exit_link[1 - bidx]
        end
        infer.inference_node(enode, tup, node.exit_reg, history)
        true

      elsif typemethodp then
        idx = notp ? bidx : 1 - bidx
        nd = node.exit_link[idx]
        if greg = get_original_reg(infer, genp, tup) then
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
        else
          infer.inference_node(node.exit_link[0], tup, node.exit_reg, history)
          infer.inference_node(node.exit_link[1], tup, node.exit_reg, history)
        end

        true
      else
        nil
      end
    end

    def self.handle_exception(infer, inst, node, tup, outreg, argc, history)
      rescuetab = node.root.rescuetab
      pos = rescuetab.pop
      if pos then
        infer.exception.clear
        rescuenode = node.root.nodes[pos]
        infer.inference_node(rescuenode, tup, inst.para[2], history)
      end

      infer.exception.each do |exreg|
        excreg = node.root.export_exception
        excreg.add_same exreg
        excreg.flush_type(tup)
      end
    end

    def self.make_ssablock(p0)
      upper = p0.upper
      tclass = p0.target_class
      parent = nil
      if upper then
        pproc = make_ssablock(upper)
        if pproc then
          parent = pproc.irep
        end
      end
      irep = Irep::get_proc_irep(p0)
      if irep then
        blk = RiteSSA::Block.new(irep, parent, tclass, p0.strict?)
        ProcType.new(Proc, blk, tclass, [], [])
      else
        nil
      end
    end

    def self.rule_send_common_aux(infer, inst, node, tup, name, intype, recreg, outreg, argc, history)
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
          # GC bug?
          a = name
          b = slfcls
          procssa = @@ruby_methodtab[name][slfcls]
          irepssa = nil
          if procssa.nil? then
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
                procssa = @@ruby_methodtab[:method_missing][cls]
                irepssa = procssa.irep

              else
                # No method found
              end
            else
              p0 = Proc::search_proc(slf, name)
              procssa = make_ssablock(p0)
              @@ruby_methodtab[name][slfcls] = procssa
              irepssa = procssa.irep
              clsobj = RiteSSA::ClassSSA.get_instance(slfcls)
              clsobj.method[name] = irepssa
            end
          else
            irepssa = procssa.irep
          end

          if irepssa then
            intype[0] = [ty]
            ntup = infer.typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), tup)
            infer.inference_block(irepssa, intype, ntup, argc, procssa)
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
          irep = Irep::get_irep_instance(slf, :method_missing)
          if irep then
            p0 = Proc::search_proc(slf, :method_missing)
            procssa = make_ssablock(p0)
            @@ruby_methodtab[name][slfcls] = procssa
            irepssa = procssa.irep
            clsobj = ClassSSA.get_instance(slfcls)
            clsobj.method[name] = irepssa
            intype[0] = [ty]
            ntup = infer.typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), tup)
            infer.inference_block(irepssa, intype, ntup, argc, procssa)
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
      if infer.exception.size > 0 then
        # exception happen
        handle_exception(infer, inst, node, tup, outreg, argc, history)
      end
      nil
    end

    def self.rule_send_common(infer, inst, node, tup, history)
      name = inst.para[0]
      argc = inst.para[1]
      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      recreg = inst.inreg[0]

      rule_send_common_aux(infer, inst, node, tup, name, intype, recreg, inst.outreg[0], argc, history)
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

      if inst.inreg[0].class == RiteSSA::Reg then
        inst.inreg[0].genpoint.inreg[0].add_same inst.inreg[0]
        inst.inreg[0].genpoint.inreg[0].flush_type(tup)
      end
      inst.outreg[0].add_same inst.inreg[0]
      inst.outreg[0].flush_type(tup)
      nil
    end
  end
end


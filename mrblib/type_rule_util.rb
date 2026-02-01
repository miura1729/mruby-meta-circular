module MTypeInf
  class GlobalExit<Exception
  end

  class TypeInferencer
    @@ruby_methodtab ||= {}

    def self.get_ruby_methodtab
      @@ruby_methodtab
    end

    def self.rule_literal_common(infer, inst, node, tup)
      val = inst.para[0]
      type = LiteralType.new(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    def self.get_original_reg(infer, inst, tup)
      case inst.op
      when :MOVE, :GETUPVAR, :START
        return inst.inreg[0]

      when :GETIV
        return inst.inreg[0]

      when :ENTER
        return true

      when :SEND
        case inst.para[0]
        when :[]
          rectype = inst.inreg[0].get_type(tup)
          idxtype = inst.inreg[1].get_type(tup)
          unless rectype and idxtype
#            raise GlobalExit.new
          end
          if rectype and rectype.size == 1 then
            if rectype[0].class_object == Array or rectype[0].class_object == Hash then
              if idxtype.size == 1 and
                  idxtype[0].is_a?(MTypeInf::LiteralType) then
                return inst.outreg[0]
              else
                return inst.outreg[0]
              end

            else
              return nil
            end
          end
          return nil

        else
          return inst.outreg[0]
        end

      when :LT
        return inst.inreg[0].genpoint.inreg[0]

      else
        return nil
      end
    end

    def self.get_jmp_target(node, bidx, inst)
      curop = inst.op
      if curop == :JMPIF then
        bidx = 1 - bidx
      end
      enode = node.exit_link[bidx]
      while enode.ext_iseq.size == 1 and
          (enode.ext_iseq[0].op == :JMPIF or enode.ext_iseq[0].op == :JMPNOT)
        if enode.ext_iseq[0].op == curop then
          enode = enode.exit_link[bidx]
        else
          enode = enode.exit_link[1 - bidx]
        end
      end
      enode
    end

    def self.rule_jmpif_common(infer, inst, node, tup, history, bidx)
      begin
        notp = false
        typemethodp = false

        genp = inst.inreg[0].genpoint
        if genp.is_a?(Fixnum) then
          genp = history[nil][-1].exit_reg[genp].genpoint
        end

        if genp.is_a?(RiteSSA::Inst) then
          while genp.op == :SEND and genp.para[0] == :!
            notp = !notp
            genp = genp.inreg[0].genpoint
          end

          case(genp.op)
          when :SEND
            addtional_type_spec = nil
            atype_spec_pos = nil
            atype_spec_neg = nil
            case genp.para[0]
            when :kind_of?, :is_a?
              typemethodp = true
              tcls = genp.inreg[1].flush_type(tup)[tup]
              genp = genp.inreg[0].genpoint
              atype_reg = get_original_reg(infer, genp, tup)
              cls = nil
              if tcls.size == 1 and tcls[0].val.class == Class then
                cls = tcls[0].val
              end
              type = PrimitiveType.new(cls)

              addtional_type_spec = [type]
              atype_spec_pos = addtional_type_spec
              atype_spec_neg = addtional_type_spec
              #            genp = genp.inreg[0].genpoint

            when :nil?
              typemethodp = true
              genp = genp.inreg[0].genpoint
              atype_reg = get_original_reg(infer, genp, tup)
              type = PrimitiveType.new(NilClass)

              addtional_type_spec = [type]
              atype_spec_pos = addtional_type_spec
              atype_spec_neg = addtional_type_spec
              #            genp = genp.inreg[0].genpoint

            when :===
              type = genp.inreg[1].get_type(tup)[0]
              pt = nil
              if type.is_a?(MTypeInf::LiteralType) then
                genp = genp.inreg[1].genpoint
                typemethodp = true
                pt = PrimitiveType.new(type.class_object)
              end

              if genp.outreg[0] and pt then
                type = genp.outreg[0].get_type(tup)[0]
                if type.is_a?(MTypeInf::LiteralType) then
                  genp = genp.inreg[0].genpoint
                  typemethodp = true
                  bool = (type.class_object == pt.class_object)
                  pt = PrimitiveType.new(bool.class)
                  addtional_type_spec = [pt]
                  atype = [pt]
                  atype_spec_pos = addtional_type_spec
                  atype_spec_neg = addtional_type_spec
                end
              end
              atype_reg = get_original_reg(infer, genp, tup)

            else
              #typemethodp = true
              #genp = genp.inreg[0].genpoint
              #atype_reg = get_original_reg(infer, genp, tup)
              #notp = !notp
              #type0 = PrimitiveType.new(NilClass)
              #type1 = PrimitiveType.new(FalseClass)

              #addtional_type_spec = [type0, type1]
              #atype_spec_pos = addtional_type_spec
              #atype_spec_neg = addtional_type_spec
            end

          when :EQ
            type = genp.inreg[0].get_type(tup)[0]
            if type.is_a?(MTypeInf::LiteralType) then
              genp = genp.inreg[1].genpoint
              typemethodp = true
              pt = PrimitiveType.new(type.class_object)
              if type.class_object == NilClass then
                addtional_type_spec = [pt]
                atype_spec_neg = addtional_type_spec
                atype_spec_pos = addtional_type_spec
              else
                atype_spec_pos = [pt]
                atype_spec_neg = [type]
              end
            end

            if genp.inreg[1] then
              type = genp.inreg[1].get_type(tup)[0]
              if type.is_a?(MTypeInf::LiteralType) then
                genp = genp.inreg[0].genpoint
                typemethodp = true
                pt = PrimitiveType.new(type.class_object)
                if type.class_object == NilClass then
                  addtional_type_spec = [pt]
                  atype_spec_pos = addtional_type_spec
                  atype_spec_neg = addtional_type_spec
                else
                  atype_spec_pos = [pt]
                  atype_spec_neg = [type]
                end
              end
            end
            atype_reg = get_original_reg(infer, genp, tup)

          when :GE
            type0 = genp.inreg[0].get_type(tup)[0]
            type1 = genp.inreg[1].get_type(tup)[0]

            if type1.is_a?(MTypeInf::LiteralType) and
                type1.val >= 0 then
              typemethodp = true
              type = NumericType.new(type0.class_object, true)
              atype_spec_pos = [type]
              atype_spec_neg = []
              atype_reg = genp.inreg[0].genpoint.inreg[0]
            end

          when :LT
            unless  genp.inreg[0].get_type(tup) and genp.inreg[1].get_type(tup)
              return nil
            end
            type0 = genp.inreg[0].get_type(tup)[0]
            type1 = genp.inreg[1].get_type(tup)[0]
            genp1 = genp.inreg[1].genpoint

            if type0.is_a?(MTypeInf::LiteralType) and
                type0.val >= 0 then
              typemethodp = true
              type = NumericType.new(type1.class_object, true)

              atype_spec_pos = [type]
              atype_spec_neg = []
              atype_reg = genp.inreg[1].genpoint.inreg[0]

            elsif genp1.is_a?(RiteSSA::Inst) and
                genp1.op == :SEND and
                (genp1.para[0] == :size or genp1.para[0] == :length) and
                (atype = genp1.inreg[0].type.values[0][0]).class_object == Array then
              typemethodp = true
              type = IndexOfArrayType.new(type0.class_object, atype, true)
              atype_spec_pos = [type]
              atype_spec_neg = []
              atype_reg = genp.inreg[0].genpoint.inreg[0]
            end

          when :ENTER
            typemethodp = true
            type0 = PrimitiveType.new(NilClass)
            type1 = PrimitiveType.new(false.class)

            addtional_type_spec = [type0, type1]
            atype_spec_pos = addtional_type_spec
            atype_spec_neg = addtional_type_spec
            notp = !notp
            atype_reg = get_original_reg(infer, genp, tup)

          else
            typemethodp = true
            type0 = PrimitiveType.new(NilClass)
            type1 = PrimitiveType.new(false.class)

            addtional_type_spec = [type0, type1]
            atype_spec_pos = addtional_type_spec
            atype_spec_neg = addtional_type_spec
            notp = !notp
            atype_reg = get_original_reg(infer, genp, tup)
          end
        end

        type = inst.inreg[0].flush_type(tup)[tup]
        atype = nil
        if genp.is_a?(RiteSSA::Inst) and genp.op != :ENTER then
          atype = genp.outreg[0].flush_type(tup)[tup]
        end

        if type && type.size == 1 then
          condtype = type[0].class_object

        elsif atype and addtional_type_spec and
            (atscl = addtional_type_spec.map {|e| e.class_object}).size > 0 and
            (atype.all? {|e| atscl.include?(e.class_object)} or
            atype.all? {|e| !atscl.include?(e.class_object)}) then
          condtype = atscl.include?(atype[0].class_object).class
        else
          condtype = nil
        end

        #p "#{inst.filename}:#{inst.line}"
        #p type.map {|e| e.class_object} if type
        #p atype.map {|e| e.class_object} if atype
        #p condtype
        #p typemethodp

        idx = inst.op == :JMPIF ? 1 - bidx : bidx
        if condtype == NilClass or condtype == FalseClass then
#        p type
#        p inst.line
#        p condtype
#        p bidx
#        p notp
#        p "foobar"
          enode = get_jmp_target(node, idx, inst)
#          p enode.ext_iseq[0].line
          history[node] ||= []
          history[nil] ||= []
          history[nil].push node
          if history[node].index(enode) == nil then
            history[node].push enode
            infer.inference_node(enode, tup, node.exit_reg, history)
          end
          history[nil].pop
          return true

        elsif condtype == TrueClass then
          enode = get_jmp_target(node, 1- idx, inst)
          history[node] ||= []
          history[nil] ||= []
          history[nil].push node
          if history[node].index(enode) == nil then
            history[node].push enode
            infer.inference_node(enode, tup, node.exit_reg, history)
          end
          history[nil].pop
          return true
        end

        if type and type.size == 1 then
          enode = get_jmp_target(node, 1 - idx, inst)
          ninst= enode.ext_iseq[0]
          history[node] ||= []
          history[nil] ||= []
          history[nil].push node
          if history[node].index(enode) == nil then
            history[node].push enode
            infer.inference_node(enode, tup, node.exit_reg, history)
          end
          history[nil].pop
          return true
        end

        if typemethodp then
          idx = notp ? idx : 1 - idx
          nd = get_jmp_target(node, idx, inst)

          if atype_reg == true then
            atype_reg = inst.inreg[0]
          end
          if atype_reg then
            atype_reg.positive_list.push atype_spec_pos
            atype_reg.refpoint.each do |ginst|
              if ginst.outreg[0] then
                ginst.outreg[0].positive_list.push atype_spec_pos
              end
            end
            history[node] ||= []
            history[nil] ||= []
            history[nil].push node
            rcthen = nil
            if history[node].index(nd) == nil then
              history[node].push nd
              rcthen = infer.inference_node(nd, tup, node.exit_reg, history)
            end
            history[nil].pop
            atype_reg.positive_list.pop
            atype_reg.refpoint.each do |ginst|
              if ginst.outreg[0] then
                ginst.outreg[0].positive_list.pop
              end
            end

            idx = 1 - idx
            nd = get_jmp_target(node, idx, inst)
            atype_reg.negative_list.push atype_spec_neg
            atype_reg.refpoint.each do |ginst|
              if ginst.outreg[0] then
                ginst.outreg[0].negative_list.push atype_spec_neg
              end
            end

            history[nil].push node
            rcelse = nil
            if history[node].index(nd) == nil then
              history[node].push nd
              rcelse = infer.inference_node(nd, tup, node.exit_reg, history)
            end
            history[nil].pop

            if rcthen == :return and false then
              # then part is return from current method, so outer block of if is
              # type limitation of else part.
              history[nd] ||= []
              nd.exit_link.each do |nd2|
                history[nil].push nd2
                if history[nd].index(nd2) == nil then
                  history[nd].push nd2
                  infer.inference_node(nd2, tup, nd.exit_reg, history)
                end
                history[nil].pop
              end
            end

            atype_reg.negative_list.pop
            atype_reg.refpoint.each do |ginst|
              if ginst.outreg[0] then
                ginst.outreg[0].negative_list.pop
              end
            end
            return true
          end
        end

        history[nil] ||= []
        history[node] ||= []
        nd = get_jmp_target(node, 0, inst)
        history[nil].push nd
        if history[node].index(nd) == nil then
          history[node].push nd
          infer.inference_node(nd, tup, node.exit_reg, history)
        end
        nd = get_jmp_target(node, 1, inst)
        history[nil][-1] = nd
        if history[node].index(nd) == nil then
          history[node].push nd
          infer.inference_node(nd, tup, node.exit_reg, history)
        end
        history[nil].pop

      rescue GlobalExit
      end

      true
    end

    def self.handle_exception(infer, inst, node, tup, outreg, argc, history)
      rescuetab = node.root.rescuetab
      handler = rescuetab.pop

      if handler then
        history[node] ||= []
        history[node].push handler
        infer.inference_node(handler, tup, inst.para[2], history)
        false

      else
        excreg = node.root.export_exception
        infer.exception.each do |excr|
          excreg.add_same excr
        end
        excreg.flush_type(tup)
        false
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
        ProcType.new(Proc, blk, tclass, nil,  [], [], pproc)
      else
        nil
      end
    end

    def self.make_intype(infer, inreg, node, tup, argc = nil)
      if argc == 127 then
        argary = inreg[1].get_type(tup)[0]
        argeles = argary.element
        if argeles then
          intype = [inreg[0].get_type(tup)]

          largc = argeles.size - 1
          largc.times do |i|
            intype[i + 1] = argeles[i].flush_type_alltup(tup)[tup]
          end

          intype.push inreg[2].get_type(tup)
          yield intype, intype.size - 2

        else
          inreg[0].flush_type(tup)
          inreg[2].flush_type(tup)
          arg0 = inreg[0].get_type(tup)
          arg2 = inreg[2].get_type(tup)
          yield [arg0, arg2], 0
        end
      else
        intype = inreg.map {|reg| reg.get_type(tup) || []}
        yield intype, argc
      end

      nil
    end

    def self.rule_send_common_aux(infer, inst, node, tup, name, intype, recreg, outreg, argc, history)
      if inst.para[5] == nil then
        lock_recreg = RiteSSA::Reg.new(inst)
        lock_recreg.setpoint.push inst
        inst.para[5] = lock_recreg
        lock_recreg.refpoint.push inst
      end

      inst.inreg[0].type.each do |tup, types|
        types.each do |type|
          inst.para[5].add_type type.clone, tup
        end
      end

      ntup = 0
      recvtypes = recreg.get_type(tup)

      @@ruby_methodtab[name] ||= {}
      misirep = nil
      irepssa = nil
      if recvtypes == nil then
        recvtypes = []
      end
      recvtypes.each do |ty|
        existf = false
        missprocssa = nil
        slf = ty.class_object_core

        if !slf.is_a?(Module) then
          next
        end

        slf.ancestors.each do |slfcls|
          irep = nil
          procssa = @@ruby_methodtab[name][slfcls]
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
                cont = @@ruletab[:METHOD][name2][slfcls]
                if cont == :reader then
                  clsobj = RiteSSA::ClassSSA.get_instance(slfcls)
                  name2ins = "@#{name2.to_s}".to_sym
                  ivreg = clsobj.get_iv(name2ins)
                  inst.outreg[0].add_same(ivreg)
                  inst.outreg[0].flush_type(tup, -1)
                  inst.line # for bug (reason is unkown)
                  #p name2ins
                  #p ivreg.id

                elsif cont == :writer then
                  clsobj = RiteSSA::ClassSSA.get_instance(slfcls)
                  name2ins = "@#{name2.to_s.chop}".to_sym
                  ivreg = clsobj.get_iv(name2ins)
                  ivreg.add_same(inst.inreg[1])
                  ivreg.flush_type(-1, tup)

                else
                  cont.call(infer, inst, node, tup, intype)
                end

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
            curirep = infer.callstack[-1][0]
            if !curirep.strict and irepssa.have_return then
              # Current method is block and send have_return method
              curirep.have_return = true
            end
            # intype[0] = [ty]
            ntup = infer.typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), tup)
            infer.callstack[-1][4] = [name, inst]

            irepssa.rasing = node.root.rasing
            rc = infer.inference_block(irepssa, intype, ntup, argc, procssa)
            curirep.call_blocks[irepssa] ||= {}
            recvtypes.each do |ty|
              curirep.call_blocks[irepssa][ty] = nil
            end
            if irepssa.effects[:iv_read] or irepssa.effects[:iv_write] then
              orgrecreg = inst.inreg[0]
              if orgrecreg.genpoint.is_a?(RiteSSA::Inst) then
                tmpreg = get_original_reg(infer, orgrecreg.genpoint, tup)
                if tmpreg.is_a?(RiteSSA::Inst) then
                  orgrecreg = tmpreg
                end
              end
              curirep.objregtab[orgrecreg] ||= {}
              if irepssa.effects[:iv_read] and !irepssa.effects[:iv_write] then
                ivevent = [:iv_read, irepssa.effects[:iv_read][0]]
                curirep.objregtab[orgrecreg][inst] = ivevent
              end
              if irepssa.effects[:iv_write] and !irepssa.effects[:iv_read] then
                ivevent = [:iv_write, irepssa.effects[:iv_write][0]]
                curirep.objregtab[orgrecreg][inst] = ivevent
              end

              if curirep.objregtab[orgrecreg].size > 1 then
                slfreg = orgrecreg.clone
                slfreg.refpoint.push inst
                slfreg.setpoint = orgrecreg.setpoint.clone
                # for simple code
                slfreg.setpoint.push inst
                curirep.objregtab[orgrecreg][nil] = slfreg
              end
            end
            if rc then
              # Retry AI
              have_evar = false
              oldmexe = infer.must_execute
              lmdinst = inst.inreg[-1].genpoint
              while lmdinst.is_a?(RiteSSA::Inst) and lmdinst.op == :MOVE
                lmdinst = lmdinst.inreg[0].genpoint
              end

              if lmdinst.is_a?(RiteSSA::Inst) and lmdinst.op == :LAMBDA then
                # return without execute.
                exregs = lmdinst.para[1]
                if exregs.size > 0 and outreg and outreg.type[tup].nil? then
                  # p "exit #{name} #{tup} #{outreg.id}"
                  infer.must_execute ||= {}
                  infer.inference_block(irepssa, intype, ntup, argc, procssa)
                  infer.must_execute = oldmexe
                  outreg.type[tup] = irepssa.retreg.type[ntup]
                end
              end
            end

            if outreg then
              outreg.add_same irepssa.retreg
              existf = true
            end
          end

          if existf then
            break
          end

          # method missing
          irep = Irep::get_irep_instance(slf, :method_missing)
          if (irep or
              (@@ruby_methodtab[:method_missing] and
              @@ruby_methodtab[:method_missing][slfcls])) and
              !missprocssa then
            if @@ruby_methodtab[:method_missing] and
                @@ruby_methodtab[:method_missing][slfcls] then
              missprocssa = @@ruby_methodtab[:method_missing][slfcls]
            else
              p0 = Proc::search_proc(slf, :method_missing)
              missprocssa = make_ssablock(p0)
              @@ruby_methodtab[:method_missing] ||= {}
              @@ruby_methodtab[:method_missing][slfcls] = missprocssa
              irepss = missprocssa.irep
              clsobj = RiteSSA::ClassSSA.get_instance(slf)
              clsobj.method[:method_missing] = irepss
            end
          end
        end

        if missprocssa then
          irepssa = missprocssa.irep
          #intype[0] = [ty]
          ncls = SymbolType.instance(Symbol, name)
          intype = [intype[0], [ncls]] + intype[1..-1]
          ntup = infer.typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), tup)
          infer.inference_block(irepssa, intype, ntup, argc + 1, missprocssa)
          outreg.add_same irepssa.retreg
          existf = true
        end

        if !existf and !infer.continue then
          p "Method missing able to call #{slf}##{name}"
          mess = "Method missing able to call #{slf}##{name} in #{inst.line}:#{inst.filename}\n"
          print mess #for debug
          infer.messages[mess] ||= 0
          infer.messages[mess] += 1
        end
      end

      outreg.flush_type(tup, ntup)
      if irepssa and irepssa.have_return then
        retreg = node.root.retreg2
        outreg2 = irepssa.retreg2
        retreg.add_same outreg2
        retreg.flush_type(tup)
      end
      node.root.import_regs.each do |reg|
        types = reg.type[-1]
        if types then
          reg.type[tup] = types.dup
        end
      end

      if infer.exception.size > 0 then
        # exception happen
        return handle_exception(infer, inst, node, tup, outreg, argc, history)
      end
      nil
    end

    def self.rule_send_common(infer, inst, node, tup, history)
      name = inst.para[0]
      argc = inst.para[1]
      rc = nil
      make_intype(infer, inst.inreg, node, tup, argc) do |intype, argc2|
        recreg = inst.inreg[0]

        rc = rule_send_common_aux(infer, inst, node, tup, name, intype, recreg, inst.outreg[0], argc2, history)
      end
      rc
    end

    def self.rule_send_cfimc(infer, inst, node, tup)
    end

    def self.rule_kernel_send(infer, inst, node, tup, intype)
      if inst.inreg[1].use_value == nil then
        infer.continue = true
        inst.inreg[1].set_use_value
      end

      mname = intype[1]
      intype = [intype[0]] + intype[2..-1]
      outr = inst.outreg[0]
      mname.each do |sym|
        mn = sym.val
        rule_send_common_aux(infer, inst, node, tup, mn, intype, inst.inreg[0], outr, intype.size, nil)
      end
      nil
    end

    def self.rule_ary_push_common(infer, inst, node, tup)
      arrtypes = inst.inreg[0].type[tup]
      valreg = inst.inreg[1]
      valreg.flush_type(tup)
      if valreg.type[tup] then
        valreg.type[tup].each do |ty|
          ty.place[true] = true
        end
      end

      arrtypes.each do |arrt|
        arrt.place[true] = true
        if arrt.class_object_core == Array then
          arrele = arrt.element

          arrele[1] ||= RiteSSA::Reg.new(nil)
          arrele[1].add_same valreg
          valreg.add_same arrele[1]
          arrele[1].flush_type(tup)

          arrele[ContainerType::UNDEF_VALUE].add_same valreg
          valreg.add_same  arrele[ContainerType::UNDEF_VALUE]
          arrele[ContainerType::UNDEF_VALUE].flush_type_alltup(tup)
        end
      end

      if inst.inreg[0].class == RiteSSA::Reg and inst.inreg[0].genpoint then
        inst.inreg[0].genpoint.outreg[0].add_same inst.inreg[0]
        inst.inreg[0].genpoint.outreg[0].flush_type(tup)
      end
      inst.outreg[0].add_same inst.inreg[0]
      inst.outreg[0].flush_type(tup)
      nil
    end

    def self.rule_compare_common(infer, inst, node, tup)
      arg0types = inst.inreg[0].flush_type(tup)[tup]
      arg1types = inst.inreg[1].flush_type(tup)[tup]
      if arg0types and arg1types then
        arg0type = arg0types[0]
        arg1type = arg1types[0]
        if arg0types.size == 1 and arg1types.size == 1 and
            arg0type.is_a?(LiteralType) and arg1type.is_a?(LiteralType) then
          res = yield(arg0type.val, arg1type.val)
          if res then
            type = LiteralType.new(TrueClass, true)
          else
            type = LiteralType.new(FalseClass, false)
          end
          inst.outreg[0].add_type(type, tup)

        else
          type = LiteralType.new(TrueClass, true)
          inst.outreg[0].add_type(type, tup)
          type = LiteralType.new(FalseClass, false)
          inst.outreg[0].add_type(type, tup)
        end
      end

      nil
    end

    def self.rule_yield_passed_block(infer, inst, node, tup, proc, type, inreg)
      make_intype(infer, inreg, node, tup, inst.para[1]) do |intype, argc|
        intype[0] = proc
#        intype[0] = [type]
#        intype = [proc] + intype
        ninst = RiteSSA::Inst.new(33, proc[0].irep, 0, node, 33) #33 is :send maybe
        ninst.para.push :call
        ninst.para.push argc + 1
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

        rule_send_common_aux(infer, ninst, node, tup, :call, intype, ninst.inreg[0], dmyreg, argc, nil)

        if type then
          inst.outreg[0].add_type type, tup
        else
#          inst.outreg[0].add_type dmyreg.get_type(tup)[0], tup
        end
      end
    end

    def self.realvalue_from_container_type(type, tup)
      result = nil
      if type.class_object == Hash then
        result = {}
        type.element.each do |key, value|
          if key != ContainerType::UNDEF_VALUE then
            result[key] = value
          end
        end

      elsif type.class_object == Array then
        result = []
        type.element.each do |key, value|
          if key != ContainerType::UNDEF_VALUE then
            result[key] = value
          end
        end
      else
        raise "Not support yet #{types[0]}"
      end

      result
    end

    def self.collect_features
      @@ruby_method_tab.each do |name, clsmtab|
        clsmtab.each do |cls, block|
        end
      end
    end
  end
end


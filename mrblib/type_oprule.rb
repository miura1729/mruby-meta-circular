module MTypeInf
  class TypeInferencer
    @@ruletab ||= {}

    def self.define_inf_rule_op(name, &block)
      @@ruletab[:OP] ||= {}
      if @@ruletab[:OP][name] then
        raise "Already defined #{name}"
      end
      @@ruletab[:OP][name] = block
    end

    define_inf_rule_op :START do |infer, inst, node, tup, history|
      selfty = inst.inreg[0].get_type(tup)[0]
      node.root.call_blocks.each do |blk, tys|
        if tys.keys[0] and selfty and tys.keys[0].class_object_core == selfty.class_object_core then
          if blk.effects[:call_iv_read] or blk.effects[:iv_read] then
            node.root.effects[:call_iv_read] = true
          end

          if blk.effects[:call_iv_write] or
              blk.effects[:iv_write] or
              blk.effects[:iv_write_lockfree] then
            node.root.effects[:call_iv_write] = true
          end
        end
      end

      inst.inreg.each_with_index do |dmy, idx|
        inst.inreg[idx].type.each do |tp, types|
          types.each do |type|
            inst.outreg[idx].add_type type.clone, tp
          end
        end
      end
      nil
    end

    define_inf_rule_op :NOP do |infer, inst, node, tup, history|
      nil
    end

    define_inf_rule_op :MOVE do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type(tup)
      inst.outreg[0].negative_list = inst.inreg[0].negative_list.clone
      inst.outreg[0].positive_list = inst.inreg[0].positive_list.clone
      olddreg = inst.para[0]
      [].push node.class        #  for GC bug (q & d hack)
      while olddreg.is_a?(RiteSSA::Reg) do
        if node.root.export_regs.include?(olddreg) then
          olddreg.add_same(inst.inreg[0])
          olddreg.flush_type(tup)
        end
        ins = olddreg.genpoint
        if ins.is_a?(RiteSSA::Inst) then
          olddreg = ins.para[0]
        else
          break
        end
      end
      nil
    end

    define_inf_rule_op :LOADL do |infer, inst, node, tup, history|
      rule_literal_common(infer, inst, node, tup)
    end

    define_inf_rule_op :LOADI do |infer, inst, node, tup, history|
      rule_literal_common(infer, inst, node, tup)
    end

    define_inf_rule_op :LOADSYM do |infer, inst, node, tup, history|
      val = inst.para[0]
      type = SymbolType.instance(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LOADNIL do |infer, inst, node, tup, history|
      rule_literal_common(infer, inst, node, tup)
      nil
    end

    define_inf_rule_op :LOADSELF do |infer, inst, node, tup, history|
      inst.inreg[0].flush_type(tup)
      inst.outreg[0].add_type(inst.inreg[0].type[tup][0], tup)
      nil
    end

    define_inf_rule_op :LOADT do |infer, inst, node, tup, history|
      rule_literal_common(infer, inst, node, tup)
    end

    define_inf_rule_op :LOADF do |infer, inst, node, tup, history|
      rule_literal_common(infer, inst, node, tup)
    end

    define_inf_rule_op :GETGLOBAL do |infer, inst, node, tup, history|
      case inst.para[0]
      when :$/
        level = infer.callstack.size
        previrep = infer.callstack.map {|e|  [e[0], e[4]]}
        type = StringType.new(String, inst, previrep, level)
        type.place[true] = true
        inst.outreg[0].add_type type, tup

      else
        inst.inreg[0].flush_type_alltup(tup)
        inst.outreg[0].add_same(inst.inreg[0])
        inst.outreg[0].flush_type(tup)
      end
      nil
    end

    define_inf_rule_op :SETGLOBAL do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
      types = inst.outreg[0].flush_type(tup)[tup]

      # update place infomation
      if types then
        types.each do |ty|
#          ty.place[true] = :GETGLOBAL
        end
      end
      if true then
        p inst.para[0]
        p infer.typetupletab.rev_table[tup]
        p inst.line
        p inst.inreg[0].id
        p inst.inreg[0].type
        p inst.inreg[0].get_type(tup)
        p inst.inreg[0].negative_list
        p inst.inreg[0].positive_list
        p tup
        p inst.inreg[0].type
#        p inst.inreg[0].type[tup].map {|ty| ty} if inst.inreg[0].type[tup]
        inst.inreg[0].type.each do |tp, tys|
          p "#{tp} #{tys.map {|ty| ty.place.keys}}"
          p inst.inreg[0].is_escape?(tup)
          p "#{tp} #{tys.map {|ty| ty.place.values}}"
          p "#{tp} #{tys.map {|ty| ty.place.keys}}"
#          p "#{tp} #{tys.map {|ty| ty.place}}"
        end
      end
      nil
    end

    #  define_inf_rule_op :GETSPECIAL do |infer, inst, node, tup, history|
    #  end

    #  define_inf_rule_op :SETSPECIAL do |infer, inst, node, tup, history|
    #  end

    define_inf_rule_op :GETIV do |infer, inst, node, tup, history|
      inreg = inst.inreg[0]

      #      inreg.flush_type_alltup(tup)
      inst.outreg[0].add_same(inreg)
      inst.outreg[0].flush_type(tup, -1)
      if inst.outreg[0].type[tup] == nil then
        inst.outreg[0].add_type PrimitiveType.new(NilClass), tup
      end

      node.root.effects[:iv_read] ||= []
      node.root.effects[:iv_read].push [inst.para[0], inst, inreg]
      #p inst.para[0]
      nil
    end

    define_inf_rule_op :SETIV do |infer, inst, node, tup, history|
      valreg = inst.inreg[0]
      valtype = valreg.flush_type(tup)[tup]

      slf = inst.inreg[1]
      # update place infomation
      previrep = infer.callstack.map {|e| [e[0], e[4]]}
      curirep = infer.callstack[-1][0].irep

      # self is immutable so type is defined statically
      slftype = slf.flush_type(tup)[tup][0]
      slfiv = inst.outreg[0]
      oty = slfiv.type[-1]
      if oty then
        oty = oty.dup
      end
      slfiv.add_same(valreg)
      cty = slfiv.flush_type(-1, tup)[-1]
      #p inst.para[0]
      #p slfiv.id

      if oty != cty then
#        slftype.version += 1
      end

      if valtype then
        valtype.each do |ty|
          ty.place[slftype] = [curirep, previrep, :SETIV, inst.line]
        end
      end

      genp = valreg.genpoint
      if (false and [:ADD, :ADDI, :SUB, :SUBI].include?(genp.op) and
          (genpa = genp.inreg[0].genpoint) and
          genpa.is_a?(RiteSSA::Inst) and
          genpa.op == :GETIV) and
          genpa.para[0] == inst.para[0] or

          [:LOADI].include?(genp.op) then
        node.root.effects[:iv_write_lockfree] ||= []
        node.root.effects[:iv_write_lockfree].push [inst.para[0], inst, slfiv, slf]
      else
        node.root.effects[:iv_write] ||= []
        node.root.effects[:iv_write].push [inst.para[0], inst, slfiv, slf]
      end

      nil
    end

    define_inf_rule_op :GETCV do |infer, inst, node, tup, history|
      inst.inreg[0].flush_type_alltup(tup)
      inst.outreg[0].add_same(inst.inreg[0])
      #p inst.para[0]
      nil
    end

    define_inf_rule_op :SETCV do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type_alltup(tup)
      nil
    end

    define_inf_rule_op :GETCONST do |infer, inst, node, tup, history|
      name = inst.para[0]
      proc = node.root
      type = nil
      constreg = proc.target_class.constant[name]
      if constreg then
        inst.outreg[0].add_same(constreg)
        type = inst.outreg[0].flush_type_alltup(tup)[0]
      end

      if type == nil then
        notfound = [nil]
        const = notfound
        while proc and !const
          const = proc.target_class.const_get(name)
          proc = proc.parent
        end

        if const == notfound then
          begin
            const = node.root.target_class.class_object.const_get(name)
          rescue NameError
            begin
              const = Object.const_get(name)
            rescue NameError
            end
          end
        end

        #      inst.para.push const
        cls = TypeSource[const.class]
        type = nil
        if cls == ContainerType then
          level = infer.callstack.size
          previrep =  infer.callstack.map {|e|  [e[0], e[4]]}
          type = cls.new(const.class, inst, previrep, level)
          if const.class == Hash
            const.each do |key, value|
              keyt = LiteralType.new(key.class, key)
              type.element[key] ||= RiteSSA::Reg.new(inst)
              #type.element[key].add_type(keyt, tup)
              if key != ContainerType::UNDEF_VALUE then
                type.key.add_type keyt, 0
              end
            end
          end

        elsif cls then
          type = LiteralType.new(const.class, const)

        elsif const.is_a?(Class) and const.ancestors.index(Exception) then
          type = LiteralType.new(const.class, const)

        else
          if const.is_a?(Module) then
            type = LiteralType.new(const.singleton_class, const)
          else
            type = LiteralType.new(const.class, const)
            type.place[true] = true
          end
        end
        inst.outreg[0].add_type(type, tup)
      end
      nil
    end

    define_inf_rule_op :SETCONST do |infer, inst, node, tup, history|
      inst.inreg[0].flush_type(tup)
      inst.outreg[0].add_same(inst.inreg[0])

      types = inst.outreg[0].flush_type_alltup(tup)[tup]
      value = nil
      case types[0]
      when LiteralType
        value = types[0].val

      when ContainerType
        value = realvalue_from_container_type(types[0], tup)

      else
        raise "Not support yet #{types[0]}"
      end

      node.root.target_class.const_set(inst.para[0], value)
      # update place infomation
      if types then
        types.each do |ty|
          ty.place[true] = [:SETCONST, inst.line]
        end
      end
      nil
    end

    define_inf_rule_op :GETMCNST do |infer, inst, node, tup, history|
      name = inst.para[0]
      srccls = inst.inreg[0].flush_type(tup)[tup][0].class_object

      const = RiteSSA::ClassSSA.get_instance(srccls).const_get(name)
      cls = TypeSource[const.class]
      type = nil
      if cls then
        type = cls.new(const.class, const)

      elsif const.is_a?(Class) and const.ancestors.index(Exception) then
        type = LiteralType.new(const.class, const)

      else
        if const.is_a?(Module) then
          type = LiteralType.new(const.singleton_class, const)
        else
          type = LiteralType.new(const.class, const)
        end
      end
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :GETUPVAR do |infer, inst, node, tup, history|
      up = inst.para[1]
      frame = inst.para[0]
      pos = inst.para[2]
      proc = infer.callstack[-1][3]
      stpos = proc.tups.index {|item| item[0] == frame}
      otup = proc.tups[stpos][1]

      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type(tup, otup)
      inst.outreg[0].negative_list = inst.inreg[0].negative_list.clone
      inst.outreg[0].positive_list = inst.inreg[0].positive_list.clone

      nil
    end

    define_inf_rule_op :SETUPVAR do |infer, inst, node, tup, history|
      up = inst.para[1]
      frame = inst.para[0]
      pos = inst.para[2]
      proc = infer.callstack[-1][3]
      stpos = proc.tups.index {|item| item[0] == frame}
      otup = proc.tups[stpos][1]
      inst.outreg[0].add_same(inst.inreg[0])
      types = inst.outreg[0].flush_type(otup, tup)[otup]
      inst.outreg[0].type[-1] = types.dup
      # update place infomation
      if types then
        types.each do |ty|
          ty.place[proc] = :SETUPVAR
        end
      end

      nil
    end

    define_inf_rule_op :JMP do |infer, inst, node, tup, history|
      history[nil] ||= []
      history[nil].push node
      infer.inference_node(node.exit_link[0], tup, node.exit_reg, history)
      history[nil].pop
      true
    end

    define_inf_rule_op :JMPIF do |infer, inst, node, tup, history|
      rule_jmpif_common(infer, inst, node, tup, history, 0)
    end

    define_inf_rule_op :JMPNOT do |infer, inst, node, tup, history|
      rule_jmpif_common(infer, inst, node, tup, history, 1)
    end

    define_inf_rule_op :ONERR do |infer, inst, node, tup, history|
      node.root.rescuetab.push inst.para[0]

      history[nil] ||= []
      history[nil].push node
      history[node] ||= []

      enode = node.exit_link[0]
      history[node].push enode
      infer.inference_node(enode, tup, node.exit_reg, history)

      history[nil].pop

      true
    end

    define_inf_rule_op :RESCUE do |infer, inst, node, tup, history|
      if inst.para[0] == 0 then
        inst.outreg[0].add_same infer.exception.pop
        inst.outreg[0].flush_type(tup)
      else
        exobj = inst.inreg[0].flush_type(tup)[tup]
        excls = inst.inreg[1].flush_type(tup)[tup]
        if exobj and excls and
            exobj.all? {|e| exobj[0].class_object == e.class_object}  and
            excls.all? {|e| excls[0].val == e.val} then
          if exobj[0].class_object == excls[0].val then
            type = PrimitiveType.new(TrueClass)
            inst.outreg[0].add_type type, tup
          else
            type = PrimitiveType.new(FalseClass)
            inst.outreg[0].add_type type, tup
          end
        else
          type = PrimitiveType.new(TrueClass)
          inst.outreg[0].add_type type, tup
          type = PrimitiveType.new(FalseClass)
          inst.outreg[0].add_type type, tup
        end
      end

      nil
    end

    define_inf_rule_op :POPERR do |infer, inst, node, tup, history|
      node.root.rescuetab.pop

      nil
    end

    define_inf_rule_op :RAISE do |infer, inst, node, tup, history|
      reg = RiteSSA::Reg.new(nil)
      infer.exception.push reg
      reg.add_same inst.inreg[0]
      reg.flush_type(tup)

      true
    end

    define_inf_rule_op :ENTER do |infer, inst, node, tup, history|
      rc = nil
      ax = inst.para[0]
      m1 = (ax >> 18) & 0x1f
      o = (ax >> 13) & 0x1f
      r = (ax >> 12) & 0x1
      m2 = (ax >> 7) & 0x1f
      #      inreg = inst.inreg.clone
      inreg = node.enter_reg[1..-1]
      uv = ContainerType::UNDEF_VALUE
      argc = infer.callstack[-1][2]
      len = m1 + o + r + m2

      # Process for many arguments
      psize = inst.para[1].size
      n = inreg.size
      while n <= psize
        inreg[n] = inst.para[1][n + 1]
        n = n + 1
      end
      if argc == 127 then
        certup = infer.callstack[-2][1]
        arytypes = inreg[0].flush_type(tup)[tup]
        arytypes.each do |arytype|
          ele = arytype.element
          anum = ele.keys.size - 1
          (m1 + o).times do |i|
            if ele[i] then
              inst.outreg[i].add_same ele[i]
              inst.outreg[i].flush_type(tup, certup)
            end
          end
          if r == 1 then
            type = inst.objcache[tup]
            if !type then
              level = infer.callstack.size
              previrep =  infer.callstack.map {|e|  [e[0], e[4]]}
              inst.objcache[tup] = type = ContainerType.new(Array, inst, previrep, level)
            end

            (anum - m1 - o).times do |i|
              nreg = type.element[i] || RiteSSA::Reg.new(inst)
              unreg = type.element[uv]
              if ele[m1 + o +  i] then
                nreg.add_same ele[m1 + o +  i]
                unreg.add_same ele[m1 + o +  i]
              else
                nreg.add_same ele[uv]
                unreg.add_same ele[uv]
              end
              nreg.flush_type_alltup(tup)
              unreg.flush_type_alltup(tup)
              type.element[i] = nreg
            end

            inst.outreg[m1 + o].add_type(type, tup)
          end

          inst.outreg[len].add_same inreg[1]
          inst.outreg[len].flush_type(tup)
        end
      else
        inreg[0].flush_type(tup)
        arg0 = inreg[0].get_type(tup)
        cls = nil
        argv = nil
        if arg0 and arg0[0] then
          cls = arg0[0].class_object
        end

        if len > 1 and argc == 1 and cls == Array then
          argv = arg0[0].element
          argv.each do |k, r|
            r.flush_type_alltup(tup)
          end
        else
          argv = inreg
        end

        (m1 + o).times do |i|
          if argv[i] then
            inst.outreg[i].add_same argv[i]
          elsif argv[ContainerType::UNDEF_VALUE] then
            inst.outreg[i].add_same argv[ContainerType::UNDEF_VALUE]
          end
          inst.outreg[i].flush_type(tup)
        end

        if r == 1 then
          type = inst.objcache[nil]
          if !type then
            level = infer.callstack.size
            previrep = infer.callstack.map {|e|  [e[0], e[4]]}
            inst.objcache[nil] = type = ContainerType.new(Array, inst, previrep, level)
          end

          (argc - m1 - o).times do |i|
            nreg = type.element[i] || RiteSSA::Reg.new(inst)
            unreg = type.element[uv]
            nreg.add_same argv[m1 + o +  i]
            unreg.add_same argv[m1 + o +  i]
            nreg.flush_type(tup)
            unreg.flush_type(tup)
            type.element[i] = nreg
          end

          inst.outreg[m1 + o].type[tup] = [type]
          inst.outreg[m1 + o + 1].add_same inreg[argc]
          inst.outreg[m1 + o + 1].flush_type(tup)
        else
          inst.outreg[m1 + o].add_same argv[m1 + o]
          inst.outreg[m1 + o].type[tup].inspect # for mruby bug
          inst.outreg[m1 + o].same.inspect # for mruby bug
          inst.outreg[m1 + o].flush_type(tup)
        end

        if o != 0 and argc > m1 + m2 then
          if r == 1 then
            pos = argc - m1 - m2
            if pos >= inst.para[3].size then
              pos = inst.para[3].size - 1
            end

          else
            pos = argc - m1 - m2
          end
          nnode = inst.para[3][pos]
          ereg = [inst.inreg[0]] + inst.outreg
          infer.inference_node(nnode, tup, ereg, history)

          rc = true
        end
      end

      rc
    end

    define_inf_rule_op :ARGARY do |infer, inst, node, tup, history|
      ax = inst.para[0]
      m1 = (ax >> 10) & 0x3f
      r = (ax >> 9) & 0x1
      m2 = (ax >> 4) & 0x1f
      lv = (ax >> 0) & 0xf

      nil
    end

    define_inf_rule_op :SEND do |infer, inst, node, tup, history|
      inst.inreg[0..-2].each do |reg|
        reg.flush_type(tup)
      end
      inst.inreg[-1].add_type(PrimitiveType.new(NilClass), tup)

      rule_send_common(infer, inst, node, tup, history)
      nil
    end

    define_inf_rule_op :SENDB do |infer, inst, node, tup, history|
      inst.inreg.each do |reg|
        reg.flush_type(tup)
      end

      rule_send_common(infer, inst, node, tup, history)
    end

    define_inf_rule_op :SUPER do |infer, inst, node, tup, history|
      intype = inst.inreg.map { |reg|
        reg.flush_type(tup)[tup]
      }
      name = infer.callstack[-2][4][0]

      reccls = intype[0][0].class_object
      supcls = reccls.superclass
      level = infer.callstack.size
      previrep = infer.callstack.map {|e|  [e[0], e[4]]}
      rect = UserDefinedType.new(supcls, inst, previrep, level)
      recreg = RiteSSA::Reg.new(nil)
      recreg.add_type rect, tup
      oreg = inst.outreg[0]
      rule_send_common_aux(infer, inst, node, tup, name, intype, recreg, oreg, inst.para[1], nil)
#      rule_send_common(infer, inst, node, tup, history)
      nil
    end

    define_inf_rule_op :RETURN do |infer, inst, node, tup, history|
      dmyreg = RiteSSA::Reg.new(inst)
      types = inst.inreg[0].get_type(tup)
      if types then
        types.each do |ty|
          dmyreg.add_type ty, tup
        end
      end
      inst.outreg[0].add_same(dmyreg)
      otup = nil
      if inst.para[0] == 2 then
        frame = inst.para[1]
        stpos = infer.callstack.index {|item| item[0] == frame}
        otup = infer.callstack[stpos][1]
        inst.outreg[0].flush_type(otup, tup)

        inst.outreg[1].add_same(inst.inreg[0])
        inst.outreg[1].flush_type(otup, tup)

        inst.outreg[2].add_same(inst.inreg[0])
        otup = infer.callstack[-1][1]
        inst.outreg[2].flush_type(otup, tup)
      else
        otup = infer.callstack[-1][1]
        inst.outreg[0].flush_type(otup, tup)
        inst.outreg[0].type
      end

      if false and !inst.outreg[0].type[otup] then
        p inst.outreg[0].type
        p inst.inreg[0].type
        p inst.inreg[0].genpoint.op if inst.inreg[0].genpoint.is_a?(RiteSSA::Inst)
        p inst.line
        p inst.filename
        p "FOOO"
      end
      :return
    end

    define_inf_rule_op :BLKPUSH do |infer, inst, node, tup, history|
      inst.outreg[0].add_same inst.inreg[0]
      nil
    end

    define_inf_rule_op :EQ do |infer, inst, node, tup, history|
      rule_compare_common(infer, inst, node, tup) {|x, y|
        x == y
      }
    end

    define_inf_rule_op :LT do |infer, inst, node, tup, history|
      rule_compare_common(infer, inst, node, tup) {|x, y|
        x < y
      }
    end

    define_inf_rule_op :LE do |infer, inst, node, tup, history|
      rule_compare_common(infer, inst, node, tup) {|x, y|
        x <= y
      }
    end

    define_inf_rule_op :GT do |infer, inst, node, tup, history|
      rule_compare_common(infer, inst, node, tup) {|x, y|
        x > y
      }
    end

    define_inf_rule_op :GE do |infer, inst, node, tup, history|
      rule_compare_common(infer, inst, node, tup) {|x, y|
        x >= y
      }
    end

    define_inf_rule_op :ADD do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = inst.inreg[1].flush_type(tup)[tup]

      arg0cls = arg0type[0].class_object
      if !(arg0cls == Fixnum or arg0cls == Float) then
        @@ruletab[:METHOD][:+][arg0cls].call(infer, inst, node, tup)

      else
        if arg1type and arg1type[0].class_object == Float then
          ty = NumericType.new(Float, false)
          inst.outreg[0].add_type ty, tup

        elsif arg0type then
          if arg1type then
            if arg1type.any? {|e| e.class_object == NilClass} then
              inst.outreg[0].add_type PrimitiveType.new(NilClass), tup
            end

            arg0type.each do |ty|
              if ty.is_a?(LiteralType) then
                ty = NumericType.new(ty.class_object, false)
              end
              inst.outreg[0].add_type ty, tup
            end
          end
        end
      end
      nil
    end

    define_inf_rule_op :SUB do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = inst.inreg[1].flush_type(tup)[tup]
      arg0cls = arg0type[0].class_object

      if !(arg0cls == Fixnum or arg0cls == Float) then
        @@ruletab[:METHOD][:-][arg0cls].call(infer, inst, node, tup)

      else
        if arg1type and arg1type[0].class_object == Float then
          ty = NumericType.new(Float, false)
          inst.outreg[0].add_type ty, tup

        elsif arg0type then
          arg0type.each do |ty|
            ty = NumericType.new(ty.class_object, false)
            inst.outreg[0].add_type ty, tup
          end
        end
      end
      nil
    end

    define_inf_rule_op :MUL do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = inst.inreg[1].flush_type(tup)[tup]

      if arg1type and arg1type[0].class_object == Float then
        ty = NumericType.new(Float, false)
        inst.outreg[0].add_type ty, tup

      elsif arg0type then
        arg0type.each do |ty|
          if ty.is_a?(LiteralType) then
            ty = NumericType.new(ty.class_object, false)
          end
          inst.outreg[0].add_type ty, tup
        end
      end
      nil
    end

    define_inf_rule_op :DIV do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = inst.inreg[1].flush_type(tup)[tup]

      if arg0type then
        if arg0type[0].class_object == Fixnum or
            arg0type[0].class_object == Float then
#          ty = PrimitiveType.new(Float)
          ty = NumericType.new(arg0type[0].class_object, false)
          inst.outreg[0].add_type ty, tup
        end
      end

      nil
    end

    define_inf_rule_op :ADDI do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = LiteralType.new(Fixnum, inst.para[1])

      if arg0type then
        arg0cls = arg0type[0].class_object
        if !(arg0cls == Fixnum or arg0cls == Float) then
          @@ruletab[:METHOD][:+][arg0cls].call(infer, inst, node, tup)

        else
          if arg0type then
            arg0type.each do |ty|
              if ty.is_a?(LiteralType) then
                postive = ty.val >= 0 && inst.para[1] >= 0
                ty = NumericType.new(ty.class_object, postive)
              end
              #ty = PrimitiveType.new(ty.class_object)
              inst.outreg[0].add_type ty, tup
            end
          end
        end
      end
      nil
    end

    define_inf_rule_op :SUBI do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = LiteralType.new(Fixnum, inst.para[1])

      if arg0type then
        arg0cls = arg0type[0].class_object

        if !(arg0cls == Fixnum or arg0cls == Float) then
          if @@ruletab[:METHOD][:-] then
            @@ruletab[:METHOD][:-][arg0cls].call(infer, inst, node, tup)
          else
            @@ruletab[:METHOD][:method_missing][arg0cls].call(infer, inst, node, tup)
          end

        else
          if arg0type then
            arg0type.each do |ty|
              if ty.is_a?(LiteralType) and ty.val >= inst.para[1] then
                ty = NumericType.new(ty.class_object, true)
              else
                ty = NumericType.new(ty.class_object, false)
              end
              inst.outreg[0].add_type ty, tup
            end
          end
        end
      end
      nil
    end

    define_inf_rule_op :ARRAY do |infer, inst, node, tup, history|
      pirep = infer.callstack[-2][0]
      if pirep and (!pirep.strict or true) then
        pirep = pirep.irep
      else
        pirep = nil
      end
      type = inst.objcache[pirep]
      if !type then
        level = infer.callstack.size
        previrep = infer.callstack.map {|e|  [e[0], e[4]]}
        inst.objcache[pirep] = type = ContainerType.new(Array, inst, previrep, level)
      end
      nilreg = type.element[ContainerType::UNDEF_VALUE]
      inst.para[0].times do |i|
        nreg = type.element[i] || RiteSSA::Reg.new(inst.inreg[i].genpoint)
        nreg.add_same inst.inreg[i]
        nreg.flush_type(tup)
        stype = inst.inreg[i].type
        stype.keys.each do |ttup|
          types = stype[ttup]
          types.each do |ty|
            ty.place[type] = :ARRAY
          end
        end
        type.element[i] = nreg
        nilreg.add_same nreg
      end

      nilreg.flush_type(tup)
      node.root.allocate_reg[tup] ||= []
      regs = node.root.allocate_reg[tup]
      reg = inst.outreg[0]
      regs.push reg #if !regs.include?(reg)
      inst.outreg[0].add_type type, tup
    end

    define_inf_rule_op :ARYCAT do |infer, inst, node, tup, history|
      arrtype = inst.inreg[0].flush_type(tup)[tup][0]
      eletype = inst.inreg[1].flush_type(tup)[tup][0]

      bpos = arrtype.element.keys.size - 2 # - 2 means include UNDEF_VALUE
      inst.para[0] = bpos
      uv = ContainerType::UNDEF_VALUE
      if !eletype.is_a?(ContainerType) then
        arrtype.element[bpos] = inst.inreg[1].dup
        arrtype.element[uv].add_same inst.inreg[1]
      else
        eletype.element.each do |key, reg|
          if key.is_a?(Fixnum) then
            arrtype.element[key] ||= RiteSSA::Reg.new(nil)
            arrtype.element[key].add_same reg
            arrtype.element[key].flush_type(tup)
            arrtype.element[uv] ||= RiteSSA::Reg.new(nil)
            arrtype.element[uv].add_same reg
            arrtype.element[uv].flush_type(tup)
          end
        end
      end

      inst.outreg[0].add_type arrtype, tup
      node.root.allocate_reg[tup] ||= []
      regs = node.root.allocate_reg[tup]
      reg = inst.outreg[0]
      regs.push reg #if !regs.include?(reg)
      nil
    end

    define_inf_rule_op :AREF do |infer, inst, node, tup, history|
      arrtype = inst.inreg[0].flush_type(tup)[tup]
      idx = inst.para[0]

      arrtype.each do |at|
        if at.class_object == Array then
          types = at.element[idx].get_type(tup)
          types.each do |ty|
            inst.outreg[0].add_type ty, tup
          end

        else
          if idx == 0 then
            inst.outreg[0].add_same inst.inreg[0]
          else
            ty = PrimitiveType.new(NilClass)
            inst.outreg[0].add_type ty, tup
          end
        end
      end
      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_op :STRING do |infer, inst, node, tup, history|
      rule_literal_common(infer, inst, node, tup)
    end

    define_inf_rule_op :STRCAT do |infer, inst, node, tup, history|
      strty0 = inst.inreg[0].flush_type(tup)[tup].select {|e| e.is_a?(LiteralType)}
      strty1 = inst.inreg[1].flush_type(tup)[tup].select {|e| e.is_a?(LiteralType)}
      if inst.outreg[0].use_value then
        # use value mode only
        if inst.outreg[0].type[tup] and inst.outreg[0].type[tup].size > 0 then
          inst.outreg[0].type[tup].clear
        end

        if strty0.size == strty1.size then
          i = 0
          strty0.each do |strty0ele|
            strty1ele = strty1[i]
            val = strty0ele.val.to_s + strty1ele.val.to_s
            inst.outreg[0].add_type LiteralType.new(String, val), tup
            i = i + 1
          end

        elsif strty0.size < strty1.size then
          strty1.each do |strty1ele|
            strty0ele = strty0[0]
            val = strty0ele.val.to_s + strty1ele.val.to_s
            inst.outreg[0].add_type LiteralType.new(String, val), tup
          end

        else
          strty0.each do |strty0ele|
            strty1ele = strty1[0]
            val = strty0ele.val.to_s + strty1ele.val.to_s
            inst.outreg[0].add_type LiteralType.new(String, val), tup
          end
        end

      else
        level = infer.callstack.size
        previrep = infer.callstack.map {|e|  [e[0], e[4]]}
        type = StringType.new(String, inst, previrep, level)
        inst.outreg[0].add_type type, tup
      end

      inst.outreg[0].type[tup].each do |ty|
        ty.place[true] = [:STRCAT, inst.line]
      end
      node.root.effects[:modify] ||= {}
      node.root.effects[:modify][inst] = inst.inreg[0].type
      nil
    end

    define_inf_rule_op :LAMBDA do |infer, inst, node, tup, history|
      slf = inst.inreg[0].flush_type(tup)[tup]
      envtypes = inst.para[1]
      cproc = infer.callstack[-1][3]
      cstop = infer.callstack[-1]
      previrep = infer.callstack[-2][0]
      if previrep and !previrep.strict then
        previrep = previrep.irep
      else
        previrep = nil
      end
      if cproc then
        tups = [[cstop[0], cstop[1]]] + cproc.tups
      else
        tups = [[cstop[0], cstop[1]]]
      end
      pty = inst.objcache[previrep]
      if !pty then
        pty = ProcType.new(Proc, inst.para[0], slf, inst.inreg[0],  envtypes, tups, cproc)
        inst.objcache[previrep] = pty
      end
      pty.slf = slf
      pty.tups[0][1] = cstop[1]
      inst.outreg[0].add_type pty, tup
      if cproc then
        cproc.place[pty] = [:LAMBDA, "#{inst.filename}##{inst.line}"]
      end
      node.root.effects[:lambda] ||= []
      node.root.effects[:lambda].push [inst, inst.para[0]]

      nil
    end

    define_inf_rule_op :HASH do |infer, inst, node, tup, history|
      type = inst.objcache[nil]
      if !type then
        level = infer.callstack.size
        previrep = infer.callstack.map {|e|  [e[0], e[4]]}
        inst.objcache[nil] = type = ContainerType.new(Hash, inst, previrep, level)
      end
      type.element[ContainerType::UNDEF_VALUE] ||= RiteSSA::Reg.new(nil)
      udefreg = type.element[ContainerType::UNDEF_VALUE]
      udefreg.add_type PrimitiveType.new(NilClass), tup
      inst.para[0].times do |i|
        nreg = type.element[i] || RiteSSA::Reg.new(nil)
        nreg.add_same inst.inreg[i * 2 + 1]
        idxtypes = inst.inreg[i * 2].flush_type(tup)[tup]
        if idxtypes then
          if idxtypes[0] and idx = idxtypes[0].val then
            type.element[idx] = nreg
          end
          idxtypes.each do |idxt|
            type.key.add_type idxt, 0
          end
        end
        udefreg.add_same inst.inreg[i * 2 + 1]
      end
      udefreg.flush_type(tup)

      node.root.allocate_reg[tup] ||= []
      regs = node.root.allocate_reg[tup]
      reg = inst.outreg[0]
      regs.push reg #if !regs.include?(reg)
      type.place[true] = true
      inst.outreg[0].add_type type, tup
    end

    define_inf_rule_op :CLASS do |infer, inst, node, tup, history|
      name = inst.para[0]
      base = inst.inreg[0].flush_type(tup)[tup]
      if base[0].class_object == NilClass then
        outer = node.root.target_class
      else
        outer = base[0].class_object
      end
      sup = inst.inreg[1].flush_type(tup)[tup]
      supobj = sup[0].class_object
      if supobj == NilClass then
        supobj = Object
      end
      regcls = nil
      cls = inst.objcache[supobj]
      if !cls then
        begin
          regcls = Object.const_get(name)
        rescue
        end
        if regcls then
          inst.objcache[supobj] = regcls
          cls = regcls
        else
          begin
            cls = Class.new(supobj)
          rescue
            p sup[0].val
            cls = Class.new(sup[0].val)
          end
          inst.objcache[supobj] = cls
        end
        clsobj = RiteSSA::ClassSSA.get_instance(cls)
        outer.const_set(name, cls)
      end

      clstype = LiteralType.new(cls.singleton_class, cls)
      inst.outreg[0].add_type clstype, tup
      nil
    end

    define_inf_rule_op :MODULE do |infer, inst, node, tup, history|
      name = inst.para[0]
      base = inst.inreg[0].flush_type(tup)[tup]
      if base[0].class_object == NilClass then
        outer = node.root.target_class
      else
        outer = base[0].class_object
      end
      mod = inst.objcache[nil]
      if !mod then
        begin
          regmod = Object.const_get(name)
        rescue
        end
        if regmod then
          inst.objcache[nil] = regmod
          mod = regmod
        else
          mod = Module.new
          inst.objcache[nil] = mod
        end
        modobj = RiteSSA::ClassSSA.get_instance(mod)
      end

      modtype = LiteralType.new(mod.class, mod)
      outer.const_set(name, mod)
      inst.outreg[0].add_type modtype, tup
      nil
    end

    define_inf_rule_op :EXEC do |infer, inst, node, tup, history|
      tclass = inst.inreg[0].flush_type(tup)[tup]
      irep = inst.para[0]
      root = node.root
      co = tclass[0].val
      irepssa = inst.objcache[co]
      if !irepssa then
        irepssa = RiteSSA::Block.new(irep, root, co, true)
        # START instruction is disable
        orginst = irepssa.nodes[0].ext_iseq[0]
        orginst.outreg[0].add_same orginst.inreg[0]
        ninst = RiteSSA::Inst.new(0, irep, 0, self, 0)
        ninst.inreg[0] = orginst.inreg[0]
        ninst.outreg[0] = orginst.outreg[0]
        irepssa.nodes[0].ext_iseq[0] = ninst
        inst.objcache[co] = irepssa
      end
      intype = [tclass]
      ntup = infer.typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), tup)
      infer.inference_block(irepssa, intype, ntup, 0, nil)
      inst.outreg[0].add_same irepssa.retreg
      inst.outreg[0].flush_type_alltup(tup)
      nil
    end

    define_inf_rule_op :METHOD do |infer, inst, node, tup, history|
      tclass = inst.inreg[0].flush_type(tup)[tup][0].val
      method = inst.inreg[1].flush_type(tup)[tup][0]
      name = inst.para[0]
      ruby_methodtab = get_ruby_methodtab
      ruby_methodtab[name] ||= {}
      ruby_methodtab[name][tclass] = method
      tclobj = RiteSSA::ClassSSA.get_instance(tclass)
      saairep = method.irep
      tclobj.method[name] = saairep
      saairep.strict = true
      nil
    end

    define_inf_rule_op :TCLASS do |infer, inst, node, tup, history|
      tcls = node.root.target_class.class_object
      tclobj = LiteralType.new(tcls.class, tcls)
      inst.outreg[0].add_type tclobj, tup
      nil
    end

    define_inf_rule_op :RANGE do |infer, inst, node, tup, history|
      type = inst.objcache[nil]
      if !type then
        level = infer.callstack.size
        previrep = infer.callstack.map {|e|  [e[0], e[4]]}
        inst.objcache[nil] = type = RangeType.new(Range, inst, previrep, level)
      end
      nreg = type.element[0] || RiteSSA::Reg.new(nil)
      nreg.add_same inst.inreg[0]
      type.element[0] = nreg
      type.element[0].flush_type(tup)

      nreg = type.element[1] || RiteSSA::Reg.new(nil)
      nreg.add_same inst.inreg[1]
      type.element[1] = nreg
      type.element[1].flush_type(tup)

      nreg = type.element[2] || RiteSSA::Reg.new(nil)
      if inst.para[0] == 1 then
        nreg.add_type LiteralType.new(TrueClass, true), tup
      else
        nreg.add_type LiteralType.new(FalseClass, false), tup
      end
      type.element[2] = nreg
      type.element[2].flush_type(tup)

      node.root.allocate_reg[tup] ||= []
      regs = node.root.allocate_reg[tup]
      reg = inst.outreg[0]
      regs.push reg #if !regs.include?(reg)
      inst.outreg[0].add_type type, tup
    end
  end
end

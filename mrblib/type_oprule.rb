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

    define_inf_rule_op :MOVE do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
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
      inst.inreg[0].flush_type_alltup(tup)
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type(tup)
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
        p inst.line
        p inst.outreg[0].type
        p inst.outreg[0].get_type(tup)
        p inst.inreg[0].negative_list
        p inst.inreg[0].positive_list
        p tup
        p inst.inreg[0].type
        p inst.inreg[0].type[tup].map {|ty| ty} if inst.inreg[0].type[tup]
        inst.inreg[0].type.each do |tp, tys|
#          p "#{tp} #{tys.map {|ty| ty.place.keys}}"
          p "#{tp} #{tys.map {|ty| ty.place}}"
        end
      end
      nil
    end

    #  define_inf_rule_op :GETSPECIAL do |infer, inst, node, tup, history|
    #  end

    #  define_inf_rule_op :SETSPECIAL do |infer, inst, node, tup, history|
    #  end

    define_inf_rule_op :GETIV do |infer, inst, node, tup, history|
      name = inst.para[0]
      slf = inst.inreg[1]
      slf.flush_type(tup)[tup].each do |slftype|
        slfcls = slftype.class_object
        inreg = RiteSSA::ClassSSA.get_instance(slfcls).get_iv(name)
        hometown = slftype.hometown

        #      inreg.flush_type_alltup(tup)
        inst.outreg[0].add_same(inreg)
        inst.outreg[0].flush_type(tup, -1)
        #p inst.para[0]
      end
      nil
    end

    define_inf_rule_op :SETIV do |infer, inst, node, tup, history|
      valreg = inst.inreg[0]
      valtype = valreg.flush_type(tup)[tup]
      name = inst.para[0]

      slf = inst.inreg[1]
      # update place infomation
      previrep = infer.callstack[-2][0].irep
      curirep = infer.callstack[-1][0].irep

      slf.flush_type(tup)[tup].each do |slftype|
        slfcls = slftype.class_object
        slfiv = RiteSSA::ClassSSA.get_instance(slfcls).get_iv(name)
        hometown = slftype.hometown
        oty = slfiv.type[-1]
        if oty then
          oty = oty.dup
        end
        slfiv.add_same(valreg)
        cty = slfiv.flush_type(-1, tup)[-1]
        if oty != cty then
          slftype.version += 1
        end

        if valtype then
          valtype.each do |ty|
            ty.place[slftype] = [curirep, previrep, :SETIV, inst.line]
          end
        end
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
        const = nil
        while proc and !const
          const = proc.target_class.const_get(name)
          proc = proc.parent
        end

        #      inst.para.push const
        cls = TypeSource[const.class]
        type = nil
        if cls then
          type = LiteralType.new(const.class, const)

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
      end
      nil
    end

    define_inf_rule_op :SETCONST do |infer, inst, node, tup, history|
      inst.inreg[0].flush_type(tup)
      inst.outreg[0].add_same(inst.inreg[0])

      types = inst.outreg[0].flush_type_alltup(tup)[tup]
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

      nil
    end

    define_inf_rule_op :RESCUE do |infer, inst, node, tup, history|

      nil
    end


    define_inf_rule_op :POPERR do |infer, inst, node, tup, history|
      node.root.rescuetab.pop

      nil
    end

    define_inf_rule_op :ENTER do |infer, inst, node, tup, history|
      rc = nil
      ax = inst.para[0]
      m1 = (ax >> 18) & 0x1f
      o = (ax >> 13) & 0x1f
      r = (ax >> 12) & 0x1
      m2 = (ax >> 7) & 0x1f
      inreg = inst.inreg.clone
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
              previrep = infer.callstack[-2][0].irep
              inst.objcache[tup] = type = ContainerType.new(Array, inst, previrep, level)
              type.element[uv] = RiteSSA::Reg.new(nil)
            end

            (anum - m1 - o).times do |i|
              nreg = type.element[i] || RiteSSA::Reg.new(nil)
              if ele[m1 + o +  i] then
                nreg.add_same ele[m1 + o +  i]
                type.element[uv].add_same  ele[m1 + o +  i]
              else
                nreg.add_same ele[uv]
                type.element[uv].add_same  ele[uv]
              end
              nreg.flush_type_alltup(tup)
              type.element[i] = nreg
            end
            type.element[uv].flush_type(tup)

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
          inst.outreg[i].add_same argv[i]
          inst.outreg[i].flush_type(tup)
        end

        if r == 1 then
          type = inst.objcache[tup]
          if !type then
            level = infer.callstack.size
            previrep = infer.callstack[-2][0].irep
            inst.objcache[tup] = type = ContainerType.new(Array, inst, previrep, level)
            type.element[uv] = RiteSSA::Reg.new(nil)
          end

          (argc - m1 - o).times do |i|
            nreg = type.element[i] || RiteSSA::Reg.new(nil)
            nreg.add_same argv[m1 + o +  i]
            type.element[uv].add_same argv[m1 + o +  i]
            nreg.flush_type(tup)
            type.element[i] = nreg
          end
          type.element[uv].flush_type(tup)

          inst.outreg[m1 + o].type[tup] = [type]
          inst.outreg[m1 + o + 1].add_same inreg[argc]
          inst.outreg[m1 + o + 1].flush_type(tup)
        else
          inst.outreg[m1 + o].add_same argv[m1 + o]
          inst.outreg[m1 + o].flush_type(tup)
        end

        if o != 0 and argc > m1 + m2 then
          pos = argc - m1 - m2
          nnode = inst.para[3][pos]
          ereg = [node.enter_reg[0]] + inst.outreg
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
      previrep = infer.callstack[-2][0].irep
      rect = UserDefinedType.new(supcls, inst, previrep, level)
      recreg = RiteSSA::Reg.new(nil)
      recreg.add_type rect, tup
      oreg = inst.outreg[0]
      rule_send_common_aux(infer, inst, node, tup, name, intype, recreg, oreg, inst.para[1], nil)
#      rule_send_common(infer, inst, node, tup, history)
      nil
    end

    define_inf_rule_op :RETURN do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
      otup = nil
      if inst.para[0] == 2 then
        frame = inst.para[1]
        stpos = infer.callstack.index {|item| item[0] == frame}
        otup = infer.callstack[stpos][1]
        inst.outreg[0].flush_type(otup, tup)
      else
        otup = infer.callstack[-1][1]
        inst.outreg[0].flush_type(otup, tup)
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

    define_inf_rule_op :EQ do |infer, inst, node, tup, history|
      inst.inreg[0].flush_type(tup)
      inst.inreg[1].flush_type(tup)

      type = LiteralType.new(TrueClass, true)
      inst.outreg[0].add_type(type, tup)
      type = LiteralType.new(FalseClass, false)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    define_inf_rule_op :LT do |infer, inst, node, tup, history|
        @@ruletab[:OP][:EQ].call(infer, inst, node, tup)
    end

    define_inf_rule_op :LE do |infer, inst, node, tup, history|
        @@ruletab[:OP][:EQ].call(infer, inst, node, tup)
    end

    define_inf_rule_op :GT do |infer, inst, node, tup, history|
        @@ruletab[:OP][:EQ].call(infer, inst, node, tup)
    end

    define_inf_rule_op :GE do |infer, inst, node, tup, history|
        @@ruletab[:OP][:EQ].call(infer, inst, node, tup)
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
      nil
    end

    define_inf_rule_op :SUBI do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = LiteralType.new(Fixnum, inst.para[1])

      if arg0type then
        arg0type.each do |ty|
          if ty.is_a?(LiteralType) then
            ty = NumericType.new(ty.class_object, false)
          end
          #ty = PrimitiveType.new(ty.class_object)
          inst.outreg[0].add_type ty, tup
        end
      end
      nil
    end

    define_inf_rule_op :ARRAY do |infer, inst, node, tup, history|
      type = inst.objcache[nil]
      if !type then
        level = infer.callstack.size
        previrep = infer.callstack[-2][0].irep
        inst.objcache[nil] = type = ContainerType.new(Array, inst, previrep, level)
      end
      nilreg = type.element[ContainerType::UNDEF_VALUE]
      inst.para[0].times do |i|
        nreg = type.element[i] || RiteSSA::Reg.new(nil)
        nreg.add_same inst.inreg[i]
        nreg.flush_type(tup)
        stype = inst.inreg[i].type
        stype.keys.each do |ttup|
          types = stype[ttup]
          types.each do |ty|
            ty.place[nreg] = :ARRAY
          end
        end
        type.element[i] = nreg
        nilreg.add_same nreg
      end

      nilreg.flush_type(tup)
      node.root.allocate_reg[tup] ||= []
      node.root.allocate_reg[tup].push inst.outreg[0]
      inst.outreg[0].add_type type, tup
    end

    define_inf_rule_op :ARYCAT do |infer, inst, node, tup, history|
      arrtype = inst.inreg[0].flush_type(tup)[tup][0]
      eletype = inst.inreg[1].flush_type(tup)[tup][0]
      type = inst.objcache[tup]
      if !type then
        level = infer.callstack.size
        previrep = infer.callstack[-2][0].irep
        inst.objcache[tup] = type = ContainerType.new(Array, inst, previrep, level)
      end
      arrtype.element.each do |key, reg|
        type.element[key] ||= RiteSSA::Reg.new(nil)
        type.element[key].add_same reg
        type.element[key].flush_type(tup)
      end

      bpos = arrtype.element.keys.size - 1
      if !eletype.is_a?(ContainerType) then
        reg = RiteSSA::Reg.new(nil)
        reg.type[tup] = [eletype]
        type.element[bpos] = reg
      else
        eletype.element.each do |key, reg|
          if key.is_a?(Fixnum) then
            type.element[bpos + key] ||= RiteSSA::Reg.new(nil)
            type.element[bpos + key].add_same reg
            type.element[bpos + key].flush_type(tup)
          end
        end
      end

      inst.outreg[0].add_type type, tup
      node.root.allocate_reg[tup] ||= []
      node.root.allocate_reg[tup].push inst.outreg[0]
      nil
    end

    define_inf_rule_op :AREF do |infer, inst, node, tup, history|
      arrtype = inst.inreg[0].flush_type(tup)[tup]
      idx = inst.para[0]

      arrtype.each do |at|
        if at.class_object == Array then
          inst.outreg[0].add_same at.element[idx]

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
      level = infer.callstack.size
      previrep = infer.callstack[-2][0].irep
      type = StringType.new(String, inst, previrep, level)

#      inst.inreg[0].add_type type, tup
#      inst.inreg[1].add_type type, tup
      inst.outreg[0].add_type type, tup

      inst.outreg[0].type[tup].each do |ty|
        ty.place[true] = [:STRCAT, inst.line]
      end
      nil
    end

    define_inf_rule_op :LAMBDA do |infer, inst, node, tup, history|
      slf = inst.inreg[0].flush_type(tup)[tup][0]
      envtypes = inst.para[1]
      cproc = infer.callstack[-1][3]
      cstop = infer.callstack[-1]
      if cproc then
        tups = [[cstop[0], cstop[1]]] + cproc.tups
      else
        tups = [[cstop[0], cstop[1]]]
      end
      pty = inst.objcache[nil]
      if !pty then
        pty = ProcType.new(Proc, inst.para[0], slf, inst.inreg[0],  envtypes, tups, cproc)
        inst.objcache[nil] = pty
      end
      inst.outreg[0].add_type pty, tup
      if cproc then
        cproc.place[pty] = [:LAMBDA, "#{inst.filename}##{inst.line}"]
      end

      nil
    end

    define_inf_rule_op :HASH do |infer, inst, node, tup, history|
      type = inst.objcache[nil]
      if !type then
        level = infer.callstack.size
        previrep = infer.callstack[-2][0].irep
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
          idx = idxtypes[0].class_object
          type.element[idx] = nreg
          idxtypes.each do |idxt|
            type.key.add_type idxt, 0
          end
        end
        udefreg.add_same inst.inreg[i * 2 + 1]
      end
      udefreg.flush_type(tup)

      node.root.allocate_reg[tup] ||= []
      node.root.allocate_reg[tup].push inst.outreg[0]
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
        inst.objcache[co] = irepssa
      end
      intype = [tclass]
      ntup = infer.typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), tup)
      infer.inference_block(irepssa, intype, ntup, 0, nil)
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
      tclobj.method[name] = method.irep
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
        previrep = infer.callstack[-2][0].irep
        inst.objcache[nil] = type = ContainerType.new(Range, inst, previrep, level)
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
      nreg.add_same inst.inreg[2]
      type.element[2] = nreg
      type.element[2].flush_type(tup)

      node.root.allocate_reg[tup] ||= []
      node.root.allocate_reg[tup].push inst.outreg[0]
      inst.outreg[0].add_type type, tup
    end
  end
end

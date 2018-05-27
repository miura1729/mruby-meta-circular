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
      inst.inreg[0].flush_type(tup)[tup]
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].negative_list = inst.inreg[0].negative_list.clone
      inst.outreg[0].positive_list = inst.inreg[0].positive_list.clone
      nil
    end

    define_inf_rule_op :LOADL do |infer, inst, node, tup, history|
      rule_literal_commin(infer, inst, node, tup)
    end

    define_inf_rule_op :LOADI do |infer, inst, node, tup, history|
      rule_literal_commin(infer, inst, node, tup)
    end

    define_inf_rule_op :LOADSYM do |infer, inst, node, tup, history|
      rule_literal_commin(infer, inst, node, tup)
    end

    define_inf_rule_op :LOADNIL do |infer, inst, node, tup, history|
      rule_literal_commin(infer, inst, node, tup)
    end

    define_inf_rule_op :LOADSELF do |infer, inst, node, tup, history|
      inst.inreg[0].flush_type(tup)
      inst.outreg[0].add_type(inst.inreg[0].type[tup][0], tup)
      nil
    end

    define_inf_rule_op :LOADT do |infer, inst, node, tup, history|
      rule_literal_commin(infer, inst, node, tup)
    end

    define_inf_rule_op :LOADF do |infer, inst, node, tup, history|
      rule_literal_commin(infer, inst, node, tup)
    end

    define_inf_rule_op :GETGLOBAL do |infer, inst, node, tup, history|
      inst.inreg[0].flush_type_alltup(tup)
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_op :SETGLOBAL do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type_alltup(tup)
#      p inst.para[0]
#      p inst.outreg[0].flush_type_alltup(tup)
#      p tup
      nil
    end

    #  define_inf_rule_op :GETSPECIAL do |infer, inst, node, tup, history|
    #  end

    #  define_inf_rule_op :SETSPECIAL do |infer, inst, node, tup, history|
    #  end

    define_inf_rule_op :GETIV do |infer, inst, node, tup, history|
      inst.inreg[0].flush_type_alltup(tup)
      inst.outreg[0].add_same(inst.inreg[0])
      #p inst.para[0]
      nil
    end

    define_inf_rule_op :SETIV do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type_alltup(tup)
      nil
    end

    define_inf_rule_op :GETCONST do |infer, inst, node, tup, history|
      if inst.para[0].is_a?(RiteSSA::Storable) then
        inst.outreg[0].add_same(inst.para[0])
      else
        paracls = inst.para[0].class
        cls = TypeSource[paracls]
        type = nil
        if  cls then
          type = cls.new(paracls, inst.para[0])
        else
          type = LiteralType.new(paracls, inst.para[0])
        end
        inst.outreg[0].add_type(type, tup)
      end
      nil
    end

    define_inf_rule_op :GETUPVAR do |infer, inst, node, tup, history|
      up = inst.para[1]
      frame = inst.para[0]
      stpos = infer.callstack.index {|item| item[0] == frame}
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type(tup, infer.callstack[stpos][1])

      nil
    end

    define_inf_rule_op :SETUPVAR do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
      nil
    end

    define_inf_rule_op :JMP do |infer, inst, node, tup, history|
    end

    define_inf_rule_op :JMPIF do |infer, inst, node, tup, history|
      rule_jmpif_common(infer, inst, node, tup, history, 0)
    end

    define_inf_rule_op :JMPNOT do |infer, inst, node, tup, history|
      rule_jmpif_common(infer, inst, node, tup, history, 1)
    end

    define_inf_rule_op :ENTER do |infer, inst, node, tup, history|
      ax = inst.para[0]
      m1 = (ax >> 18) & 0x1f
      o = (ax >> 13) & 0x1f
      r = (ax >> 12) & 0x1
      m2 = (ax >> 7) & 0x1f

      argc = infer.typetupletab.rev_table[tup].size - 2
      len = m1 + o + r + m2

      if r == 1 then
        type = ContainerType.new(Array)
        if argc > len then
          (argc - m1 - o).times do |i|
            nreg = RiteSSA::Reg.new(nil)
            nreg.add_same inst.inreg[m1 + o +  i]
            type.element[i] = nreg
          end
        end
        inst.outreg[m1 + o].add_type type, tup
      else
        inst.outreg[m1 + o].add_same inst.inreg[m1 + o]
      end

      (m1 + o).times do |i|
        inst.outreg[i].add_same inst.inreg[i]
      end
      nil
    end

    define_inf_rule_op :SEND do |infer, inst, node, tup, history|
      inst.inreg[0..-2].each do |reg|
        reg.flush_type(tup)
      end
      inst.inreg[-1].add_type(LiteralType.new(NilClass, nil), tup)

      rule_send_common(infer, inst, node, tup)
    end

    define_inf_rule_op :SENDB do |infer, inst, node, tup, history|
      inst.inreg.each do |reg|
        reg.flush_type(tup)
      end

      rule_send_common(infer, inst, node, tup)
    end

    define_inf_rule_op :RETURN do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
      inst.outreg[0].flush_type(tup)
      nil
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

      if arg1type and arg1type[0].class_object == Float then
        arg1type.each do |ty|
          inst.outreg[0].add_type ty, tup
        end
      elsif arg0type then
        arg0type.each do |ty|
          inst.outreg[0].add_type ty, tup
        end
      end
      nil
    end

    define_inf_rule_op :SUB do |infer, inst, node, tup, history|
      @@ruletab[:OP][:ADD].call(infer, inst, node, tup)
    end

    define_inf_rule_op :MUL do |infer, inst, node, tup, history|
      @@ruletab[:OP][:ADD].call(infer, inst, node, tup)
    end

    define_inf_rule_op :DIV do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = inst.inreg[0].flush_type(tup)[tup]

      if arg0type then
        if arg0type[0].class_object == Fixnum or
            arg0type[0].class_object == Float then
          ty = PrimitiveType.new(Float)
          inst.outreg[0].add_type ty, tup
        end
      end

      nil
    end

    define_inf_rule_op :ADDI do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = LiteralType.new(Fixnum, inst.para[1])

      if arg0type then
        arg0type.each do |ty|
          inst.outreg[0].add_type ty, tup
        end
      end
      nil
    end

    define_inf_rule_op :SUBI do |infer, inst, node, tup, history|
        @@ruletab[:OP][:ADDI].call(infer, inst, node, tup)
    end

    define_inf_rule_op :ARRAY do |infer, inst, node, tup, history|
      type = inst.objcache
      if !type then
        inst.objcache = type = ContainerType.new(Array)
      end
      nilreg = RiteSSA::Reg.new(nil)
      type.element[nil] = nilreg
      inst.para[0].times do |i|
        nreg = RiteSSA::Reg.new(nil)
        nreg.add_same inst.inreg[i]
        type.element[i] = nreg
        nilreg.add_same inst.inreg[i]
      end

      inst.outreg[0].add_type type, tup
    end

    define_inf_rule_op :ARYCAT do |infer, inst, node, tup, history|
      type = ContainerType.new(Array)

      inst.inreg[0].add_type type, tup
      inst.inreg[1].add_type type, tup
      inst.outreg[0].add_type type, tup
      nil
    end

    define_inf_rule_op :STRING do |infer, inst, node, tup, history|
      rule_literal_commin(infer, inst, node, tup)
    end

    define_inf_rule_op :STRCAT do |infer, inst, node, tup, history|
      type = ContainerType.new(String)

      inst.inreg[0].add_type type, tup
      inst.inreg[1].add_type type, tup
      inst.outreg[0].add_type type, tup
      nil
    end

    define_inf_rule_op :LAMBDA do |infer, inst, node, tup, history|
      slf = inst.inreg[0].flush_type(tup)[tup][0]
      pty = ProcType.new(Proc, inst.para[0], slf)
      inst.outreg[0].add_type pty, tup
      nil
    end

    define_inf_rule_op :RANGE do |infer, inst, node, tup, history|
      type = inst.objcache
      if !type then
        inst.objcache = type = ContainerType.new(Range)
      end
      nreg = RiteSSA::Reg.new(inst)
      nreg.add_same inst.inreg[0]
      type.element[0] = nreg
      type.element[0].flush_type(tup)
      nreg = RiteSSA::Reg.new(inst)
      nreg.add_same inst.inreg[1]
      type.element[1] = nreg
      type.element[1].flush_type(tup)

      inst.outreg[0].add_type type, tup
    end
  end
end

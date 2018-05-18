module MTypeInf
  class TypeInferencer
    @@ruletab ||= {}
    @@ruby_methodtab ||= {}

    def self.define_inf_rule_op(name, &block)
      @@ruletab[:OP] ||= {}
      if @@ruletab[:OP][name] then
        raise "Already defined #{name}"
      end
      @@ruletab[:OP][name] = block
    end

    define_inf_rule_op :MOVE do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
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

    #  define_inf_rule_op :GETGLOBAL do |infer, inst, node, tup, history|
    #  end

    #  define_inf_rule_op :SETGLOBAL do |infer, inst, node, tup, history|
    #  end

    #  define_inf_rule_op :GETSPECIAL do |infer, inst, node, tup, history|
    #  end

    #  define_inf_rule_op :SETSPECIAL do |infer, inst, node, tup, history|
    #  end

    define_inf_rule_op :GETIV do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
      nil
    end

    define_inf_rule_op :SETIV do |infer, inst, node, tup, history|
      inst.outreg[0].add_same(inst.inreg[0])
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
    end

    define_inf_rule_op :JMPNOT do |infer, inst, node, tup, history|
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
          when :nil?
            typemethodp = true
            type = LiteralType.new(nil.class, nil)

            addtional_type_spec = [type]
            genp = genp.inreg[0].genpoint
          end
        end
      end

      if typemethodp then
        idx = notp ? 1 : 0
        nd = node.exit_link[idx]
        genp.inreg[0].positive_list.push addtional_type_spec
        genp.inreg[0].refpoint.each do |reg|
          reg.outreg[0].positive_list.push  addtional_type_spec
        end
        infer.inference_node(nd, tup, node.exit_reg, history)
        genp.inreg[0].positive_list.pop
        genp.inreg[0].refpoint.each do |reg|
          reg.outreg[0].positive_list.pop
        end

        idx = 1 - idx
        nd = node.exit_link[idx]
        genp.inreg[0].negative_list.push addtional_type_spec
        genp.inreg[0].refpoint.each do |reg|
          reg.outreg[0].negative_list.push addtional_type_spec
        end
        infer.inference_node(nd, tup, node.exit_reg, history)
        genp.inreg[0].negative_list.pop
        genp.inreg[0].refpoint.each do |reg|
          reg.outreg[0].negative_list.pop
        end

        true
      else
        nil
      end
    end

    define_inf_rule_op :ENTER do |infer, inst, node, tup, history|
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
      inst.inreg[0..-1].each do |reg|
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
        @@ruletab[:OP][:EQ].call(self, inst, node, tup)
    end

    define_inf_rule_op :LE do |infer, inst, node, tup, history|
        @@ruletab[:OP][:EQ].call(self, inst, node, tup)
    end

    define_inf_rule_op :GT do |infer, inst, node, tup, history|
        @@ruletab[:OP][:EQ].call(self, inst, node, tup)
    end

    define_inf_rule_op :GE do |infer, inst, node, tup, history|
        @@ruletab[:OP][:EQ].call(self, inst, node, tup)
    end

    define_inf_rule_op :ADD do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = inst.inreg[0].flush_type(tup)[tup]

      if arg0type[0].class_object == Fixnum or
          arg0type[0].class_object == Float then
        arg0type.each do |ty|
          inst.outreg[0].add_type ty, tup
        end
      end

      nil
    end

    define_inf_rule_op :SUB do |infer, inst, node, tup, history|
        @@ruletab[:OP][:ADD].call(self, inst, node, tup)
    end

    define_inf_rule_op :MUL do |infer, inst, node, tup, history|
        @@ruletab[:OP][:ADD].call(self, inst, node, tup)
    end

    define_inf_rule_op :DIV do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = inst.inreg[0].flush_type(tup)[tup]

      if arg0type[0].class_object == Fixnum or
          arg0type[0].class_object == Float then
        ty = PrimitiveType.new(Float)
        inst.outreg[0].add_type ty, tup
      end

      nil
    end

    define_inf_rule_op :ADDI do |infer, inst, node, tup, history|
      arg0type = inst.inreg[0].flush_type(tup)[tup]
      arg1type = LiteralType.new(Fixnum, inst.para[1])

      arg0type.each do |ty|
        inst.outreg[0].add_type ty, tup
      end
      nil
    end

    define_inf_rule_op :SUBI do |infer, inst, node, tup, history|
        @@ruletab[:OP][:ADDI].call(self, inst, node, tup)
    end

    define_inf_rule_op :ARRAY do |infer, inst, node, tup, history|
      type = ContainerType.new(Array)
      nilreg = RiteSSA::Reg.new(nil)
      type.element[nil] = nilreg
      inst.para[0].times do |i|
        nreg = RiteSSA::Reg.new(nil)
        nreg.add_same inst.inreg[i]
        type.element[i] = nreg
#        nreg.flush_type_alltup(tup)
        nilreg.add_same inst.inreg[i]
 #       nilreg.flush_type_alltup(tup)
      end

      inst.outreg[0].add_type type, tup
    end

    define_inf_rule_op :LAMBDA do |infer, inst, node, tup, history|
      pty = ProcType.new(Proc, inst.para[0])
      inst.outreg[0].add_type pty, tup
      nil
    end
  end
end

module CodeGenC
  class CodeGen
    @@ruletab ||= {}

    def self.define_ccgen_rule_op(name, &block)
      @@ruletab[:CCGEN] ||= {}
      if @@ruletab[:CCGEN][name] then
        raise "Already defined #{name}"
      end
      @@ruletab[:CCGEN][name] = block
    end

    define_ccgen_rule_op :MOVE do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LOADSELF do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LOADI do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LOADL do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LOADSYM do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LOADNIL do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :GETCONST do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :ENTER do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :JMP do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :JMPIF do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      r = 0
      inst.inreg[0].type[tup].each do |ty|
        if ty.class_object == NilClass or
            ty.class_object == FalseClass then
          r |= 1
        else
          r |= 2
        end
      end
      if cond == true or r == 2 then
        [node.exit_link[1]]
      elsif cond == false or r == 1 then
        [node.exit_link[0]]
      else
        ccgen.pcode << "if (#{cond}) goto L#{node.exit_link[1].id}; else goto L#{node.exit_link[0].id};\n"
        [node.exit_link[1], node.exit_link[0]]
      end
    end

    define_ccgen_rule_op :JMPNOT do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      r = 0
      inst.inreg[0].type[tup].each do |ty|
        if ty.class_object == NilClass or
            ty.class_object == FalseClass then
          r |= 1
        else
          r |= 2
        end
      end
      if cond == true or r == 2 then
        [node.exit_link[0]]
      elsif cond == false or r == 1 then
        [node.exit_link[1]]
      else
        ccgen.pcode << "if (#{cond}) goto L#{node.exit_link[0].id}; else goto L#{node.exit_link[1].id};\n"
        [node.exit_link[0], node.exit_link[1]]
      end
    end

    define_ccgen_rule_op :SEND do |ccgen, inst, node, infer, history, tup|
      op_send_aux(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :SENDB do |ccgen, inst, node, infer, history, tup|
      op_send_aux(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :RETURN do |ccgen, inst, node, infer, history, tup|
      retval = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      if retval then
        ccgen.pcode << "return #{retval};\n"
      else
        ccgen.pcode << "return mrb_nil_value();\n"
      end
      nil
    end

    define_ccgen_rule_op :EQ do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LT do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :LE do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :GT do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :GE do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :ADD do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :SUB do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :MUL do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :DIV do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :ADDI do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :SUBI do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :ARRAY do |ccgen, inst, node, infer, history, tup|
      reg = inst.outreg[0]
      aescape = is_escape?(reg)
      vals = inst.inreg.map {|reg|
        src = reg_real_value(ccgen, reg, node, tup, infer, history)
      }

      dstt = nil
      if aescape then
        dstt = :mrb_value
      else
        dstt = get_ctype(ccgen, inst, inst.outreg[0], tup)
      end

      i = 0
      vals2 = inst.inreg.map {|reg|
        srct = get_ctype(ccgen, inst, reg, tup)
        rc = gen_type_conversion(dstt, srct, vals[i])
        i = i + 1
        rc
      }

      if aescape then
        ccgen.hcode << "mrb_value v#{reg.id};"
        ccgen.pcode << "{\n"
        ccgen.pcode << "mrb_value tmpele[] = {\n"
        ccgen.pcode << vals2.join(', ')
        ccgen.pcode << "\n};\n"
        ccgen.pcode << "v#{reg.id} = mrb_ary_new_from_values(mrb, #{vals.size}, tmpele);\n"
        ccgen.pcode << "}\n"
      else
        type = get_ctype_aux(ccgen, inst, reg, tup)
        if type == :array then
          uv = MTypeInf::ContainerType::UNDEF_VALUE
          ereg = reg.type[tup][0].element[uv]
          etype = get_ctype_aux(ccgen, inst, ereg, tup)
          ccgen.pcode << "#{etype} v#{reg.id}[] = {\n"
          ccgen.pcode << vals2.join(', ')
          ccgen.pcode << "\n};\n"
        end
      end
      nil
    end

    define_ccgen_rule_op :LAMBDA do |ccgen, inst, node, infer, history, tup|
      # make env struct
      envreg = inst.para[1]
      proc = inst.outreg[0].type[tup][0]
      tups = inst.outreg[0].type.keys
      tupsize = tups.size
      if envreg.size > 0 then
        ccgen.hcode << "struct env#{proc.id} {\n"
        envreg.each do |reg|
          ccgen.hcode << "#{gen_declare(self, inst, reg, tup)};\n"
        end
        ccgen.hcode << "};\n"
        ccgen.hcode << "struct proc#{proc.id} {\n"
        ccgen.hcode << "int id;\n"
        ccgen.hcode << "void *code[tupsize];\n"
        ccgen.hcode << "struct env#{proc.id} env;\n"
      else
        ccgen.hcode << "struct proc#{proc.id} {\n"
        ccgen.hcode << "int id;\n"
        ccgen.hcode << "void *code[#{tupsize}];\n"
      end

      cproc = ccgen.callstack[-1][0]
      if !cproc.irep.strict then
        ccgen.hcode << "struct env#{cproc.id} prev;\n"
      end
      ccgen.hcode << "mrb_value self;\n"
      ccgen.hcode << "};\n"

      regno = inst.outreg[0].id
      ccgen.pcode << "struct proc#{proc.id} v#{regno};\n"
      ccgen.pcode << "v#{regno}.id = #{proc.id};\n"
      ccgen.pcode << "v#{regno}.self = self;\n"
      tups.each_with_index do |tp, i|
        bfunc = gen_block_func("p#{proc.id}", proc.slf.class_object, inst.para[3], tp)
        ccgen.pcode << "v#{regno}.code[i] = (void *)#{bfunc};\n"
        minf = [bfunc, proc, tp]
        ccgen.using_method.push minf
      end

      print ccgen.hcode
    end
  end
end

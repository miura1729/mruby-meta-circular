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
      "goto L#{node.exit_link[0].id};\n"
    end

    define_ccgen_rule_op :JMPIF do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      if cond == true then
        "goto L#{node.exit_link[1].id};\n"
      elsif cond == false
        "goto L#{node.exit_link[0].id};\n"
      else
        "if (#{cond}) goto L#{node.exit_link[1].id}; else goto L#{node.exit_link[0].id};\n"
      end
    end

    define_ccgen_rule_op :JMPNOT do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      if cond == true then
        "goto L#{node.exit_link[0].id};\n"
      elsif cond == false
        "goto L#{node.exit_link[1].id};\n"
      else
        "if (#{cond}) goto L#{node.exit_link[0].id}; else goto L#{node.exit_link[1].id};\n"
      end
    end

    define_ccgen_rule_op :SEND do |ccgen, inst, node, infer, history, tup|
      op_send_aux(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_op :RETURN do |ccgen, inst, node, infer, history, tup|
      retval = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      ccgen.ccode << "return #{retval};\n"
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
        p dstt
        p srct
        rc = gen_type_conversion(dstt, srct, vals[i])
        i = i + 1
        rc
      }

      if aescape then
        ccgen.ccode << "mrb_value v#{reg.id} = mrb_ary_new_from_values(mrb, #{vals.size}, #{vals2.join(', ')});\n"
      else
        type = get_ctype_aux(ccgen, inst, reg, tup)
        if type == :array then
          uv = MTypeInf::ContainerType::UNDEF_VALUE
          ereg = reg.type[tup][0].element[uv]
          etype = get_ctype_aux(ccgen, inst, ereg, tup)
          ccgen.ccode << "#{etype} v#{reg.id}[] = {\n"
          ccgen.ccode << vals2.join(', ')
          ccgen.ccode << "\n};\n"
        end
      end
      nil
    end

    define_ccgen_rule_op :LAMBDA do |ccgen, inst, node, infer, history, tup|
      # make env struct
      envreg = inst.para[1]
      proc = inst.outreg[0].type[tup][0]
      if envreg.size > 0 then
        ccgen.hcode << "struct env#{proc.id} {\n"
        envreg.each do |reg|
          ccgen.hcode << "#{gen_declare(self, inst, reg, tup)};\n"
        end
        ccgen.hcode << "};\n"
        ccgen.hcode << "struct proc#{proc.id} {\n"
        ccgen.hcode << "struct env#{proc.id} env;\n"
      else
        ccgen.hcode << "struct proc#{proc.id} {\n"
      end

      cproc = ccgen.callstack[-1][0]
      if !cproc.irep.strict then
        ccgen.hcode << "struct env#{cproc.id} prev;\n"
      end
      ccgen.hcode << "void *code;\n"
      ccgen.hcode << "};\n"

      print ccgen.hcode
    end
  end
end


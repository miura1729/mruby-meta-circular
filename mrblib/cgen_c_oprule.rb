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

    define_ccgen_rule_op :ENTER do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :JMP do |ccgen, inst, node, infer, history, tup|
      "goto L#{node.exit_link[0].id};\n"
    end

    define_ccgen_rule_op :JMPIF do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node, tup, infer)
      "if (#{cond}) goto L#{node.exit_link[1].id}; else goto L#{node.exit_link[0].id};\n"
    end

    define_ccgen_rule_op :JMPNOT do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node, tup, infer)
      "if (#{cond}) goto L#{node.exit_link[0].id}; else goto L#{node.exit_link[1].id};\n"
    end

    define_ccgen_rule_op :SEND do |ccgen, inst, node, infer, history, tup|
      name = inst.para[0]
      intype = inst.inreg.map {|reg| reg.flush_type(tup)[tup] || []}
      intype[0] = [intype[0][0]]
      rectype = intype[0][0].class_object
      if @@ruletab[:CCGEN_METHOD][name] and mproc = @@ruletab[:CCGEN_METHOD][name][rectype] then
        mproc.call(ccgen, inst, node, infer, history, tup)
      else
        mtab = MTypeInf::TypeInferencer.get_ruby_methodtab
        proc = mtab[inst.para[0]][rectype]
        decl = gen_declare(ccgen, inst, inst.outreg[0], tup)

        utup = infer.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), tup)
        fname = gen_method_func(name, rectype, utup)

        args = inst.inreg.map {|reg| reg_real_value(ccgen, reg, node, tup, infer)}.join(", ")
        ccgen.ccode << "#{decl} = #{fname}(mrb, #{args});\n"
        minf = [name, proc, utup]
        if ccgen.using_method.index(minf) == nil then
          ccgen.using_method.push minf
        end
      end
      nil
    end

    define_ccgen_rule_op :RETURN do |ccgen, inst, node, infer, history, tup|
      retval = reg_real_value(ccgen, inst.inreg[0], node, tup, infer)
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
        src = reg_real_value(ccgen, reg, node, tup, infer)
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
  end
end


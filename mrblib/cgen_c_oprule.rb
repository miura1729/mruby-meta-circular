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

    define_ccgen_rule_op :ENTER do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :SEND do |ccgen, inst, node, infer, history, tup|
      name = inst.para[0]
      mtab = MTypeInf::TypeInferencer.get_ruby_methodtab
      intype = inst.inreg.map {|reg| reg.type[tup] || []}
      utup = infer.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), 0)
      rectype = inst.inreg[0].type[tup][0].class_object
      proc = mtab[inst.para[0]][rectype]
      decl = gen_declare(ccgen, inst, inst.outreg[0], tup)
      fname = gen_method_func(name, rectype, utup)
      args = inst.inreg.map {|reg| reg_real_value(ccgen, reg, node)}.join(", ")
      ccgen.ccode << "#{decl} = #{fname}(#{args})"
      ccgen.ccode << "\n"
      minf = [name, proc, utup]
      if ccgen.using_method.index(minf) == nil then
        ccgen.using_method.push minf
      end
      nil
    end

    define_ccgen_rule_op :RETURN do |ccgen, inst, node, infer, history, tup|
      retval = reg_real_value(ccgen, inst.inreg[0], node)
      ccgen.ccode << "return #{retval}"
      ccgen.ccode << "\n"
      nil
    end

    define_ccgen_rule_op :EQ do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :SUBI do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :ADD do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_op :JMPNOT do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node)
      "if (#{cond}) goto L#{node.exit_link[0].id} else goto L#{node.exit_link[1].id}\n"
    end

    define_ccgen_rule_op :JMPIF do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node)
      "if (#{cond}) goto L#{node.exit_link[1].id} else goto L#{node.exit_link[0].id}\n"
    end

    define_ccgen_rule_op :JMP do |ccgen, inst, node, infer, history, tup|
      "goto L#{node.exit_link[0].id}\n"
    end
  end
end


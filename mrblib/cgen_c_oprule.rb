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
      p inst.inreg[0].id
      p inst.outreg[0].id
    end

    define_ccgen_rule_op :LOADSELF do |ccgen, inst, node, infer, history, tup|
    end

    define_ccgen_rule_op :LOADI do |ccgen, inst, node, infer, history, tup|
    end

    define_ccgen_rule_op :ENTER do |ccgen, inst, node, infer, history, tup|
    end

    define_ccgen_rule_op :SEND do |ccgen, inst, node, infer, history, tup|
      name = inst.para[0]
      mtab = MTypeInf::TypeInferencer.get_ruby_methodtab
      intype = inst.inreg.map {|reg| reg.type[tup] || []}
      utup = infer.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), 0)
      rectype = inst.inreg[0].type[tup][0].class_object
      proc = mtab[inst.para[0]][rectype]
      decl = gen_declare(ccgen, inst, inst.outreg[0], tup)
      fname = gen_method_func(name, rectype, tup)
      args = inst.inreg.map {|reg| reg_real_value(ccgen, reg, node)}.join(", ")
      ccgen.ccode << "#{decl} = #{fname}(#{args})"
      ccgen.ccode << "\n"
      ccgen.using_method.push [name, proc, utup]
    end

    define_ccgen_rule_op :RETURN do |ccgen, inst, node, infer, history, tup|
      retval = reg_real_value(ccgen, inst.inreg[0], node)
      ccgen.ccode << "return #{retval}"
      ccgen.ccode << "\n"
    end

    define_ccgen_rule_op :EQ do |ccgen, inst, node, infer, history, tup|
      arg0 = reg_real_value(ccgen, inst.inreg[0], node)
      arg1 = reg_real_value(ccgen, inst.inreg[1], node)
      decl = gen_declare(ccgen, inst, inst.outreg[0], tup)
      ccgen.ccode << "#{decl} = (#{arg0} == #{arg1})"
      ccgen.ccode << "\n"
    end

    define_ccgen_rule_op :JMPNOT do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node)
      ccgen.ccode << "if (#{cond}) goto L#{node.exit_link[0].id} else goto L#{node.exit_link[1].id} "
      ccgen.ccode << "\n"
      print ccgen.ccode
    end

    define_ccgen_rule_op :JMPIF do |ccgen, inst, node, infer, history, tup|
      cond = reg_real_value(ccgen, inst.inreg[0], node)
      ccgen.ccode << "if (#{cond}) goto L#{node.exit_link[1].id} else goto L#{node.exit_link[0].id} "
      ccgen.ccode << "\n"
    end

    define_ccgen_rule_op :JMP do |ccgen, inst, node, infer, history, tup|
      ccgen.ccode << "goto L#{node.exit_link[0].id}"
      ccgen.ccode << "\n"
    end
  end
end


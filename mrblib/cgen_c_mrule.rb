module CodeGenC
  class CodeGen
    @@ruletab ||= {}

    def self.define_ccgen_rule_method(name, rec, &block)
      @@ruletab[:CCGEN_METHOD] ||= {}
      @@ruletab[:CCGEN_METHOD][name] ||= {}
      if @@ruletab[:CCGEN_METHOD][name][rec] then
        raise "Already defined #{name}"
      end
      @@ruletab[:CCGEN_METHOD][name][rec] = block
    end

    def self.define_ccgen_rule_class_method(name, rec, &block)
      rec = class << rec
              self
            end
      @@ruletab[:CCGEN_METHOD] ||= {}
      @@ruletab[:CCGEN_METHOD][name] ||= {}
      if @@ruletab[:CCGEN_METHOD][name][rec] then
        raise "Already defined #{name}"
      end
      @@ruletab[:CCGEN_METHOD][name][rec] = block
    end

    define_ccgen_rule_method :-@, Fixnum do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      dstt = get_ctype(ccgen, nreg, tup)
      src, srct = (reg_real_value_noconv(ccgen, ireg, node, tup, infer, history))
      src = gen_type_conversion(ccgen, :mrb_int, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_int, "-#{src}", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :-@, Float do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      dstt = get_ctype(ccgen, nreg, tup)
      src, srct = (reg_real_value_noconv(ccgen, ireg, node, tup, infer, history))
      src = gen_type_conversion(ccgen, :mrb_float, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_float, "-#{src}", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_class_method :sqrt, Math do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[1]
      dstt = get_ctype(ccgen, nreg, tup)
      src, srct = (reg_real_value_noconv(ccgen, ireg, node, tup, infer, history))
      src = gen_type_conversion(ccgen, :mrb_float, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_float, "sqrt(#{src})", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_class_method :sin, Math do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[1]
      dstt = get_ctype(ccgen, nreg, tup)
      src, srct = (reg_real_value_noconv(ccgen, ireg, node, tup, infer, history))
      src = gen_type_conversion(ccgen, :mrb_float, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_float, "sin(#{src})", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_class_method :cos, Math do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[1]
      dstt = get_ctype(ccgen, nreg, tup)
      src, srct = (reg_real_value_noconv(ccgen, ireg, node, tup, infer, history))
      src = gen_type_conversion(ccgen, :mrb_float, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_float, "cos(#{src})", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :to_f, Fixnum do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src = reg_real_value(ccgen, ireg, oreg,  node, tup, infer, history)
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :to_f, Float do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src = reg_real_value(ccgen, ireg, oreg,  node, tup, infer, history)
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :p, Object do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      src, srct = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))
      src2 = gen_type_conversion(ccgen, :mrb_value, srct, src, tup, node, infer, history)
      ccgen.pcode << "mrb_p(mrb, #{src2});\n"
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :rand, Object do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      dstt = get_ctype(ccgen, nreg, tup)
      src = "(mrb_float)rand()"
      src = gen_type_conversion(ccgen, dstt, :mrb_float, src, tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :!, TrueClass do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      dstt = get_ctype(ccgen, nreg, tup)
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      src, srct = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))
      src = gen_type_conversion(ccgen, :mrb_value, srct, src, tup, node, infer, history)
      src = "(!mrb_test(#{src}))"
      src = gen_type_conversion(ccgen, dstt, :mrb_bool, src, tup, node, infer, history)

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :nil?, Array do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      dstt = get_ctype(ccgen, nreg, tup)
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      src, srct = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))
      src = gen_type_conversion(ccgen, :mrb_value, srct, src, tup, node, infer, history)
      src = "mrb_nil_p(#{src})"
      src = gen_type_conversion(ccgen, dstt, :mrb_bool, src, tup, node, infer, history)

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :[], Array do |ccgen, inst, node, infer, history, tup|
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      elereg = inst.inreg[0].type[tup][0].element[uv]
      nreg = inst.outreg[0]
      aryreg = inst.inreg[0]
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src, srct = reg_real_value_noconv(ccgen, aryreg, node, tup, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      idx = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      if is_escape?(inst.inreg[0]) then
        src = "mrb_ary_ref(mrb, #{src}, #{idx})"
        src = gen_type_conversion(ccgen, dstt, :mrb_value, src, tup, node, infer, history)
      else
        srct = get_ctype(ccgen, elereg, tup)
        if srct == :nil then
          src = "mrb_nil_value()"
        else
          src = "#{src}[#{idx}]"
          src = gen_type_conversion(ccgen, dstt, srct, src, tup, node, infer, history)
        end
      end

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :[]=, Array do |ccgen, inst, node, infer, history, tup|
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      elereg = inst.inreg[0].type[tup][0].element[uv]
      slf, slft = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))
      val, valt = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      idx = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      if is_escape?(inst.inreg[0]) then
        val = gen_type_conversion(ccgen, :mrb_value, valt, val, tup, node, infer, history)
        src = "mrb_ary_set(mrb, #{slf}, #{idx}, #{val})"
      else
        srct = get_ctype(ccgen, elereg, tup)
        val = gen_type_conversion(ccgen, srct, valt, val, tup, node, infer, history)
        src = "#{slf}[#{idx}] = #{val}"
      end

      ccgen.pcode << "v#{nreg.id} = #{val};\n"
      nil
    end

    define_ccgen_rule_method :push, Array do |ccgen, inst, node, infer, history, tup|
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src, srct = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      val, valt = reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history)
      val = gen_type_conversion(ccgen, :mrb_value, valt, val, tup, node, infer, history)
      ccgen.pcode <<  "mrb_ary_push(mrb, #{src}, #{val});\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :length, Array do |ccgen, inst, node, infer, history, tup|
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))[0]
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      if is_escape?(inst.inreg[0]) then
        src = "ARY_LEN(mrb_ary_ptr(#{src}))"
        src = gen_type_conversion(ccgen, dstt, :mrb_int, src, tup, node, infer, history)
      else
        src = gen_type_conversion(ccgen, dstt, :mrb_int, inst.inreg[0].type[tup][0].element.keys.size - 1, tup, node, infer, history)
      end

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :call, Proc do |ccgen, inst, node, infer, history, tup|
      procty = get_ctype(ccgen, inst.inreg[0], tup)
      intype = inst.inreg.map {|reg| reg.type[tup] || []}
      ptype = intype[0][0]
      proc = inst.inreg[0]
      intype[0] = [ptype.slf]
      nreg = inst.outreg[0]
      if intype[0].size == 1 then
        # store proc object only 1 kind.
        utup = infer.typetupletab.get_tupple_id(intype, ptype, tup)
        procvar = (reg_real_value_noconv(ccgen, proc, node, tup, infer, history))[0]
        codeno = ptype.using_tup[utup]
        outtype = get_ctype(ccgen, nreg, tup)
        if procty == :mrb_value then
          fname = "(MRB_PROC_CFUNC(mrb_proc_ptr(#{procvar})))"
          fname = "((#{outtype} (*)())(((void **)#{fname})[#{codeno}]))"
        else
          fname = "((#{outtype} (*)())((struct proc#{ptype.id} *)(#{procvar}))->code[#{codeno}])"
        end
        args = inst.inreg.map {|reg|
          (reg_real_value_noconv(ccgen, reg, node, tup, infer, history))[0]
        }.join(", ")
        ccgen.dcode << CodeGen::gen_declare(ccgen, nreg, tup)
        ccgen.dcode << ";\n"
        ccgen.pcode << "v#{nreg.id} = #{fname}(mrb, #{args});\n"
      end
      nil
    end

    define_ccgen_rule_method :new, Class do |ccgen, inst, node, infer, history, tup|
      recvtypes = inst.inreg[0].flush_type_alltup(tup)[tup]
      argc = inst.para[1]
      inreg = inst.inreg.clone
      oreg = inst.outreg[0]
      clsssa = RiteSSA::ClassSSA.get_instance(oreg.type[tup][0].class_object)

      if recvtypes.size == 1 then
        recvt = recvtypes[0].class_object

        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup)};\n"
        if is_escape?(oreg) then
          ccgen.pcode << "v#{oreg.id} = mrb_ary_new_capa(mrb, #{clsssa.iv.size});\n"
        else
          clsid = ccgen.using_class[clsssa] ||= "cls#{clsssa.id}"
          ccgen.pcode << "v#{oreg.id} = alloca(sizeof(struct #{clsid}));\n"
        end

        inreg[0] = oreg
        op_send_aux(ccgen, inst, inreg, nil, node, infer, history, tup, :initialize)
      end
      nil
    end
  end
end

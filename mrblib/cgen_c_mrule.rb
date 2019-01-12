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
      srct = get_ctype(ccgen, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(:mrb_int, srct, src)
      src = gen_type_conversion(dstt, :mrb_int, "-#{src}")
      nreg = inst.outreg[0]
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :-@, Float do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(:mrb_float, srct, src)
      src = gen_type_conversion(dstt, :mrb_float, "-#{src}")
      nreg = inst.outreg[0]
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_class_method :sqrt, Math do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[1], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[1], node, tup, infer, history)
      src = gen_type_conversion(:mrb_float, srct, src)
      src = gen_type_conversion(dstt, :mrb_float, "sqrt(#{src})")
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_class_method :sin, Math do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[1], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[1], node, tup, infer, history)
      src = gen_type_conversion(:mrb_float, srct, src)
      src = gen_type_conversion(dstt, :mrb_float, "sin(#{src})")
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_class_method :cos, Math do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[1], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[1], node, tup, infer, history)
      src = gen_type_conversion(:mrb_float, srct, src)
      src = gen_type_conversion(dstt, :mrb_float, "cos(#{src})")
      nreg = inst.outreg[0]
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :to_f, Fixnum do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(dstt, srct, src)
      nreg = inst.outreg[0]
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :to_f, Float do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(dstt, srct, src)
      nreg = inst.outreg[0]
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :p, Object do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[1], tup)
      src = reg_real_value(ccgen, inst.inreg[1], node, tup, infer, history)
      src2 = gen_type_conversion(:mrb_value, srct, src)
      ccgen.pcode << "mrb_p(mrb, #{src2});\n"
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :rand, Object do |ccgen, inst, node, infer, history, tup|
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = "(mrb_float)rand()"
      src = gen_type_conversion(dstt, :mrb_float, src)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :!, TrueClass do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(:mrb_value, srct, src)
      src = "(!mrb_test(#{src}))"
      src = gen_type_conversion(dstt, :mrb_bool, src)

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :nil?, Array do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(:mrb_value, srct, src)
      src = "mrb_nil_p(#{src})"
      src = gen_type_conversion(dstt, :mrb_bool, src)

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :[], Array do |ccgen, inst, node, infer, history, tup|
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      elereg = inst.inreg[0].type[tup][0].element[uv]
      srct = get_ctype(ccgen, elereg, tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      idx = reg_real_value(ccgen, inst.inreg[1], node, tup, infer, history)
      if is_escape?(inst.inreg[0]) then
        src = "mrb_ary_ref(mrb, #{src}, #{idx})"
        src = gen_type_conversion(dstt, :mrb_value, src)
      else
        src = "#{src}[#{idx}]"
        if srct == :nil then
          src = "mrb_nil_value()"
        else
          src = gen_type_conversion(dstt, srct, src)
        end
      end

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :[]=, Array do |ccgen, inst, node, infer, history, tup|
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      elereg = inst.inreg[0].type[tup][0].element[uv]
      slft = get_ctype(ccgen, elereg, tup)
      valt = get_ctype(ccgen, inst.inreg[2], tup)
      slf = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      val = reg_real_value(ccgen, inst.inreg[1], node, tup, infer, history)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      idx = reg_real_value(ccgen, inst.inreg[1], node, tup, infer, history)
      if is_escape?(inst.inreg[0]) then
        val = gen_type_conversion(:mrb_value, valt, val)
        src = "mrb_ary_set(mrb, #{slf}, #{idx}, #{val})"
      else
        val = gen_type_conversion(slft, valt, val)
        src = "#{slf}[#{idx}] = #{val}"
      end

      ccgen.pcode << "v#{nreg.id} = #{val};\n"
      nil
    end

    define_ccgen_rule_method :push, Array do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst.inreg[0], tup)
      valt = get_ctype(ccgen, inst.inreg[1], tup)
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      val = reg_real_value(ccgen, inst.inreg[1], node, tup, infer, history)
      val = gen_type_conversion(:mrb_value, valt, val)
      ccgen.pcode <<  "mrb_ary_push(mrb, #{src}, #{val});\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :length, Array do |ccgen, inst, node, infer, history, tup|
      dstt = get_ctype(ccgen, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup)
      ccgen.dcode << ";\n"
      if is_escape?(inst.inreg[0]) then
        src = "ARY_LEN(mrb_ary_ptr(#{src}))"
        src = gen_type_conversion(dstt, :mrb_int, src)
      else
        src = gen_type_conversion(dstt, :mrb_int, inst.inreg[0].type[tup][0].element.keys.size - 1)
      end

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :call, Proc do |ccgen, inst, node, infer, history, tup|
      intype = inst.inreg.map {|reg| reg.type[tup] || []}
      ptype = intype[0][0]
      proc = inst.inreg[0]
      intype[0] = [ptype.slf]
      nreg = inst.outreg[0]
      if intype[0].size == 1 then
        # store proc object only 1 kind.
        utup = infer.typetupletab.get_tupple_id(intype, ptype, tup)
        procvar = reg_real_value(ccgen, proc, node, tup, infer, history)
        codeno = ptype.using_tup[utup]
        outtype = get_ctype(ccgen, nreg, tup)
        fname = "((#{outtype} (*)())((struct proc#{ptype.id} *)(#{procvar}))->code[#{codeno}])"
        args = inst.inreg.map {|reg|
          reg_real_value(ccgen, reg, node, tup, infer, history)
        }.join(", ")
        ccgen.dcode << CodeGen::gen_declare(self, nreg, tup)
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

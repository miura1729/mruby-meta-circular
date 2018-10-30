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

    define_ccgen_rule_method :!, TrueClass do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst, inst.outreg[0], tup)
      dst = gen_declare(ccgen, inst, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer)
      src = gen_type_conversion(:mrb_value, srct, src)
      src = "(!mrb_test(#{src}))"
      src = gen_type_conversion(dstt, :mrb_bool, src)

      ccgen.ccode << "#{dst} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :nil?, Array do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst, inst.outreg[0], tup)
      dst = gen_declare(ccgen, inst, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer)
      src = gen_type_conversion(:mrb_value, srct, src)
      src = "mrb_nil_p(#{src})"
      src = gen_type_conversion(dstt, :mrb_bool, src)

      ccgen.ccode << "#{dst} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :[], Array do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer)
      dst = gen_declare(ccgen, inst, inst.outreg[0], tup)
      idx = reg_real_value(ccgen, inst.inreg[1], node, tup, infer)
      if is_escape?(inst.inreg[0]) then
        src = "mrb_ary_ref(mrb, #{src}, #{idx})"
        src = gen_type_conversion(dstt, :mrb_value, src)
      else
        src = "#{src}[#{idx}]"
        src = gen_type_conversion(dstt, srct, src)
      end

      ccgen.ccode << "#{dst} = #{src};\n"
      nil
    end
  end
end

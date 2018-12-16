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
    define_ccgen_rule_method :p, Object do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst, inst.inreg[1], tup)
      src = reg_real_value(ccgen, inst.inreg[1], node, tup, infer, history)
      src2 = gen_type_conversion(:mrb_value, srct, src)
      ccgen.pcode << "mrb_p(mrb, #{src2});\n"
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, inst, nreg, tup)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :!, TrueClass do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst, inst.outreg[0], tup)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, inst, nreg, tup)
      ccgen.dcode << ";\n"
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(:mrb_value, srct, src)
      src = "(!mrb_test(#{src}))"
      src = gen_type_conversion(dstt, :mrb_bool, src)

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :nil?, Array do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst, inst.outreg[0], tup)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, inst, nreg, tup)
      ccgen.dcode << ";\n"
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(:mrb_value, srct, src)
      src = "mrb_nil_p(#{src})"
      src = gen_type_conversion(dstt, :mrb_bool, src)

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :[], Array do |ccgen, inst, node, infer, history, tup|
      srct = get_ctype(ccgen, inst, inst.inreg[0], tup)
      dstt = get_ctype(ccgen, inst, inst.outreg[0], tup)
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer, history)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, inst, nreg, tup)
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

    define_ccgen_rule_method :call, Proc do |ccgen, inst, node, infer, history, tup|
      intype = inst.inreg.map {|reg| reg.type[tup] || []}
      ptype = intype[0][0]
      intype[0] = [ptype.slf]
      if intype[0].size == 1 then
        # store proc object only 1 kind.
      end
    end
  end
end

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

    define_ccgen_rule_method :[], Array do |ccgen, inst, node, infer, history, tup|
      src = reg_real_value(ccgen, inst.inreg[0], node, tup, infer)
      idx = reg_real_value(ccgen, inst.inreg[1], node, tup, infer)
      ccgen.ccode << "v#{inst.outreg[0].id}[#{idx}] = #{src};\n"
      nil
    end
  end
end

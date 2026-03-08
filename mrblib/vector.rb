module MMC_EXT
  class Vector<Array
  end

  class Bitmap
  end
end

module MTypeInf
  class TypeInferencer
    define_inf_rule_method :map_with_index!, MMC_EXT::Vector do |infer, inst, node, tup|
      p inst.inreg[1].type
      p inst.inreg[1].get_type(tup)[0].irep.nodes[0].enter_reg
      inst.outreg[0].add_same inst.inreg[0]
      nil
    end

    define_inf_rule_method :pcmpestri128, MMC_EXT::Vector do |infer, inst, node, tup|
      type = NumericType.new(Fixnum, true)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :pcmpestrm128, MMC_EXT::Vector do |infer, inst, node, tup|
      type = SIMDType.new(MMC_EXT::Vector, :char, 16)
      inst.outreg[0].type[tup] = [type]
      nil
    end
  end
end

module CodeGenC
  class CodeGen
    define_ccgen_rule_method :pcmpestri128, MMC_EXT::Vector do |ccgen, inst, node, infer, history, tup|
      base = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))[0]
      base_num = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      target = (reg_real_value_noconv(ccgen, inst.inreg[2], node, tup, infer, history))[0]
     target_num = (reg_real_value_noconv(ccgen, inst.inreg[3], node, tup, infer, history))[0]
     para = (reg_real_value_noconv(ccgen, inst.inreg[4], node, tup, infer, history))[0]
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = __builtin_ia32_pcmpestri128(#{base}, #{base_num}, #{target}, #{target_num}, #{para});\n"
    end

    define_ccgen_rule_method :pcmpestrm128, MMC_EXT::Vector do |ccgen, inst, node, infer, history, tup|
      base = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))[0]
      base_num = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      target = (reg_real_value_noconv(ccgen, inst.inreg[2], node, tup, infer, history))[0]
     target_num = (reg_real_value_noconv(ccgen, inst.inreg[3], node, tup, infer, history))[0]
     para = (reg_real_value_noconv(ccgen, inst.inreg[4], node, tup, infer, history))[0]
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = __builtin_ia32_pcmpestrm128(#{base}, #{base_num}, #{target}, #{target_num}, #{para});\n"
    end
  end
end

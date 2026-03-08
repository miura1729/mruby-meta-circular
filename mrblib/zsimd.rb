class StringView
end

module MMC_EXT
  module SIMD
    class Find
    end

    class Select
    end

    class SelectBitmap
    end
  end
end

module MTypeInf
  class TypeInferencer
    define_inf_rule_method :_simd_check, StringView do |infer, inst, node, tup|
      block = inst.inreg[1].get_type(tup)[0].irep
      effects = block.effects
      inst.outreg[0].type[tup] =  [LiteralType.new(NilClass, nil)]
      if !effects[:return].nil? then
        type = nil
        effects[:return].values.each do |reteff|
          genvalins = reteff[0].genpoint
          if !genvalins.is_a?(RiteSSA::Inst) or genvalins.op != :SEND then
            break
          end
          slfreg = genvalins.inreg[0]
          positive = reteff[1]
          if genvalins.para[0] != :st or  !positive or positive.size > 1 then
            break
          end

          refinement = positive[0]
          if !refinement.is_a?(RefinementType) then
            break
          end
          predicate = refinement.predicate
          arg0reg = refinement.args[0]
          arg1reg = refinement.args[1]
          arg0ty = arg0reg.type.values[0][0]
          arg1ty = arg1reg.type.values[0][0]

          if predicate == :start_with and arg1ty.is_a?(LiteralType) and arg1ty.val.is_a?(String) and arg1ty.val.size < 8 and effects[:return].size == 1 then
            level = infer.callstack.size
            previrep =  infer.callstack.map {|e|  [e[0], e[4]]}
            type = ContainerType.new(MMC_EXT::SIMD::Find, inst, previrep, level)
            type.element[0] = arg1reg
            inst.outreg[0].type[tup] =  [type]

          elsif predicate == :include? and arg0ty.is_a?(RangeType)  and
              arg0ty.element[0].type.values[0][0].is_a?(LiteralType) and
              arg0ty.element[1].type.values[0][0].is_a?(LiteralType) then
            level = infer.callstack.size
            previrep =  infer.callstack.map {|e|  [e[0], e[4]]}
            type ||= ContainerType.new(MMC_EXT::SIMD::Select, inst, previrep, level)
            type.element[type.element.size - 1] = arg0reg
            inst.outreg[0].type[tup] =  [type]

          else
            inst.outreg[0].type[tup] =  [PrimitiveType.new(NilClass)]
            break
          end
        end
      end

      if !effects[:apush].nil? then
        type = nil
        elseflag = false
        effects[:apush].values.each do |apheff|
          valtype = apheff[1].type.values
          slfreg = apheff[2]
          positive = apheff[3]

          refinement = nil
          if positive then
            refinement = positive[0]
          end
          if !refinement.is_a?(RefinementType) then
            elseflag = true
            next
          end
          predicate = refinement.predicate
          arg0reg = refinement.args[0]
          arg1reg = refinement.args[1]
          arg0ty = arg0reg.type.values[0][0]
          arg1ty = arg1reg.type.values[0][0]

          if predicate == :include? and arg0ty.is_a?(RangeType)  and
              arg0ty.element[0].type.values[0][0].is_a?(LiteralType) and
              arg0ty.element[1].type.values[0][0].is_a?(LiteralType) then
            level = infer.callstack.size
            previrep =  infer.callstack.map {|e|  [e[0], e[4]]}
            type ||= ContainerType.new(MMC_EXT::SIMD::SelectBitmap, inst, previrep, level)
            type.element[type.element.size - 1] = arg0reg

          else
            inst.outreg[0].type[tup] =  [PrimitiveType.new(NilClass)]
            elseflag = false
            break
          end
        end

        if elseflag then
          effects[:apush].values.each do |apheff|
           apheff[0].type.values[0][0].place.delete(true)
          end
          inst.outreg[0].type[tup] =  [type]
        end
      end
      nil
    end

    define_inf_rule_method :to_simd, MMC_EXT::SIMD::Find do |infer, inst, node, tup|
      type = SIMDType.new(MMC_EXT::Vector, :char, 16)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :to_simd, MMC_EXT::SIMD::Select do |infer, inst, node, tup|
      type = SIMDType.new(MMC_EXT::Vector, :char, 16)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :to_simd, MMC_EXT::SIMD::SelectBitmap do |infer, inst, node, tup|
      type = SIMDType.new(MMC_EXT::Vector, :char, 16)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :target, MMC_EXT::SIMD::Find do |infer, inst, node, tup|
      findtype = inst.inreg[0].type[tup][0]
      types = findtype.element[0].type.values[0]
      inst.outreg[0].type[tup] = types
      nil
    end

    define_inf_rule_method :target, MMC_EXT::SIMD::Select do |infer, inst, node, tup|
      seltype = inst.inreg[0].type[tup][0]
      eles = seltype.element
      type = LiteralType.new(String, " " * eles.size)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :target, MMC_EXT::SIMD::SelectBitmap do |infer, inst, node, tup|
      seltype = inst.inreg[0].type[tup][0]
      eles = seltype.element
      type = LiteralType.new(String, " " * eles.size)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :to_simd, Array do |infer, inst, node, tup|
      aryty = inst.inreg[0].type[tup][0]
      if aryty.nil? then
        aryty = inst.inreg[0].type[tup][0]
      end
      aryele = aryty.element.values[0]
      elecls = aryele.type.values[0][0].class_object
      if elecls == CodeGenC::BYTE then
        ntype = SIMDType.new(MMC_EXT::Vector, :char, 16)

      elsif elecls == Float
        ntype = SIMDType.new(MMC_EXT::Vector, :double, 16)

      elsif elecls == Fixnum
        ntype = SIMDType.new(MMC_EXT::Vector, :int, 16)

      else
        p "Unkonwn class #{aryele.type.values[0][0].class_object}"
      end

      inst.outreg[0].type[tup] = [ntype]
      nil
    end

    define_inf_rule_method :local_variable_get, Binding do |infer, inst, node, tup|
      binding = inst.inreg[0].get_type(tup)[0]
      varsymt = inst.inreg[1].get_type(tup)[0]
      if varsymt.is_a?(LiteralType) then
        varsym = varsymt.val
        preg = binding.preg
        block = preg.type.values[0][0]
        lv = block.parent.irep.irep.lv
        env = block.env
        envno = block.envno
        regno = envno.index(lv[varsym])
        rreg = env[regno]
        inst.outreg[0].type[tup] = rreg.type.values[0]
      end
      nil
    end
  end
end

module CodeGenC
  class CodeGen
    define_ccgen_rule_method :_simd_check, StringView do |ccgen, inst, node, infer, history, tup|
      # No code generate this methed only for type
      nil
    end

    define_ccgen_rule_method :to_simd, MMC_EXT::SIMD::Find do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      type = ireg.type[tup][0]
      types = type.element[0].type.values[0]
      src = "{"
      types[0].val.each_byte do |b|
        src << "#{b}, "
      end
      src << "}"
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = (#{get_ctype(ccgen, nreg, tup, infer)})#{src};\n"
      nil
    end

    define_ccgen_rule_method :to_simd, MMC_EXT::SIMD::Select do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      type = ireg.type[tup][0]
      eles = type.element
      src = "{"
      (eles.size - 1).times do |i|
        range = eles[i].type.values[0][0]
        fst = range.element[0].type.values[0][0].val
        lst = range.element[1].type.values[0][0].val
        src << "#{fst}, #{lst}, "
      end
      src << "}"
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = (#{get_ctype(ccgen, nreg, tup, infer)})#{src};\n"
      nil
    end

    define_ccgen_rule_method :to_simd, MMC_EXT::SIMD::SelectBitmap do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      type = ireg.type[tup][0]
      eles = type.element
      src = "{"
      (eles.size - 1).times do |i|
        range = eles[i].type.values[0][0]
        fst = range.element[0].type.values[0][0].val
        lst = range.element[1].type.values[0][0].val
        src << "#{fst}, #{lst}, "
      end
      src << "}"
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = (#{get_ctype(ccgen, nreg, tup, infer)})#{src};\n"
      nil
    end

    define_ccgen_rule_method :target, MMC_EXT::SIMD::Find do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      type = ireg.type[tup][0]
      types = type.element[0].type.values[0]
      src = types[0].val
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = \"#{src}\";\n"
      nil
    end

    define_ccgen_rule_method :target, MMC_EXT::SIMD::Select do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_method :target, MMC_EXT::SIMD::SelectBitmap do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_method :to_simd, Array do |ccgen, inst, node, infer, history, tup|
      off = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      ary, aryt = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      nreg = inst.outreg[0]
      src = "(#{ary} + #{off})"
      src = "__builtin_ia32_loaddqu(#{src})"
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = (#{src});\n"
      nil
    end

    define_ccgen_rule_method :local_variable_get, Binding do |ccgen, inst, node, infer, history, tup|
      binding = inst.inreg[0].get_type(tup)[0]
      varsymt = inst.inreg[1].get_type(tup)[0]
      if varsymt.is_a?(MTypeInf::LiteralType) then
        varsym = varsymt.val
        preg = binding.preg
        block = preg.type.values[0][0]
        lv = block.parent.irep.irep.lv
        env = block.env
        envno = block.envno
        regno = envno.index(lv[varsym])
        rreg = env[regno]
        nreg = inst.outreg[0]
        src = "v#{preg.id}"
        src = "((struct proc#{block.id} *)(#{src}))->env[#{regno}].v#{rreg.id}"
        ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
        ccgen.dcode << ";\n"
        ccgen.pcode << "v#{nreg.id} = (#{src});\n"
      end
      nil
    end
  end
end

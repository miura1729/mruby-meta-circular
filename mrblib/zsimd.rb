class View
end

module MMC_EXT
  module SIMD
    class Find
    end

    class Select
    end

    class SelectBitmap
    end

    class NumericVec
    end

    class AddVec<NumericVec
    end

    class SubVec<NumericVec
    end

    class MulVec<NumericVec
    end
  end

end

module MTypeInf
  class TypeInferencer
    define_inf_rule_method :_simd_check, View do |infer, inst, node, tup|
      blockreg = inst.inreg[1]
      blockty = blockreg.get_type(tup)[0]
      block = blockty.irep
      effects = block.effects
      inst.outreg[0].type[tup] =  [LiteralType.new(NilClass, nil)]
      type = nil
      if !effects[:return].nil? then
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

          if predicate == :start_with and
              ((arg1ty.is_a?(LiteralType) and arg1ty.val.is_a?(String) and arg1ty.val.size < 8) or
              arg1ty.is_a?(StringType)) and
              effects[:return].size == 1 then
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
            ty = apheff[0].type.values[0][0]
            ks = ty.place[:push]
            if ks then
              ty.escape_cache = nil
              ks[0][0] = false
            end
          end
          inst.outreg[0].type[tup] =  [type]
        end
      end

      if type.is_a?(ContainerType) then
        type.element[:block] = blockreg
      end

      nil
    end

    define_inf_rule_method :_simd_check, Array do |infer, inst, node, tup|
      blockreg = inst.inreg[1]
      blockty = blockreg.get_type(tup)[0]
      block = blockty.irep
      effects = block.effects
      inst.outreg[0].type[tup] =  [LiteralType.new(NilClass, nil)]
      type = nil

      if !effects[:apush].nil? then
        type = nil
        effects[:apush].values.each do |apheff|
          valreg = apheff[1]
          valins = valreg.genpoint
          if !valins.is_a?(RiteSSA::Inst) then
            next
          end

          arg0 = valins.inreg[0]
          arg1 = valins.inreg[1]
          tins0 = nil
          while (!arg0.is_a?(RiteSSA::ParmReg)) and (tins0 = arg0.genpoint).op == :MOVE
            arg0 = tins0.inreg[0]
          end

          tins1 = nil
          while (!arg1.is_a?(RiteSSA::ParmReg)) and (tins1 = arg1.genpoint).op == :MOVE
            arg1 = tins1.inreg[0]
          end

          unless (arg0.is_a?(RiteSSA::ParmReg) and arg1.is_a?(RiteSSA::ParmReg)) or
              (tins0.op == :ENTER and tins1.op == :ENTER)
            next
          end

          level = infer.callstack.size
          previrep =  infer.callstack.map {|e|  [e[0], e[4]]}

          case valins.op
          when :ADD
            type ||= ContainerType.new(MMC_EXT::SIMD::AddVec, inst, previrep, level)

          when :SUB
            type ||= ContainerType.new(MMC_EXT::SIMD::SubVec, inst, previrep, level)

          when :MUL
            type ||= ContainerType.new(MMC_EXT::SIMD::MulVec, inst, previrep, level)
          end
          type.element[0] = valreg
          inst.outreg[0].type[tup] =  [type]
        end

      else
        inst.outreg[0].type[tup] =  [PrimitiveType.new(NilClass)]
        elseflag = false
        return
      end

      effects[:apush].values.each do |apheff|
        ty = apheff[0].type.values[0][0]
        ks = ty.place[:push]
        if ks then
          ty.escape_cache = nil
          ks[0][0] = false
        end
      end

      if type.is_a?(ContainerType) then
        type.element[:block] = blockreg
      end

      nil
    end

    define_inf_rule_method :to_simd, MMC_EXT::SIMD::Find do |infer, inst, node, tup|
      type =  inst.inreg[0].type[tup][0]
      type = SIMDType.new(MMC_EXT::Vector, :char, 16)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :to_simd, MMC_EXT::SIMD::Select do |infer, inst, node, tup|
      type = SIMDType.new(MMC_EXT::Vector, :char, 16)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :to_simd, MMC_EXT::SIMD::NumericVec do |infer, inst, node, tup|
      type =  inst.inreg[0].type[tup][0]
      restype = type.element[0].type.values[0][0].class_object
      if restype == Fixnum
        type = SIMDType.new(MMC_EXT::Vector, :int, 4)
        inst.outreg[0].type[tup] = [type]
      else
        type = SIMDType.new(MMC_EXT::Vector, :double, 2)
        inst.outreg[0].type[tup] = [type]
      end
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
      type = LiteralType.new(String, "  " * (eles.size - 2)) #- 2 means UNDEF and :block
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :target, MMC_EXT::SIMD::SelectBitmap do |infer, inst, node, tup|
      seltype = inst.inreg[0].type[tup][0]
      eles = seltype.element
      type = LiteralType.new(String, "  " * (eles.size - 2)) #- 2 means UNDEF and :block
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :to_simd, Array do |infer, inst, node, tup|
      aryty = inst.inreg[0].type[tup][0]
      if (ntype = aryty.is_simd) == nil then
        if aryty.nil? then
          aryty = inst.inreg[0].type.values[0][0]
        end
        aryele = aryty.element.values[0]
        elecls = aryele.type.values[0][0].class_object
        if elecls == CodeGenC::BYTE then
          ntype = SIMDType.new(MMC_EXT::Vector, :char, 16)

        elsif elecls == Float
          ntype = SIMDType.new(MMC_EXT::Vector, :double, 2)

        elsif elecls == Fixnum
          ntype = SIMDType.new(MMC_EXT::Vector, :int, 4)

        else
          p "Unkonwn class #{aryele.type.values[0][0].class_object}"
        end
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
    define_ccgen_rule_method :_simd_check, View do |ccgen, inst, node, infer, history, tup|
      # No code generate this methed only for type
      nil
    end

    define_ccgen_rule_method :_simd_check, Array do |ccgen, inst, node, infer, history, tup|
      # No code generate this methed only for type
      nil
    end

    define_ccgen_rule_method :to_simd, MMC_EXT::SIMD::Find do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      type = ireg.type[tup][0]
      types = type.element[0].type.values[0]
      if types[0].is_a?(MTypeInf::LiteralType) then
        src = "{"
        types[0].val.each_byte do |b|
          src << "#{b}, "
        end
        src << "}"
        src = "v#{nreg.id} = (#{get_ctype(ccgen, nreg, tup, infer)})#{src};\n"
      else
        breg = type.element[0].genpoint.inreg[0]
        reg = type.element[:block]
        regnm = reg_real_value_noconv(ccgen, reg, node, tup, infer, history)[0]
        procid = reg.type[tup][0].id
        src = "(((struct proc#{procid} *)#{regnm})->env->v#{breg.id})"
        src = "memcpy(&v#{nreg.id}, #{src}, 16);\n"
      end
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << src
      nil
    end

    define_ccgen_rule_method :to_simd, MMC_EXT::SIMD::Select do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      type = ireg.type[tup][0]
      eles = type.element
      src = "{"
      (eles.size - 2).times do |i|
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
      (eles.size - 2).times do |i|
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

    define_ccgen_rule_method :to_simd, MMC_EXT::SIMD::NumericVec do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_method :target, MMC_EXT::SIMD::Find do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      type = ireg.type[tup][0]
      types = type.element[0].type.values[0]
      if types[0].is_a?(MTypeInf::LiteralType) then
        src = "\"#{types[0].val}\""
      else
        breg = type.element[0].genpoint.inreg[0]
        reg = type.element[:block]
        regnm = reg_real_value_noconv(ccgen, reg, node, tup, infer, history)[0]
        procid = reg.type[tup][0].id
        src = "(((struct proc#{procid} *)#{regnm})->env->v#{breg.id})"
      end
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :target, MMC_EXT::SIMD::Select do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_method :target, MMC_EXT::SIMD::SelectBitmap do |ccgen, inst, node, infer, history, tup|
      nil
    end

    define_ccgen_rule_method :to_simd, Array do |ccgen, inst, node, infer, history, tup|
      slfty = inst.inreg[0].type[tup][0]
      if slfty.is_a?(MTypeInf::ContainerType) and slfty.is_simd then
        return
      end

      off = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      ary, aryt = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      nreg = inst.outreg[0]
      ty = nreg.type.values[0][0]
      elety = ty.etype
      src = "(#{ary} + #{off})"
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"

      case elety
      when :char
        src = "__builtin_ia32_loaddqu(#{src})"
        ccgen.pcode << "v#{nreg.id} = (#{src});\n"

      when :int
        ccgen.pcode << "__builtin_memcpy(&v#{nreg.id}, #{src}, 16);\n"

      when :double
        ccgen.pcode << "__builtin_memcpy(&v#{nreg.id}, #{src}, 16);\n"

      else
        p "Unkonwn class #{ty}"
      end

      nil
    end

    define_ccgen_rule_method :local_variable_get, Binding do |ccgen, inst, node, infer, history, tup|
      binding = inst.inreg[0].get_type(tup)[0]
      varsymt = inst.inreg[1].get_type(tup)[0]
      if varsymt.is_a?(MTypeInf::LiteralType) then
        varsym = varsymt.val
        preg = binding.preg
        cnode = node
        block = preg.type.values[0][0]
        lv = block.parent.irep.irep.lv
        if preg.is_a?(RiteSSA::ParmReg) then
          while cnode
            preg = cnode.enter_reg[preg.genpoint]
            cnode = cnode.enter_link[0]
          end
        end

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

module MMC_EXT
  class Vector<Array
  end

  class Bitmap
  end
end

module MTypeInf
  class SIMDType<PrimitiveType
    def initialize(co, etype, size, *rest)
      super(co, *rest)
      @etype = etype
      @size = size
    end

    attr :etype
    attr :size
  end

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

    define_inf_rule_method :[]=, MMC_EXT::Bitmap do |infer, inst, node, tup|
      if inst.inreg.size == 4 then
        inst.outreg[0].add_same inst.inreg[2]
        inst.outreg[0].flush_type(tup)

      elsif inst.inreg.size == 5 then
        # v[i, size] = n
        inst.outreg[0].add_same inst.inreg[3]
        inst.outreg[0].flush_type(tup)

      else
        raise "multiple argument not support yet in Array::[]="
      end

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

    define_ccgen_rule_method :[]=, MMC_EXT::Bitmap do |ccgen, inst, node, infer, history, tup|
      if inst.inreg.size == 4 then
        inst.outreg[0].add_same inst.inreg[2]

      elsif inst.inreg.size == 5 then
        # v[i, size] = n
        inst.inreg[0].flush_type(tup)
        inst.inreg[1].flush_type(tup)
        inst.inreg[2].flush_type(tup)
        inst.outreg[0].flush_type(tup)
        nreg = inst.outreg[0]
        vecreg = inst.inreg[0]
        idxreg = inst.inreg[1]
        sizreg = inst.inreg[2]
        valreg = inst.inreg[3]
        vectypes = vecreg.get_type(tup)
        idxtypes = idxreg.get_type(tup)
        sizetypes = sizreg.get_type(tup)
        vec, vect = (reg_real_value_noconv(ccgen, vecreg, node, tup, infer, history))
        idx, idxt = (reg_real_value_noconv(ccgen, idxreg, node, tup, infer, history))
        val, valt = (reg_real_value_noconv(ccgen, valreg, node, tup, infer, history))
        case idxtypes[0]
        when MTypeInf::IndexOfArrayType
          if idxtypes[0].base_array.element_num == vectypes[0].element_num and
              sizetypes[0].is_a?(MTypeInf::LiteralType) then
            size = sizetypes[0].val
            case size
            when 8
            when 16
#              val = gen_type_conversion(ccgen, :mrb_int, valt, val, tup, node, infer, history, nreg)
              ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
              ccgen.dcode << ";\n"
              ccgen.pcode << "((uint16_t *)#{vec})[#{idx}] = (uint16_t)(#{val});\n"
              ccgen.pcode << "v#{nreg.id} = (uint16_t)(#{val});\n"

            when 32
            when 64
            else
              raise "Not support this size"
            end
          else
            raise "Cant use this index type"
          end

        else
          raise "This index type not support yet in Bitmap::[]= #{idxtypes[0]}"
        end
      else
        raise "multiple argument not support yet in Array::[]="
      end
    end
  end
end

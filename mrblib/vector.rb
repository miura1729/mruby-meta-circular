module MMC_EXT
  class Vector
  end

  class TaggedVector
  end

  class Bitmap<Array
  end

  class ASTNode
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

    def inspect(level = 0)
      "#{@class_object.inspect}<type=#{@etype} size=#{@size}>"
    end
  end

  class TaggedSIMDType<PrimitiveType
    def initialize(co, *rest)
      super(co, *rest)
      @offtab = {}
      @vectab = {}
    end

    attr :offtab
    attr :vectab

    def inspect(level = 0)
      "#{@class_object.inspect}<off=#{@offtab} vec=#{@vectab}>"
    end
  end

  class ASTNodeType<PrimitiveType
    def initialize(co, name, child, *rest)
      super(co, *rest)
      @name = name
      @child = child
    end

    attr :child
    attr :name

    def inspect(level = 0)
      "#{@class_object.inspect}<name=#{@name} child=#{@child}>"
    end
  end

  class TypeInferencer
    define_inf_rule_method :[], MMC_EXT::Vector do |infer, inst, node, tup|
      valnum = inst.inreg.size - 2
      vec = inst.inreg[0]
      vect = vec.type[tup][0]
      if valnum != vect.size then
        p "Not match vector and tag #{vect} in #{inst.filename}:#{inst.line}"
      end
      otype = TaggedSIMDType.new(MMC_EXT::TaggedVector)
      valnum.times do |i|
        sym = inst.inreg[i + 1].type.values[0][0].val
        otype.offtab[sym] = i
        otype.vectab[sym] = vec
      end

      inst.outreg[0].type[tup] = [otype]
      nil
    end

    define_inf_rule_method :[], MMC_EXT::TaggedVector do |infer, inst, node, tup|
      slf = inst.inreg[0].type[tup][0]
      valnum = inst.inreg.size - 2
      syms = []
      allsym = true
      alladd = true
      valnum.times do |i|
        nsym = inst.inreg[i + 1].type.values[0][0]
        if nsym.is_a?(SymbolType) then
          sym = nsym.val
          syms << sym
          alladd = false

        elsif nsym.is_a?(ASTNodeType) then
          syms << [nsym.name, nsym.child[0], nsym.child[1]]
          allsym = false
        end
      end

      if allsym or alladd then
        inst.outreg[0].add_type(slf.vectab.values[0].type.values[0][0], tup)
      else
        raise "Not support yet"
      end

      nil
    end

    define_inf_rule_method :+, MMC_EXT::TaggedVector do |infer, inst, node, tup|
      slf = inst.inreg[0].type.values[0][0]
      oth = inst.inreg[1].type.values[0][0]

      rtype = TaggedSIMDType.new(MMC_EXT::TaggedVector)
      slf.offtab.each do |sym, off|
        rtype.offtab[sym] = off
      end
      oth.offtab.each do |sym, off|
        if rtype.offtab[sym] then
          p "Duplicate symbol #{sym}"
        end
        rtype.offtab[sym] = off
      end

      slf.vectab.each do |sym, vec|
        rtype.vectab[sym] = vec
      end

      oth.vectab.each do |sym, vec|
        rtype.vectab[sym] = vec
      end

      inst.outreg[0].type[tup] = [rtype]
      nil
    end

    define_inf_rule_method :+, Symbol do |infer, inst, node, tup|
      ty0 = inst.inreg[0].type.values[0][0]
      ty1 = inst.inreg[1].type.values[0][0]
      type = ASTNodeType.new(MMC_EXT::ASTNode, :+, [ty0, ty1])
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :map_with_index!, MMC_EXT::Vector do |infer, inst, node, tup|
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

    define_inf_rule_method :add128, MMC_EXT::Vector do |infer, inst, node, tup|
      type = NumericType.new(Fixnum, true)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :sub128, MMC_EXT::Vector do |infer, inst, node, tup|
      type = NumericType.new(Fixnum, true)
      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :mul128, MMC_EXT::Vector do |infer, inst, node, tup|
      slftype = inst.inreg[0].type[tup][0]
      inst.outreg[0].type[tup] = [slftype]
      nil
    end

    define_inf_rule_method :to_a, MMC_EXT::Vector do |infer, inst, node, tup|
      orgtype = inst.inreg[0].type[tup][0]
      level = infer.callstack.size
      previrep =  infer.callstack.map {|e|  [e[0], e[4]]}
      type = ContainerType.new(Array, inst, previrep, level)
      orgtype.size.times do |i|
        type.element[i] = RiteSSA::Reg.new(inst)
        nelet = nil
        case orgtype.etype
        when :int
          nelet = NumericType.new(Fixnum, false)
        when :double
          nelet = NumericType.new(Float, false)
        else
          p "Unkown type #{orgtype.etype}\n"
        end

        type.element[i].type[tup] = [nelet]
      end
      #type.element[ContainerType::UNDEF_VALUE].type[tup] = [orgtype.etype]

      # You can translate array <-> vector(SIMD) by zero cost. Only type conversion
      # Because vectir us akugbed need from CPU
      # is_simd stored original vector type or nil
      type.is_simd = orgtype

      inst.outreg[0].type[tup] = [type]
      nil
    end

    define_inf_rule_method :to_simd, MMC_EXT::Vector do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[0]
      inst.outreg[0].flush_type(tup)
      nil
    end

    define_inf_rule_method :[], MMC_EXT::Bitmap do |infer, inst, node, tup|
      if inst.inreg.size == 3 then
        type = LiteralType.new(TrueClass, true)
        inst.outreg[0].add_type(type, tup)
        type = LiteralType.new(FalseClass, false)
        inst.outreg[0].add_type(type, tup)

      elsif inst.inreg.size == 4 then
        # v[i, size]
        size = inst.inreg[2].type[tup][0].val
        case size
        when 8, 16, 32, 64
          type = NumericType.new(Fixnum, true)
          inst.outreg[0].add_type(type, tup)
        end

      else
        raise "multiple argument not support yet in Array::[]="
      end

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

    define_inf_rule_method :copy, Array  do |infer, inst, node, tup|
      inst.outreg[0].add_same inst.inreg[0]
      inst.outreg[0].flush_type(tup)
    end
  end
end

module CodeGenC
  class CodeGen
    define_ccgen_rule_method :[], MMC_EXT::Vector do |ccgen, inst, node, infer, history, tup|
    end

    define_ccgen_rule_method :[], MMC_EXT::TaggedVector do |ccgen, inst, node, infer, history, tup|
      slf = inst.inreg[0].type[tup][0]
      valnum = inst.inreg.size - 2
      syms = []
      vregs = []
      srcvecs = []
      offs = []
      allsym = true
      alladd = true
      valnum.times do |i|
        nsym = inst.inreg[i + 1].type.values[0][0]
        if nsym.is_a?(MTypeInf::SymbolType) then
          sym = nsym.val
          syms << sym
          offs << slf.offtab[sym]
          nreg = slf.vectab[sym]
          srcvecs << nreg
          if !vregs.include?(nreg) then
            vregs.push nreg
          end
          alladd = false

        elsif nsym.is_a?(MTypeInf::ASTNodeType) then
          sym0 = nsym.child[0].val
          sym1 = nsym.child[1].val
          sym = [nsym.name, sym0, sym1]
          syms << sym
          off = [slf.offtab[sym0], slf.offtab[sym1]]
          offs << off
          nreg = [slf.vectab[sym0], slf.vectab[sym1]]
          srcvecs << [sym[0], nreg, off]
          allsym = false

        end
      end

      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"

      if allsym then
        mask = offs.zip(srcvecs).map {|off, creg| off + ((creg == vregs[0]) ? 0 : 4) }.join(' ,')
        src = vregs.map {|inreg|
          (reg_real_value_noconv(ccgen, inreg, node, tup, infer, history))[0]
        }.join(", ")
        src = "v#{nreg.id} = __builtin_shuffle(#{src}, (v4si){#{mask}});\n"

      elsif alladd then
        src = srcvecs.map {|op, reg, off|
          r0 = (reg_real_value_noconv(ccgen, reg[0], node, tup, infer, history))[0]
          r1 = (reg_real_value_noconv(ccgen, reg[1], node, tup, infer, history))[0]
          "(#{r0}[#{off[0]}] #{op} #{r1}[#{off[1]}])"
        }
        src = "v#{nreg.id} = (v4si){#{src.join(', ')}};\n"
      end

      ccgen.pcode << src
    end

    define_ccgen_rule_method :+, MMC_EXT::TaggedVector do |ccgen, inst, node, infer, history, tup|
    end

    define_ccgen_rule_method :+, Symbol do |ccgen, inst, node, infer, history, tup|
    end

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

    define_ccgen_rule_method :mul128, MMC_EXT::Vector do |ccgen, inst, node, infer, history, tup|
      arg0 = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      arg1 = (reg_real_value_noconv(ccgen, inst.inreg[2], node, tup, infer, history))[0]
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = (#{arg0}) * (#{arg1});\n"
    end

    define_ccgen_rule_method :[], MMC_EXT::Bitmap do |ccgen, inst, node, infer, history, tup|
      if inst.inreg.size == 3 then

      elsif inst.inreg.size == 4 then
        # v[i, size] = n
        inst.inreg[0].flush_type(tup)
        inst.inreg[1].flush_type(tup)
        inst.inreg[2].flush_type(tup)
        inst.outreg[0].flush_type(tup)
        nreg = inst.outreg[0]
        vecreg = inst.inreg[0]
        idxreg = inst.inreg[1]
        sizreg = inst.inreg[2]
        vectypes = vecreg.get_type(tup)
        idxtypes = idxreg.get_type(tup)
        sizetypes = sizreg.get_type(tup)
        vec, vect = (reg_real_value_noconv(ccgen, vecreg, node, tup, infer, history))
        idx, idxt = (reg_real_value_noconv(ccgen, idxreg, node, tup, infer, history))
        valid = false
        case idxtypes[0]
        when MTypeInf::IndexOfArrayType
          if idxtypes[0].base_array.element_num == vectypes[0].element_num then
            valid = true
          end

        when MTypeInf::LiteralType
          if vectypes[0].element_num and idxtypes[0].val < vectypes[0].element_num then
            valid = true
          end

        when MTypeInf::NumericType
          if vectypes[0].element_num and idxtypes[0].val < vectypes[0].element_num then
            valid = :check
          end

        else
          raise "Cant use this index type"
        end

        if sizetypes[0].is_a?(MTypeInf::LiteralType) then
          size = sizetypes[0].val
          case size
          when 8, 16, 32, 64
            ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
            ccgen.dcode << ";\n"

            if valid == :chack
              # TODO add range check
            end

            val = "((uint#{size}_t *)#{vec})[#{idx}/#{size}]"
            ccgen.pcode << "v#{nreg.id} = (#{val});\n"

          else
            raise "Not support this size"
          end

        else
          raise "This size type not support yet in Bitmap::[] #{sizetypes[0]}"
        end

      else
        raise "multiple argument not support yet in Array::[]"
      end
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
          staticsize = (idxtypes[0].base_array and
            idxtypes[0].base_array.is_a?(MTypeInf::ContainerType) and
            idxtypes[0].base_array.element_num and
            ectypes[0].element_num and
            idxtypes[0].base_array.element_num < vectypes[0].element_num)

          samesize = (idxtypes[0].base_array == vectypes[0].sizebase)

          if (staticsize or samesize) and
              sizetypes[0].is_a?(MTypeInf::LiteralType) then
            size = sizetypes[0].val
            case size
            when 8, 16, 32, 64
              ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
              ccgen.dcode << ";\n"
              valx = gen_type_conversion(ccgen, :mrb_int, valt, val, tup, node, infer, history, nreg)
              ccgen.pcode << "((uint#{size}_t *)#{vec})[#{idx}/#{size}] = (#{valx});\n"
              ccgen.pcode << "v#{nreg.id} = (#{val});\n"

            else
              raise "Not support this size"
            end
          else
            raise "Cant use this index type"
          end

        when MTypeInf::NumericType
          if sizetypes[0].is_a?(MTypeInf::LiteralType) then
            size = sizetypes[0].val
            case size
            when 8, 16, 32, 64
              # TODO Add range check
              ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
              ccgen.dcode << ";\n"
              val = gen_type_conversion(ccgen, :mrb_int, valt, val, tup, node, infer, history, nreg)
              ccgen.pcode << "((uint#{size}_t *)#{vec})[#{idx}/#{size}] = (#{val});\n"
              ccgen.pcode << "v#{nreg.id} = (#{val});\n"

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

    define_ccgen_rule_method :to_a, MMC_EXT::Vector do |ccgen, inst, node, infer, history, tup|
      ireg = inst.inreg[0]
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      src = (reg_real_value_noconv(ccgen, ireg, node, tup, infer, history))[0]
      ccgen.pcode << "v#{nreg.id} = (void *)&#{src};\n"
    end

    define_ccgen_rule_method :to_simd, MMC_EXT::Vector do |ccgen, inst, node, infer, history, tup|
      ireg = inst.inreg[0]
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      src = reg_real_value(ccgen, ireg, nreg, node, tup, infer, history)
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
    end

    define_ccgen_rule_method :copy, Array do |ccgen, inst, node, infer, history, tup|
      dst = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))[0]
      src = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      ccgen.pcode << "__builtin_memcpy(#{dst}, &#{src}, 16);\n"
    end
  end
end

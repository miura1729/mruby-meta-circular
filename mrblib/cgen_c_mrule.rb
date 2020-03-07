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

    def self.alias_ccgen_rule_method(nname, oname, rec)
      @@ruletab[:CCGEN_METHOD] ||= {}
      @@ruletab[:CCGEN_METHOD][nname] ||= {}
      if @@ruletab[:CCGEN_METHOD][nname][rec] then
        raise "Already defined #{name}"
      end
      @@ruletab[:CCGEN_METHOD][nname][rec] = @@ruletab[:CCGEN_METHOD][oname][rec]
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
      dstt = get_ctype(ccgen, nreg, tup, infer)
      src, srct = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
      src = gen_type_conversion(ccgen, :mrb_int, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_int, "-#{src}", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :-@, Float do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[0]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      src, srct = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
      src = gen_type_conversion(ccgen, :mrb_float2, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_float2, "-#{src}", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_class_method :sqrt, Math do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[1]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      src, srct = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
      src = gen_type_conversion(ccgen, :mrb_float2, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_float2, "sqrt(#{src})", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_class_method :sin, Math do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[1]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      src, srct = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
      src = gen_type_conversion(ccgen, :mrb_float2, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_float2, "sin(#{src})", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_class_method :cos, Math do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      ireg = inst.inreg[1]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      src, srct = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
      src = gen_type_conversion(ccgen, :mrb_float2, srct, src, tup, node, infer, history)
      src = gen_type_conversion(ccgen, dstt, :mrb_float2, "cos(#{src})", tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :begin, Range do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      lsttype = inst.outreg[0].flush_type(tup)[tup][0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      slf = inst.inreg[0]
      litty = nreg.type[tup][0]
      if litty.is_a?(MTypeInf::LiteralType) then
        ccgen.pcode << "v#{nreg.id} = #{litty.val};\n"

      elsif !slf.is_escape?(tup) then
        fst = slf.type[tup][0].element[0]
        src = reg_real_value(ccgen, fst, nreg, node, tup, infer, history)
        ccgen.pcode << "v#{nreg.id} = #{src};\n"

      else
        src, srct = reg_real_value_noconv(ccgen, slf, node, tup, infer, history)
        src = "(mrb_range_ptr(mrb, #{src}))->edges->beg"
        dstt = get_ctype(ccgen, nreg, tup, infer)
        src = gen_type_conversion(ccgen, dstt, srct, src, tup, node, infer, history)
        ccgen.pcode << "v#{nreg.id} = #{src};\n"
      end
      nil
    end

    alias_ccgen_rule_method :first, :begin, Range

    define_ccgen_rule_method :last, Range do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      lsttype = inst.outreg[0].flush_type(tup)[tup][0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      slf = inst.inreg[0]
      litty = nreg.type[tup][0]
      if litty.is_a?(MTypeInf::LiteralType) then
        ccgen.pcode << "v#{nreg.id} = #{litty.val};\n"

      elsif !slf.is_escape?(tup) then
        lst = slf.type[tup][0].element[1]
        src = reg_real_value(ccgen, lst, nreg, node, tup, infer, history)
        ccgen.pcode << "v#{nreg.id} = #{src};\n"

      else
        src, srct = reg_real_value_noconv(ccgen, slf, node, tup, infer, history)
        src = "(mrb_range_ptr(mrb, #{src}))->edges->end"
        dstt = get_ctype(ccgen, nreg, tup, infer)
        src = gen_type_conversion(ccgen, dstt, srct, src, tup, node, infer, history)
        ccgen.pcode << "v#{nreg.id} = #{src};\n"
      end
      nil
    end

    define_ccgen_rule_method :exclude_end?, Range do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      eetype = inst.outreg[0].type[tup][0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{eetype.val ? 1 : 0};\n"
      nil
    end

    define_ccgen_rule_method :to_f, Fixnum do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src = reg_real_value(ccgen, ireg, oreg,  node, tup, infer, history)
      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :to_f, Float do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src = reg_real_value(ccgen, ireg, oreg,  node, tup, infer, history)
      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :to_i, Fixnum do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src = reg_real_value(ccgen, ireg, oreg,  node, tup, infer, history)
      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :>>, Fixnum do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :>>)
      }
      nil
    end

    define_ccgen_rule_method :<<, Fixnum do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :<<)
      }
      nil
    end

    define_ccgen_rule_method :&, Fixnum do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :&)
      }
      nil
    end

    define_ccgen_rule_method :|, Fixnum do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :|)
      }
      nil
    end

    define_ccgen_rule_method :to_i, Float do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src = reg_real_value(ccgen, ireg, oreg,  node, tup, infer, history)
      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :p, Object do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      src, srct = reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history)
      src2 = gen_type_conversion(ccgen, :mrb_value, srct, src, tup, node, infer, history)
      ccgen.pcode << "mrb_p(mrb, #{src2});\n"
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :!=, BasicObject do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :!=)
      }
      nil
    end

    define_ccgen_rule_method :kind_of?, Object do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      res = nreg.type[tup]
      if res.size == 1 then
        ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
        ccgen.dcode << ";\n"
        ccgen.pcode "v#{nreg.id} = #{res[0].val}; /* kind_of? */\n"
      end
      nil
    end

    define_ccgen_rule_method :__printstr__, Kernel do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      src = reg_real_value(ccgen, inst.inreg[1], nreg, node, tup, infer, history)
      ccgen.pcode << "mrb_printstr(mrb, #{src});\n"
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :rand, Kernel do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      src = "(((mrb_float)(rand() % 0x7fffffff)) / (double)0x7fffffff)"
      src = gen_type_conversion(ccgen, dstt, :mrb_float2, src, tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :printf, Kernel do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      args = inst.inreg.map {|reg|
        (reg_real_value_noconv(ccgen, reg, node, tup, infer, history))[0]
      }.join(", ")
      src = "printf(#{args})"
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :!, TrueClass do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      src, srct = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(ccgen, :mrb_value, srct, src, tup, node, infer, history)
      src = "(!mrb_test(#{src}))"
      src = gen_type_conversion(ccgen, dstt, :mrb_bool, src, tup, node, infer, history)

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :nil?, Array do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      src, srct = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      src = gen_type_conversion(ccgen, :mrb_value, srct, src, tup, node, infer, history)
      src = "mrb_nil_p(#{src})"
      src = gen_type_conversion(ccgen, dstt, :mrb_bool, src, tup, node, infer, history)

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :[], Array do |ccgen, inst, node, infer, history, tup|
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      eele = inst.inreg[0].type[tup][0].element
      elereg = eele[uv]
      nreg = inst.outreg[0]
      aryreg = inst.inreg[0]
      dstt = get_ctype(ccgen, inst.outreg[0], tup, infer)
      src, srct = reg_real_value_noconv(ccgen, aryreg, node, tup, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      idx = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      if inst.inreg[0].is_escape?(tup) then
        src = "mrb_ary_ref(mrb, #{src}, #{idx})"
        src = gen_type_conversion(ccgen, dstt, :mrb_value, src, tup, node, infer, history)
      else
        etup = tup
        if elereg.type[etup] == nil then
          etup = elereg.type.keys[0]
        end
        srct = get_ctype(ccgen, elereg, etup, infer)
        if srct == :nil then
          src = "mrb_nil_value()"
        else
          src = "#{src}[#{gen_array_range_check(ccgen, inst, tup, idx)}]"
          src = gen_type_conversion(ccgen, dstt, srct, src, tup, node, infer, history)
        end
      end

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :[]=, Array do |ccgen, inst, node, infer, history, tup|
      uv = MTypeInf::ContainerType::UNDEF_VALUE
      elereg = inst.inreg[0].type[tup][0].element[uv]
      slf, slft = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      val, valt = reg_real_value_noconv(ccgen, inst.inreg[2], node, tup, infer, history)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      idx = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      if inst.inreg[0].is_escape?(tup) then
        val2 = gen_type_conversion(ccgen, :mrb_value, valt, val, tup, node, infer, history)
        ccgen.pcode << "mrb_ary_set(mrb, #{slf}, #{idx}, #{val2});\n"
      else
        srct = get_ctype(ccgen, elereg, tup, infer)
        val2 = gen_type_conversion(ccgen, srct, valt, val, tup, node, infer, history)
        ccgen.pcode << "#{slf}[#{gen_array_range_check(ccgen, inst, tup, idx)}] = #{val2};\n"
      end

      ccgen.pcode << "v#{nreg.id} = #{val};\n"
      nil
    end

    define_ccgen_rule_method :push, Array do |ccgen, inst, node, infer, history, tup|
      dstt = get_ctype(ccgen, inst.outreg[0], tup, infer)
      src, srct = reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history)
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      val, valt = reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history)
      val = gen_type_conversion(ccgen, :mrb_value, valt, val, tup, node, infer, history)
      ccgen.pcode <<  "mrb_ary_push(mrb, #{src}, #{val});\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :length, Array do |ccgen, inst, node, infer, history, tup|
      dstt = get_ctype(ccgen, inst.outreg[0], tup, infer)
      src = (reg_real_value_noconv(ccgen, inst.inreg[0], node, tup, infer, history))[0]
      nreg = inst.outreg[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      if inst.inreg[0].is_escape?(tup) then
        src = "ARY_LEN(mrb_ary_ptr(#{src}))"
        src = gen_type_conversion(ccgen, dstt, :mrb_int, src, tup, node, infer, history)
      else
        src = gen_type_conversion(ccgen, dstt, :mrb_int, inst.inreg[0].type[tup][0].element.keys.size - 1, tup, node, infer, history)
      end

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
      nil
    end

    alias_ccgen_rule_method :size, :length, Array

    define_ccgen_rule_class_method :new, Array do |ccgen, inst, node, infer, history, tup|
      #recvtypes = inst.inreg[0].flush_type_alltup(tup)[tup]
      recvtypes = inst.inreg[0].flush_type(tup)[tup]
      argc = inst.para[1]
      oreg = inst.outreg[0]
      otype = oreg.type[tup][0]
      initsize = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]

      if recvtypes.size == 1 then
        uv = MTypeInf::ContainerType::UNDEF_VALUE
        eele = inst.outreg[0].type[tup][0].element
        recvt = recvtypes[0].class_object

        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"

        if initsize == "mrb_nil_value()" then
          initsize = inst.outreg[0].type[tup][0].element.size
        end

        if oreg.is_escape?(tup) then
          gen_gc_table(ccgen, inst, node, infer, history, tup)
          ccgen.pcode << "mrb->ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_ary_new_capa(mrb, #{initsize});\n"
          ccgen.pcode << "for (int i = 0;i < #{initsize}; i++) ARY_PTR(mrb_ary_ptr(v#{oreg.id}))[i] = mrb_nil_value();\n"
          ccgen.pcode << "ARY_SET_LEN(mrb_ary_ptr(v#{oreg.id}), #{initsize});\n"
          ccgen.callstack[-1][1] = true
        else
          etup = tup
          ereg = eele[uv]
          if ereg.type[tup] == nil then
            etup = ereg.type.keys[0]
          end
          etype = get_ctype_aux(ccgen, ereg, etup, infer)

          if otype.place.keys.any? {|e|
              e.is_a?(MTypeInf::UserDefinedType) or
              e == :return_fst or
              (e.is_a?(MTypeInf::ContainerType) and
                e.place.keys.any? {|e1|
                  e1.is_a?(MTypeInf::UserDefinedType) or
                  e1 == :return_fst
                })
            } then
            ccgen.pcode << "v#{oreg.id} = prevgctab->caller_alloc;\n"
            ccgen.pcode << "prevgctab->caller_alloc += sizeof(#{etype}) * #{initsize + 1};\n"
          else
            ccgen.pcode << "v#{oreg.id} = alloca(sizeof(#{etype}) * #{initsize + 1});\n"
          end
          if etype == :mrb_value then
            ccgen.pcode << "for (int i = 0;i < #{initsize}; i++) v#{oreg.id}[i] = mrb_nil_value();\n"
            ccgen.pcode << "v#{oreg.id}[#{initsize}].value.ttt = MRB_TT_FREE;\n"
            csize = ccgen.gccomplex_size
            ccgen.gccomplex_size += 1
            ccgen.pcode << "gctab->complex[#{csize}] = v#{oreg.id};\n"
            ccgen.pcode << "gctab->csize = #{ccgen.gccomplex_size};\n"
          end
        end
      end
      nil
    end

    define_ccgen_rule_method :call, Proc do |ccgen, inst, node, infer, history, tup|
      procty = get_ctype(ccgen, inst.inreg[0], tup, infer)
      intype = inst.inreg.map {|reg| reg.type[tup] || []}
      ptype = intype[0][0]
      proc = inst.inreg[0]
      intype[0] = [ptype.slf]
      nreg = inst.outreg[0]
      if intype[0].size == 1 then
        # store proc object only 1 kind.
        utup = infer.typetupletab.get_tupple_id(intype, ptype, tup)
        procvar = (reg_real_value_noconv(ccgen, proc, node, tup, infer, history))[0]

        regs =  ptype.irep.allocate_reg[utup]
        if regs
          regs = regs.uniq
          regstr = ""
          rets = regs.inject([]) {|res, reg|
            rsize = gen_typesize(ccgen, reg, utup, infer)
            if rsize then
              res << rsize
            end
            res
          }
          if rets.size > 0 then
            ccgen.caller_alloc_size += 1
            ccgen.pcode << "gctab->caller_alloc = alloca(#{rets.join(' + ')});\n"
          end
        end

        codeno = ptype.using_tup[utup]
        outtype0 = get_ctype(ccgen, ptype.irep.retreg, utup, infer)
        outtype = get_ctype(ccgen, nreg, tup, infer)
        args = inst.inreg[0..-2].map {|reg|
          (reg_real_value_noconv(ccgen, reg, node, tup, infer, history))[0]
        }.join(", ")
        reg = inst.inreg[-1]
        tys = reg.type[tup]
        if tys and tys.size == 1 and tys[0].class_object != NilClass then
          args << ", "
          args << (reg_real_value_noconv(ccgen, reg, node, tup, infer, history))[0]
        end
        args << ", gctab"
        argt = inst.inreg.map {|reg|
          gen_declare(ccgen, reg, tup, infer)
        }.join(", ")
        argt << ", struct gctab *"
        if procty == :mrb_value then
          fname = "(MRB_PROC_CFUNC(mrb_proc_ptr(#{procvar})))"
          fname = "((#{outtype0} (*)(mrb_tsate *, #{argt}))(((void **)#{fname})[#{codeno}]))"
          ccgen.using_block.push ccgen.proctab[ptype.irep][codeno]
        elsif ccgen.proctab[ptype.irep] then
          minf = ccgen.proctab[ptype.irep][codeno]
          fname = minf[0]
          ccgen.using_block.push minf
        else
          fname = "((#{outtype0} (*)(mrb_state *, #{argt}))((struct proc#{ptype.id} *)(#{procvar}))->code[#{codeno}])"
          raise "Unnown proc"
        end

        ccgen.dcode << CodeGen::gen_declare(ccgen, nreg, tup, infer)
        ccgen.dcode << ";\n"
        src = "(#{fname}(mrb, #{args}))"
        src = gen_type_conversion(ccgen, outtype, outtype0, src, tup, node, infer, history)
        ccgen.pcode << "v#{nreg.id} = #{src};\n"
      end
      nil
    end

    define_ccgen_rule_method :to_s, String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src = reg_real_value(ccgen, ireg, oreg,  node, tup, infer, history)

      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :[], String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      strreg = inst.inreg[0]
      dstt = get_ctype(ccgen, inst.outreg[0], tup, infer)
      src, srct = reg_real_value_noconv(ccgen, strreg, node, tup, infer, history)
      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      idx = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      if inst.inreg[0].type[tup][0].is_a?(MTypeInf::StringType) then
        if idx < 0 then
          src = "#{src}[strlen(#{src}) - #{idx}]"
        else
          src = "#{src}[#{idx}]"
        end
        src = gen_type_conversion(ccgen, dstt, srct, src, tup, node, infer, history)
      else
        src = "mrb_str_aref(mrb, #{src}, #{idx})"
        src = gen_type_conversion(ccgen, dstt, :mrb_value, src, tup, node, infer, history)
      end

      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :new, Class do |ccgen, inst, node, infer, history, tup|
#      recvtypes = inst.inreg[0].flush_type_alltup(tup)[tup]
      recvtypes = inst.inreg[0].flush_type(tup)[tup]
      argc = inst.para[1]
      inreg = inst.inreg.clone
      oreg = inst.outreg[0]
      otype = oreg.type[tup][0]
      clsssa = RiteSSA::ClassSSA.get_instance(otype.class_object)

      if recvtypes.size == 1 then
        recvt = recvtypes[0].class_object

        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"
        if oreg.is_escape?(tup) then
          gen_gc_table(ccgen, inst, node, infer, history, tup)
          ccgen.pcode << "mrb->ud = (void *)gctab;\n"
          ccgen.pcode << "v#{oreg.id} = mrb_ary_new_capa(mrb, #{clsssa.iv.size});\n"
          ccgen.pcode << "for (int i = 0;i < #{clsssa.iv.size}; i++) ARY_PTR(mrb_ary_ptr(v#{oreg.id}))[i] = mrb_nil_value();\n"
          ccgen.pcode << "ARY_SET_LEN(mrb_ary_ptr(v#{oreg.id}), #{clsssa.iv.size});\n"
          ccgen.callstack[-1][1] = true
        else
          ccgen.using_class[clsssa] ||= {}
          nilobj = MTypeInf::PrimitiveType.new(NilClass)
          ivtypes = [nilobj]
          clsssa.iv.each do |nm, reg|
            val = reg.flush_type(tup)[tup]
            if !val then
              val = reg.flush_type_alltup(tup)[tup]
            end
            ivtypes.push val
          end
          ivtup = infer.typetupletab.get_tupple_id(ivtypes, nilobj, tup)
          clsssa.iv.each do |nm, reg|
            if reg.type[tup] then
              reg.type[ivtup] = reg.type[tup].map {|e| e}
            end
          end
          clsid = ["cls#{clsssa.id}_#{ivtup}", otype.hometown]
          ccgen.using_class[clsssa][ivtup] ||= clsid
          if otype.place.keys.any? {|e|
              e.is_a?(MTypeInf::UserDefinedType) or
              e == :return_fst or
              (e.is_a?(MTypeInf::ContainerType) and
                e.place.keys.any? {|e1|
                  e1.is_a?(MTypeInf::UserDefinedType) or
                  e1 == :return_fst
                })
            } then
            ccgen.pcode << "v#{oreg.id} = prevgctab->caller_alloc;\n"
            ccgen.pcode << "prevgctab->caller_alloc += sizeof(struct #{clsid[0]});\n"
          else
            ccgen.pcode << "v#{oreg.id} = alloca(sizeof(struct #{clsid[0]}));\n"
          end
          csize = ccgen.gcobject_size
          i = 0
          clsssa.iv.each do |name, reg|
            if !reg.type.values[0][0].is_gcobject? then
              next
            end
            if reg.is_escape?(ivtup) then
              code = "v#{oreg.id}->v#{reg.id} = mrb_nil_value();\n"
              code << "gctab->object[#{ccgen.gcobject_size}] = &v#{oreg.id}->v#{reg.id};"
              code <<  "/* #{name} */\n"
              ccgen.gcobject_size += 1
            else
              code = "v#{oreg.id}->v#{reg.id} = NULL;\n"
            end
            ccgen.pcode << code
            i += 1
          end
          if csize != ccgen.gcobject_size then
            ccgen.pcode << "gctab->osize = #{ccgen.gcobject_size};\n"
          end
        end

        inreg[0] = oreg
        #op_send_initialize(ccgen, inst, inreg, nil, node, infer, history, tup, :initialize)
        op_send_aux(ccgen, inst, inreg, nil, node, infer, history, tup, :initialize)
      end
      nil
    end
  end
end

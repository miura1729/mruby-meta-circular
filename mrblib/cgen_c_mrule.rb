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
      litty = nreg.get_type(tup)[0]
      if nreg.get_type(tup).size == 1 and litty.is_a?(MTypeInf::LiteralType) then
        ccgen.pcode << "v#{nreg.id} = #{litty.val};\n"

      elsif !slf.is_escape?(tup) then
        slfcd, ty = reg_real_value_noconv(ccgen, slf, node, tup, infer, history)
        ccgen.pcode << "v#{nreg.id} = #{slfcd}->first;\n"

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
      litty = nreg.get_type(tup)[0]
      if nreg.get_type(tup).size == 1 and litty.is_a?(MTypeInf::LiteralType) then
        ccgen.pcode << "v#{nreg.id} = #{litty.val};\n"

      elsif !slf.is_escape?(tup) then
        slfcd, ty = reg_real_value_noconv(ccgen, slf, node, tup, infer, history)
        ccgen.pcode << "v#{nreg.id} = #{slfcd}->last;\n"

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
      eetype = inst.outreg[0].get_type(tup)[0]
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{eetype.val ? 1 : 0};\n"
      nil
    end

    define_ccgen_rule_method :[]=, Hash do |ccgen, inst, node, infer, history, tup|
      hreg = inst.inreg[0]
      kreg = inst.inreg[1]
      vreg = inst.inreg[2]
      oreg = inst.outreg[0]
      dstt = get_ctype(ccgen, oreg, tup, infer)
      hashsrc = reg_real_value(ccgen, hreg, oreg, node, tup, infer, history)
      keysrc = reg_real_value(ccgen, kreg, oreg, node, tup, infer, history)
      valsrc = reg_real_value(ccgen, vreg, oreg, node, tup, infer, history)
      src = "mrb_hash_set(mrb, #{hashsrc}, #{keysrc}, #{valsrc})"
      src = gen_type_conversion(ccgen, dstt, :mrb_value, src, tup, node, infer, history)
      ccgen.pcode << "#{src}\n;"
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

    define_ccgen_rule_method :==, Fixnum do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :==)
      }
      nil
    end

    define_ccgen_rule_method :>>, Fixnum do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :>>)
      }
      nil
    end

    define_ccgen_rule_method :<<, Fixnum do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :<<)
      }
      nil
    end

    define_ccgen_rule_method :&, Fixnum do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :&)
      }
      nil
    end

    define_ccgen_rule_method :|, Fixnum do |ccgen, inst, node, infer, history, tup|
      do_if_multi_use(ccgen, inst, node, infer, history, tup) {
        gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.para[1], :|)
      }
      nil
    end

    define_ccgen_rule_method :chr, Fixnum do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src, srct = reg_real_value_noconv(ccgen, ireg,  node, tup, infer, history)

      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "{ char *tmpstr = alloca(2);\n"
      ccgen.pcode << "tmpstr[0] = #{src};\n"
      ccgen.pcode << "tmpstr[1] = '\\0';\n"
      gen_gc_table(ccgen, inst, node, infer, history, tup)
      ccgen.pcode << "mrb->ud = (void *)gctab;\n"
      if oreg.is_escape?(tup) then
        ccgen.pcode << "v#{oreg.id} = mrb_str_new_cstr(mrb, tmpstr);\n"
      else
        ccgen.pcode << "v#{oreg.id} = tmpstr;\n"
      end
      ccgen.pcode << "}\n"
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

      gen_gc_table(ccgen, inst, node, infer, history, tup)
      ccgen.pcode << "mrb->ud = (void *)gctab;\n"
      ccgen.pcode << "mrb_p(mrb, #{src2});\n"
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{nreg.id} = #{src};\n"

      nil
    end

    define_ccgen_rule_method :!=, BasicObject do |ccgen, inst, node, infer, history, tup|

      nreg = inst.outreg[0]
      src, srct = gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :==)
      ccgen.pcode << "v#{nreg.id} = (!(#{src}));\n"
      nil
    end

    define_ccgen_rule_method :kind_of?, Object do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      res = nreg.get_type(tup)
      if res.size == 1 then
        ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
        ccgen.dcode << ";\n"
        ccgen.pcode "v#{nreg.id} = #{res[0].val}; /* kind_of? */\n"
      end
      nil
    end

    define_ccgen_rule_method :===, Kernel do |ccgen, inst, node, infer, history, tup|
      gen_term_top(ccgen, inst, node, tup, infer, history, inst.inreg[0], inst.inreg[1], :==)
      nil
    end

    define_ccgen_rule_method :__printstr__, Kernel do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      src = reg_real_value(ccgen, inst.inreg[1], nreg, node, tup, infer, history)
      ccgen.pcode << "printf(\"%s\", #{src});\n"
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

    define_ccgen_rule_method :sprintf, Kernel do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      argc = inst.para[1]
      args = nil
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"

      ccgen.pcode << "{\n"
      ccgen.pcode << "char *buf = alloca(256);\n"
      if argc == 127 then
        argv = inst.inreg[1].type[tup][0].element
        argc = argv.size + 1
        if argv[MTypeInf::ContainerType::UNDEF_VALUE] then
          argc = argc - 1
        end

        argregs = node.root.nodes[0].enter_reg
      else
        argregs = inst.inreg
      end
      argsv = []
      (argc - 1).times do |i|
        val = (reg_real_value_noconv(ccgen, argregs[i + 1], node, tup, infer, history))[0]
        argsv.push val
      end
      args = argsv.join(', ')
      src = "sprintf(buf, #{args})"
      ccgen.pcode << "#{src};v#{nreg.id} = buf;\n"
      ccgen.pcode << "}\n"

      nil
    end

    define_ccgen_rule_method :raise, Kernel do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      arg, argt = reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history)
      arg = gen_type_conversion(ccgen, :mrb_value, argt, arg, tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "mrb_exc_raise(mrb, mrb_make_exception(mrb, 1, &#{arg}));\n"

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

    define_ccgen_rule_method :==, NilClass do |ccgen, inst, node, infer, history, tup|
      nreg = inst.outreg[0]
      argcls = inst.inreg[1].get_type(tup)[0].class_object
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      if argcls == NilClass then
        src, srct = reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history)
        src = gen_type_conversion(ccgen, :mrb_value, srct, src, tup, node, infer, history)
        src = "mrb_nil_p(#{src})"
        ccgen.pcode << "v#{nreg.id} = #{src};\n"
      else
        ccgen.pcode << "v#{nreg.id} = 0;\n"
      end
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
      eele = inst.inreg[0].get_type(tup)[0].element
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
      elereg = inst.inreg[0].get_type(tup)[0].element[uv]
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

    define_ccgen_rule_method :__svalue, Array do |ccgen, inst, node, infer, history, tup|
      aryreg = inst.inreg[0]
      eele = aryreg.get_type(tup)[0].element
      elereg = eele[0]
      nreg = inst.outreg[0]
      dstt = get_ctype(ccgen, nreg, tup, infer)
      src, srct = reg_real_value_noconv(ccgen, aryreg, node, tup, infer, history)
      ccgen.dcode << gen_declare(ccgen, nreg, tup, infer)
      ccgen.dcode << ";\n"
      if inst.inreg[0].is_escape?(tup) then
        src = "(ARY_PTR(mrb_ary_ptr(src))[0])"
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
          src = "#{src}[0]"
          src = gen_type_conversion(ccgen, dstt, srct, src, tup, node, infer, history)
        end
      end

      ccgen.pcode << "v#{nreg.id} = #{src};\n"
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

    alias_ccgen_rule_method :<<, :push, Array

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
        src = gen_type_conversion(ccgen, dstt, :mrb_int, inst.inreg[0].get_type(tup)[0].element.keys.size - 1, tup, node, infer, history)
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
      otype = oreg.get_type(tup)[0]
      initsize = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]

      if recvtypes.size == 1 then
        uv = MTypeInf::ContainerType::UNDEF_VALUE
        eele = inst.outreg[0].get_type(tup)[0].element
        recvt = recvtypes[0].class_object

        ccgen.dcode << "#{gen_declare(ccgen, oreg, tup, infer)};\n"

        if initsize == "mrb_nil_value()" then
          initsize = inst.outreg[0].get_type(tup)[0].element.size
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
          if ereg.get_type(tup) == nil then
            etup = ereg.type.keys[0]
          end
          etype = get_ctype_aux(ccgen, ereg, etup, infer)

          if can_use_caller_area(otype) then
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
      ret_chk = 3
      nreg = inst.outreg[0]
      MTypeInf::TypeInferencer::make_intype(infer, inst.inreg, node, tup, inst.para[1]) do |intype, argc|
        ptype = intype[0][0]
        proc = inst.inreg[0]
        intype[0] = [ptype.slf]
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
          tys = reg.get_type(tup)
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
            ret_chk = 0
            ret_chk |= minf[1].irep.have_break ? 1 : 0
            ret_chk |= minf[1].irep.have_return ? 2 : 0
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
      end

      if ret_chk then
        retsrc = reg_real_value(ccgen, nreg, node.root.retreg, node, tup, infer, history)
        if ret_chk & 1 != 0 then
          ccgen.have_ret_handler = true
          ccgen.pcode << "if (gctab->ret_status) return #{retsrc};\n"
        end

        if ret_chk & 2 != 0 then
          ccgen.have_ret_handler = true
          ccgen.pcode << "if (gctab->ret_status) {\n"
          ccgen.pcode << "prevgctab->ret_status = 2;\n"
          ccgen.pcode << "return #{retsrc};\n"
          ccgen.pcode << "}\n"
        end
      end
      nil
    end

    def self.gen_get_strbuf(oreg, osize, tup)
      otype = oreg.get_type(tup)[0]
      size = otype.size
      if can_use_caller_area(otype) and size then
        "char *tmpstr = prevgctab->caller_alloc; prevgctab->caller_alloc += ((#{(size / 4).to_i} + 1) * 4);"
      else
        "char *tmpstr = alloca(#{osize});"
      end
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

    define_ccgen_rule_method :to_f, String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src = reg_real_value(ccgen, ireg, oreg,  node, tup, infer, history)

      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :to_i, String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src = reg_real_value(ccgen, ireg, oreg,  node, tup, infer, history)

      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
      nil
    end

    define_ccgen_rule_method :downcase, String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      ireg = inst.inreg[0]
      src, srct = reg_real_value_noconv(ccgen, ireg, node, tup, infer, history)
      src = gen_type_conversion(ccgen, :mrb_value, srct, src, tup, node, infer, history)
      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
#      ccgen.pcode << "v#{oreg.id} = mrb_str_downcase(mrb, #{src});\n"
      ccgen.pcode << "v#{oreg.id} = #{src};\n"
    end

    define_ccgen_rule_method :size, String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      sreg = inst.inreg[0]
      strsrc = reg_real_value_noconv(ccgen, sreg,  node, tup, infer, history)
      stype = sreg.get_type(tup)[0]
      sizesrc = nil
      if sreg.get_type(tup).size == 1 and stype.is_a?(MTypeInf::LiteralType) then
        sizesrc = stype.val.size.to_s

      elsif stype.is_escape? then
        sizesrc = "RSTRING_LEN(#{strsrc})"

      else
        sizesrc = "strlen(#{strsrc})"
      end
      otype = get_ctype(ccgen, oreg, tup, infer)
      val = gen_type_conversion(ccgen, otype, :mrb_int, sizesrc, tup, node, infer, history)

      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = #{val};\n"
      nil
    end

    define_ccgen_rule_method :==, String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      sreg0 = inst.inreg[0]
      strsrc0, srct0 = reg_real_value_noconv(ccgen, sreg0,  node, tup, infer, history)
      stype0 = sreg0.get_type(tup)[0]
      sreg1 = inst.inreg[1]
      strsrc1, srct1 = reg_real_value_noconv(ccgen, sreg1,  node, tup, infer, history)
      stype1 = sreg1.get_type(tup)[0]
      if sreg0.get_type(tup).size != 1 or stype0.is_escape? then
        strsrc0 = "RSTRING_PTR(#{strsrc0})"
      end
      if sreg0.get_type(tup).size != 1 or stype1.is_escape? then
        strsrc1 = "RSTRING_PTR(#{strsrc1})"
      end

      src = "(!strcmp(#{strsrc0}, #{strsrc1}))"
      otype = get_ctype(ccgen, oreg, tup, infer)
      val = gen_type_conversion(ccgen, otype, :mrb_bool, src, tup, node, infer, history)

      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = #{val};\n"
      nil
    end

    define_ccgen_rule_method :+, String do |ccgen, inst, node, infer, history, tup|
      @@ruletab[:CCGEN][:STRCAT].call(ccgen, inst, node, infer, history, tup)
      nil
    end

    define_ccgen_rule_method :[], String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      strreg = inst.inreg[0]
      dstt = get_ctype(ccgen, inst.outreg[0], tup, infer)
      src, srct = reg_real_value_noconv(ccgen, strreg, node, tup, infer, history)
      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      idxtype = inst.inreg[1].get_type(tup)[0]
      idx = (reg_real_value_noconv(ccgen, inst.inreg[1], node, tup, infer, history))[0]
      strtype = inst.inreg[0].get_type(tup)[0]
      if strreg.get_type(tup).size == 1 and strtype.is_a?(MTypeInf::LiteralType) then
        strval = strtype.val
        #strval = src
        ccgen.pcode << "{ #{gen_get_strbuf(oreg, 2, tup)}\n"
        if inst.inreg[1].get_type(tup).size == 1 and idxtype.is_a?(MTypeInf::LiteralType) then
          src = unescape_string(strval[idxtype.val])
        else
          src = "#{unescape_string(strval)}[#{idx}]"
        end
        ccgen.pcode << "tmpstr[0] = #{src};\n"
        ccgen.pcode << "tmpstr[1] = '\\0';\n"

        src = gen_type_conversion(ccgen, dstt, srct, "tmpstr", tup, node, infer, history)
        ccgen.pcode << "v#{oreg.id} = #{src};\n"
        ccgen.pcode << "};\n"

      elsif !strtype.is_escape? then
        if  idxtype.class_object == Fixnum then
          if idxtype.is_a?(MTypeInf::LiteralType) then
            ccgen.pcode << "{ #{gen_get_strbuf(oreg, 2, tup)}\n"
            if idx < 0 then
              src = "#{src}[strlen(#{src}) + #{idx}]"
            else
              src = "#{src}[#{idx}]"
            end
          else
            ccgen.pcode << "{ #{gen_get_strbuf(oreg, 2, tup)}\n"
            if idxtype.positive then
              src = "#{src}[#{idx}]"
            else
              src1 = "#{src}[#{idx}]"
              src2 = "#{src}[strlen(#{src}) + #{idx}]"
              src = "((#{idx} > 0) ? (#{src1}) : (#{src2}))"
            end
          end

          ccgen.pcode << "tmpstr[0] = #{src};\n"
          ccgen.pcode << "tmpstr[1] = '\\0';\n"

        elsif idxtype.class_object == Range then
          if idxtype.is_escape? then
            # TODO boxed Ramge support

          else
            ccgen.pcode << "{ \n"
            ccgen.pcode << "int last = ((#{idx}->last < 0) ? strlen(#{src}) + #{idx}->last : #{idx}->last);\n"
            ccgen.pcode << "char *tmpstr = alloca(last - #{idx}->first + 1);\n" 
            ccgen.pcode << "memcpy(tmpstr, #{src} + #{idx}->first, last - #{idx}->first);\n"
            ccgen.pcode << "tmpstr[last] = \'\\0\';\n"
          end

        else
          raise "Not Support index #{idxtype}"
        end

        src = gen_type_conversion(ccgen, dstt, srct, "tmpstr", tup, node, infer, history)
        ccgen.pcode << "v#{oreg.id} = #{src};\n"
        ccgen.pcode << "};\n"
      else
        src = "mrb_str_aref(mrb, #{src}, #{idx})"
        src = gen_type_conversion(ccgen, dstt, :mrb_value, src, tup, node, infer, history)
        ccgen.pcode << "v#{oreg.id} = #{src};\n"
      end

      nil
    end

    define_ccgen_rule_method :include?, String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      strreg = inst.inreg[0]
      parareg = inst.inreg[1]
      str, strt = reg_real_value_noconv(ccgen, strreg, node, tup, infer, history)
      str = gen_type_conversion(ccgen, :mrb_value, strt, str, tup, node, infer, history)

      para, parat = reg_real_value_noconv(ccgen, parareg, node, tup, infer, history)
      para = gen_type_conversion(ccgen, [:char, "*"], parat, para, tup, node, infer, history)

      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = (mrb_str_index(mrb, #{str}, #{para}, strlen(#{para}), 0) >= 0);\n"
    end

    define_ccgen_rule_method :index, String do |ccgen, inst, node, infer, history, tup|
      oreg = inst.outreg[0]
      strreg = inst.inreg[0]
      parareg = inst.inreg[1]
      str, strt = reg_real_value_noconv(ccgen, strreg, node, tup, infer, history)
      str = gen_type_conversion(ccgen, :mrb_value, strt, str, tup, node, infer, history)

      para, parat = reg_real_value_noconv(ccgen, parareg, node, tup, infer, history)
      para = gen_type_conversion(ccgen, [:char, "*"], parat, para, tup, node, infer, history)

      ccgen.dcode << gen_declare(ccgen, oreg, tup, infer)
      ccgen.dcode << ";\n"
      ccgen.pcode << "v#{oreg.id} = mrb_fixnum_value(mrb_str_index(mrb, #{str}, #{para},strlen(#{para}), 0));\n"
    end

    define_ccgen_rule_method :new, Class do |ccgen, inst, node, infer, history, tup|
#      recvtypes = inst.inreg[0].flush_type_alltup(tup)[tup]
      recvtypes = inst.inreg[0].flush_type(tup)[tup]
      argc = inst.para[1]
      inreg = inst.inreg.clone
      oreg = inst.outreg[0]
      otype = oreg.get_type(tup)[0]
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
            if reg.get_type(tup) then
              reg.type[ivtup] = reg.get_type(tup).map {|e| e}
            end
          end
          clsid = ["cls#{clsssa.id}_#{ivtup}", otype.hometown]
          ccgen.using_class[clsssa][ivtup] ||= clsid
          if can_use_caller_area(otype) then
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

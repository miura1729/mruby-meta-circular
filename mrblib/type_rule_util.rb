module MTypeInf
  class TypeInferencer
    @@ruby_methodtab ||= {}

    def self.rule_send_common (infer, inst, node, tup)
      name = inst.para[0]
      intype = inst.inreg.map {|reg| reg.type[tup]}
      ntup = infer.typetupletab.get_tupple_id(intype)


      recvtypes = inst.inreg[0].flush_type_alltup(tup)[tup]
      exitf = nil

      recvtypes.each do |ty|
        slf = ty.class_object
        slfcls = slf
        @@ruby_methodtab[name] ||= {}
        irepssa = nil
        if @@ruby_methodtab[name] then
          irepssa = @@ruby_methodtab[name][slfcls]
        end

        irep = nil
        if irepssa.nil? then
          irep = Irep::get_irep(slf, name)
          if irep == nil then
            if @@ruby_methodtab[:method_missing] and @@ruby_methodtab[:method_missing][slfcls] then
              irep = Irep::get_irep(slf, :method_missing)

            elsif @@ruletab[:METHOD][name] and @@ruletab[:METHOD][name][slfcls] then
              # written in C or method mmsing  or no method error
              @@ruletab[:METHOD][name][slfcls].call(infer, inst, node, tup)

            else
              p "Method missing able to call #{slf}##{name}"
              # No method found
            end
          else
            irepssa =  RiteSSA::Block.new(irep, nil, slfcls)
            @@ruby_methodtab[name][slfcls] = irepssa
          end
        end

        if irepssa then
          infer.inference_block(irepssa, intype, ntup)
          inst.outreg[0].add_same irepssa.retreg
        end
      end

      inst.outreg[0].flush_type(tup, ntup)
      nil
    end

    def self.rule_send_cfimc(infer, inst, node, tup)
    end
  end
end


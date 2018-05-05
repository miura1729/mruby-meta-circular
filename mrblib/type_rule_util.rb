module MTypeInf
  class TypeInferencer
    @@ruby_methodtab ||= {}

    def self.rule_send_common (infer, inst, node, tup)
      name = inst.para[0]
      intype = inst.inreg.map {|reg| reg.type[tup]}
      ntup = infer.typetupletab.get_tupple_id(intype)

      inst.inreg[0].type[tup].each do |ty|
        slf = ty.class_object
        slfcls = slf
        @@ruby_methodtab[name] ||= {}
        irepssa = nil
        if @@ruby_methodtab[name] then
          irepssa = @@ruby_methodtab[name][slfcls]
        end
        if irepssa.nil? then
          irep = Irep::get_irep(slf, name)
          if irep == nil then
            # written in C or method mmsing  or no method error
            if @@ruletab[:METHOD][name] and @@ruletab[:METHOD][name][slfcls] then
              @@ruletab[:METHOD][name][slfcls].call(infer, inst, node, tup)
              return nil
            elsif @@ruby_methodtab[:method_missing] && @@ruby_methodtab[:method_missing][slfcls] then
              irep = Irep::get_irep(slf, :method_missing)
            else
              p "Method missing"
              # No method found
            end
          end

          irepssa =  RiteSSA::Block.new(irep, nil, slfcls)
          @@ruby_methodtab[name][slfcls] = irepssa
        end

        if irepssa.retreg.flush_type(ntup)[ntup].nil? then
          infer.inference_block(irepssa, intype)
        end
        inst.outreg[0].add_same irepssa.retreg
      end
      inst.outreg[0].flush_type(tup, ntup)
      nil
    end

    def self.rule_send_cfimc(infer, inst, node, tup)
    end
  end
end


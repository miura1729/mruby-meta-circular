module MTypeInf
  class TypeInferencer
    @@ruby_methodtab ||= {}

    def self.rule_literal_commin(infer, inst, node, tup)
      val = inst.para[0]
      type = LiteralType.new(val.class, val)
      inst.outreg[0].add_type(type, tup)
      nil
    end

    def self.rule_send_common_aux(infer, inst, node, tup, name, intype, recreg)
      ntup = infer.typetupletab.get_tupple_id(intype)
      recvtypes = recreg.flush_type_alltup(tup)[tup]

      @@ruby_methodtab[name] ||= {}
      misirep = nil
      recvtypes.each do |ty|
        existf = false
        slf = ty.class_object
        slf.ancestors.each do |slfcls|
          irep = nil

          irepssa = @@ruby_methodtab[name][slfcls]
          if irepssa.nil? then
            irep = Irep::get_irep(slf, name)
            if irep == nil then
              if @@ruletab[:METHOD][name] and @@ruletab[:METHOD][name][slfcls] then
                # written in C or method mmsing  or no method error
                existf = true
                @@ruletab[:METHOD][name][slfcls].call(infer, inst, node, tup)

              else
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
            existf = true
          end

          if existf then
            break
          end
        end
        if !existf then
          # No method fuound
          irep = Irep::get_irep(slf, :method_missing)
          if irep then
            irepssa =  RiteSSA::Block.new(irep, nil, slfcls)
            @@ruby_methodtab[name][slfcls] = irepssa
            infer.inference_block(irepssa, intype, ntup)
            inst.outreg[0].add_same irepssa.retreg
          else
            print "Method missing able to call #{slf}##{name} in #{inst.line}:#{inst.filename}\n"
          end
        end
      end

      inst.outreg[0].flush_type(tup, ntup)
      nil
    end

    def self.rule_send_common (infer, inst, node, tup)
      name = inst.para[0]
      intype = inst.inreg.map {|reg| reg.type[tup]}
      recreg = inst.inreg[0]

      rule_send_common_aux(infer, inst, node, tup, name, intype, recreg)
    end

    def self.rule_send_cfimc(infer, inst, node, tup)
    end
  end
end


module CodeGenC
  class CodeGen
    def initialize
      @using_method = []
      @callstack = []
      @ccode = ""
      init_code
    end

    def init_code
      @ccode = <<'EOS'
#include "mruby.h"
EOS
    end

    attr :ccode
    attr :codestack
    attr :using_method

    def is_live_reg_aux(node, reg, pos, hash)
      if hash[node] then
        return nil
      end
      hash[node] = true
      if reg.refpoint.size > 1 then
        return true
      elsif reg.refpoint.size == 0 then
        return nil
      else
        if reg.refpoint[0].op == :NOP then
          rc = node.exit_link.any? {|n|
            is_live_reg_aux(n, n.enter_reg[pos], pos, hash)
          }
          return rc

        else
          return true
        end
      end
    end

    def is_live_reg?(node, reg, pos)
      is_live_reg_aux(node, reg, pos, {})
    end

    def code_gen(proc, ti)
      block = proc.irep
      topobj = TOP_SELF
      ty = MTypeInf::LiteralType.new(topobj.class, topobj)
      intype = [[ty]]
      tup = ti.typetupletab.get_tupple_id(intype, MTypeInf::PrimitiveType.new(NilClass), 0)
      code_gen_block(block, ti, :main, proc, tup, intype)
      @using_method.each do |name, proc, utup|
        intype = ti.typetupletab.rev_table[utup]
        code_gen_block(proc.irep, ti, name, proc, utup, intype)
      end

      print @ccode
    end

    def code_gen_block(block, ti, name, proc, tup, intype)
      @callstack.push [proc]
      fname = CodeGen::gen_method_func(name, intype[0][0].class_object, tup)
      topnode = block.nodes[0]
      args = ""
      intype[1...-2].each_with_index do |ty, i|
        args << ", "
        args << CodeGen::gen_declare(self, nil, topnode.enter_reg[i + 1], tup)
      end
      rettype = CodeGen::get_type(self, block, block.retreg, tup)
      @ccode << "#{rettype} #{fname}(mrb_state *mrb, mrb_value self#{args}) {\n"
      code_gen_node(block.nodes[0], ti, name, {}, tup)
      @ccode << "}\n"
      @callstack.pop
    end

    def code_gen_node(node, ti, name, history, tup)
      history[node] = true
      @ccode << "L#{node.id}: \n"
      rc = nil
      node.ext_iseq.each do |ins|
        p ins.op
        rc = @@ruletab[:CCGEN][ins.op].call(self, ins, node, ti, history, tup)
      end

      node.exit_link.each do |nd|
        dclf = nil
        if nd.enter_link.size > 1 then
          if nd.enter_link.count {|n| history[n]} == 1 then
            declf = true
          end

          nd.enter_reg.each_with_index do |nreg, i|
            if is_live_reg?(nd, nreg, i) then
              sreg = node.exit_reg[i]
              src = CodeGen::reg_real_value(self, sreg, node, tup, ti)
              dst = nil
              if declf then
                dst = CodeGen::gen_declare(self, nil, nreg, tup)
              else
                dst = "v#{nreg.id}"
              end
              @ccode << "#{dst} = #{src};\n"
            end
          end
        end
      end

      if rc then
        @ccode << rc
      elsif node.exit_link[0] then
        @ccode << "goto L#{node.exit_link[0].id};\n"
      end

      node.exit_link.each do |nd|
        if history[nd] == nil then
          code_gen_node(nd, ti, name, history, tup)
        end
      end
    end
  end
end

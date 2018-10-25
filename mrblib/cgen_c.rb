module CodeGenC
  class CodeGen
    def initialize
      @using_method = []
      @callstack = []
      @ccode = ""
      init_code
    end

    def init_code
      @code = <<'EOS'
#include "mruby.h"
EOS
    end

    attr :ccode
    attr :codestack
    attr :using_method

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

      p @ccode
    end

    def code_gen_block(block, ti, name, proc, tup, intype)
      @callstack.push [proc]
      fname = CodeGen::gen_method_func(name, intype[0], tup)
      topnode = block.nodes[0]
      args = ""
      intype[1...-2].each_with_index do |ty, i|
        args << ", "
        args << CodeGen::gen_declare(self, nil, topnode.enter_reg[i], tup)
      end
      @ccode << "mrb_value #{name}(mrb_state *mrb, mrb_value self#{args}) {\n"
      code_gen_node(block.nodes[0], ti, name, {}, tup)
      @ccode << "}\n"
    end

    def code_gen_node(node, ti, name, history, tup)
      @ccode << "L#{node.id}: \n"
      node.ext_iseq.each do |ins|
        p ins.op
        rc = @@ruletab[:CCGEN][ins.op].call(self, ins, node, ti, history, tup)
      end

      node.exit_link.each do |nd|
        if history[node] == nil then
          history[node] = true
          code_gen_node(nd, ti, name, history, tup)
        end
      end
    end
  end
end

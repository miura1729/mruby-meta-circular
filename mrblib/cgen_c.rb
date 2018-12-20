module CodeGenC
  class CodeGen
    def initialize
      @using_method = []
      @using_block = []
      @callstack = []
      @ccode = ""
      @hcode = ""
      @pcode = ""
      @dcode = ""
      init_code
    end

    def init_code
      @hcode = <<'EOS'
#include <mruby.h>
#include <mruby/value.h>
#include <mruby/array.h>
#include <mruby/throw.h>

typedef void *gproc;
EOS
    end

    attr :ccode
    attr :hcode
    attr :dcode
    attr :pcode
    attr :callstack
    attr :using_method
    attr :using_block

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
      nilty = MTypeInf::LiteralType.new(NilClass,  nil)
      intype = [[ty], nil, nil]
      code_gen_method(block, ti, :main_Object_0, proc, 0, intype)
      @using_method.each do |name, proc, utup|
        intype = ti.typetupletab.rev_table[utup]
        code_gen_method(proc.irep, ti, name, proc, utup, intype)
      end
      @using_block.each do |name, proc, utup|
        intype = ti.typetupletab.rev_table[utup]
        code_gen_block(proc.irep, ti, name, proc, utup, intype)
      end

      main = <<'EOS'
int main(int argc, char **argv)
{
  mrb_state *mrb = mrb_open();
  struct mrb_jmpbuf c_jmp;

  MRB_TRY(&c_jmp) {
    mrb->jmp = &c_jmp;
    main_Object_0(mrb, mrb_top_self(mrb));
  }
  MRB_CATCH(&c_jmp) {
    mrb_p(mrb, mrb_obj_value(mrb->exc));
    return 1;
  }
  MRB_END_EXC(&c_jmp);

  return 0;
}
EOS


      @ccode = @hcode + main + @ccode
    end

    def code_gen_method(block, ti, name, proc, tup, intype)
      @callstack.push [proc]
      topnode = block.nodes[0]
      args = ""
      intype[0...-2].each_with_index do |ty, i|
        args << ", "
        args << CodeGen::gen_declare(self, nil, topnode.enter_reg[i], tup)
      end
      rettype = CodeGen::get_ctype(self, block, block.retreg, tup)
      @ccode << "#{rettype} #{name}(mrb_state *mrb#{args}) {\n"
      @hcode << "#{rettype} #{name}(mrb_state *#{args});\n"
      @dcode = ""
      @pcode = ""
      code_gen_node(block.nodes[0], ti, name, {}, tup)
      @ccode << @dcode
      @ccode << @pcode
      @ccode << "}\n"
      @callstack.pop
    end

    def code_gen_block(block, ti, name, proc, tup, intype)
      @callstack.push [proc]
      topnode = block.nodes[0]
      args = ", gproc gproc"
      intype[1...-2].each_with_index do |ty, i|
        args << ", "
        args << CodeGen::gen_declare(self, nil, topnode.enter_reg[i + 1], tup)
      end
      rettype = CodeGen::get_ctype(self, block, block.retreg, tup)
      @hcode << "#{rettype} #{name}(mrb_state *#{args});\n"
      @ccode << "#{rettype} #{name}(mrb_state *mrb#{args}) {\n"
      slfdecl = CodeGen::gen_declare(self, nil, topnode.enter_reg[0], tup)
      @ccode << "struct proc#{proc.id} *proc = (struct proc#{proc.id} *)gproc;\n"
      @ccode << "#{slfdecl} = proc->self;\n"
      @dcode = ""
      @pcode = ""
      code_gen_node(block.nodes[0], ti, name, {}, tup)
      @ccode << @dcode
      @ccode << @pcode
      @ccode << "}\n"
      @callstack.pop
    end

    def code_gen_node(node, ti, name, history, tup)
      history[node] = true
      @pcode << "L#{node.id}:; \n"
      rc = nil
      node.ext_iseq.each do |ins|
        #p ins.op # for debug
        rc = @@ruletab[:CCGEN][ins.op].call(self, ins, node, ti, history, tup)
      end

      node.exit_link.each do |nd|
        dclf = nil
        if nd.enter_link.size > 1 then
          if nd.enter_link.count {|n| history[n]} == 1 then
            declf = true
          end

          nd.enter_reg.each_with_index do |nreg, i|
            if is_live_reg?(nd, nreg, i) and
                !(nreg.is_a?(RiteSSA::ParmReg) and nreg.genpoint == 0) then
              sreg = node.exit_reg[i]
              src = CodeGen::reg_real_value(self, sreg, node, tup, ti, history)
              if declf then
                @dcode << CodeGen::gen_declare(self, nil, nreg, tup)
                @dcode << ";\n"
              end
              @pcode << "v#{nreg.id} = #{src};\n"
            end
          end
        end
      end

      if rc == nil and history[node.exit_link[0]] then
        @pcode << "goto L#{node.exit_link[0].id};\n"
      end

      nxnode = rc ? rc : node.exit_link
      nxnode.each do |nd|
        if history[nd] == nil then
          code_gen_node(nd, ti, name, history, tup)
        end
      end
    end
  end
end

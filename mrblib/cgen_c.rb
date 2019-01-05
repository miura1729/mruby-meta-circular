module CodeGenC
  class CodeGen
    @@escape_cache = {}
    def initialize
      @using_method = []
      @using_block = []
      @using_class = {}
      @defined_method = {}
      @defined_block = {}
      @defined_class = {}
      @defined_env = {}
      @callstack = []
      @scode = ""               # structure definituon
      @hcode = ""               # prototype, variable
      @dcode = ""               # local declaration
      @pcode = ""               # program code
      @ccode = ""               # whole program

      @clstab = {}
      init_code
    end

    def init_code
      @scode = <<'EOS'
#include <mruby.h>
#include <mruby/value.h>
#include <mruby/array.h>
#include <mruby/throw.h>
#undef mrb_int

typedef void *gproc;
EOS
    end

    attr :scode
    attr :hcode
    attr :ccode
    attr :dcode
    attr :pcode
    attr :callstack
    attr :using_method
    attr :using_block
    attr :using_class

    attr :clstab

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

      fin = false
      while !fin
        fin = true
        @using_method.each do |name, proc, utup|
          if !@defined_method[name] then
            intype = ti.typetupletab.rev_table[utup]
            code_gen_method(proc.irep, ti, name, proc, utup, intype)
            @defined_method[name] = true
            fin = false
          end
        end

        @using_block.each do |name, proc, utup|
          if !@defined_block[name] then
            intype = ti.typetupletab.rev_table[utup]
            code_gen_block(proc.irep, ti, name, proc, utup, intype)
            @defined_block[name] = true
            fin = false
          end
        end

        @using_class.each do |clsssa, id|
          if !@defined_class[id] then
            @scode << "struct #{id} {\n"
            clsssa.iv.each do |name, reg|
              @scode << "#{CodeGen::gen_declare(self, reg, 0)}; /* #{name} */\n"
            end
            @scode << "};\n"
            @defined_class[id] = true
            fin = false
          end
        end
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


      @ccode = @scode + @hcode + main + @ccode
    end

    def code_gen_method_aux(block, ti, name, proc, tup)
      @dcode = ""
      @pcode = ""
      pproc = proc.parent
      if block.export_regs.size > 0 or
          (pproc and pproc.irep.export_regs.size > 0) then
        if !@defined_env[proc] then
          @defined_env[proc] = true
          @scode << "struct env#{proc.id} {\n"
          block.export_regs.each do |reg|
            @scode << "#{CodeGen::gen_declare(self, reg, tup)};\n"
          end
          if pproc and pproc.irep.export_regs.size > 0 then
            @scode << "struct env#{pproc.id} *prev;\n"
          end
          @scode << "};\n"
        end
        @dcode << "struct env#{proc.id} env;\n"
      end
      code_gen_node(block.nodes[0], ti, name, {}, tup)
      if @callstack[-1][1] then
        @dcode << "int ai = mrb_gc_arena_save(mrb);\n"
      end
      @ccode << @dcode
      @ccode << @pcode
      @ccode << "}\n"
      @callstack.pop
    end

    def code_gen_method(block, ti, name, proc, tup, intype)
      if !block.nodes[0] then
        return
      end
      @callstack.push [proc, nil] # 2nd need mrb_gc_arena_restore generate
      topnode = block.nodes[0]
      args = ""
      intype[0...-2].each_with_index do |ty, i|
        args << ", "
        args << CodeGen::gen_declare(self, topnode.enter_reg[i], tup)
      end
      rettype = CodeGen::get_ctype(self, block.retreg, tup)
      @ccode << "#{rettype} #{name}(mrb_state *mrb#{args}) {\n"
      @hcode << "#{rettype} #{name}(mrb_state *#{args});\n"
      code_gen_method_aux(block, ti, name, proc, tup)
    end

    def code_gen_block(block, ti, name, proc, tup, intype)
      @callstack.push [proc, nil] # 2nd need mrb_gc_arena_restore generate
      topnode = block.nodes[0]
      args = ", gproc cgproc"
      intype[1...-2].each_with_index do |ty, i|
        args << ", "
        args << CodeGen::gen_declare(self, topnode.enter_reg[i + 1], tup)
      end
      rettype = CodeGen::get_ctype(self, block.retreg, tup)
      @ccode << "#{rettype} #{name}(mrb_state *mrb#{args}) {\n"
      @hcode << "#{rettype} #{name}(mrb_state *#{args});\n"
      slfdecl = CodeGen::gen_declare(self, topnode.enter_reg[0], tup)
      @ccode << "struct proc#{proc.id} *proc = (struct proc#{proc.id} *)cgproc;\n"
      @ccode << "#{slfdecl} = proc->self;\n"
      code_gen_method_aux(block, ti, name, proc, tup)
    end

    def code_gen_node(node, ti, name, history, tup)
      history[node] = true
      @pcode << "L#{node.id}:; \n"
      rc = nil
      node.ext_iseq.each do |ins|
        #p ins.op # for debug
        begin
          rc = @@ruletab[:CCGEN][ins.op].call(self, ins, node, ti, history, tup)
        rescue NoMethodError => e
          p "#{ins.op} #{ins.filename}##{ins.line}"
          raise e
        end
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
                @dcode << CodeGen::gen_declare(self, nreg, tup)
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

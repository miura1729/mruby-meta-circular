module CodeGenC
  class CodeGen
    @@escape_cache = {}
    def initialize
      @using_method = []
      @using_block = []
      @using_class = {}
      @using_proc = []
      @defined_method = {}
      @defined_block = {}
      @defined_class = {}
      @defined_env = {}
      @callstack = []
      @gccode = ""              # GC function definition
      @scode = ""               # structure definition
      @hcode = ""               # prototype, variable
      @dcode = ""               # local declaration
      @pcode = ""               # program code
      @ccode = ""               # whole program

      @gcsingle_size = 0
      @prev_gcsignle = []
      @gccomplex_size = 0

      @clstab = {}
      @proctab = {}
      init_code
    end

    def init_code
      @scode = <<'EOS'
#include <mruby.h>
#include <mruby/value.h>
#include <mruby/array.h>
#include <mruby/throw.h>
#include <mruby/proc.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#undef mrb_int
typedef mrb_float mrb_float2;

#define mrb_float_value2(n) ({\
  mrb_value rc;               \
  rc.f = n;                   \
  rc;                         \
})

typedef void *gproc;
struct gctab {
  int size;
  int csize;
  struct gctab *prev;
  mrb_value **complex;
  mrb_value *single[0];
};

void mrb_mark_local(mrb_state *mrb)
{
  struct gctab *curtab = (struct gctab *)mrb->ud;
  while (curtab) {
    for (int i = curtab->size; i--;) {
      mrb_gc_mark(mrb, mrb_basic_ptr(*curtab->single[i]));
    }

    for (int i = curtab->csize; i--;) {
      mrb_value *cptr = curtab->complex[i];
      for (int j = 0; cptr[j].value.ttt != MRB_TT_FREE; j++) {
        mrb_gc_mark(mrb, mrb_basic_ptr(cptr[j]));
      }
    }
    curtab = curtab->prev;
  }
}
EOS
    end

    attr :gccode
    attr :scode
    attr :hcode
    attr :ccode
    attr :dcode
    attr :pcode
    attr :callstack
    attr :using_method
    attr :using_block
    attr :using_class
    attr :using_proc

    attr :clstab
    attr :proctab

    attr_accessor :gcsingle_size
    attr :prev_gcsingle
    attr_accessor :gccomplex_size

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
            block = proc.irep
            block.is_export_env = block.repsreg.any? {|reg|
              CodeGen::get_ctype(self, reg, utup) == :mrb_value
            }
            intype = ti.typetupletab.rev_table[utup]
            code_gen_method(block, ti, name, proc, utup, intype)
            @defined_method[name] = true
            fin = false
          end
        end

        @using_block.each do |name, proc, utup, procty|
          if !@defined_block[name] then
            block = proc.irep
            block.is_export_env = block.repsreg.any? {|reg|
              CodeGen::get_ctype(self, reg, utup) == :mrb_value
            }
            intype = ti.typetupletab.rev_table[utup]
            code_gen_block(block, ti, name, proc, utup, intype, procty)
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
            @defined_class[id] = clsssa
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
    main_Object_0(mrb, mrb_top_self(mrb), NULL);
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
      pproc = proc.parent
      if block.export_regs.size > 0 or pproc
        if !@defined_env[proc] then
          @defined_env[proc] = true
          @scode << "struct env#{proc.id} {\n"
          block.export_regs.each do |reg|
            @scode << "#{CodeGen::gen_declare(self, reg, tup)};\n"
          end
          if pproc then
            @scode << "struct env#{pproc.id} *prev;\n"
          end
          @scode << "};\n"
        end
        @dcode << "struct env#{proc.id} env;\n"
        @dcode << "struct REnv *venv = NULL;\n"
      end
      node = block.nodes[0]
      node.enter_reg.each_with_index {|ireg, i|
        if node.root.export_regs.include?(ireg) then
          src = reg_real_value(ccgen, ireg, ureg,
                         node, tup, infer, history)
          ccgen.pcode << "env.v#{reg.id} = #{src};\n"
        end
      }
      code_gen_node(node, ti, name, {}, tup)
      if @callstack[-1][1] then
        @dcode << "int ai = mrb_gc_arena_save(mrb);\n"
      end

      # create of gctable
      @dcode << "struct gctab *gctab = (struct gctab *)alloca(sizeof(struct gctab) + #{@gcsingle_size} * sizeof(mrb_value *));\n"
      @dcode << "gctab->prev = prevgctab;\n"

      if @gccomplex_size > 0 then
        @dcode << "gctab->complex = alloca(sizeof(mrb_value *) * #{@gccomplex_size});\n"
      else
        @dcode << "gctab->complex = NULL;\n"
      end

      if @gcsingle_size == 0 then
        @dcode << "gctab->size = 0;\n"
      end

      if @gccomplex_size == 0 then
        @dcode << "gctab->csize = 0;\n"
      end

      # Construct code
      @ccode << @dcode
      @ccode << @gccode
      @ccode << @pcode
      @ccode << "}\n"
      @callstack.pop
    end

    def code_gen_method(block, ti, name, proc, tup, intype)
      if !block.nodes[0] then
        return
      end
      @dcode = ""
      @gccode = ""
      @pcode = ""
      @gcsingle_size = 0
      @prev_gcsingle = []
      @gccomplex_size = 0
      @callstack.push [proc, nil] # 2nd need mrb_gc_arena_restore generate
      topnode = block.nodes[0]
      args = ""
      intype[0...-2].each_with_index do |ty, i|
        args << ", "
        args << CodeGen::gen_declare(self, topnode.enter_reg[i], tup)
      end
      rettype = CodeGen::get_ctype(self, block.retreg, tup)
      if rettype.is_a?(Array) then
        case rettype[0]
        when :gproc
          rettype = :gproc
        end
      end

#      @ccode << "static #{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
#      @hcode << "static #{rettype} #{name}(mrb_state *#{args},struct gctab *);\n"
      @ccode << "#{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
      @hcode << "#{rettype} #{name}(mrb_state *#{args},struct gctab *);\n"
      code_gen_method_aux(block, ti, name, proc, tup)
    end

    def code_gen_block(block, ti, name, proc, tup, intype, procty)
      @dcode = ""
      @gccode = ""
      @pcode = ""
      @gcsingle_size = 0
      @prev_gcsingle = []
      @gccomplex_size = 0
      @callstack.push [proc, nil, procty] # 2nd need mrb_gc_arena_restore generate
      topnode = block.nodes[0]
      if procty == :mrb_value then
        args = ",mrb_value mrbproc"
      else
        args = ", gproc cgproc"
      end
      intype[1...-2].each_with_index do |ty, i|
        args << ", "
        args << CodeGen::gen_declare(self, topnode.enter_reg[i + 1], tup)
      end
      rettype = CodeGen::get_ctype(self, block.retreg, tup)
      if rettype.is_a?(Array) then
        case rettype[0]
        when :gproc
          rettype = :gproc
        end
      end

#      @ccode << "static #{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
#      @hcode << "static #{rettype} #{name}(mrb_state *#{args}, struct gctab *);\n"
      @ccode << "#{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
      @hcode << "#{rettype} #{name}(mrb_state *#{args}, struct gctab *);\n"
      slfdecl = CodeGen::gen_declare(self, topnode.enter_reg[0], tup)
      pproc = proc.parent
      envp = block.export_regs.size > 0 or pproc
      if procty == :mrb_value then
        @ccode << "struct RProc *proc;\n"
        @pcode << "proc = mrb_proc_ptr(mrbproc);\n"
        @ccode << "#{slfdecl};\n"
        @pcode << "self = proc->e.env->stack[0];\n"
        @pcode << "env.prev = proc->e.env->stack;\n"
      else
        @ccode << "struct proc#{proc.id} *proc = (struct proc#{proc.id} *)cgproc;\n"
        @ccode << "#{slfdecl} = proc->self;\n"
        @pcode << "env.prev = proc->env;\n"
      end
      code_gen_method_aux(block, ti, name, proc, tup)
    end

    def code_gen_node(node, ti, name, history, tup)
      @gcsingle_size = 0
      history[node] = true
      @pcode << "L#{node.id}:; \n"
      rc = nil
      node.ext_iseq.each do |ins|
        #p ins.op # for debug
        begin
          rc = @@ruletab[:CCGEN][ins.op].call(self, ins, node, ti, history, tup)
#        rescue NoMethodError => e
        rescue  Object => e
          p "#{ins.op} #{ins.filename}##{ins.line} #{ins.para}"
          raise e
        end
      end
      # @prev_gcsingle.clear

      node.exit_link.each do |nd|
        declf = nil
        if nd.enter_link.size > 1 then
          if nd.enter_link.count {|n| history[n]} == 1 then
            declf = true
          end

          nd.enter_reg.each_with_index do |nreg, i|
            if is_live_reg?(nd, nreg, i) and
                !(nreg.is_a?(RiteSSA::ParmReg) and nreg.genpoint == 0) then
              sreg = node.exit_reg[i]
              src = CodeGen::reg_real_value(self, sreg, nreg,  node, tup, ti, history)
              if declf then
                @dcode << CodeGen::gen_declare(self, nreg, tup)
                @dcode << ";\n"
              end
              @pcode << "v#{nreg.id} = #{src};\n"
            end
          end
        end
      end

      if rc then
        @pcode << rc[0]
      elsif history[node.exit_link[0]] then
        @pcode << "goto L#{node.exit_link[0].id};\n"
      end


      nxnode = rc ? rc[1] : node.exit_link
      nxnode.each do |nd|
        if history[nd] == nil then
          code_gen_node(nd, ti, name, history, tup)
        end
      end
    end
  end
end

module CodeGenC
  class CodeGen
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

      @gcsingle_psize = 0
      @gcsingle_size = 0
      @prev_gcsignle = []
      @gccomplex_size = 0
      @gcobject_size = 0
      @caller_alloc_size = 0

      @have_ret_handler = false

      @clstab = {}
      @proctab = {}

      @tmp_attribute = {}
      @method_attribute = {}

      @range_types = []
      init_code
    end

    def init_code
      @scode = <<'EOS'
#include <mruby.h>
#include <mruby/value.h>
#include <mruby/array.h>
#include <mruby/hash.h>
#include <mruby/throw.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/range.h>
#include <mruby/error.h>
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

#define mmc_boxing_array(src, size, boxing_func)  ({      \
  mrb_value ary;                                          \
  ary = mrb_ary_new_capa(mrb, (size));                    \
  for (int i = 0; i < (size); i++) {                      \
    mrb_ary_push(mrb, ary, (boxing_func)(mrb, (src)[i])); \
  }                                                       \
  ary;                                                    \
}) 

typedef void *gproc;
struct gctab {
  int size;
  int csize;
  int osize;
  struct gctab *prev;
  mrb_value **complex;
  mrb_value **object;
  void *caller_alloc;
  int ret_status;
  mrb_value *single[0];
};

void mrb_mark_local(mrb_state *mrb)
{
  struct gctab *curtab = (struct gctab *)mrb->ud;
  while (curtab) {
    for (int i = curtab->size; i--;) {
      mrb_gc_mark(mrb, mrb_basic_ptr(*curtab->single[i]));
    }

    for (int i = curtab->osize; i--;) {
      mrb_gc_mark(mrb, mrb_basic_ptr(*curtab->object[i]));
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

    attr_accessor :gcsingle_psize
    attr_accessor :gcsingle_size
    attr :prev_gcsingle
    attr_accessor :gccomplex_size
    attr_accessor :gcobject_size
    attr_accessor :caller_alloc_size

    attr_accessor :have_ret_handler

    attr :tmp_attribute
    attr :method_attribute

    attr :range_types

    def get_reg_pos(reg)
      if reg.is_a?(RiteSSA::ParmReg) then
        pos = reg.genpoint
      elsif reg.is_a?(RiteSSA::Reg) then
        ginst = reg.genpoint
        if ginst.op == :ENTER
          pos = ginst.outreg.index(reg) + 1
        else
          pos = ((ginst.code) >> 23) & 0x1ff
        end
      else
        raise
      end
    end

    def is_live_reg_aux(node, reg, pos, hash)
      if hash[node] then
        return nil
      end
      hash[node] = true
      if reg.refpoint.size > 1 then
        return true
      elsif reg.refpoint.size == 0 then
        return nil
      elsif reg.refpoint[0].op == :NOP then
        rc = node.exit_link.any? {|n|
          is_live_reg_aux(n, n.enter_reg[pos], pos, hash)
        }
        return rc

      else
        return true
      end
    end

    def is_live_reg?(node, reg, pos = nil)
      # get pos
      if pos == nil then
        pos = get_reg_pos(reg)
      end
      is_live_reg_aux(node, reg, pos, {})
    end

    def is_live_reg_local_aux(node, reg, pos, hash)
      if hash[node] then
        return nil
      end
      hash[node] = true
      if reg.refpoint then
        refpoint = reg.refpoint.select {|e| e.op != :GETUPVAR }
      else
        refpoint = nil
      end
      if refpoint.size > 1 then
        return true
      elsif refpoint.size == 0 then
        return nil
      elsif refpoint[0].op == :NOP then
        rc = node.exit_link.any? {|n|
          is_live_reg_local_aux(n, n.enter_reg[pos], pos, hash)
        }
        return rc

      else
        return true
      end
    end

    def is_live_reg_local?(node, reg, pos = nil)
      # get pos
      if pos == nil then
        pos = get_reg_pos(reg)
      end
      is_live_reg_local_aux(node, reg, pos, {})
    end

    def is_virgin_reg_aux(node, reg, argtype, pos, hash)
      if hash[node] then
        return true
      end
      hash[node] = true
      if !reg.is_a?(RiteSSA::ParmReg)
        if reg.genpoint.op != :ENTER then
          return false
        end
        if pos < argtype.size - 2 then
          return false
        else
          return true
        end

      elsif node.enter_link.size == 0 then
        if pos < argtype.size - 2 then
          return false
        else
          return true
        end

      else
        rc = node.enter_link.all? {|n|
          is_virgin_reg_aux(n, n.exit_reg[pos], argtype, pos, hash)
        }
        return rc
      end
    end

    def is_virgin_reg?(node, reg, argtype, pos = nil)
      # get pos
      if pos == nil then
        pos = get_reg_pos(reg)
      end
      is_virgin_reg_aux(node, reg, argtype, pos, {})
    end

    def code_gen(proc, ti)
#      p ti.typetupletab.rev_table[94]
#      p ti.typetupletab.rev_table[94][0][0].place
      # for mruby bug
      block = proc.irep
      topobj = TOP_SELF
      ty = MTypeInf::LiteralType.new(topobj.class, topobj)
      nilty = MTypeInf::PrimitiveType.new(NilClass)
      intype = [[ty], [nilty]]
      tup = ti.typetupletab.get_tupple_id(intype, nilty, 0, true)
      intype.push nil
      intype.push nil
      code_gen_method(block, ti, :main_Object_0, proc, tup, intype, nil, nil)

      fin = false
      while !fin
        fin = true
        @using_method.each do |name, proc, utup, pproc, namesym|
          if !@defined_method[name] then
            block = proc.irep
            block.is_export_env = block.repsreg.any? {|reg|
              CodeGen::get_ctype(self, reg, utup, ti) == :mrb_value
            }
            intype = ti.typetupletab.rev_table[utup]
            attr = @method_attribute[[namesym, intype[0][0].class_object]]
            code_gen_method(block, ti, name, proc, utup, intype, pproc, attr)
            @defined_method[name] = true
            fin = false
          end
        end

        @using_block.each do |name, proc, utup, procty, pproc|
          if !@defined_block[name] then
            block = proc.irep
            block.is_export_env = block.repsreg.any? {|reg|
              CodeGen::get_ctype(self, reg, utup, ti) == :mrb_value
            }
            intype = ti.typetupletab.rev_table[utup]
            code_gen_block(block, ti, name, proc, utup, intype, procty, pproc)
            @defined_block[name] = true
            fin = false
          end
        end

        @using_class.each do |clsssa, tupid|
          tupid.each do |tup, val|
            id = val[0]
            hometown = val[1]
            if !@defined_class[id] then
              @scode << "struct #{id} {\n"
              clsssa.iv.each do |name, reg|
                @scode << "#{CodeGen::gen_declare(self, reg, tup, ti)}; /* #{name} */\n"
                # @scode << "mrb_value v#{reg.id}; /* #{name} */\n"
              end
              @scode << "};\n"
              @defined_class[id] = clsssa
              fin = false
            end
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

    def code_gen_method_aux(block, ti, name, proc, tup, pproc, attr)
      pproc = proc.parent
      aregs = block.allocate_reg[tup]
      useheap = nil
      if aregs then
        useheap = aregs.any? {|reg|
          !CodeGen::gen_typesize(self, reg, tup, ti)
        }
      end

      if block.export_regs.size > 0 or pproc
        if !@defined_env[proc] then
          @defined_env[proc] = true
          @scode << "struct env#{proc.id} {\n"
          block.export_regs.each do |reg|
            @scode << "#{CodeGen::gen_declare(self, reg, tup, ti)};\n"
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
                         node, tup, ti, history)
          ccgen.pcode << "env.v#{reg.id} = #{src};\n"
        end
      }
      code_gen_node(node, ti, name, {}, tup)
      if useheap then
        @dcode << "int ai = mrb_gc_arena_save(mrb);\n"
      end


      # create of gctable
      gcsiz = @gcsingle_size + @gccomplex_size + @gcobject_size + @caller_alloc_size
      if gcsiz > 0 then
        @dcode << "struct gctab *gctab = (struct gctab *)alloca(sizeof(struct gctab) + #{@gcsingle_size} * sizeof(mrb_value *));\n"
        @dcode << "gctab->prev = prevgctab;\n"

        if @gccomplex_size > 0 then
          @dcode << "gctab->complex = alloca(sizeof(mrb_value *) * #{@gccomplex_size});\n"
        else
          @dcode << "gctab->complex = NULL;\n"
        end

        if @gcobject_size > 0 then
          @dcode << "gctab->object = alloca(sizeof(mrb_value *) * #{@gcobject_size});\n"
        else
          @dcode << "gctab->object = NULL;\n"
        end

        @dcode << "gctab->size = 0;\n"
        @dcode << "gctab->csize = 0;\n"
        @dcode << "gctab->osize = 0;\n"
        @gcsingle_psize = 0
        @dcode << "gctab->ret_status = 0;\n"
      elsif @have_ret_handler then
        @dcode << "struct gctab *gctab = (struct gctab *)alloca(sizeof(struct gctab));\n"
        @dcode << "gctab->ret_status = 0;\n"
      else
        @dcode << "struct gctab *gctab = prevgctab;\n"
      end


      # Construct code
      @ccode << @dcode
      @ccode << @gccode
      @ccode << @pcode
      if useheap then
        @ccode << "mrb_gc_arena_restore(mrb, ai);\n"
      end
      @ccode << "}\n"
      @callstack.pop
    end

    def code_gen_method(block, ti, name, proc, tup, intype, pproc, attr)
      if !block.nodes[0] then
        return
      end
      @dcode = ""
      @gccode = ""
      @pcode = ""
      @gcsingle_size = 0
      @prev_gcsingle = []
      @gccomplex_size = 0
      @gcobject_size = 0
      @caller_alloc_size = 0
      @callstack.push [proc, nil, nil] # 2nd need mrb_gc_arena_restore generate
      topnode = block.nodes[0]
      intype[0...-2].each_with_index do |tys, i|
        ereg = topnode.enter_reg[i]
        ereg.type[tup] = tys.dup
      end
      recvr = topnode.enter_reg[0]
      if !block.retreg.type[tup] then
        # not traverse yet. Maybe escape analysis

        ti.callstack.push [proc.irep, nil, nil, pproc, nil]
        ti.inference_block(block, intype[0..-3], tup, intype.size, proc)
      end
      args = ""
      pos = 0
      intype[0...-3].each_with_index do |tys, i|
        args << ", "
        ereg = topnode.enter_reg[i]
        args << CodeGen::gen_declare(self, ereg, tup, ti)
        pos = i
      end
      tys = intype[-3]
      if tys and (tys.size != 1 or tys[0].class_object != NilClass) then
        args << ", "
        args << CodeGen::gen_declare(self, topnode.enter_reg[pos + 1], tup, ti)
      end

      rettype = CodeGen::get_ctype(self, block.retreg, tup, ti)
      if rettype.is_a?(Array) then
        case rettype[0]
        when :gproc
          rettype = :gproc

        else
          rettype = rettype[0..1].join(' ')
        end
      end

      sect = nil
      if attr then
        sect = attr[:section]
      end

      if sect then
        @ccode << "static #{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
        @hcode << "static #{rettype} #{name}(mrb_state *#{args},struct gctab *) __attribute__ ((section(\"#{sect}\"), noinline));\n"
      else
        @ccode << "static #{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
        @hcode << "static #{rettype} #{name}(mrb_state *#{args},struct gctab *);\n"
      end
      code_gen_method_aux(block, ti, name, proc, tup, pproc, attr)
    end

    def code_gen_block(block, ti, name, proc, tup, intype, procty, pproc)
      @dcode = ""
      @gccode = ""
      @pcode = ""
      @gcsingle_size = 0
      @prev_gcsingle = []
      @gccomplex_size = 0
      @gcobject_size = 0
      @caller_alloc_size = 0
      @callstack.push [proc, nil, procty] # 2nd need mrb_gc_arena_restore generate
      topnode = block.nodes[0]
      recvr = topnode.enter_reg[0]
      if !recvr.type[tup] then
        # not traverse yet. Maybe escape analysis
        ti.inference_block(block, intype[0..-3], tup, intype.size, proc)
      end
      if procty == :mrb_value then
        args = ",mrb_value mrbproc"
      else
        args = ", gproc cgproc"
      end
      pos = 0
      intype[1...-3].each_with_index do |ty, i|
        args << ", "
        args << CodeGen::gen_declare(self, topnode.enter_reg[i + 1], tup, ti)
        pos = i + 1
      end
      tys = intype[-3]
      if tys and (tys.size != 1 or tys[0].class_object != NilClass) then
        args << ", "
        args << CodeGen::gen_declare(self, topnode.enter_reg[pos + 1], tup, ti)
      end
      rettype = CodeGen::get_ctype(self, block.retreg, tup, ti)
      if rettype.is_a?(Array) then
        case rettype[0]
        when :gproc
          rettype = :gproc
        end
      end

      @ccode << "static #{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
      @hcode << "static #{rettype} #{name}(mrb_state *#{args}, struct gctab *);\n"
#      @ccode << "#{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
#      @hcode << "#{rettype} #{name}(mrb_state *#{args}, struct gctab *);\n"
      slfdecl = CodeGen::gen_declare(self, topnode.enter_reg[0], tup, ti)
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
      code_gen_method_aux(block, ti, name, proc, tup, pproc, nil)
    end

    def code_gen_node(node, ti, name, history, tup)
      history[node] = true
      @pcode << "L#{node.id}:; \n"
      rc = nil
      node.ext_iseq.each do |ins|
        #p "#{ins.op} #{ins.filename}##{ins.line}"  # for debug
        begin
          rc = @@ruletab[:CCGEN][ins.op].call(self, ins, node, ti, history, tup)
#        rescue NoMethodError => e
        rescue  Object => e
          p "#{ins.op} #{ins.filename}##{ins.line} #{ins.para}"
#          p "#{ti.typetupletab.rev_table[tup]} (#{tup})"
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
              if declf then
                @dcode << CodeGen::gen_declare(self, nreg, tup, ti)
                @dcode << ";\n"
              end
              argt = ti.typetupletab.rev_table[tup]
              if !is_virgin_reg?(node, sreg, argt) then
                src = CodeGen::reg_real_value(self, sreg, nreg, node, tup, ti, history)
                @pcode << "v#{nreg.id} = #{src};\n"
              end
              if node.root.export_regs.include?(nreg) then
                @pcode << "env.v#{nreg.id} = v#{nreg.id};\n"
              end
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

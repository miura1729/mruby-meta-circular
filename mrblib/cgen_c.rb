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
      @decl_tab = {}
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
      @symtab = {}

      @tmp_attribute = {}
      @method_attribute = {}

      @range_types = []

      @compiled_method = {}

      @unlock_instruction = {}
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
#include <mruby/variable.h>
#include <mruby/throw.h>
#include <mruby/data.h>
#include <mruby/class.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <pthread.h>
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

typedef mrb_value mrb_value_mutex;
struct mutex_wrapper {
  pthread_mutex_t mp;
};

void mrb_mutex_free(mrb_state *mrb, void *data)
{
  struct mutex_wrapper *mutex_data = (struct mutex_wrapper *)data;
  pthread_mutex_destroy(&mutex_data->mp);
  mrb_free(mrb, data);
}

mrb_data_type mutex_data_header = {"Mutex Data", mrb_mutex_free};

void mrb_mark_local(mrb_state *mrb)
{
  struct gctab *curtab = (struct gctab *)mrb->allocf_ud;
  while (curtab) {
    for (int i = curtab->size; i--;) {
      if (!mrb_immediate_p(*curtab->single[i])) {
        mrb_gc_mark(mrb, mrb_basic_ptr(*curtab->single[i]));
      }
    }

    for (int i = curtab->osize; i--;) {
      if (!mrb_immediate_p(*curtab->object[i])) {
        mrb_gc_mark(mrb, mrb_basic_ptr(*curtab->object[i]));
      }
    }

    for (int i = curtab->csize; i--;) {
      mrb_value *cptr = curtab->complex[i];
      for (int j = 0; cptr[j].value.ttt != MRB_TT_FREE; j++) {
        if (!mrb_immediate_p(cptr[i])) {
          mrb_gc_mark(mrb, mrb_basic_ptr(cptr[j]));
        }
      }
    }
    curtab = curtab->prev;
  }
}

struct thread_list {
  struct pthead_t *thread;
  struct thread_list *next;
  struct thread_list *prev;
};

struct mmc_system {
  struct RClass *pthread_class;
  struct thread_list *thread_list;
  pthread_mutex_t *io_mutex;
  pthread_mutex_t *gc_mutex;
};
EOS
    end

    attr :gccode
    attr :scode
    attr :hcode
    attr :ccode
    attr :dcode
    attr :pcode
    attr :callstack
    attr :decl_tab
    attr :using_method
    attr :using_block
    attr :using_class
    attr :using_proc

    attr :defined_method
    attr :defined_block
    attr :defined_class

    attr :clstab
    attr :proctab
    attr :symtab

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

    attr :compiled_method

    attr :unlock_instruction

    def get_reg_pos(reg)
      if reg.is_a?(RiteSSA::ParmReg) then
        reg.genpoint

      elsif reg.is_a?(RiteSSA::InstanceVariable) then
        nil

      elsif reg.is_a?(RiteSSA::Reg) then
        ginst = reg.genpoint
        if ginst.is_a?(RiteSSA::Inst) then
          if ginst.op == :ENTER or ginst.op == :START then
            ginst.outreg.index(reg) + 1
          else
            ((ginst.code) >> 23) & 0x1ff
          end
        else
          nil
        end
      else
        p reg.class
        raise
      end
    end

    def is_live_reg_aux(node, reg, pos, hash)
      if hash[node] then
        return nil
      end
      hash[node] = true
      if reg.refpoint.size > 1 then
#        p reg.refpoint.map {|r| r.op}
        return true
      elsif reg.refpoint.size == 0 then
        return nil
      elsif reg.refpoint[0].op == :NOP then
        rc = node.exit_link.any? {|n|
          is_live_reg_aux(n, n.enter_reg[pos], pos, hash)
        }
        return rc

      elsif reg.refpoint[0].op == :MOVE then
        rc = reg.refpoint[0].outreg.any? {|dr| is_live_reg?(node, dr, pos)}
        return rc

      else
        return true
      end
    end

    def is_live_reg?(node, reg, pos = nil)
      # get pos
      if pos == nil then
        pos = get_reg_pos(reg)
        if pos == nil then
          return nil
        end
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

      elsif reg.refpoint[0].op == :MOVE then
        rc = reg.refpoint[0].outreg.any? {|dr| is_live_reg?(node, dr, pos)}
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
        return !(pos < argtype.size - 2)

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
      code_gen_method(block, ti, :main_Object_0, proc, tup, intype, nil, nil, true)

      fin = false
      while !fin
        fin = true
        @using_method.each do |name, proc, utup, pproc, namesym, usereturn|
          if !@defined_method[name] then
            block = proc.irep
            block.is_export_env = block.repsreg.any? {|reg|
              CodeGen::get_ctype(self, reg, utup, ti) == :mrb_value
            }
            intype = ti.typetupletab.rev_table[utup]
            attr = @method_attribute[[namesym, intype[0][0].class_object]]

            if !block.retreg.type[utup] then
              utup2 = block.retreg.type.keys[0]
              if block.retreg.type[utup2] then
                block.retreg.flush_type(utup2, utup)
              else
                # not traverse yet. Maybe escape analysis
                ti.callstack.push [proc.irep, nil, nil, pproc, nil]
                ti.inference_block(block, intype[0..-3], utup, intype.size, proc)
              end
            end

            code_gen_method(block, ti, name, proc, utup, intype, pproc, attr, usereturn)
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
              if clsssa.class_object == MMC_EXT::Thread then
                @scode << "pthread_t thread;\n"
                @scode << "void *argv;\n"
                @scode << "void (*thread_func)(void *);\n"
              else
                clsssa.iv.each do |name, reg|
                  @scode << "#{CodeGen::gen_declare(self, reg, tup, ti)}; /* #{name} */\n"
                  # @scode << "mrb_value v#{reg.id}; /* #{name} */\n"
                end
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
  struct gctab *gctab = (struct gctab *)alloca(sizeof(struct gctab));
  struct RClass *pthread_class;
  struct thread_list *thread_list;
  pthread_mutex_t *io_mutex;
  pthread_mutex_t *gc_mutex;
  struct mmc_system *mmc_system;
  gctab->prev = NULL;
  gctab->size = 0;
  gctab->csize = 0;
  gctab->osize = 0;
  gctab->ret_status = 0;

  mmc_system = malloc(sizeof(struct mmc_system));
  pthread_class = mrb_define_class(mrb, "Pthread", mrb->object_class);
  mmc_system->pthread_class = pthread_class;
  thread_list = malloc(sizeof(struct thread_list));
  thread_list->thread = pthread_self();
  thread_list->next = NULL;
  thread_list->prev = NULL;
  mmc_system->thread_list = thread_list;
  io_mutex = malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(io_mutex, NULL);
  mmc_system->io_mutex = io_mutex;
  gc_mutex = malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(gc_mutex, NULL);
  mmc_system->gc_mutex = gc_mutex;
  mrb->ud = (void *)mmc_system;

  MRB_SET_INSTANCE_TT(((struct mmc_system *)mrb->ud)->pthread_class, MRB_TT_DATA);

  MRB_TRY(&c_jmp) {
    mrb->jmp = &c_jmp;
    main_Object_0(mrb, mrb_top_self(mrb), gctab);
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
            @scode << "#{CodeGen::gen_declare(self, reg, tup, ti, false, true)};\n"
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
          src = CodeGen::reg_real_value(self, ireg, ireg, node, tup, ti, {})
          self.pcode << "env.v#{ireg.id} = #{src};\n"
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
        @dcode << "gctab->prev = prevgctab;\n"
        @dcode << "gctab->size = 0;\n"
        @dcode << "gctab->csize = 0;\n"
        @dcode << "gctab->osize = 0;\n"
        @dcode << "gctab->ret_status = 0;\n"
      else
        @dcode << "struct gctab *gctab = prevgctab;\n"
      end


      @callstack.pop
      if useheap then
        @pcode << "mrb_gc_arena_restore(mrb, ai);\n"
      end

      # Construct code
      res = ""
      res << @dcode
      res << @gccode
      res << @pcode
      res << "}\n"

      res
    end

    def code_gen_method(block, ti, name, proc, tup, intype, pproc, attr, usereturn)
      if !block.nodes[0] then
        return
      end
      @dcode = ""
      @gccode = ""
      @pcode = ""
      @decl_tab.clear
      @gcsingle_size = 0
      @prev_gcsingle = []
      @gccomplex_size = 0
      @gcobject_size = 0
      @caller_alloc_size = 0
      @callstack.push [proc, nil, nil, usereturn] # 2nd need mrb_gc_arena_restore generate
      topnode = block.nodes[0]
      intype[0...-2].each_with_index do |tys, i|
        ereg = topnode.enter_reg[i]
        ereg.type[tup] = tys.dup
      end
      recvr = topnode.enter_reg[0]
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

      res = code_gen_method_aux(block, ti, name, proc, tup, pproc, attr)

      isuniq = true
      @compiled_method[proc.irep] ||= {}
      @compiled_method[proc.irep][name] ||= {}
      ccm = @compiled_method[proc.irep][name]
      ccodes = [rettype, args, @gccode, @dcode, @pcode]
      if ccm.size > 0 then
        ccm.each do |ptup, codes|
          if codes.is_a?(Array) and codes == ccodes then
            ccodes = ptup
            isuniq = false
            break
          end
        end
      end
      ccm[tup] = ccodes

      if isuniq then
        if sect then
          @ccode << "static #{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
          @hcode << "static #{rettype} #{name}(mrb_state *#{args},struct gctab *) __attribute__ ((section(\"#{sect}\"), noinline));\n"
        else
          @ccode << "static #{rettype} #{name}(mrb_state *mrb#{args}, struct gctab *prevgctab) {\n"
          @hcode << "static #{rettype} #{name}(mrb_state *#{args},struct gctab *);\n"
        end
        @ccode << res
      else
        name2 = name.gsub(tup.to_s, ccm[tup].to_s)
        ccode.gsub!(name, name2)
      end
    end

    def code_gen_block(block, ti, name, proc, tup, intype, procty, pproc)
      @dcode = ""
      @gccode = ""
      @pcode = ""
      @decl_tab.clear
      @gcsingle_size = 0
      @prev_gcsingle = []
      @gccomplex_size = 0
      @gcobject_size = 0
      @caller_alloc_size = 0
      @callstack.push [proc, nil, procty, true] # 2nd need mrb_gc_arena_restore generate
      topnode = block.nodes[0]
      recvr = topnode.enter_reg[0]
      if !recvr.get_type(tup) then
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

        else
          rettype = rettype[0..1].join(' ')
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
        if proc.parent.parent or proc.env.size > 0 then
          @pcode << "env.prev = proc->env;\n"
        end
      end
      @ccode << code_gen_method_aux(block, ti, name, proc, tup, pproc, nil)
    end

    def code_gen_node(node, ti, name, history, tup)
#      p name
      history[node] = true
      @pcode << "L#{node.id}:; \n"
      rc = nil
      node.ext_iseq.each do |ins|
        #p "#{ins.op} #{ins.filename}##{ins.line}"  # for debug
        begin
          rc = @@ruletab[:CCGEN][ins.op].call(self, ins, node, ti, history, tup)
          if (ireg = @unlock_instruction[ins]) then
            if ireg.is_a?(RiteSSA::ParmReg) and ireg.genpoint == 0 then
              @pcode << "pthread_mutex_unlock(&((struct mutex_wrapper *)(DATA_PTR(mutexself)))->mp);\n"
            else
              @pcode << "pthread_mutex_unlock(&((struct mutex_wrapper *)(DATA_PTR(v#{ireg.id})))->mp);\n"
            end
            @unlock_instruction.delete(ins)
          end
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
        if nd.enter_link.size > 0 then
          if nd.enter_link.count {|n| history[n]} == 1 then
            declf = true
          end

          nd.enter_reg.each_with_index do |nreg, i|
            if is_live_reg?(nd, nreg, i) and
                !(nreg.is_a?(RiteSSA::ParmReg) and nreg.genpoint == 0) then
              if nd.enter_link.size > 1 then
                sreg = node.exit_reg[i]
                if declf then
                  @dcode << CodeGen::gen_declare(self, nreg, tup, ti)
                  @dcode << ";\n"
                end
                if !(nreg.refpoint.size == 1 and [:GETUPVAR].include?(nreg.refpoint[0].op)) then
                  argt = ti.typetupletab.rev_table[tup]
                  if !is_virgin_reg?(node, sreg, argt) then
                    src = CodeGen::reg_real_value(self, sreg, nreg, node, tup, ti, history)
                    @pcode << "v#{nreg.id} = #{src}; /* #{nd.enter_link.count {|n| history[n]}}*/ \n"
                  end
                end

                if node.root.export_regs.include?(nreg) then
                  @pcode << "env.v#{nreg.id} = v#{nreg.id};\n"
                end
              else
                if node.root.export_regs.include?(nreg) then
                  sreg = node.exit_reg[i]
                  argt = ti.typetupletab.rev_table[tup]
                  if declf then
                    @dcode << CodeGen::gen_declare(self, nreg, tup, ti)
                    @dcode << ";\n"
                  end
                  if !is_virgin_reg?(node, sreg, argt) then
                    src = CodeGen::reg_real_value(self, sreg, nreg, node, tup, ti, history)
                    @pcode << "v#{nreg.id} = #{src}; /* #{nd.enter_link.count {|n| history[n]}}*/ \n"
                  end
                  @pcode << "env.v#{nreg.id} = v#{nreg.id};\n"
                end
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

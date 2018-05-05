module RiteSSA
  class Storable
    def initialize(ins)
      @genpoint = ins
      @type = {}
      @refpoint = []
      @same = []
    end

    def inspect
      "<#{self.class} type: #{@type} same: #{@same}>"
    end

    def add_type(ty, tup)
      @type[tup] ||= []
      arr = @type[tup]
      arr.each_with_index do |ele, i|
        if ele.class_object == ty.class_object and
            ele.is_a?(MTypeInf::PrimitiveType) then
          return
        end

        if ele.class_object == ty.class_object and
            ty.is_a?(MTypeInf::PrimitiveType) then
          arr[i] = MTypeInf::PrimitiveType.new(ele.class_object)
          return
        end

        if ele == ty then
          case ele
          when MTypeInf::LiteralType
            if ele.val != ty.val then
              arr[i] = MTypeInf::PrimitiveType.new(ele.class_object)
            end

            return

          when MTypeInf::ContainerType
            # TODO Merge element types

            return

          when MTypeInf::UserDefinedType, MTypeInf::PrimitiveType
            return

          when MTypeInf::ProcType
            if ele.irep == ty.irep then
              return
            end
          end
        end

#        elsif ele < ty then
#
#        end
      end
      arr.push ty
    end

    def add_same(st)
      if st != self and @same.index(st) == nil then
        @same.push(st)
      end
    end

    def flush_type(dtup, stup = nil)
      if stup == nil then
        stup = dtup
      end
      @same.each do |var|
        var.flush_type(stup)
        tys = var.type[stup]
        if tys then
          tys.each do |ty|
            add_type(ty, dtup)
          end
        end
      end

     @same = []
      @type
    end

    attr :refpoint
    attr :type
  end

  class Reg<Storable
    def initialize(ins)
      super
    end

    attr_accessor :type
  end

  class SelfReg<Reg
    def initialize(ins)
      super
      @ivlist = {}
      @cvlist = {}
    end

    def get_iv(sym)
      @ivlist[sym] || InstanceVariable.new(sym)
    end

    def get_cv(sym)
      @cvlist[sym] || InstanceVariable.new(sym)
    end
  end

  class ParmReg<Reg
    def initialize(ins)
      super
    end
  end

  class InstanceVariable<Storable
    def initialize(name)
      @name = name
      @type = nil
    end

    attr_accessor :type
  end

  class Inst
    def initialize(op, irep, pc)
      @pc = pc
      @op = Irep::OPTABLE_SYM[op]
      @irep = irep
      @inreg = []
      @outreg = []
      @para = []
    end

    attr :inreg
    attr :outreg
    attr :para
    attr :pc
    attr :op
  end

  class RiteDAGNode
    include RiteOpcodeUtil

    def initialize(irep, pos, iseq, root)
      @root = root
      @irep = irep
      @pos = pos
      @iseq = iseq
      @ext_iseq = nil
      @enter_link = []
      @exit_link = []
      @enter_reg = nil
      @exit_reg = nil
    end

    attr :pos
    attr :iseq
    attr :ext_iseq
    attr :enter_link
    attr :exit_link
    attr :enter_reg
    attr_accessor :exit_reg

    def make_ext_iseq
      @enter_reg = regtab.clone
      @ext_iseq = []
      pc = @pos

      @iseq.each do |code|
        op = get_opcode(code)
        inst = Inst.new(op, @irep, pc)
        @ext_iseq.push inst
        case Irep::OPTABLE_SYM[op]
        when :NOP

        when :MOVE
          inreg = regtab[getarg_b(code)]
          inreg.refpoint.push inst
          inst.inreg.push inreg
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :LOADL
          inst.para.push @irep.pool[getarg_bx(code)]
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :LOADI
          inst.para.push getarg_sbx(code)
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :LOADSYM
          inst.para.push @irep.syms[getarg_bx(code)]
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :LOADNIL
          inst.para.push nil
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :LOADSELF
          inreg = regtab[0]
          inreg.refpoint.push inst
          inst.inreg.push inreg
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :LOADT
          inst.para.push true
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :LOADF
          inst.para.push false
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :GETGLOBAL
          inst.para.push @irep.syms[getarg_bx(code)]
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg

        when :SETGLOBAL
          inst.para.push @irep.syms[getarg_bx(code)]
          inreg = regtab[getarg_a(code)]
          inreg.refpoint.push inst
          inst.inreg.push inreg

        when :GETSPECIAL
          inst.para.push @irep.syms[getarg_bx(code)]
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg

        when :SETSPECIAL
          inst.para.push @irep.syms[getarg_bx(code)]
          inreg = regtab[getarg_a(code)]
          inreg.refpoint.push inst
          inst.inreg.push inst

        when :GETIV
          name = @irep.syms[getarg_b(code)]
          srcreg = regtab[0].get_iv(name)
          srcreg.refpoint.push inst
          inst.inreg.push srcreg
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :SETIV
          inreg = regtab[getarg_b(code)]
          inreg.refpoint.push inst
          inst.inreg.push inreg
          name = @irep.syms[getarg_b(code)]
          dstvar = regtab[0].get_iv(name)
          inst.outreg.push dstvar

        when :GETCV
          name = @irep.syms[getarg_b(code)]
          srcreg = regtab[0].get_cv(name)
          srcreg.refpoint.push inst
          inst.inreg.push srcreg
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :SETCV
          inreg = regtab[getarg_b(code)]
          inreg.refpoint.push inst
          inst.inreg.push inreg
          name = @irep.syms[getarg_b(code)]
          dstvar = regtab[0].get_cv(name)
          inst.outreg.push dstvar

        when :GETCONST
          name = @irep.syms[getarg_b(code)]
          src = @root.target_class.const_get(name)
          inst.inreg.push src
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :SETCONST
          name = @irep.syms[getarg_b(code)]
          inreg = regtab[getarg_b(code)]
          inreg.refpoint.push inst
          inst.inreg.push inreg
          name = @irep.syms[getarg_b(code)]
          dstreg = Reg.new(inst)
          @root.target_class.set_constant(name, dstreg)
          inst.outreg.push dstreg

        when :GETMCONST
          name = @irep.syms[getarg_b(code)]
          src = @root.target_class.class.const_get(name)
          inst.inreg.push src
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :SETCONST
          name = @irep.syms[getarg_b(code)]
          inreg = regtab[getarg_b(code)]
          inreg.refpoint.push inst
          inst.inreg.push inreg
          name = @irep.syms[getarg_b(code)]
          dstreg = Reg.new(inst)
          @root.target_class.class.set_constant(name, dstreg)
          inst.outreg.push dstreg

        when :GETUPVAR
          up = getarg_c(code)
          pos = getarg_b(code)
          curframe = @root
          (up + 1).times do
            curframe = curframe.parent
          end
          inst.para.push curframe
          inst.para.push up
          inst.para.push pos
          srcreg = curframe.regtab[pos]
          srcreg.refpoint.push inst
          inst.inreg.push srcreg
          dstreg = Reg.new(inst)
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :SETUPVAR
          up = getarg_c(code)
          pos = getarg_b(code)
          curframe = @root
          (up + 1).times do
            curframe = curframe.parent
          end
          inst.para.push up
          inst.para.push pos
          srcreg = regtab[getarg_a(code)]
          srcreg.refpoint.push inst
          inst.inreg.push srcreg
          dstreg = Reg.new(inst)
          curframe.regtab[pos] = dstreg
          inst.outreg.push dstreg

        when :JMP
          off = getarg_sbx(code)
          jc = @root.nodes[pc + off]
          inst.para.push jc

        when :JMPIF, :JMPNOT
          inreg = regtab[getarg_a(code)]
          inreg.refpoint.push inst
          inst.inreg.push inreg
          off = getarg_sbx(code)
          jc = @root.nodes[pc + off]
          inst.para.push jc

        when :ONERR
          inst.para.push getarg_sbx(code)

        when :RESCUE
          flg = getarg_c(code)
          if flg == 1 then
            # exception object compare mode

          end

        when :POPERR
          inreg = regtab[getarg_a(code)]
          inreg.refpoint.push inst
          inst.inreg.pus inreg

        when :RAISE
          inreg = regtab[getarg_a(code)]
          inreg.refpoint.push inst
          inst.inreg.pus inreg

        when :EPUSH
          inst.para.push @irep.pool[getarg_bx(code)]

        when :EPOP
          inst.para.push @irep.pool[getarg_a(code)]

        when :SEND, :SENDB, :FSEND, :ADD, :SUB,
              :MUL, :DIV, :EQ, :LT, :LE, :GT, :GE then
          name = @irep.syms[getarg_b(code)]
          inst.para.push name

          a = getarg_a(code)
          num = getarg_c(code)
          inreg = regtab[a]
          inreg.refpoint.push inst
          inst.inreg.push inreg  # push self
          inst.para.push num
          if num == 127 then
            reg = regtab[a + 1]
            inreg.refpoint.push inst
            inst.inreg.push reg
            reg = regtab[a + 2]
            inreg.refpoint.push inst
            inst.inreg.push reg
          else
            num.times do |i|
              reg = regtab[a + 1 + i]
              inreg.refpoint.push inst
              inst.inreg.push reg
            end
          end

          if Irep::OPTABLE_SYM[op] == :SENDB then
            reg = regtab[a + 1 + num]
            inreg.refpoint.push inst
            inst.inreg.push reg
          else
            nilreg = Reg.new(inst)
            inst.inreg.push nilreg
          end

          dstreg = Reg.new(inst)
          regtab[a] = dstreg
          inst.outreg.push dstreg

        when :CALL
          dstreg = Reg.new(inst)
          regtab[a] = dstreg
          inst.outreg.push dstreg

        when :SUPER
          a = getarg_a(code)
          num = getarg_c(code)
          inreg = regtab[a]
          inreg.refpoint.push inst
          inst.inreg.push inreg  # push self
          inst.para.push num
          if num == 127 then
            reg = regtab[a + 1]
            inreg.refpoint.push inst
            inst.inreg.push reg
            reg = regtab[a + 2]
            inreg.refpoint.push inst
            inst.inreg.push reg
          else
            num.times do |i|
              reg = regtab[a + 1 + i]
              inreg.refpoint.push inst
              inst.inreg.push reg
            end
          end
          dstreg = Reg.new(inst)
          regtab[a] = dstreg
          inst.outreg.push dstreg

        when :ARGARY
          inst.para.push getarg_bx(code)
          dstreg = Reg.new(inst)
          regtab[a] = dstreg
          inst.outreg.push dstreg

        when :ENTER
          inst.para.push getarg_ax(code)

        when :RETURN
          a = getarg_a(code)
          inst.para.push getarg_bx(code)
          inreg = regtab[getarg_a(code)]
          inreg.refpoint.push inst
          inst.inreg.push inreg
          dstreg = @root.retreg
          regtab[getarg_a(code)] = dstreg
          inst.outreg.push dstreg

        when :ADDI, :SUBI then
          name = @irep.syms[getarg_b(code)]
          inst.para.push name

          a = getarg_a(code)
          x = getarg_c(code)
          inreg = regtab[a]
          inreg.refpoint.push inst
          inst.inreg.push inreg  # push self
          inst.para.push x
          dstreg = Reg.new(inst)
          regtab[a] = dstreg
          inst.outreg.push dstreg

        when :LAMBDA
          a = getarg_a(code)
          dstreg = Reg.new(inst)
          regtab[a] = dstreg
          inst.outreg.push dstreg
          bn = getarg_b(code)
          nlambda = Block.new(@irep.reps[bn], @root, @root.target_class)
          inst.para.push nlambda
          @root.reps[bn] = nlambda

        else
        end
        pc = pc + 1
      end

      @exit_reg = regtab.clone
    end

    def regtab
      @root.regtab
    end

    def inspect
      res = ""
      res << "#{pos}  \n"
      res << "enter: "
      @enter_link.each do |ele|
        res << "#{ele.pos}, "
      end
      res << "\n"
      p @ext_iseq
      @ext_iseq.each_with_index do |ele, i|
#        res << "#{Irep::disasm(ele, @irep)}\n"
        res << ele.inspect + "\n"
      end
      res << "exit: "
      @exit_link.each do |ele|
        res << "#{ele.pos}, "
      end
      res << "\n"
      res
    end
  end

  class Block
    include RiteOpcodeUtil

    def initialize(irep, parent, tclass)
      iseq = irep.iseq
      @reps = []
      @irep = irep
      @target_class = tclass
      @parent = parent
      @nodes = {}
      @retreg = Reg.new(@irep)

      @regtab = nil
      block_head = collect_block_head(iseq)
      block_head.each_cons(2) do |bg, ed|
        @regtab = [SelfReg.new(@irep)]
        (@irep.nregs - 1).times do
          @regtab.push ParmReg.new(@irep)
        end
        dag = RiteDAGNode.new(irep, bg, iseq[bg...ed], self)
        dag.make_ext_iseq
        @nodes[bg] = dag
      end

      # construct link
      @nodes.each do |beg, dag|
        lastpos = beg + dag.iseq.size - 1
        lastins = dag.iseq[-1]
        case(Irep::OPTABLE_SYM[get_opcode(lastins)])
        when :JMP
          @nodes[lastpos + getarg_sbx(lastins)].enter_link << dag
          dag.exit_link << @nodes[lastpos + getarg_sbx(lastins)]

        when :JMPIF, :JMPNOT
          @nodes[lastpos + 1].enter_link << dag
          @nodes[lastpos + getarg_sbx(lastins)].enter_link << dag
          dag.exit_link << @nodes[lastpos + 1]
          dag.exit_link << @nodes[lastpos + getarg_sbx(lastins)]

        when :RETURN

        else
          @nodes[lastpos + 1].enter_link << dag
          dag.exit_link << @nodes[lastpos + 1]
        end
      end

    end

    attr :parent
    attr :target_class
    attr :regtab
    attr :nodes
    attr :reps
    attr :retreg

    def collect_block_head(iseq)
      res = [0]
      iseq.each_with_index do |ins, pos|
        case Irep::OPTABLE_SYM[get_opcode(ins)]
        when :JMPIF, :JMPNOT
          res.push (pos + getarg_sbx(ins))
          res.push (pos + 1)

        when :JMP
          res.push (pos + getarg_sbx(ins))

        when :RETURN
          res.push (pos + 1)
        end
      end

      res.sort.uniq
    end

    def inspect
      res = ""
      @nodes.each do |pos, dag|
        res << dag.inspect
      end

      res
    end
  end
end

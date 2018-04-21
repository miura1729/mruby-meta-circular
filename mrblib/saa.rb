module RiteSSA
  class Storable
    def initialize(ins)
      @genpoint = ins
      @type = nil
    end
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
    def initialize(op, irep)
      @op = op
      @irep = irep
      @inreg = []
      @outreg = []
      @para = []
    end

    attr :inreg
    attr :outreg
  end

  class RiteDAGNode
    include RiteOpcodeUtil

    def initialize(irep, pos, iseq, root)
      @root = root
      @irep = irep
      @pos = pos
      @iseq = iseq
      @ext_iseq = make_iseq_ext
      @enter_link = []
      @exit_link = []
      @enter_reg = {}
      @exit_reg = {}
    end

    attr :pos
    attr :iseq
    attr :ext_iseq
    attr :enter_link
    attr :exit_link
    attr :enter_reg
    attr :exit_reg

    def make_iseq_ext
      @enter_reg = regtab.clone
      @ext_iseq = []

      @iseq.each do |code|
        op = get_opcode(code)
        inst = Inst.new(op, @irep)
        @ext_iseq.push inst
        case Irep::OPTABLE[op]
        when :NOP

        when :MOVE
          inst.inreg.push regtab[getarg_b(op)]
          dstreg = Reg.new(inst)
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :LOADL
          inst.para.push @irep.pool[getarg_bx(op)]
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :LOADI
          inst.para.push getarg_sbx(op)
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :LOADSYM
          inst.para.push @irep.syms[getarg_bx(op)]
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :LOADNIL
          inst.para.push nil
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :LOADSELF
          inst.inreg.push regtab[0]
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :LOADT
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg
          inst.para.push true

        when :LOADF
          inst.para.push true
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :GETGLOBAL
          inst.para.push @irep.syms[getarg_bx(op)]
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg

        when :SETGLOBAL
          inst.para.push @irep.syms[getarg_bx(op)]
          inst.inreg.push regtab[getarg_a(op)]

        when :GETSPECIAL
          inst.para.push @irep.syms[getarg_bx(op)]
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg

        when :SETSPECIAL
          inst.para.push @irep.syms[getarg_bx(op)]
          inst.inreg.push regtab[getarg_a(op)]

        when :GETIV
          name = @irep.syms[getarg_b(op)]
          srcreg = regtab[0].get_iv(name)
          inst.inreg.push srcreg
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :SETIV
          inst.inreg.push regtab[getarg_b(op)]
          name = @irep.syms[getarg_b(op)]
          dstvar = regtab[0].get_iv(name)
          inst.outreg.push dstvar

        when :GETCV
          name = @irep.syms[getarg_b(op)]
          srcreg = regtab[0].get_cv(name)
          inst.inreg.push srcreg
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :SETCV
          inst.inreg.push regtab[getarg_b(op)]
          name = @irep.syms[getarg_b(op)]
          dstvar = regtab[0].get_cv(name)
          inst.outreg.push dstvar

        when :GETCONST
          name = @irep.syms[getarg_b(op)]
          src = @root.target_class.const_get(name)
          inst.inreg.push val
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :SETCONST
          name = @irep.syms[getarg_b(op)]
          inst.inreg.push regtab[getarg_b(op)]
          name = @irep.syms[getarg_b(op)]
          dstreg = Reg.new
          @root.target_class.set_constant(name, dstreg)
          inst.outreg.push dstreg

        when :GETMCONST
          name = @irep.syms[getarg_b(op)]
          src = @root.target_class.class.const_get(name)
          inst.inreg.push val
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :SETCONST
          name = @irep.syms[getarg_b(op)]
          inst.inreg.push regtab[getarg_b(op)]
          name = @irep.syms[getarg_b(op)]
          dstreg = Reg.new
          @root.target_class.class.set_constant(name, dstreg)
          inst.outreg.push dstreg

        when :GETUPVAR
          up = getarg_c(op)
          pos = getarg_b(op)
          curframe = @root
          (up - 1).times do
            curframe = curframe.parent
          end
          srcreg = curframe.regtab[pos]
          inst.inreg.push srcreg
          dstreg = Reg.new(inst)
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg

        when :SETUPVAR
          up = getarg_c(op)
          pos = getarg_b(op)
          curframe = @root
          (up - 1).times do
            curframe = curframe.parent
          end
          dstreg = curframe.regtab[pos]
          srcreg = regtab[getarg_a(op)]
          inst.inreg.push srcreg
          inst.outreg.push dstreg

        when :JMP
          inst.para.push getarg_sbx(op)

        when :JMPIF, :JMPNOT
          inst.inreg.pus regtab[getarg_a(op)]
          inst.para.push getarg_sbx(op)

        when :ONERR
          inst.para.push getarg_sbx(op)

        when :RESCUE
          flg = getarg_c(op)
          if flg == 1 then
            # exception object compare mode

          end

        when :POPERR
          inst.inreg.pus regtab[getarg_a(op)]

        when :RAISE
          inst.inreg.pus regtab[getarg_a(op)]

        when :EPUSH
          inst.para.push @irep.pool[getarg_bx(op)]

        when :EPOP
          inst.para.push @irep.pool[getarg_a(op)]

        when :SEND, :SENDB
          a = getarg_a(op)
          name = @irep.syms[getarg_b(op)]
          num = getarg_c(op)
          inst.inreg.push regtab[a]
          inst.para.push name
          inst.para.push num
          dstreg = Reg.new(inst)
          regtab[a] = dstreg
          inst.outreg.push dstreg # push self
          inst.inreg.push dstreg # push self
          if num == 127 then
            inst.inreg.push regtab[a + 1];
            inst.inreg.push regtab[a + 2];
          else
            num.times do |i|
              inst.inreg.push regtab[a + 1 + i]
            end
          end

          if Irep::OPTABLE[op] == :SENDB then
            inst.inreg.push regtab[a + 1 + num]
          else
            inst.inreg.push nil
          end

        else
        end
      end
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
      @iseq.each_with_index do |ele, i|
        res << "#{Irep::disasm(ele, @irep)}\n"
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
      @irep = irep
      @target_class = tclass
      @parent = parent
      @nodes = {}
      @regtab = [SelfReg.new(@irep)]
      (@irep.nlocals - 1).times do
        @regtab.push ParmReg.new(@irep)
      end
      block_head = collect_block_head(iseq)
      block_head.each_cons(2) do |bg, ed|
        @nodes[bg] = RiteDAGNode.new(irep, bg, iseq[bg...ed], self)
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

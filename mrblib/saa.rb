module RiteSSA
  
  class Reg
    def initialize(ins)
      @genpoint = ins
      @type = nil
    end
    
    attr_accessor :type
  end
  
  class ParmReg<Reg
    def initialize(ins)
      super
    end
  end
  
  class Inst
    def initialize(op, irep)
      @op = op
      @irep = irep
      @inreg = []
      @outreg = []
      @para = []
    end
    
    attr_accessor :inreg
    attr_accessor :outreg
  end
  
  class RiteDAGNode
    def initialize(irep, pos, iseq)
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
      res = []
      regtab = []
      @irep.nlocals.times do
        regtab.push PramReg.new(@irep)
      end
      @enter_reg = regtab.clone
      
      @iseq.each do |code|
        op = get_opcode(code)
        inst = Inst.new(op, @irep)
        @ext_iseq.push inst
        case OPTABLE[op]
        when :NOP
          
        when :MOVE
          dstreg = Reg.new(inst)
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg
          inst.inreg.push regtab[getarg_b(op)]
          
        when :LOADL
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg
          inst.para.push @irep.pool[getarg_bx(op)]
          
        when :LOADI
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg
          inst.para.push getarg_sbx(op)
          
        when :LOADSYM
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg
          inst.para.push @irep.syms[getarg_bx(op)]
          
        when :LOADNIL
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg
          inst.para.push nil
          
        when :LOADSELF
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg
          inst.inreg.push regtab[0]
          
        when :LOADT
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg
          inst.para.push true
          
        when :LOADF
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.outreg.push dstreg
          inst.para.push true
          
        when :GETGLOBAL
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.para.push @irep.syms[getarg_bx(op)]
          
        when :SETGLOBAL
          dstreg = Reg.new
          regtab[getarg_a(op)] = dstreg
          inst.para.push @irep.syms[getarg_bx(op)]
          
        else
          p OPTABLE[op]
        end
      end
    end
    
    def reg_analyze
      
    end
    
    def inspect
      res = ""
      res << "enter: "
      @enter_link.each do |ele|
        res << "#{ele.pos}, "
      end
      res << "\n"
      @iseq.each_with_index do |ele, i|
        res << "#{Irep::disasm(ele, @irep)}\n"
      end
      res
    end
  end
  
  class Block
    include RiteOpcodeUtil
    
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
    
    def initialize(irep)
      iseq = irep.iseq
      @nodes = {}
      block_head = collect_block_head(iseq)
      block_head << iseq.size
      block_head.each_cons(2) do |bg, ed|
        @nodes[bg] = RiteDAGNode.new(irep, bg, iseq[bg...ed])
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
          p dag
          @nodes[lastpos + 1].enter_link << dag
          dag.exit_link << @nodes[lastpos + 1]
        end
      end
    end
    
    def inspect
      res = ""
      @nodes.each do |pos, dag|
        res << "#{pos}  \n"
        res << dag.inspect
      end
      
      res
    end
  end
end

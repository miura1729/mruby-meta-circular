class RiteDAGNode
  def initialize(irep, pos, iseq)
    @irep = irep
    @pos = pos
    @iseq = iseq
    @enter_link = []
    @exit_link = []
  end

  attr :iseq
  attr :enter_link
  attr :exit_link

  def inspect
    res = ""
    @iseq.each_with_index {|ele, i|
      res << "#{Irep::disasm(ele, @irep)}\n"
    }
    res
  end
end

class RiteSSA
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
      end
    end

    res.sort.uniq
  end

  def initialize(irep)
    iseq = irep.iseq
    @blocks = {}
    block_head = collect_block_head(iseq)
    block_head << iseq.size
    block_head.each_cons(2) do |bg, ed|
      @blocks[bg] = RiteDAGNode.new(irep, bg, iseq[bg...ed])
    end

    # construct link
    @blocks.each do |beg, dag|
      lastpos = beg + dag.iseq.size - 1
      lastins = dag.iseq[-1]
      case(Irep::OPTABLE_SYM[get_opcode(lastins)])
      when :JMP
        @blocks[lastpos + getarg_sbx(lastins)].enter_link << dag
        dag.exit_link << @blocks[lastpos + getarg_sbx(lastins)]

      when :JMPIF, :JMPNOT
        @blocks[lastpos + 1].enter_link << dag
        @blocks[lastpos + getarg_sbx(lastins)].enter_link << dag
        dag.exit_link << @blocks[lastpos + 1]
        dag.exit_link << @blocks[lastpos + getarg_sbx(lastins)]


      when :RETURN

      else
        p dag
        @blocks[lastpos + 1].enter_link << dag
        dag.exit_link << @blocks[lastpos + 1]
      end
    end

    def inspect
      res = ""
      @blocks.each do |pos, dag|
        res << "#{pos}  \n"
        res << dag.inspect
      end

      res
    end
  end
end

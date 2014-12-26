class RiteSAA
  def collect_block_head(iseq)
    res = [0]
    iseq.each_with_index do |ins, pos|
      case Irep::OPTABLE_SYM[get_opcode(ins)]
      when :JMPIF, :JMPNOT, :JMP
        res.push (pos + getarg_sbx(ins))
      end
    end

    res.sort.uniq
  end

  def initialize(irep)
    iseq = irep.iseq
    @blocks = []
    block_head = collect_block_head(iseq)
    block_head << iseq.size
    block_head.each_cons(2) do |bg, ed|
      @blocks.push iseq[bg...ed]
    end

    @blocks.each do |blk|
      if blk
        blk.each do |code|
          p Irep::OPTABLE_SYM[get_opcode(code)]
        end
      end
      p "-----"
    end
  end
end

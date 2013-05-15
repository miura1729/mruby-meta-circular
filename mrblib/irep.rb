module RiteOpcodeUtil
  def get_opcode(op)
    op & 0x7f
  end

  def getarg_a(op)
    (op >> 23) & 0x1ff
  end

  def getarg_b(op)
    (op >> 14) & 0x1ff
  end

  def getarg_c(op)
    (op >> 7) & 0x7f
  end

  def getarg_bx(op)
    (op >> 7) & 0xffff
  end

  def getarg_sbx(op)
    ((op >> 7) & 0xffff) - (0xffff >> 1)
  end
end

module RiteVM
  include RiteOpcodeUtil

  def rite_init
    @regs = []
    @pc = 0
  end

  def eval
    rite_init
    while true
      cop = iseq[@pc]
      case Irep::OPTABLE_SYM[get_opcode(cop)]
      when :NOP

      when :MOVE
        @regs[getarg_a(cop)] = @regs[getarg_b(cop)]

      when :LOADL
        @regs[getarg_a(cop)] = pool[getarg_bx(i)]

      when :LOADI
        @regs[getarg_a(cop)] = getarg_sbx(cop)

      when :ADDI
        @regs[getarg_a(cop)] += getarg_c(cop)

      when :JMP
        @pc = @pc + getarg_sbx(cop)
        next

      when :RETURN
        return @regs[getarg_a(cop)]

      else
        printf("Unkown code %s \n", Irep::OPTABLE_SYM[get_opcode(cop)])
      end

      @pc = @pc + 1
    end
  end
end

class Irep
  OPTABLE_SYM = []
  OPTABLE.each do |ent|
    OPTABLE_SYM.push ent.to_sym
  end

  include RiteVM
end

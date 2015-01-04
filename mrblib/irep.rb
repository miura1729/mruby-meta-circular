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

  def getarg_ax(op)
    (op >> 7) & 0x1ffffff
  end

  def getarg_bx(op)
    (op >> 7) & 0xffff
  end

  def getarg_sbx(op)
    ((op >> 7) & 0xffff) - (0xffff >> 1)
  end

  def mk_opcode(op)
    op & 0x7f
  end

  def mkarg_a(op)
    (op & 0x1ff) << 23
  end

  def mkarg_b(op)
    (op & 0x1ff) << 14
  end

  def mkarg_c(op)
    (op & 0x7f) << 7
  end

  def mkarg_bx(op)
    (op & 0xffff) << 7
  end

  def mkarg_sbx(op)
    mkarg_bx(op + (0xffff >> 1))
  end

  def mkop_A(op, a)
    (mk_opcode(op) | mkarg_a(a))
  end

  def mkop_AB(op, a, b)
    (mkop_A(op, a) | mkarg_b(b))
  end

  def mkop_ABC(op, a, b, c)
    (mkop_AB(op, a, b) | mkarg_c(c))
  end

  def mkop_ABx(op, a, b)
    (mkop_A(op, a) | mkarg_bx(b))
  end

  def mkop_AsBx(op, a, b)
    (mkop_A(op, a) | mkarg_sbx(b))
  end
end

class RiteVM
  include RiteOpcodeUtil

  def initialize
    @stack = []
    @callinfo = []
    @pc = 0
    @sp = 0
    @bp = 0
    @cp = 0
  end

  def eval(irep)
    while true
      cop = irep.iseq[@pc]
      case Irep::OPTABLE_SYM[get_opcode(cop)]
      when :NOP

      when :MOVE
        @stack[@sp + getarg_a(cop)] = @stack[@sp + getarg_b(cop)]

      when :LOADL
        @stack[@sp + getarg_a(cop)] = irep.pool[getarg_bx(cop)]

      when :LOADI
        @stack[@sp + getarg_a(cop)] = getarg_sbx(cop)

      when :LOADSYM
        @stack[@sp + getarg_a(cop)] = irep.syms[getarg_bx(cop)]

      when :LOADSELF
        @stack[@sp + getarg_a(cop)] = @stack[@sp]

      when :LOADT
        @stack[@sp + getarg_a(cop)] = true

      when :ADD
        @stack[@sp + getarg_a(cop)] += @stack[@sp + getarg_a(cop) + 1]

      when :ADDI
        @stack[@sp + getarg_a(cop)] += getarg_c(cop)

      when :SUB
        @stack[@sp + getarg_a(cop)] -= @stack[@sp + getarg_a(cop) + 1]

      when :SUBI
        @stack[@sp + getarg_a(cop)] -= getarg_c(cop)

      when :MUL
        @stack[@sp + getarg_a(cop)] *= @stack[@sp + getarg_a(cop) + 1]

      when :DIV
        @stack[@sp + getarg_a(cop)] /= @stack[@sp + getarg_a(cop) + 1]

      when :EQ
        val = (@stack[@sp + getarg_a(cop)] == @stack[@sp + getarg_a(cop) + 1])
        @stack[@sp + getarg_a(cop)] = val

      when :JMP
        @pc = @pc + getarg_sbx(cop)
        next

      when :JMPIF
        if @stack[@sp + getarg_a(cop)] then
          @pc = @pc + getarg_sbx(cop)
          next
        end

      when :JMPNOT
        if !@stack[@sp + getarg_a(cop)] then
          @pc = @pc + getarg_sbx(cop)
          next
        end

      when :ENTER

      when :SEND
        a = getarg_a(cop)
        mid = irep.syms[getarg_b(cop)]
        n = getarg_c(cop)
        newirep = Irep::get_irep(@stack[@sp + a], mid)
        if newirep then
          @callinfo[@cp] = @sp
          @cp += 1
          @callinfo[@cp] = @pc
          @cp += 1
          @callinfo[@cp] = irep
          @cp += 1
          @sp += a
          @pc = 0
          irep = newirep

          next
        else
          args = []
          n.times do |i|
            args.push @stack[@sp + a + i + 1]
          end

          @stack[@sp + a] = @stack[@sp + a].send(mid, *args)
        end

      when :RETURN
        if @cp == 0 then
          return @stack[@sp + getarg_a(cop)]
        else
          @stack[@sp] = @stack[@sp + getarg_a(cop)]
          @cp -= 1
          irep = @callinfo[@cp]
          @cp -= 1
          @pc = @callinfo[@cp]
          @cp -= 1
          @sp = @callinfo[@cp]
        end
      else
        printf("Unkown code %s \n", Irep::OPTABLE_SYM[get_opcode(cop)])
      end

      @pc = @pc + 1
    end
  end

  def to_relocate_iseq(irep)
    res = []
    labels = {}
    syms = irep.syms
    lno = 0
    irep.iseq.each_with_index do |cop, pos|
      case Irep::OPTABLE_SYM[get_opcode(cop)]
        when :JMPIF, :JMPNOT, :JMP
        lno = lno + 1
        labels[pos + getarg_sbx(cop)] = lno
      end
    end

    irep.iseq.each_with_index do |cop, pos|
      code = Irep::OPTABLE_SYM[get_opcode(cop)]
      if labels[pos] then
        res.push labels[pos]
      end
      case code
      when :NOP

      when :MOVE
        res.push [code, getarg_a(cop), getarg_b(cop)]

      when :LOADL
        res.push [code, getarg_a(cop), getarg_bx(cop)]

      when :LOADI
        res.push [code, getarg_a(cop), getarg_sbx(cop)]

      when :LOADSYM
        res.push [code, getarg_a(cop), getarg_bx(cop)]

      when :LOADSELF
        res.push [code, getarg_a(cop)]

      when :LOADT
        res.push [code, getarg_a(cop)]

      when :ADD,
        :SUB,
        :MUL,
        :DIV,
        :EQ,
        :RETURN
        res.push [code, getarg_a(cop)]

      when :ADDI,
        :SUBI
        res.push [code, getarg_a(cop), getarg_c(cop)]

      when :JMP, :JMPIF, :JMPNOT
        res.push [code, getarg_a(cop), labels[pos + getarg_sbx(cop)]]

      when :ENTER
        res.push [code, cop]

      when :SEND
        res.push [code, getarg_a(cop), syms[getarg_b(cop)], getarg_c(cop)]

      else
        printf("Unkown code %s \n", Irep::OPTABLE_SYM[get_opcode(cop)])
      end
    end

    res
  end
end

class Irep
  extend  RiteOpcodeUtil

  OPTABLE_SYM = []
  OPTABLE_KIND = []
  OPTABLE_CODE = {}
  OPTABLE[0].each do |ent|
    OPTABLE_SYM.push ent.to_sym
  end
  OPTABLE[1].each do |ent|
    OPTABLE_KIND.push ent
  end
  op = 0
  OPTABLE_SYM.each do |sym|
    OPTABLE_CODE[sym] = op
    op = op + 1
  end

  def self.disasm(code, irep)
    res = OPTABLE_SYM[get_opcode(code)].to_s
    res += " "
    case OPTABLE_KIND[get_opcode(code)]
    when 1                      # A
      res += "R#{getarg_a(code)}"

    when 2                      # Ax
      res += "#{getarg_ax(code)}"

    when 3                      # Bx
      res += "#{getarg_bx(code)}"
      
    when 4                      # AB
      res += "R#{getarg_a(code)}"
      res += ", "
      res += "R#{getarg_b(code)}"

    when 5                      # AC
      res += "R#{getarg_a(code)}"
      res += ", "
      res += "R#{getarg_c(code)}"

    when 6                     # ABx
      res += "R#{getarg_a(code)}"
      res += ", "
      res += "#{getarg_bx(code)}"

    when 7                     # AsBx
      res += "R#{getarg_a(code)}"
      res += ", "
      res += "#{getarg_sbx(code)}"

    when 9                     # ABC
      res += "R#{getarg_a(code)}"
      res += ","
      res += "#{getarg_b(code)}"
      res += ","
      res += "#{getarg_c(code)}"

    when 10                     # ASC
      res += "R#{getarg_a(code)}"
      res += ","
      res += ":#{irep.syms[getarg_b(code)]}"
      res += ","
      res += "#{getarg_c(code)}"
    end

    res
  end
end

module CodeGen
  include RiteOpcodeUtil
  # Reg Mapping
  #   R0  SELF
  #   R1  SP
  #   R2  WORKING
  #      :
  #   @max_using_reg

  SYMS = [:stack, :sp, :+]
  STACK_SYM = 0
  SP_SYM = 0
  ADD_SYM = 2

  def gen_getReg(dst, src)
    tmp0 = @max_using_reg
    tmp1 = tmp0 + 1
    tmp2 = tmp1 + 1
    [
      mkop_ABx(Irep::OPTABLE_CODE[:GETIV], tmp0, STACK_SYM),
      mkop_ABx(Irep::OPTABLE_CODE[:LOADI], tmp1, src),
      mkop_ABx(Irep::OPTABLE_CODE[:GETIV], tmp2, SP_SYM),
      mkop_ABC(Irep::OPTABLE_CODE[:ADD], tmp1, ADD_SYM, 1),
      mkop_ABC(Irep::OPTABLE_CODE[:AREF], dst, tmp0, tmp1),
    ]
  end

  def gen_setReg(dst, val)
    tmp0 = @max_using_reg
    tmp1 = tmp0 + 1
    tmp2 = tmp1 + 1
    tmp3 = tmp2 + 1
    [
      mkop_ABx(Irep::OPTABLE_CODE[:GETIV], tmp0, STACK_SYM),
      mkop_ABx(Irep::OPTABLE_CODE[:LOADI], tmp1, val),
      mkop_ABx(Irep::OPTABLE_CODE[:GETIV], tmp2, SP_SYM),
      mkop_ABC(Irep::OPTABLE_CODE[:ADD], tmp1, ADD_SYM, 1),
      mkop_ABC(Irep::OPTABLE_CODE[:AREF], tmp1, tmp0, tmp1),
      mkop_ABx(Irep::OPTABLE_CODE[:LOADI], tmp2, val),
      mkop_ABx(Irep::OPTABLE_CODE[:GETIV], tmp3, SP_SYM),
      mkop_ABC(Irep::OPTABLE_CODE[:ADD], tmp2, ADD_SYM, 1),
      mkop_ABC(Irep::OPTABLE_CODE[:ASET], tmp1, tmp0, tmp2),
    ]
  end
end

class FibVM
  include RiteOpcodeUtil
  include CodeGen

  def initialize
    # For Interpriter
    @stack = []
    @callinfo = []
    @pc = 0
    @sp = 0
    @bp = 0
    @cp = 0

    # For JIT
    @max_using_reg = 2
    @prof_info = {}
    @proc_tab = {}
  end

  def eval(irep)
    @prof_info[irep] ||= []
    @proc_tab [irep] ||= []
    while true
      @prof_info[irep][@pc] ||= 0
      @prof_info[irep][@pc] += 1
      times = @prof_info[irep][@pc]
      if times  > 10 then
      end
      cop = irep.iseq[@pc]
      case Irep::OPTABLE_SYM[get_opcode(cop)]
      when :NOP

      when :MOVE
        @stack[@sp + getarg_a(cop)] = @stack[@sp + getarg_b(cop)]

      when :LOADL
        @stack[@sp + getarg_a(cop)] = irep.pool[getarg_bx(cop)]

      when :LOADI
        @stack[@sp + getarg_a(cop)] = getarg_sbx(cop)

      when :LOADSELF
        @stack[@sp + getarg_a(cop)] = @stack[@sp]

      when :ADD
        @stack[@sp + getarg_a(cop)] += @stack[@sp + getarg_a(cop) + 1]

      when :SUBI
        @stack[@sp + getarg_a(cop)] -= getarg_c(cop)

      when :EQ
        val = (@stack[@sp + getarg_a(cop)] == @stack[@sp + getarg_a(cop) + 1])
        @stack[@sp + getarg_a(cop)] = val

      when :JMP
        @pc = @pc + getarg_sbx(cop)
        next

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
          @prof_info[irep] ||= []

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
end

def fib(n)
  if n == 1 then
    1
  elsif n == 0 then
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end

def fibt
  fib(20)
end

a = Irep::get_irep(self, :fibt)
vm = FibVM.new
p vm.eval(a)

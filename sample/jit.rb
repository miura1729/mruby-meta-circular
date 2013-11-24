module CodeGen
  include RiteOpcodeUtil
  # Reg Mapping
  #   R0  SELF
  #   R1  SP
  #   R2  WORKING
  #      :
  #   @max_using_reg

  SYMS = [:@stack, :@sp, :@pc, :+, :-, :[], :[]=, :p]
  STACK_SYM = 0
  SP_SYM = 1
  PC_SYM = 2
  ADD_SYM = 3
  SUB_SYM = 4
  AREF_SYM = 5
  ASET_SYM = 6
  P_SYM = 7

  def gen_get_reg(dst, src)
    tmp0 = @max_using_reg
    tmp1 = tmp0 + 1
    tmp2 = tmp1 + 1
    [
      mkop_ABx(Irep::OPTABLE_CODE[:GETIV], tmp0, STACK_SYM),
      mkop_AsBx(Irep::OPTABLE_CODE[:LOADI], tmp1, src),
      mkop_ABx(Irep::OPTABLE_CODE[:GETIV], tmp2, SP_SYM),
      mkop_ABC(Irep::OPTABLE_CODE[:ADD], tmp1, ADD_SYM, 1),
      mkop_ABC(Irep::OPTABLE_CODE[:SEND], tmp0, AREF_SYM, 1),
      mkop_AB(Irep::OPTABLE_CODE[:MOVE], dst, tmp0),
    ]
  end

  def gen_set_reg(dst, val)
    tmp0 = @max_using_reg
    tmp1 = tmp0 + 1
    tmp2 = tmp1 + 1
    [
      mkop_ABx(Irep::OPTABLE_CODE[:GETIV], tmp0, STACK_SYM),
      mkop_AsBx(Irep::OPTABLE_CODE[:LOADI], tmp1, dst),
      mkop_ABx(Irep::OPTABLE_CODE[:GETIV], tmp2, SP_SYM),
      mkop_ABC(Irep::OPTABLE_CODE[:ADD], tmp1, ADD_SYM, 1),
      mkop_AB(Irep::OPTABLE_CODE[:MOVE], tmp2, val),
      mkop_ABC(Irep::OPTABLE_CODE[:SEND], tmp0, ASET_SYM, 2),
    ]
  end

  def gen_exit(rreg)
    code = []
    tmp0 = @max_using_reg
    @max_using_reg += 1
    code += [
      mkop_AsBx(Irep::OPTABLE_CODE[:LOADI], tmp0, @pc),
      mkop_ABx(Irep::OPTABLE_CODE[:SETIV], tmp0, PC_SYM),
    ]
    code += gen_get_reg(tmp0, rreg)
    code.push mkop_A(Irep::OPTABLE_CODE[:RETURN], tmp0)
    @max_using_reg -= 1

    code
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
    @irep = nil
    @irepid =nil

    # For JIT
    @prof_info = {}
    @proc_tab = {}
    @entry = nil
    @max_using_reg = 2
    @code = []
    @pool = []
  end

  def add_pool(val)
    if idx = @pool.index(val) then
      return idx
    else
      idx = @pool.size
      @pool.push val
      return idx
    end
  end

  def stop_compile
    # Reset working
    @entry = nil
    @max_using_reg = 2
    @code = []
    @pool = []
  end

  def eval(irep)
    @irep = irep
    @irepid = @irep.id
    @prof_info[@irepid] ||= []
    @proc_tab [@irepid] ||= []
    while true
      if @proc_tab[@irepid][@pc] then
        if @entry then
          if @code.size > 1 then
            @code += gen_exit(0)
            @proc_tab[@irepid][@entry] = Irep.new_irep(@code, @pool, CodeGen::SYMS, 10, 2).to_proc
          end
          stop_compile
        end
        # printf ">> %d \n", @pc
        @proc_tab[@irepid][@pc].call(self)
        # printf "<< %d \n", @pc
      end

      cop = @irep.iseq[@pc]

      if !@proc_tab[@irepid][@pc] then
        @prof_info[@irepid][@pc] ||= 0
        @prof_info[@irepid][@pc] += 1

        times = @prof_info[@irepid][@pc]
        if times  > 20 then
          if @entry == nil then
            @entry = @pc
            @code.push mkop_AB(Irep::OPTABLE_CODE[:MOVE], 0, 1)
          end

          case Irep::OPTABLE_SYM[get_opcode(cop)]
          when :NOP

          when :MOVE
            tmp = @max_using_reg
            @max_using_reg += 1
            @code += gen_get_reg(tmp, getarg_b(cop))
            @code += gen_set_reg(getarg_a(cop), tmp)
            @max_using_reg -= 1

          when :LOADL
            sidx = add_pool(@irep.pool[getarg_bx(cop)])
            tmp = @max_using_reg
            @max_using_reg += 1
            @code.push mkop_ABx(Irep::OPTABLE_CODE[:LOADL], tmp, sidx)
            @code += gen_set_reg(getarg_a(cop), tmp)
            @max_using_reg -= 1

          when :LOADI
            tmp = @max_using_reg
            @max_using_reg += 1
            @code.push mkop_AsBx(Irep::OPTABLE_CODE[:LOADI], tmp, getarg_sbx(cop))
            @code += gen_set_reg(getarg_a(cop), tmp)
            @max_using_reg -= 1

          when :LOADSELF
            tmp = @max_using_reg
            @max_using_reg += 1
            @code += gen_get_reg(tmp, 0)
            @code += gen_set_reg(getarg_a(cop), tmp)
            @max_using_reg -= 1

          when :ADD
            tmp0 = @max_using_reg
            tmp1 = tmp0 + 1
            @max_using_reg += 2
            @code += gen_get_reg(tmp0, getarg_a(cop))
            @code += gen_get_reg(tmp1, getarg_a(cop) + 1)
            @code.push mkop_ABC(Irep::OPTABLE_CODE[:ADD], tmp0, ADD_SYM, 1)
            @code += gen_set_reg(getarg_a(cop), tmp0)
            @max_using_reg -= 2

          when :SUBI
            tmp0 = @max_using_reg
            @max_using_reg += 1
            @code += gen_get_reg(tmp0, getarg_a(cop))
            @code.push mkop_ABC(Irep::OPTABLE_CODE[:SUBI], tmp0, SUB_SYM, getarg_c(cop))
            @code += gen_set_reg(getarg_a(cop), tmp0)
            @max_using_reg -= 1

          when :ENTER
            # Do nothing

          when :JMP
            # Do nothing

          when :JMPNOT
            exit_code = gen_exit(0)
            tmp0 = @max_using_reg
            @max_using_reg += 1
            @code += gen_get_reg(tmp0, getarg_a(cop))
            off = exit_code.size
            if @stack[@sp + getarg_a(cop)] then
              @code.push mkop_AsBx(Irep::OPTABLE_CODE[:JMPNOT], tmp0, off)
              @code += exit_code
            else
              @code.push mkop_AsBx(Irep::OPTABLE_CODE[:JMPIF], tmp0, off)
              @code += exit_code
            end
            @max_using_reg -= 1

          else
            # Return to VM
            if @code.size > 1 then
              @code += gen_exit(0)
              @proc_tab[@irepid][@entry] = Irep.new_irep(@code, @pool, CodeGen::SYMS, 10, 2).to_proc
            end
            stop_compile
          end
        end
      end

      case Irep::OPTABLE_SYM[get_opcode(cop)]
      when :NOP

      when :MOVE
        @stack[@sp + getarg_a(cop)] = @stack[@sp + getarg_b(cop)]

      when :LOADL
        @stack[@sp + getarg_a(cop)] = @irep.pool[getarg_bx(cop)]

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
        mid = @irep.syms[getarg_b(cop)]
#        p "SEND #{@stack[@sp + a + 1]}"
        n = getarg_c(cop)
        newirep = Irep::get_irep(@stack[@sp + a], mid)
        if newirep then
          @callinfo[@cp] = @sp
          @cp += 1
          @callinfo[@cp] = @pc
          @cp += 1
          @callinfo[@cp] = @irep
          @cp += 1
          @sp += a
          @pc = 0
          @irep = newirep
          @irepid = @irep.id
          @prof_info[@irepid] ||= []
          @proc_tab[@irepid] ||= []

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
          @irep = @callinfo[@cp]
          @irepid = @irep.id
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
p fibt

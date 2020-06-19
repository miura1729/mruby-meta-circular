# -*- coding: cp932 -*-
# �R�[�h�����p�̃��C�u����
module CodeGen
  include RiteOpcodeUtil
  # Reg Mapping
  #   R0  SELF
  #   R1  SP
  #   R2  WORKING
  #      :
  #   @max_using_reg

  SYMS = [:@stack, :@sp, :@pc, :@callinfo, :@irep, @irepid, :+, :-, :[], :[]=, :p, :==]
  STACK_SYM = 0
  SP_SYM = 1
  PC_SYM = 2
  CALLINFO_SYM = 3
  IREP_SYM = 4
  IREPID_SYM = 5
  ADD_SYM = 6
  SUB_SYM = 7
  AREF_SYM = 8
  ASET_SYM = 9
  P_SYM = 10
  EQ_SYM = 11
  OPTABLE_CODE = Irep::OPTABLE_CODE
  OPTABLE_SYM = Irep::OPTABLE_SYM

  #�@FibVM�̃��W�X�^�̓��e��Rite VM�̃��W�X�^�Ɋi�[����
  def gen_get_reg(dst, src)
    tmp0 = @max_using_reg
    tmp1 = tmp0 + 1
    tmp2 = tmp1 + 1
    [
      mkop_ABx(OPTABLE_CODE[:GETIV], tmp0, STACK_SYM),
      mkop_ABx(OPTABLE_CODE[:GETIV], tmp1, SP_SYM),
      mkop_ABC(OPTABLE_CODE[:ADDI], tmp1, ADD_SYM, src),
      mkop_ABC(OPTABLE_CODE[:SEND], tmp0, AREF_SYM, 1),
      mkop_AB(OPTABLE_CODE[:MOVE], dst, tmp0),
    ]
  end

  #�@RITE VM�̃��W�X�^�̓��e��FibVM�̃��W�X�^�Ɋi�[����
  def gen_set_reg(dst, val)
    tmp0 = @max_using_reg
    tmp1 = tmp0 + 1
    tmp2 = tmp1 + 1
    [
      mkop_ABx(OPTABLE_CODE[:GETIV], tmp0, STACK_SYM),
      mkop_ABx(OPTABLE_CODE[:GETIV], tmp1, SP_SYM),
      mkop_ABC(OPTABLE_CODE[:ADDI], tmp1, ADD_SYM, dst),
      mkop_AB(OPTABLE_CODE[:MOVE], tmp2, val),
      mkop_ABC(OPTABLE_CODE[:SEND], tmp0, ASET_SYM, 2),
    ]
  end

  #�@stack, sp�ɒl��push����
  def gen_push_reg(stack, sp, val)
    tmp0 = @max_using_reg
    tmp1 = tmp0 + 1
    tmp2 = tmp1 + 1
    [
      mkop_ABx(OPTABLE_CODE[:GETIV], tmp0, stack),
      mkop_ABx(OPTABLE_CODE[:GETIV], tmp1, sp),
      mkop_AB(OPTABLE_CODE[:MOVE], tmp2, val),
      mkop_ABC(OPTABLE_CODE[:SEND], tmp0, ASET_SYM, 2),
      mkop_ABx(OPTABLE_CODE[:GETIV], tmp1, sp),
      mkop_ABC(OPTABLE_CODE[:ADDI], tmp1, ADD_SYM, 1),
      mkop_ABx(OPTABLE_CODE[:SETIV], tmp1, sp),
    ]
  end

  #�@RITE VM�ɖ߂�
  def gen_exit(rreg)
    code = []
    tmp0 = @max_using_reg
    @max_using_reg += 1
    # @pc��ݒ肷��R�[�h
    code += [
      mkop_AsBx(OPTABLE_CODE[:LOADI], tmp0, @pc),
      mkop_ABx(OPTABLE_CODE[:SETIV], tmp0, PC_SYM),
    ]
    #�@�߂�l�̐ݒ�(�߂�l��FibVM�̃��W�X�^�ɓ����Ă��邱�Ƃɒ���)
    code += gen_get_reg(tmp0, rreg)
    # �߂�
    code.push mkop_A(OPTABLE_CODE[:RETURN], tmp0)
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

  # �萔�e�[�u���ɒ萔��ǉ�����B���łɂ���ꍇ�͍ė��p����
  def add_pool(val)
    if idx = @pool.index(val) then
      return idx
    else
      idx = @pool.size
      @pool.push val
      return idx
    end
  end

  # �R���p�C���𒆒f����
  def stop_compile
    if @code.size > 1 then
      @code += gen_exit(0)
      @proc_tab[@irepid][@entry] = Irep.new_irep(@code, @pool, CodeGen::SYMS, 10, 2).to_proc
    end

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
      # �������݂�PC�̖��߂ɃR���p�C�����ꂽ�R�[�h�����݂�����
      a = @proc_tab[@irepid][@pc]
      if a then
        if @entry then
          # �����A�R���p�C�����Ȃ�R���p�C���𒆎~����
          stop_compile
        end
        # �R�[�h���s
        a.call(self)
      end

      # �R�[�h����荞�ށB�R���p�C�����ꂽ�R�[�h�����s�����@pc���ω�����
      # �\���̂��邱�Ƃɒ���
      cop = @irep.iseq[@pc]

      if !@proc_tab[@irepid][@pc] then
        @prof_info[@irepid][@pc] ||= 0
        @prof_info[@irepid][@pc] += 1

        times = @prof_info[@irepid][@pc]
        if times  > 20 then
          if @entry == nil then
            @entry = @pc
            @code.push mkop_AB(OPTABLE_CODE[:MOVE], 0, 1)
          end

          case OPTABLE_SYM[get_opcode(cop)]
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
            @code.push mkop_ABx(OPTABLE_CODE[:LOADL], tmp, sidx)
            @code += gen_set_reg(getarg_a(cop), tmp)
            @max_using_reg -= 1

          when :LOADI
            tmp = @max_using_reg
            @max_using_reg += 1
            @code.push mkop_AsBx(OPTABLE_CODE[:LOADI], tmp, getarg_sbx(cop))
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
            @code.push mkop_ABC(OPTABLE_CODE[:ADD], tmp0, ADD_SYM, 1)
            @code += gen_set_reg(getarg_a(cop), tmp0)
            @max_using_reg -= 2

          when :SUBI
            tmp0 = @max_using_reg
            @max_using_reg += 1
            @code += gen_get_reg(tmp0, getarg_a(cop))
            @code.push mkop_ABC(OPTABLE_CODE[:SUBI], tmp0, SUB_SYM, getarg_c(cop))
            @code += gen_set_reg(getarg_a(cop), tmp0)
            @max_using_reg -= 1

          when :EQ
            tmp0 = @max_using_reg
            tmp1 = tmp0 + 1
            @max_using_reg += 2
            @code += gen_get_reg(tmp0, getarg_a(cop))
            @code += gen_get_reg(tmp1, getarg_a(cop) + 1)
            @code.push mkop_ABC(OPTABLE_CODE[:EQ], tmp0, EQ_SYM, 1)
            @code.push mkop_AB(OPTABLE_CODE[:MOVE], tmp1, tmp0)
            @code += gen_set_reg(getarg_a(cop), tmp1)
            @max_using_reg -= 2

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
              @code.push mkop_AsBx(OPTABLE_CODE[:JMPNOT], tmp0, off)
              @code += exit_code
            else
              @code.push mkop_AsBx(OPTABLE_CODE[:JMPIF], tmp0, off)
              @code += exit_code
            end
            @max_using_reg -= 1

          when :SEND2
            a = getarg_a(cop)
            mid = @irep.syms[getarg_b(cop)]
            newirep = Irep::get_irep(@stack[@sp + a], mid)
            p newirep

            stop_compile

          else
            # �T�|�[�g����Ă��Ȃ�����
            stop_compile
          end
        else
          # ���s�p�x�����Ȃ�����
          stop_compile
        end
      end

      case OPTABLE_SYM[get_opcode(cop)]
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
          n = getarg_c(cop)
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
        printf("Unknown code %s \n", OPTABLE_SYM[get_opcode(cop)])
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
  fib(24)
end

a = Irep::get_irep(self, :fibt)
vm = FibVM.new
p vm.eval(a)
#p fibt

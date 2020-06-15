# -*- coding: cp932 -*-
class FibVM
  include RiteOpcodeUtil
  OPTABLE_CODE = Irep::OPTABLE_CODE
  OPTABLE_SYM = Irep::OPTABLE_SYM

  def initialize
    # For Interpriter
    @stack = []                 # �X�^�b�N(@sp����ʂ����W�X�^�Ƃ��Ĉ���)
    @callinfo = []              # ���\�b�h�Ăяo���ŌĂяo�����̏����i�[
    @pc = 0                     # ���s���閽�߂̈ʒu
    @sp = 0                     # �X�^�b�N�|�C���^
    @cp = 0                     # callinfo�̃|�C���^
    @irep = nil                 # ���ݎ��s���̖��ߗ�I�u�W�F�N�g
    @irepid =nil                # ���ߗ�I�u�W�F�N�g��id(JIT�p)
  end

  def eval(irep)
    @irep = irep
    @irepid = @irep.id
    while true
      #�@���߃R�[�h�̎��o��
      cop = @irep.iseq[@pc]

      case OPTABLE_SYM[get_opcode(cop)]
        # �������Ȃ�
      when :NOP

        # MOVE Ra, Rb�Ń��W�X�^Ra�Ƀ��W�X�^Rb�̓��e���Z�b�g����
      when :MOVE
        @stack[@sp + getarg_a(cop)] = @stack[@sp + getarg_b(cop)]

        # LOADL Ra, pb �Ń��W�X�^Ra�ɒ萔�e�[�u��(pool)��pb�Ԗڂ̒l���Z�b�g����
      when :LOADL
        @stack[@sp + getarg_a(cop)] = @irep.pool[getarg_bx(cop)]

        # LOADI Ra, n �Ń��W�X�^Ra��Fixnum�̒l n���Z�b�g����
      when :LOADI
        @stack[@sp + getarg_a(cop)] = getarg_sbx(cop)

        # LOADSELF Ra �Ń��W�X�^Ra�Ɍ��݂�self���Z�b�g����
      when :LOADSELF
        @stack[@sp + getarg_a(cop)] = @stack[@sp]

        # ADD Ra, Rb �Ń��W�X�^Ra��Ra+Rb���Z�b�g����
      when :ADD
        @stack[@sp + getarg_a(cop)] += @stack[@sp + getarg_a(cop) + 1]

        # SUB Ra, n �Ń��W�X�^Ra��Ra-n���Z�b�g����
      when :SUBI
        @stack[@sp + getarg_a(cop)] -= getarg_c(cop)

        # EQ Ra ��Ra��R(a+1)���ׂē����Ȃ�true, �Ⴄ�Ȃ�false��Ra�ɃZ�b�g����
      when :EQ
        val = (@stack[@sp + getarg_a(cop)] == @stack[@sp + getarg_a(cop) + 1])
        @stack[@sp + getarg_a(cop)] = val

        # JMP n��pc��n�������₷�B�������An�͕����t��
      when :JMP
        @pc = @pc + getarg_sbx(cop)
        next

        # JMPNOT Ra, n�ł���Ra��nil��false�Ȃ�pc��n�������₷�B�������An�͕����t��
      when :JMPNOT
        if !@stack[@sp + getarg_a(cop)] then
          @pc = @pc + getarg_sbx(cop)
          next
        end

        # ���\�b�h�̐擪�ň����̃Z�b�g�A�b�v���閽�߁B�ʓ|�Ȃ̂ŏڍׂ͏ȗ�
      when :ENTER

        # SEND Ra, mid, anum��Ra�����V�[�o�ɂ��ăV���{��mid�̖��O�̃��\�b�h��
        # �Ăяo���B�������A������anum����AR(a+1), R(a+2)... R(a+anum)������
      when :SEND
        a = getarg_a(cop)
        mid = @irep.syms[getarg_b(cop)]
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

          next
        else
          args = []
          n.times do |i|
            args.push @stack[@sp + a + i + 1]
          end

          @stack[@sp + a] = @stack[@sp + a].send(mid, *args)
        end

        # RETURN Ra�ŌĂяo�����̃��\�b�h�ɖ߂�BRa���߂�l�ɂȂ�
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

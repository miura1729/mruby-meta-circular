# -*- coding: cp932 -*-
class FibVM
  include RiteOpcodeUtil
  OPTABLE_CODE = Irep::OPTABLE_CODE
  OPTABLE_SYM = Irep::OPTABLE_SYM

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
    @prof_info = {}             # プロファイラ用
    @proc_tab = {}              # 生成したProcを入れておく
    @entry = nil                # 現在コンパイル中のコードの先頭(RITEのコード)
                                # 現在コンパイル中かのフラグも兼ねる
    @max_using_reg = 2          # 現在使用しているレジスタの最大番号
    @code = []                  # コンパイルして生成されたコード
    @pool = []                  # コンパイルして生成された定数テーブル
  end

  def eval(irep)
    @irep = irep

    # 命令列の生アドレスを得る
    @irepid = @irep.id

    # irepごとにエントリーを作成する
    @prof_info[@irepid] ||= []
    @proc_tab [@irepid] ||= []
    while true
      cop = @irep.iseq[@pc]

      #　@pcに対応するRITE VMコードがない場合
      # (今はまだコード生成しないから常に無い)
      if !@proc_tab[@irepid][@pc] then
        # 今のpcの命令の実行回数１回増やす
        @prof_info[@irepid][@pc] ||= 0
        @prof_info[@irepid][@pc] += 1

        times = @prof_info[@irepid][@pc]
        if times  > 20 then
          #　コンパイラの宝石箱や
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
  fib(20)
end

a = Irep::get_irep(self, :fibt)
vm = FibVM.new
p vm.eval(a)
p fibt

# -*- coding: cp932 -*-
class FibVM
  include RiteOpcodeUtil
  OPTABLE_CODE = Irep::OPTABLE_CODE
  OPTABLE_SYM = Irep::OPTABLE_SYM

  def initialize
    # For Interpriter
    @stack = []                 # スタック(@spより上位をレジスタとして扱う)
    @callinfo = []              # メソッド呼び出しで呼び出し元の情報を格納
    @pc = 0                     # 実行する命令の位置
    @sp = 0                     # スタックポインタ
    @cp = 0                     # callinfoのポインタ
    @irep = nil                 # 現在実行中の命令列オブジェクト
    @irepid =nil                # 命令列オブジェクトのid(JIT用)
  end

  def eval(irep)
    @irep = irep
    @irepid = @irep.id
    while true
      #　命令コードの取り出し
      cop = @irep.iseq[@pc]

      case OPTABLE_SYM[get_opcode(cop)]
        # 何もしない
      when :NOP

        # MOVE Ra, RbでレジスタRaにレジスタRbの内容をセットする
      when :MOVE
        @stack[@sp + getarg_a(cop)] = @stack[@sp + getarg_b(cop)]

        # LOADL Ra, pb でレジスタRaに定数テーブル(pool)のpb番目の値をセットする
      when :LOADL
        @stack[@sp + getarg_a(cop)] = @irep.pool[getarg_bx(cop)]

        # LOADI Ra, n でレジスタRaにFixnumの値 nをセットする
      when :LOADI
        @stack[@sp + getarg_a(cop)] = getarg_sbx(cop)

        # LOADSELF Ra でレジスタRaに現在のselfをセットする
      when :LOADSELF
        @stack[@sp + getarg_a(cop)] = @stack[@sp]

        # ADD Ra, Rb でレジスタRaにRa+Rbをセットする
      when :ADD
        @stack[@sp + getarg_a(cop)] += @stack[@sp + getarg_a(cop) + 1]

        # SUB Ra, n でレジスタRaにRa-nをセットする
      when :SUBI
        @stack[@sp + getarg_a(cop)] -= getarg_c(cop)

        # EQ Ra でRaとR(a+1)を比べて同じならtrue, 違うならfalseをRaにセットする
      when :EQ
        val = (@stack[@sp + getarg_a(cop)] == @stack[@sp + getarg_a(cop) + 1])
        @stack[@sp + getarg_a(cop)] = val

        # JMP nでpcをnだけ増やす。ただし、nは符号付き
      when :JMP
        @pc = @pc + getarg_sbx(cop)
        next

        # JMPNOT Ra, nでもしRaがnilかfalseならpcをnだけ増やす。ただし、nは符号付き
      when :JMPNOT
        if !@stack[@sp + getarg_a(cop)] then
          @pc = @pc + getarg_sbx(cop)
          next
        end

        # メソッドの先頭で引数のセットアップする命令。面倒なので詳細は省略
      when :ENTER

        # SEND Ra, mid, anumでRaをレシーバにしてシンボルmidの名前のメソッドを
        # 呼び出す。ただし、引数はanum個あり、R(a+1), R(a+2)... R(a+anum)が引数
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

        # RETURN Raで呼び出し元のメソッドに戻る。Raが戻り値になる
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
        printf("Unkown code %s \n", OPTABLE_SYM[get_opcode(cop)])
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

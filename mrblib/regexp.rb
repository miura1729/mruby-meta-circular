module RegexpCompiler
  OPTABLE_CODE = Irep::OPTABLE_CODE

  SYM_AREF = 0
  SYM_EQ = 1
  SYM_PLUS = 2
  SYM_RAISE = 3
  SYM_SIZE = 4
  SYM_PUSH = 5
  SYM_POP = 6
  SYM_LAST = 7

  REG_STRING = 1
  REG_POSTACK = 2
  REG_CURPOS = 3
  REG_BEGPOS = 4
  REG_ENDPOS = 6
  REG_RC = 7

  REG_ARG0 = 8
  REG_ARG1 = 9

  def gen_comp_current_end(offset)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG0, REG_BEGPOS)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG1, REG_ENDPOS)
    @code.push mkop_ABC(OPTABLE_CODE[:EQ], REG_ARG0, SYM_EQ, 1)
    @code.push mkop_AsBx(OPTABLE_CODE[:JMPNOT], REG_ARG0, offset)
  end

  def gen_match_letter_aux(ch)
    @pool.push ch
    sidx = @pool.size - 1
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG0, REG_STRING)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG1, REG_CURPOS)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_AREF, 1)
    @code.push mkop_ABx(OPTABLE_CODE[:LOADL], REG_ARG1, sidx)
    @code.push mkop_ABC(OPTABLE_CODE[:EQ], REG_ARG0, SYM_EQ, 1)
  end

  def gen_match_letter(ch)
    gen_match_letter_aux(ch)
    @code.push mkop_AsBx(OPTABLE_CODE[:JMPIF], REG_ARG0, 3)
    @code.push mkop_A(OPTABLE_CODE[:LOADNIL], REG_RC)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_RAISE, 0)
    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], REG_CURPOS, SYM_PLUS, 1)
  end

  def gen_match_dot
    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], REG_CURPOS, SYM_PLUS, 1)
  end

  # push BEGPOS
  def gen_match_star_push_begpos
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG0, REG_POSTACK)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG1, REG_BEGPOS)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_PUSH, 1)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_BEGPOS, REG_CURPOS)
    @code.push mkop_sBx(OPTABLE_CODE[:ONERR], 2)
  end

  def gen_match_star(ch)
    gen_match_star_push_begpos
    @code.push mkop_sBx(OPTABLE_CODE[:JMP], 3 + 5 + 6 + 1)

    @code.push mkop_A(OPTABLE_CODE[:RESCUE], 0)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_CURPOS, REG_BEGPOS)

    gen_match_letter_aux(ch)  # Gen code size == 5

    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], REG_BEGPOS, SYM_PLUS, 1)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_CURPOS, REG_BEGPOS)
    @code.push mkop_AsBx(OPTABLE_CODE[:JMPIF], REG_ARG0, -5 - 3 - 2 - 1)

    # POP REG_BEG2POS
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG0, REG_POSTACK)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_POP, 0)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_BEGPOS, REG_ARG0)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_RAISE, 0)

    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], REG_CURPOS, SYM_PLUS, 1)
  end

  def gen_match_star_dot
    # push BEGPOS
    gen_match_star_push_begpos
    @code.push mkop_sBx(OPTABLE_CODE[:JMP], 13)

    @code.push mkop_A(OPTABLE_CODE[:RESCUE], 0)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_CURPOS, REG_BEGPOS)

    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], REG_BEGPOS, SYM_PLUS, 1)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_CURPOS, REG_BEGPOS)

    gen_comp_current_end(-9)

    # POP REG_BEG2POS
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG0, REG_POSTACK)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_POP, 0)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_BEGPOS, REG_ARG0)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_RAISE, 0)

    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], REG_CURPOS, SYM_PLUS, 1)
  end
end

class Regexp
  include RiteOpcodeUtil
  include RegexpCompiler

  @@cache = {}
  def initialize(regexp)
    @regexp = regexp
    @code = []
    @pool = []
    @proc = nil
  end

  def compile(regexp)
    @code.push mkop_AsBx(OPTABLE_CODE[:LOADI], REG_BEGPOS, 0)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG0, REG_STRING)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_SIZE, 0)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ENDPOS, REG_ARG0)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_CURPOS, REG_BEGPOS)

    @code.push mkop_sBx(OPTABLE_CODE[:ONERR], 2)
    @code.push mkop_sBx(OPTABLE_CODE[:JMP], 8)
    @code.push mkop_A(OPTABLE_CODE[:RESCUE], 0)
    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], REG_BEGPOS, SYM_PLUS, 1)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_CURPOS, REG_BEGPOS)
    gen_comp_current_end(-8)

    escape = false
    i = 0
    regexp.each_char do |ch|
      if escape then
        gen_match_letter(ch)
        escape = false
      else
        case ch
        when '\\'
          escape = true

        when '.'
          case regexp[i + 1]
          when '*'
            gen_match_star_dot

          else
            gen_match_dot
          end

        when '*'

        else
          case regexp[i + 1]
          when '*'
            gen_match_star(ch)

          else
            gen_match_letter(ch)
          end
        end
      end
      i = i + 1
    end
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_RC, REG_BEGPOS)
    @code.push mkop_A(OPTABLE_CODE[:RETURN], REG_RC)
    irep = Irep.new_irep(@code, @pool, [:[], :==, :+, :raise, :size, :push, :pop, :last], 10, 2)

    @proc = irep.to_proc
  end

  def self.compile(regexp)
    ins = @@cache[regexp]
    unless ins then
      ins = self.new(regexp)
      ins.compile(regexp)
      @@cache[regexp] = ins
    end

    ins
  end

  def =~(str)
    unless @proc
      compile(@regexp)
    end

    a = []
    b = @proc.call(str, a)
    if a[0] then
      a[0]
    else
      b
    end
  end
end

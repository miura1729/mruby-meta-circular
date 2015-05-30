module RegexpCompiler
  OPTABLE_CODE = Irep::OPTABLE_CODE

  SYM_AREF = 0
  SYM_EQ = 1
  SYM_PLUS = 2
  SYM_RAISE = 3
  SYM_SIZE = 4

  REG_STRING = 1
  REG_CURPOS = 2
  REG_BEGPOS = 3
  REG_ENDPOS = 4
  REG_RC = 5

  REG_ARG0 = 6
  REG_ARG1 = 7

  def gen_match_letter(ch)
    @pool.push ch
    sidx = @pool.size - 1
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG0, REG_STRING)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG1, REG_CURPOS)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_AREF, 1)
    @code.push mkop_ABx(OPTABLE_CODE[:LOADL], REG_ARG1, sidx)
    @code.push mkop_ABC(OPTABLE_CODE[:EQ], REG_ARG0, SYM_EQ, 1)
    @code.push mkop_AsBx(OPTABLE_CODE[:JMPIF], REG_ARG0, 3)
    @code.push mkop_A(OPTABLE_CODE[:LOADNIL], REG_RC)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], REG_ARG0, SYM_RAISE, 0)
    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], REG_CURPOS, SYM_PLUS, 1)
  end

  def gen_match_dot(ch)
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
    @code.push mkop_sBx(OPTABLE_CODE[:JMP], 9)
    @code.push mkop_A(OPTABLE_CODE[:RESCUE], 0)
    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], REG_BEGPOS, SYM_PLUS, 1)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_CURPOS, REG_BEGPOS)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG0, REG_BEGPOS)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_ARG1, REG_ENDPOS)
    @code.push mkop_ABC(OPTABLE_CODE[:EQ], REG_ARG0, SYM_EQ, 1)
    @code.push mkop_AsBx(OPTABLE_CODE[:JMPNOT], REG_ARG0, -8)
    @code.push mkop_A(OPTABLE_CODE[:RETURN], REG_RC)

    escape = false
    regexp.each_char do |ch|
      if escape then
        gen_match_letter(ch)
      else
        case ch
        when '\\'
          escape = true
          
        when '.'
          gen_match_dot(ch)

        when '*'
          
        else
          gen_match_letter(ch)
        end
      end
    end
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], REG_RC, REG_BEGPOS)
    @code.push mkop_A(OPTABLE_CODE[:RETURN], REG_RC)
    irep = Irep.new_irep(@code, @pool, [:[], :==, :+, :raise, :size], 10, 2)

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

    @proc.call(str)
  end
end

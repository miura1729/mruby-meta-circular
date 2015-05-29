module RegexpCompiler
  OPTABLE_CODE = Irep::OPTABLE_CODE
  def gen_match_letter(ch)
    @pool.push ch
    sidx = @pool.size - 1
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], 3, 1)
    @code.push mkop_AB(OPTABLE_CODE[:MOVE], 4, 0)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], 3, 0, 1)
    @code.push mkop_ABx(OPTABLE_CODE[:LOADL], 4, sidx)
    @code.push mkop_ABC(OPTABLE_CODE[:EQ], 3, 1, 1)
    @code.push mkop_AsBx(OPTABLE_CODE[:JMPIF], 3, 2)
    @code.push mkop_ABC(OPTABLE_CODE[:SEND], 0, 3, 1)
    @code.push mkop_ABC(OPTABLE_CODE[:ADDI], 0, 2, 1)
  end
end

class Regexp
  OPTABLE_CODE = Irep::OPTABLE_CODE
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
    @code.push mkop_AsBx(OPTABLE_CODE[:LOADI], 0, 0)
    @code.push mkop_sBx(OPTABLE_CODE[:ONERR], 2)
    @code.push mkop_sBx(OPTABLE_CODE[:JMP], 4)
    @code.push mkop_A(OPTABLE_CODE[:RESCUE], 0)
    @code.push mkop_A(OPTABLE_CODE[:LOADF], 0)
    @code.push mkop_A(OPTABLE_CODE[:RETURN], 0)

    escape = false
    regexp.each_char do |ch|
      if escape then
        gen_match_letter(ch)
      else
        case ch
        when '\\'
          escape = true
          
        when '*'
          
        else
          gen_match_letter(ch)
        end
      end
    end
    @code.push mkop_A(OPTABLE_CODE[:LOADT], 0)
    @code.push mkop_A(OPTABLE_CODE[:RETURN], 0)
    irep = Irep.new_irep(@code, @pool, [:[], :==, :+, :raise], 10, 2)
    irep.iseq.each_with_index {|ele, i|
      print "#{Irep::disasm(ele, irep)} \n"
    }

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

    @proc.call(str, Exception.new)
  end
end

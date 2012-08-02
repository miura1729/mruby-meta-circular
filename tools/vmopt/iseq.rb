module VmOpt
  module InstUtil
    def get_opcode(code)
      code & 0x7f
    end

    def getarg_A(code)
      (code >> 23) & 0x1ff
    end

    def getarg_B(code)
      (code >> 14) & 0x1ff
    end

    def getarg_Bx(code)
      (code >> 7) & 0xffff
    end

    def getarg_C(code)
      (code >> 7) & 0x7f
    end
  end
end


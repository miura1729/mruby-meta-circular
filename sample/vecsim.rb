MTypeInf::inference_main {
class Array
  def zip(other, &block)
    if _not_execute then
      block.call(self[0], other[0])
    end

    a = _simd_check(block)
    case a
    when MMC_EXT::SIMD::AddVec
      rsimd = a.to_simd
      asimd = self.to_simd(0)
      bsimd = other.to_simd(0)
      return rsimd.add128(asimd, bsimd)

    when MMC_EXT::SIMD::SubVec
      rsimd = a.to_simd
      asimd = self.to_simd(0)
      bsimd = other.to_simd(0)
      return rsimd.sub128(asimd, bsimd)

    when MMC_EXT::SIMD::MulVec
      rsimd = a.to_simd
      asimd = self.to_simd(0)
      bsimd = other.to_simd(0)
      rsimd.mul128(asimd, bsimd)

    else
      ary = []
      arg = arg.map{|a|a.to_a}
      i = 0
      self.each do |*val|
        a = []
        a.push(val.__svalue)
        idx = 0
        while idx < arg.size
          a.push(arg[idx][i])
          idx += 1
        end
        ary.push(a)
        i += 1
      end
      ary
    end
  end
end

def mul(a, b)
  res = []
  a.to_a.zip(b.to_a) {|x, y|
    res << (x * y)
  }
end

def main
  r = mul([1, 2, 3], [4, 5, 5])
  s = [2, -2, 1].to_simd(0)
  r = mul(r, r)
  pp r.to_a
  r = (r[:a, :b, :c, :d] + s[:e, :f,:g, :h])[:a, :e, :b, :d]
  r1 = (r[:a, :b, :c, :d] + s[:e, :f,:g, :h])[:a + :b, :c + :d, :e + :f, :g + :h]
  pp r.to_a
  pp r1.to_a
  nil
end

  main
}


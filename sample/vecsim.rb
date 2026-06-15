class Array
  def zip(other, &block)
    if _not_execute then
      yield self[0], other[0]
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
  r = mul(r, r)
  pp r.to_a
  nil
end

MTypeInf::inference_main {
  main
}


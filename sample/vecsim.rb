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
      ret = rsimd.mul128(asimd, bsimd)
      res = block.binding.local_variable_get(:res)
      res.copy ret
      nil
    else
    end
  end
end

def main
  res = []
  [1, 2, 3].zip([4, 5, 6]) {|x, y|
    res << (x * y)
  }
  pp res
  nil
end

MTypeInf::inference_main {
  main
}

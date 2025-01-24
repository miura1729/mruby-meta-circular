class Count
  def initialize
    @c = 2
  end

  def skip_cnt(res)
    while res[@c]
      @c += 1
    end
    @c
  end

  def c
    @c
  end
end

def kakutani(n, res)
  re = res[n]
  if re.nil? then
    if n & 1 == 0 then
      n = n >> 1
    else
      n = n * 3 + 1
    end

    r = kakutani(n, res) + 1
    res[n] = r
    return r
  else
    return re
  end
end

def foo
  c = Count.new
  r = [0, 0]

  th = MMC_EXT::Thread.new(c, r) {|cnt, res|
    n = cnt.c
    while n < 100
      n = cnt.skip_cnt(res)
      kakutani(n, res)
    end
  }

  p r
end

MTypeInf::inference_main {
  foo
}


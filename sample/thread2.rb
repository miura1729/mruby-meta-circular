class Count
  def initialize
    @c = 2
  end

  def skip_cnt(res)
    while res[@c]
      @c += 1
    end
    n = @c
    @c += 1
    n
  end

  def c
    @c
  end
end

def kakutani(n, res)
  re = res[n]
  if re.nil? then
    nn = n
    if nn & 1 == 0 then
      nn = nn >> 1
    else
      nn = nn * 3 + 1
    end

    r = kakutani(nn, res) + 1
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
    while n < 1000
      n = cnt.skip_cnt(res)
    p n
      kakutani(n, res)
    end
  }

  th.join
  p r
end

MTypeInf::inference_main {
  foo
}


class Count
  def initialize
    @c = 2
  end

  def skip_cnt(res)
    pp "skip"
    while res[@c]
      pp @c
      @c += 1
    end
    n = @c
    pp n
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
  i = 0
  c = Count.new
  r = [0, 0]

  th = MMC_EXT::Thread.new(c, r) {|cnt, res|
    n = cnt.c
    while n < 1000
      n = cnt.skip_cnt(res)
      pp "thread 1 out #{n}"
      kakutani(n, res)
    end
  }
  th2 = MMC_EXT::Thread.new(c, r) {|cnt, res|
    n = cnt.c
    while n < 1000
      n = cnt.skip_cnt(res)
      pp "thread 2 out #{n}"
      kakutani(n, res)
    end
  }

  th.join
  th2.join
  pp r
  r.each do |e|
    if e == nil then
      pp e
      pp i
      break
    end
    i += 1
  end
end

MTypeInf::inference_main {
  foo
}


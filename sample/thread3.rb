class Count
  def initialize
    @c = 2
  end

  def skip_cnt(res)
#    pp "skip"
    while res[@c]
#      pp @c
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
  r = {}
  r[0] = 0
  r[1] = 0
  ths = []

  2.times do |tn|
    ths.push  MMC_EXT::Thread.new(c, r, tn) {|cnt, res, tn|
      n = cnt.c
      while n < 8000
        n = cnt.skip_cnt(res)
#       pp "thread #{tn} out #{n}"
        kakutani(n, res)
      end
    }
  end

  ths.each do |th|
    th.join
  end
#  pp r
end

def kkk
  100.times do |i|
    foo
  end
end

MTypeInf::inference_main {
  kkk
}


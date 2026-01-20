class StringBuf
  def initialize(buf)
    @strage = buf
    @st = 0
    @ed = 0
  end

  def strage
    @strage
  end

  def []=(idx, val)
    @strage[@st + idx] = val
  end

  def [](idx, val)
    @strage[@st + idx]
  end

  def ed=(val)
    @ed = val
  end

  def ed(val)
    @ed
  end
end

class String
  def to_stringbuf()
    res = StringBuf.new(Array.new(1024))
    i = 0
    len = self.size
    while i < len
      res[i] = self._fetch(i)
      i = i + 1
    end
    res.ed = i
    $foo = res

    res
  end
end

MTypeInf::inference_main {
  a = "foo".to_stringbuf
  p a[2]
  nil
}

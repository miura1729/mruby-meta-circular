class StringBuf
  def initialize(buf, capa)
    @strage = buf
    @capa = capa
    @st = 0
    @ed = 0
  end

  def strage
    @strage
  end

  def []=(idx, val)
    @strage._set(@st + idx, val)
    if @ed < idx then
      @ed = idx
    end
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

  def st=(val)
    @st = val
  end

  def st(val)
    @st
  end

  def foo
    true
  end

  def each
    case foo
    when true
      pp "foooo"

    else
      i = 0
      while i < @ed
        nv = StringBuf.new(@strage, @capa)
        nv.st = @st + i
        yield nv
        i = i + 1
      end
    end
  end

  def find(target)
    0
  end
end

class String
  def to_stringbuf
    res = StringBuf.new(Array.new(1024), 1024)
    i = 0
    len = self.size
    while i < len
      res[i] = self._fetch(i)
      i = i + 1
    end
    res.ed = i

    res
  end
end

MTypeInf::inference_main {
  a = "foo".to_stringbuf
  a.find("oo")
  a.each {|sstr| if sstr then return sstr.st end}
  pp a[2]
  nil
}

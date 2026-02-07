class StringView
  def initialize(buf, capa)
    @strage = buf
    @capa = capa
    @st = 0
    @ed = 0
  end

  def start_with(str)
    @strage[st] == str[0]
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

  def each(&block)
    if _not_execute then
      nv = StringView.new(@strage, @capa)
      yield nv
    end

    case _simd_check(block)
    when :foo
      pp "foooo"

    else
      nv = StringView.new(@strage, @capa)
      i = 1
     while i < @ed
        nv.st = @st + i
        yield nv
        i = i + 1
      end
    end
  end

  def find(target)
    each do |sview|
      if sview.start_with target then
        return sview.st
      end
    end
  end
end

class String
  def to_stringbuf
    res = StringView.new(Array.new(1024), 1024)
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

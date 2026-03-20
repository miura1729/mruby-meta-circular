class View
  def initialize(buf, capa)
    @strage = buf
    @capa = capa
    @st = 0
    @ed = 0
  end

  def start_with(str)
    @strage == str._fetch(0)
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

  def size
    @ed - @st + 1 # + 1 means sentinel
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

  def to_simd(offset)
    @strage.to_simd(@st + offset)
  end

  def each(&block)
    nv = View.new(@strage, @capa)
    if _not_execute then
      yield nv
    end

    a = _simd_check(block)
    case a
    when MMC_EXT::SIMD::Select
      tgsimd = a.to_simd
      target = a.target
      tsize = target.size
      i = 0
      while i < size
        visimd = to_simd(i)
        visize = @ed - i
        if visize > 16 then
          visize = 16
        end
        ret = tgsimd.pcmpestri128(tsize, visimd, visize, 4)
        if ret < visize then
          return i + ret
        end
        i = i + 16
      end
      return -1

    when MMC_EXT::SIMD::Find
      tgsimd = a.to_simd
      target = a.target
      tsize = target.size
      i = 0
      while i < size
        visimd = to_simd(i)
        visize = @ed - i
        if visize > 16 then
          visize = 16
        end
        ret = tgsimd.pcmpestri128(tsize, visimd, visize, 0xc)
        if ret < visize - tsize + 1 then
          return i + ret
        end
        i = i + 16 - tsize
      end
      return -1

    when MMC_EXT::SIMD::SelectBitmap
      tgsimd = a.to_simd
      target = a.target
      tsize = target.size
      i = 0
      while i < size
        visimd = to_simd(i)
        visize = @ed - i
        if visize > 16 then
          visize = 16
        end
        res = block.binding.local_variable_get(:res)
        ret = tgsimd.pcmpestrm128(tsize, visimd, visize, 4)
        res[i, 16] = ret
        i = i + 16
      end
      res[i, 16] = 1
      return -1

    else
      i = 1
      while i < size
        nv.st = @st + i
        nv.ed = @ed - i
        yield nv
        i = i + 1
      end
      return 0
    end
  end

  def ttt_aux
    each do |sview|
      if (0..0x20).include?(sview[0]) then
        return sview.st

      elsif (0x30..0x30).include?(sview[0]) then
        return sview.st
      end
      -1
    end
  end

  def aaa
    res = Array.new(size)
    a = self
    each do |sview|
      if (0..0x20).include?(sview[0]) then
        res << true

      elsif (0x30..0x30).include?(sview[0]) then
        res << true

      else
        res << false
      end
    end
    res
  end

  def fff
    ttt_aux
  end
end

class String
  def to_view
    res = View.new(Array.new(1024), 1024)
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

def foo(a)
  a.each do |sstr|
    if sstr then return sstr.st end
  end
  -1
end

def find1(a)
  a.each { |sview|
    if sview.start_with "awk" then
      return sview.st
    end
    -1
  }
end

def find2(a)
  a.each { |sview|
    if sview.start_with "map" then
      return sview.st
    end
    -1
  }
end

def find3(a)
  a.each { |sview|
    if sview.start_with "pet" then
      return sview.st
    end
    -1
  }
end

def main
  #    0123456789abcdefghijklmnopqrstuvwxyz
  a = "sad  0fsd0m m0ma  se@pmapee qwaaaabbbc ccpetaaaawk".to_view
  pp find1(a)
  pp find2(a)
  pp find3(a)
  pp a.fff
  pp a.fff
  bb = a.aaa
  pp bb.index(true)
  pp bb[0, 16]
  pp bb[16, 16]
  pp bb[32, 16]
  pp bb[48, 16]
  pp bb[64, 16]
#  foo(a
#  pp a[2]
  nil
end

MTypeInf::inference_main {
  main
}

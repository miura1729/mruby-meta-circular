MTypeInf::inference_main {
class Chan
  def initialize
    @a = []
    @b = []
  end

  def a
    @a
  end

  def b
    @b
  end
end

def bar(c)
  c
end

def foo
  c = Chan.new
  b = []
  c.a.push 1
  c.b.push "ab"
  MMC_EXT::Thread.new(c, 1) {|chan, i|
    :foo
    a = chan.a.pop
    bar(chan)
    a
  }
  c.b.pop
end

  foo
}


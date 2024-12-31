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
    @a
  end
end

def bar(c)
  c
end

def foo
  c = Chan.new
  c.a.push 1
  MMC_EXT::Thread.new(c, 1) {|chan, i|
    :foo
    a = chan.a.pop
    bar(chan)
    chan.a
  }
end

  foo
}


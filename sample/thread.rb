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

  def foo
    @a = [:a]
  end
end

def bar(c)
  c
end

def foo
  c = Chan.new
  b = []
  c.a.push 1
  c.a.push 2
  c.b.push "ab"
  th = MMC_EXT::Thread.new(c, 1) {|chan, i|
    :foo
    a = chan.a.pop
    bar(chan)
    chan.foo
    pp a
  }
  c.foo
  pp c.b.pop
  th.join
  pp "end"
  nil
end

MTypeInf::inference_main {
  foo
}


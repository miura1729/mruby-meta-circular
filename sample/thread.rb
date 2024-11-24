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

def foo
  c = Chan.new
  c.a.push 1
  MMC_EXT::Thread.new(c) {|chan|
    :foo
    p chan.a
  }
end

  foo
}


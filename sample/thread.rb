class Chan
  def initialize
    @a = []
    @b = []
  end

#  attr_accessor :a
  def a
    @a
  end
  attr_accessor :b
end

def foo 
  c = Chan.new
  c.a.push 1
  MMC_EXT::Thread.new(c) {|chan|
#    :foo
    p chan.a
  }
end

MTypeInf::inference_main {
  foo
}

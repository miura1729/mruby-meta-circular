class Array
  def each(&block)
    idx = 0
    while idx < length
      block.call(self[idx])
      idx += 1
    end
    self
  end

  def map(&block)
    ary = []
    each{|val| ary.push(block.call(val))}
    ary
  end
end

def foo
  j = 1
#  [1, 2, 3].each {|a| p a + j}
  p [1, 2, 3].map {|a| p a + j}
end

MTypeInf::inference_main {
  foo
}

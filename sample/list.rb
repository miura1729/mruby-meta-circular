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
  a = [1, 2, 3]
  a.map {|a| j = j + a;p j}
  p j
end

MTypeInf::inference_main {
  p foo
}

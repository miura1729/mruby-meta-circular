module Enumerable
  def each_with_index(&block)
    return to_enum :each_with_index unless block

    i = 0
    self.each{|*val|
      block.call(val.__svalue, i)
      i += 1
    }
    self
  end
end

MTypeInf::inference_main {
class A
  %w[a b].each_with_index {|str, i| define_method(str) { i } }
end

p A.new.a + A.new.b
}

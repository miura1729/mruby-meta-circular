module Enumerable
  def collect(&block)
    return to_enum :collect unless block

    ary = []
    self.each{|val| ary.push(block.call(val))}
    ary
  end

  def any?(&block)
    if block
      self.each{|val| return true if block.call(val)}
    else
      self.each{|val| return true if val}
    end
    false
  end

  def each_with_index(&block)
    return to_enum :each_with_index unless block

    i = 0
    self.each{|val|
      block.call(val, i)
      i += 1
    }
    self
  end

  def entries
    ary = []
    self.each{|val|
      # __svalue is an internal method
      ary.push val
    }
    ary
  end

  def detect(ifnone=nil, &block)
    ret = ifnone
    self.each{|val|
      if block.call(val)
        ret = val
        break
      end
    }
    ret
  end
end

class TypeVariable
  include Enumerable
end

class Foo<TypeVariable
end

MTypeInf::inference_main {
  slf = Foo.new
  blk = lambda {|a| TypeVariable.new}
  slf.collect(&blk)
  blk = lambda {|a| TypeVariable.new}
  slf.any?(&blk)
  blk = lambda {|a| TypeVariable.new}
  slf.detect(TypeVariable.new, &blk)
  blk = lambda {|a| TypeVariable.new}
  slf.each_with_index(&blk)
  blk = lambda {|a| TypeVariable.new}
  slf.entries(&blk)
}




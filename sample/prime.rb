#class Range
#  include Inline
#  make_inline_method  :all?
#end

module Enumerable
  def each(&block)
    return to_enum :each unless block

    idx = 0
    while idx < length
      block.call(self[idx])
      idx += 1
    end
    self
  end

  def all?(&block)
    self.each{|val|
      return false unless block.call(val)
      nil
    }
    true
  end

  def select(&block)
    ary = []
    self.each{|*val|
      ary.push(val.__svalue) if block.call(*val)
      nil
    }
    ary
  end
end

def is_prime? n
  (2...n-1).all? { |i| n % i != 0 }
end

def sexy_primes n
  (9..n).map do |i|
    [i - 6, i]
  end.select do |i|
    i.all? { |i| is_prime? i }
  end
end

MTypeInf::inference_main {
  p (sexy_primes 100_00).size
}

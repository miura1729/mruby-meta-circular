module Enumerable
  def all?(&block)
    st = true
    if block
      self.each{|val|
        unless block.call(val)
          st = false
          break
        end
      }
    else
      self.each{|val|
        unless val
          st = false
          break
        end
      }
    end
    st
  end

  def map(&block)
    ary = []
    self.each{|val|
      ary.push(block.call(val))
    }
    ary
  end
end

def is_prime? n
  (2...n).all? { |i| n % i != 0 }
end

def sexy_primes n
  (9..n).map do |i|
    [i - 6, i]
  end.select do |i|
    i.all? { |i| is_prime? i }
  end
end

a = Time.now
sexy_primes 100_00
b = Time.now

puts b - a

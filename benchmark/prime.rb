class Range

  ##
  # Calls the given block for each element of +self+
  # and pass the respective element.
  #
  # ISO 15.2.14.4.4
  def each(&block)
    val = self.first
    unless val.respond_to? :succ
      raise TypeError, "can't iterate"
    end

    last = self.last
    return self if (val <=> last) > 0

    while((val <=> last) < 0)
      block.call(val)
      val = val.succ
    end

    if not exclude_end? and (val <=> last) == 0
      block.call(val)
    end
    self
  end
end

module Enumerable

  ##
  # Call the given block for each element
  # which is yield by +each+. Return false
  # if one block value is false. Otherwise
  # return true. If no block is given and
  # +self+ is false return false.
  #
  # ISO 15.3.2.2.1
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
p (sexy_primes 100_00).size
b = Time.now

puts b - a

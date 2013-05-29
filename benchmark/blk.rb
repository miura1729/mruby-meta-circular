#GC.disable

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
      p val
    end

    if not exclude_end? and (val <=> last) == 0
      block.call(val)
    end
    self
  end
end

(1..3).each do |j|
  n = []
  (1..200).each do |i|
    n.push i
  end
  p n
end
1000.times do |j|
  2000.times do |i|
#    p "#{i} #{j}"
#    p i
  end
end

#
# PArray - Fast vector/matrix library for number cranching
#
module PArray
  class PVector
  end

  class PVector4
    def initialize(a, b, c, d)
      self[0] = a
      self[1] = b
      self[2] = c
      self[3] = d
    end

    def inspect
      "#<Vec4 [#{self[0]}, #{self[1]}, #{self[2]}, #{self[3]}]>"
    end

    def +(other)
      PVector4.new(self[0] + other[0], self[1] + other[1], self[2] + other[2], self[3] + other[3])
    end

    def -(other)
      PVector4.new(self[0] - other[0], self[1] - other[1], self[2] - other[2], self[3] - other[3])
    end

    def cross(other)
#      PVector4.new(@b * other.c - @c * other.b, @c * other.a - @a * other.c, @a * other.b - @b * other.b, 0)
    end
  end
end

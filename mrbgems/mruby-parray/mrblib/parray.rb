#
# PArray - Fast vector/matrix library for number cranching
#
module PArray
  class PVector
  end

  class PVector4
    include MMM

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
      PVector4[self[0] + other[0], self[1] + other[1], self[2] + other[2], self[3] + other[3]]
    end

    def -(other)
      PVector4[self[0] - other[0], self[1] - other[1], self[2] - other[2], self[3] - other[3]]
    end

    def cross(other)
      PVector4[self[1] * other[2] - self[2] * other[1], self[2] * other[0] -self[0] * other[2], self[0] * other[1] - self[1] * other[0], 0.0]
    end

    def dot(other)
      self[0] * other[0] + self[1] * other[1] +
      self[2] * other[2] + self[3] * other[3]
    end

    def length
      Math.sqrt(self[0] * self[0] + self[1] * self[1] +
           self[2] * self[2] + self[3] * self[3])
    end

    def normalize
      len = length
      if len > 1.0e-17 then
        v = PVector4[
            self[0] / len,
            self[1] / len,
            self[2] / len,
            self[3] / len
        ]
      else
        v = PVector4[
            self[0],
            self[1],
            self[2],
            self[3]
        ]
      end
      v
    end
  end
end

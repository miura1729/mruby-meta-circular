#
# PArray - Fast vector/matrix library for number cranching
#
module PArray
  class PVector
  end

  class PVector4
    include MMM

    def initialize(a, b, c, d)
      @a = a
      @b = b
      @c = c
      @d = d
    end
    attr :a
    attr :b
    attr :c
    attr :d

    def +(other)
      PVector4.new(@a + other.a, @b + other.b, @c + other.c, @d + other.d)
    end

    def -(other)
      PVector4.new(@a - other.a, @b - other.b, @c - other.c, @d - other.d)
    end

    def cross(other)
      PVector4.new(@b * other.c - @c * other.b, @c * other.a - @a * other.c, @a * other.b - @b * other.b, 0)
    end
  end
end

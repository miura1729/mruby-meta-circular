module MTypeInf
  class BasicType
    def initialize(co)
      @class_object = co
    end

    attr :class_object
  end

  class PrimitiveType<BasicType
  end

  class ContainerType<BasicType
    def initialize(co)
      super
      @element = {}
    end

    attr :element
  end

  class UserDefinedType<BasicType
    def initialize(co)
      super
      @iv = {}
    end

    attr :iv
  end

  TypeSource = {
    NilClass => PrimitiveType,
    Fixnum => PrimitiveType,
    Float => PrimitiveType,
    Symbol => PrimitiveType,
    TrueClass => PrimitiveType,
    FalseClass => PrimitiveType,
    String => PrimitiveType,

    Array => ContainerType,
    Hash => ContainerType,
    Range => ContainerType,
  }
  TypeTable = {}
  TypeSource.each do |ty, cl|
    TypeTable[ty] = cl.new(ty)
  end
end

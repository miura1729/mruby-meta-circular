module MTypeInf
  class BasicType
    def initialize(co, *rest)
      @class_object = co
    end

    def ==(other)
      self.class == other.class &&
      @class_object == other.class_object
    end

    attr :class_object
  end

  class PrimitiveType<BasicType
  end

  class LiteralType<BasicType
    def initialize(co, val, *rest)
      super
      @element = {}
      @val = val
    end

    attr :element
    attr :val
  end

  class ContainerType<BasicType
    def initialize(co, *rest)
      super
      @element = {}
    end

    attr :element
  end

  class ProcType<BasicType
    def initialize(co, irep, *rest)
      super
      @irep = irep
    end

    attr :irep
  end

  class UserDefinedType<BasicType
    @@class_tab = {}

    def initialize(co, *rest)
      super
    end
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

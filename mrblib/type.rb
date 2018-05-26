module MTypeInf
  class BasicType
    def initialize(co, *rest)
      @class_object = co
    end

    def ==(other)
      self.class == other.class &&
      @class_object == other.class_object
    end

    def type_equal(other, tup)
      self == other
    end

    attr :class_object

    def merge(arr)
      if @class_object != Class || 1 then
        arr.each_with_index do |ele, i|
          if ele.class_object == @class_object then
            if ele.is_a?(MTypeInf::PrimitiveType) then
              return

            elsif is_a?(MTypeInf::PrimitiveType) then
              arr[i] = MTypeInf::PrimitiveType.new(ele.class_object)
              return
            end
          end

          if ele == self then
            case ele
            when MTypeInf::LiteralType
              if ele.val != @val and ele.class_object != Class then
                arr[i] = MTypeInf::PrimitiveType.new(ele.class_object)
              end

              return

            when MTypeInf::ContainerType
              ele.element.each do |idx, reg|
                if reg then
                  reg.add_same @element[idx]
                end
              end

              return

            when MTypeInf::UserDefinedType,
              MTypeInf::PrimitiveType,
              MTypeInf::ProcType
              return

            else
              raise self

            end
          end

          #        elsif ele < self then
          #
          #        end
        end
      end

      arr.push self
    end

    def inspect_element
      inspect
    end
  end

  class PrimitiveType<BasicType
  end

  class LiteralType<BasicType
    def initialize(co, val, *rest)
      super
      @val = val
    end

    attr :val
  end

  class ContainerType<BasicType
    def initialize(co, *rest)
      super
      @element = {}
      reg = RiteSSA::Reg.new(nil)
#      reg.add_type PrimitiveType.new(NilClass, nil), 0
      @element[nil] = reg
    end

    def type_equal(other, tup)
      if self.class != other.class ||
          @class_object != other.class_object then
        return false
      end
      # return other.element == @element

      stype = @element[nil].type[tup]
      if stype == nil then
        return true
      end
      dtype = other.element[nil].type[tup]
      if dtype == nil then
        return true
      end

      stype.size.times do |i|
        if !stype[i].type_equal(dtype[i], tup) then
          return false
        end
      end
      return true
    end

    def inspect
      res = "<#{@class_object} element=["
      @element.each do |key, val|
        res << "#{key.inspect}="
        val.type.each do |tup, tys|
          res << "#{tup} = ["
          tys.each do |ty|
            res << "#{ty.inspect_element}|"
          end
          res << "]\n"
        end
        res << "\n"
      end
      res << "]>"
      res
    end

    def inspect_element
      "<#{@class_object} element=...>"
    end

    attr :element
  end

  class ProcType<BasicType
    def initialize(co, irep, slf, *rest)
      super
      @irep = irep
      @slf = slf
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @irep == other.irep
    end

    attr :irep
    attr :slf
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

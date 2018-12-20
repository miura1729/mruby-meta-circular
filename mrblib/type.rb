module MTypeInf
  class BasicType
    UNDEF_VALUE = [:undef]
    def initialize(co, *rest)
      @class_object = co
      @place = {}
    end

    def ==(other)
      self.class == other.class &&
      @class_object == other.class_object
    end

    def type_equal(other, tup)
      self == other
    end

    attr :class_object
    attr :place

    def merge(arr)
      clsobj = @class_object
      primobj = MTypeInf::PrimitiveType
      selfprimp = is_a?(primobj)
      selfcontp = is_a?(MTypeInf::ContainerType)
      if clsobj != Class then
        i = 0
        ed = arr.size
        while i < ed
          ele = arr[i]
          if ele.class_object == clsobj then
            if ele.is_a?(primobj) then
              return false

            elsif selfprimp then
              arr[i] = self
              return false
            end

            if ele == self then
              case ele
              when MTypeInf::LiteralType
                if ele.val != @val then
                  if ele.class_object != Class  then
                    arr[i] = primobj.new(ele.class_object)
                    return false
                  end

                else
                  return false
                end

              when MTypeInf::ContainerType
                if ele.element[UNDEF_VALUE] == @element[UNDEF_VALUE] then
                  return false
                end

              when MTypeInf::UserDefinedType,
                primobj,
                MTypeInf::ProcType,
                MTypeInf::ExceptionType,
                MTypeInf::SymbolType,
                MTypeInf::FiberType

                return false

              else
                raise self
              end
            end
          end

          i += 1
        end
      else
        #        elsif ele < self then
        #
        #        end
        arr.each_with_index do |ele, i|
          if ele.is_a?(MTypeInf::LiteralType) and ele.val == @val then
            return false
          end
        end
      end

      arr.push self
      return true
    end

    def inspect_aux(hist)
      @class_object.inspect
    end

    def inspect
      inspect_aux({})
    end

    def inspecto_element
      inspect
    end
  end

  class PrimitiveType<BasicType
  end

  class ExceptionType<BasicType
  end

  class LiteralType<BasicType
    def initialize(co, val, *rest)
      super
      @val = val
    end

    def inspect_aux(hist)
      case  @val
      when NilClass, TrueClass, FalseClass
        "#{@class_object.inspect}"

      else
        "#{@class_object.inspect} val=#{@val.inspect}"
      end
    end

    attr :val
  end

  class SymbolType<LiteralType
    @@symtab = {}

    def self.instance(klass, val)
      if @@symtab[val] then
        @@symtab[val]
      else
        @@symtab[val] = SymbolType.new(klass, val)
      end
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @val == other.val
    end

    def inspect_aux(hist)
      "#{@class_object.inspect}(:#{@val})"
    end
  end

  class ContainerType<BasicType
    def initialize(co, *rest)
      super
      @element = {}
      reg = RiteSSA::Reg.new(nil)
#      reg.add_type PrimitiveType.new(NilClass, nil), 0
      @element[UNDEF_VALUE] = reg
      reg = RiteSSA::Reg.new(nil)
      @key = reg
    end

    def ==(other)
#      self.class == other.class &&
#        @class_object == other.class_object &&
#        @element == other.element
      equal?(other)
    end

    def type_equal(other, tup)
      if self.class != other.class ||
          @class_object != other.class_object then
        return false
      end
      return other.element[UNDEF_VALUE] == @element[nil]
    end

    def inspect_aux(hist)
      if hist[self] then
        return "<#{@class_object} ...>"
      end
      hist[self] = true

      elearr = []
      @element.each do |key0, ele|
        ele.type.each do |tup, tys|
          ele.get_type(tup).each do |ty|
            elearr << ty.inspect_aux(hist)
          end
        end
      end
      hist.delete(self)
      "#{@class_object.inspect}<#{elearr.uniq.join('|')}>"
    end

    def inspecto
      res = "<#{@class_object} element=["
      @element.each do |key, val|
        res << "#{key.inspect}="
        val.type.each do |tup, tys|
          res << "#{tup} = ["
          val.get_type(tup).each do |ty|
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
    attr :key
  end

  class ProcType<BasicType
    @@num = 0

    def initialize(co, irep, slf, env, tups, *rest)
      super
      @id = @@num
      @@num += 1
      @irep = irep
      @slf = slf
      @env = env
      @tups = tups
      @using_tup = []
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @irep == other.irep &&
        @env == other.env
    end

    def inspect
      "#{@class_object.inspect}<irep=#{@irep.irep.id.to_s(16)} env=#{env}>"
    end

    attr :id
    attr :irep
    attr :slf
    attr :env
    attr :tups
    attr :using_tup
  end

  class FiberType<BasicType
    @@num = 0

    def initialize(co, proc, *rest)
      super
      @id = @@num
      @@num += 1
      @proc = proc
      @ret = RiteSSA::Reg.new(nil)
    end

    def ==(other)
      self.class == other.class &&
        @proc == other.proc
    end

    def inspect
      "#{@class_object.inspect}<proc=#{@proc} ret =#{@ret.type}>"
    end

    attr :id
    attr :proc
    attr :ret
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

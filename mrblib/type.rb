module MTypeInf
  class BasicType
    UNDEF_VALUE = [:undef]
    @@place = {}
    def initialize(co, *rest)
      @class_object = co
#      @@place[@class_object] = {}
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

    def place
#      @@place[@class_object]
      @place
    end

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
          ele.place.merge!(place)
#          place.merge!(ele.place)
          if ele.class_object == clsobj then
            if ele.is_a?(primobj) then
              return false

            elsif selfprimp then
              arr[i] = self
              return false
            end

            if ele.class == self.class then
              case ele
              when MTypeInf::SymbolType
                arr.push self
                arr.uniq!
                return false

              when primobj,
                MTypeInf::ProcType,
                MTypeInf::ExceptionType,
                MTypeInf::FiberType

                return false

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

              when MTypeInf::UserDefinedType
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
#      "#{@class_object.inspect}"
      "#{@class_object.inspect}"
    end

    def inspect
      inspect_aux({})
    end

    def inspecto_element
      inspect
    end

    def is_escape?(cache = {})
      plist = place
      if plist.size == 0 then
        return false
      end
      if cache[plist] then
        return false
      end

      plist.any? {|e, val|
        case e
        when :return
          is_gcobject?

        when ProcType
          cache[e.place] = true
          e.is_escape?(cache)

        when RiteSSA::Reg
#          e.is_escape?(nil, cache)
          true

        when TrueClass
          true

        else
          true
        end
      }
    end

    def is_gcobject?
      false
    end
  end

  class PrimitiveType<BasicType
  end

  class ExceptionType<BasicType
    def is_gcobject?
      true
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object
    end
  end

  class LiteralType<BasicType
    def initialize(co, val, *rest)
      super
      @val = val
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @val == other.val
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
      @place = {}
    end

    def place
      @place
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @element == other.element
#      equal?(other)# && is_escape? == other.is_escape?
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

    def is_gcobject?
      true
    end

    attr :element
    attr :key
  end

  class ProcType<BasicType
    @@tab = []

    def self.gettab
      @@tab
    end

    def initialize(co, irep, slf, slfreg, env, tups, pproc,  *rest)
      super
      @id = @@tab.size
      @@tab.push self
      @irep = irep
      @slf = slf
      @slfreg = slfreg
      @env = env
      @tups = tups
      @using_tup = {}
      @parent = pproc
      @inspect_stack = []
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @irep == other.irep &&
        @env == other.env
    end

    def inspect
      if @inspect_stack.include?(self) then
        "#{@class_object.inspect}<irep=#{@irep.irep.id.to_s(16)} env=...>"
      else
        @inspect_stack.push self
        envstr = env.map {|reg|
          envty = reg.flush_type_alltup(0)[0]
          if envty then
            envty.map {|e| e.inspect}.join('|')
          else
            ''
          end
        }.join(', ')
        rc = "#{@class_object.inspect}<irep=#{@irep.irep.id.to_s(16)} env=[#{envstr}]>"
        @inspect_stack.pop
        rc
      end
    end

    def is_gcobject?
      true
    end

    attr :id
    attr :irep
    attr :slf
    attr :slfreg
    attr :env
    attr :tups
    attr :using_tup
    attr :parent
  end

  class FiberType<BasicType
    @@tab = []

    def initialize(co, proc, *rest)
      super
      @id = @@tab.size
      @@tab.push self
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

    def is_gcobject?
      true
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

    def is_gcobject?
      true
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object
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

module MTypeInf
  class BasicType
    UNDEF_VALUE = [:undef]
    def initialize(co, *rest)
      @class_object = co
      @hometown = nil
      @place = {}
      @escape_cache = nil
      @version = 0
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        is_escape? == other.is_escape?
    end

    def type_equal(other, tup)
      self == other
    end

    attr :class_object
    attr_accessor :hometown
    attr_accessor :place
    attr_accessor :version

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
                    if @val.is_a?(Numeric) then
                      positive = ele.val >= 0 && @val >= 0
                      arr[i] = NumericType.new(ele.class_object, positive)
                    else
                      arr[i] = primobj.new(ele.class_object)
                    end
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

              when MTypeInf::TypeVarType
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

    def inspect_aux(hist, level)
      "#{@class_object.inspect}"
#      "#{@class_object.inspect} e=#{is_escape?}"
    end

    def inspect(level = 0)
      inspect_aux({}, level)
    end

    def is_escape?(hist = {})
      if !is_gcobject? then
        return false
      end

      if @escape_cache then
        return @escape_cache
      end

      if hist[self] then
        return false
      end
      hist[self] = true

      plist = place
      if plist.size == 0 then
        return false
      end

      @escape_cache = plist.any? {|e, val|
        case e
        when :return
          true

        when :return_fst
          false

        when ProcType
          # for debug ProcType is independing.
          e.is_escape?(hist)

        when UserDefinedType
          e.is_escape?(hist)

        when ContainerType
          e.is_escape?(hist)

        when :returniv
          val[0].is_escape?(hist)
          #true

        else
          # true ... etc
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

  class NumericType<PrimitiveType
    def initialize(co, positive = false, *rest)
      super(co, *rest)
      @positive = positive
    end

    def inspect_aux(hist, level)
#      "#{@class_object.inspect} e=#{is_escape?} pos =#{@positive}"
      "#{@class_object.inspect}"
    end

    attr :positive
  end

  class ExceptionType<BasicType
    def is_gcobject?
      true
    end
  end

  class StringType<BasicType
    def is_gcobject?
      true
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        is_escape? == other.is_escape?
    end
  end

  class LiteralType<BasicType
    def initialize(co, val, *rest)
      super(co, *rest)
      @val = val
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @val == other.val &&
        is_escape? == other.is_escape?
    end

    def type_equal(other, tup)
      self.class == other.class &&
        @class_object == other.class_object &&
        @val == other.val &&
        is_escape? == other.is_escape?
    end

    def inspect_aux(hist, level)
      case  @val
      when NilClass, TrueClass, FalseClass
        "#{@class_object.inspect}"

      else
        if level < 1 then
#          "#{@class_object.inspect} val=#{@val.inspect} e=#{is_escape?}"
          "#{@class_object.inspect} val=#{@val.inspect}"
        else
#          "#{@class_object.inspect} e=#{is_escape?}"
          "#{@class_object.inspect}"
        end
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

    def inspect_aux(hist, level)
      if level < 2 then
        "#{@class_object.inspect}(:#{@val})"
      else
        "#{@class_object.inspect}"
      end
    end
  end

  class ContainerType<BasicType
    def initialize(co, ht, *rest)
      super(co, *rest)
      @element = {}
      @hometown = ht
      reg = RiteSSA::Reg.new(nil)
#      reg.add_type PrimitiveType.new(NilClass, nil), 0
      @element[UNDEF_VALUE] = reg
      reg = RiteSSA::Reg.new(nil)
      @key = reg
      @immidiate_only = true
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @element.size == other.element.size &&
        @element == other.element &&
        is_escape? == other.is_escape?
#      equal?(other)# && is_escape? == other.is_escape?
    end

    def inspect_aux(hist, level)
      if hist[self] then
        return "<#{@class_object} ...> e=#{is_escape?}"
      end
      hist[self] = true

      elearr = []
      @element.each do |key0, ele|
        ele.type.each do |tup, tys|
          ele.get_type(tup).each do |ty|
            elearr << ty.inspect_aux(hist, level)
          end
        end
      end
      hist.delete(self)
      if level < 3 then
#        "#{@class_object.inspect}<#{elearr.uniq.join('|')}> e=#{is_escape?}"
        "#{@class_object.inspect}<#{elearr.uniq.join('|')}>"
      else
#        "#{@class_object.inspect}<> e=#{is_escape?}"
        "#{@class_object.inspect}<>"
      end
    end

    def inspect_element(level)
      "<#{@class_object} element=...>"
    end

    def is_gcobject?
      true
    end

    attr :element
    attr :key
    attr_accessor :immidiate_only
  end

  class ProcType<BasicType
    @@tab = []

    def self.gettab
      @@tab
    end

    def initialize(co, irep, slf, slfreg, env, tups, pproc,  *rest)
      super(co, *rest)
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
        @env == other.env &&
        is_escape? == other.is_escape?
    end

    def inspect(level = 0)
      if @inspect_stack.include?(self) then
        if level < 1 then
          "#{@class_object.inspect}<irep=#{@irep.irep.id.to_s(16)} env=...>"
        else
          "#{@class_object.inspect}<>"
        end
      else
        @inspect_stack.push self
        envstr = env.map {|reg|
          envty = reg.flush_type_alltup(0)[0]
          if envty then
            envty.map {|e| e.inspect(level)}.join('|')
          else
            ''
          end
        }.join(', ')
        if level < 1 then
          rc = "#{@class_object.inspect}<irep=#{@irep.irep.id.to_s(16)} env=[#{envstr}]>"
        else
          rc = "#{@class_object.inspect}<>"
        end
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
      super(co, *rest)
      @id = @@tab.size
      @@tab.push self
      @proc = proc
      @ret = RiteSSA::Reg.new(nil)
    end

    def ==(other)
      self.class == other.class &&
        @proc == other.proc &&
        is_escape? == other.is_escape?
    end

    def inspect(level = 0)
      if level < 1 then
        "#{@class_object.inspect}<proc=#{@proc} ret =#{@ret.type}>"
      else
        "#{@class_object.inspect}<>"
      end
    end

    def is_gcobject?
      true
    end

    attr :id
    attr :proc
    attr :ret
  end

  class UserDefinedType<BasicType
    @@instances = []

    def initialize(co, ht, *rest)
      super(co, *rest)
      @hometown = ht
#      @@instances.push self
    end

#    def initialize_copy(obj)
#      @@instances.push self
#    end

#    def self.reset_hometown
#      @@instances.each do |ins|
#        ins.hometown = nil
#      end
#    end

    def is_gcobject?
      true
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
#        @hometown == other.hometown &&
        @version == other.version &&
        is_escape? == other.is_escape?
    end

    def type_equal(other, tup)
      self.class == other.class &&
        @class_object == other.class_object &&
#        @hometown == other.hometown &&
        @version == other.version &&
        is_escape? == other.is_escape?
    end
  end

  TypeSource = {
    NilClass => PrimitiveType,
    Fixnum => NumericType,
    Float => NumericType,
    Symbol => PrimitiveType,
    TrueClass => PrimitiveType,
    FalseClass => PrimitiveType,
    String => StringType,

    Array => ContainerType,
    Hash => ContainerType,
    Range => ContainerType,
  }
  TypeTable = {}
  TypeSource.each do |ty, cl|
    if cl == ContainerType then
      TypeTable[ty] = cl.new(ty, nil)
    else
      TypeTable[ty] = cl.new(ty)
    end
  end
end

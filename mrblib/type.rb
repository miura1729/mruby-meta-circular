module MTypeInf
  class BasicType
    UNDEF_VALUE = [:undef]
    def initialize(co, *rest)
      @class_object = co
      @hometown = nil
      @phometowns = nil
      @level = 0
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
    attr_accessor :phometowns
    attr_accessor :level
    attr_accessor :place
    attr_accessor :version

    def merge(arr)
      clsobj = @class_object
      primobj = MTypeInf::PrimitiveType
      selfprimp = is_a?(primobj)
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

                    elsif @val.is_a?(String) then
                      arr[i] = StringType.new(ele.class_object, nil, nil, 0)

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

              when MTypeInf::StringType
                return false

              when MTypeInf::TypeVarType
                return false

              when MTypeInf::RegClassType
                return false

              when MTypeInf::MemClassType
                return false

              when MTypeInf::ExpType
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
#      "#{@class_object.inspect}"
      "#{@class_object.inspect} e=#{is_escape?} l=#{@level}"
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

      plist = @place
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

        when UserDefinedType, ContainerType
          if e.is_escape?(hist) then
            true

          elsif @level <= e.level then
            false

          elsif e.hometown.irep == @phometowns[-2].irep then
            false

          elsif e.hometown.irep == @phometowns[-3].irep then
            if @phometowns[-2].is_a?(RiteSSA::Block)
              base = @phometowns[-2].allocate_reg
            else
              base = @phometowns[-2].node.root.allocate_reg
            end
            reg = @hometown.outreg[0]
            stup = reg.type.keys[0]
            base.each do |ptup, regs|
              reg.type[ptup] ||= reg.type[stup]
              if !regs.include?(reg) then
                regs.push reg
              end
            end
            false

          else
#            p "LINE #{val[0].line(0)} #{val[1].line(0)} #{@hometown.irep.line(0)} #{@phometowns.line(0)}"
            p "LINE #{@level} #{e.level}"
            p e.hometown.irep
            p @phometowns
            p @hometown.irep

#            p "LINE #{@hometown.irep.line(0)} #{@phometowns.line(0)} #{e.hometown.irep.line(0)} #{e.phometowns.line(0)}"
            true
          end

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
      "#{@class_object.inspect} e=#{is_escape?} pos =#{@positive}"
#      "#{@class_object.inspect}"
    end

    attr :positive
  end

  class ExceptionType<BasicType
    def is_gcobject?
      true
    end
  end

  class StringType<PrimitiveType
    def initialize(co, ht, pht, level, size = nil, *rest)
      super(co, *rest)
      @size = size
      @hometown = ht
      @phometowns = pht
      @level = level
    end

    def is_gcobject?
      true
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        is_escape? == other.is_escape?
    end

    attr :size
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
        "Literal #{@class_object.inspect}"

      else
        if level < 1 then
          "#{@class_object.inspect} val=#{@val.inspect} e=#{is_escape?}"
#          "Literal #{@class_object.inspect} val=#{@val.inspect}"
        else
          "#{@class_object.inspect} e=#{is_escape?}"
#          "Literal #{@class_object.inspect}"
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
    def initialize(co, ht, pht, level, *rest)
      super(co, *rest)
      @element = {}
      @hometown = ht
      @phometowns = pht
      @level = level
      reg = RiteSSA::Reg.new(nil)
#      reg.add_type PrimitiveType.new(NilClass, nil), 0
      @element[UNDEF_VALUE] = reg
      reg = RiteSSA::Reg.new(nil)
      @key = reg
      @immidiate_only = true
    end

    def is_escape?(hist = {})
      if !@immidiate_only then
        true
      else
        super(hist)
      end
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @element.size == other.element.size &&
        @hometown == other.hometown &&
        @element == other.element &&
        is_escape? == other.is_escape?
#      equal?(other)# && is_escape? == other.is_escape?
    end

    def inspect_aux(hist, level)
      if hist[self] then
        return "<#{@class_object} ...> e=#{is_escape?} l=#{@level}"
      end
      hist[self] = true

      elearr = []
      @element.each do |key0, ele|
        ele.type.each do |tup, tys|
          ele.get_type(tup).each do |ty|
            elearr << "#{key0} => #{ty.inspect_aux(hist, level)}"
          end
        end
      end
      hist.delete(self)
      if level < 3 then
        "#{@class_object.inspect}<#{elearr.uniq.join('|')}> e=#{is_escape?} l=#{@level}"
#        "#{@class_object.inspect}<#{elearr.uniq.join('|')}>"
      else
        "#{@class_object.inspect}<> e=#{is_escape?} l=#{@level}"
#        "#{@class_object.inspect}<>"
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

    def initialize(co, ht, pht, level, *rest)
      super(co, *rest)
      @hometown = ht
      @phometowns = pht
      @level = level
    end

    def is_gcobject?
      true
    end

    def ==(other)
      self.class == other.class &&
        @class_object == other.class_object &&
        @hometown == other.hometown &&
        @version == other.version &&
        is_escape? == other.is_escape?
    end

    def type_equal(other, tup)
      self.class == other.class &&
        @class_object == other.class_object &&
        @hometown == other.hometown &&
        @version == other.version &&
        is_escape? == other.is_escape?
    end
  end

  class UserDefinedStaticType<UserDefinedType
    def is_gcobject?
      false
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
    if cl == ContainerType or cl == StringType then
      TypeTable[ty] = cl.new(ty, nil, nil, 0)
    else
      TypeTable[ty] = cl.new(ty)
    end
  end
end

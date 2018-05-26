TOP_SELF = self
module MTypeInf
  def self.inference_main(&b)
    irep = Irep::get_proc_irep(b)
    ti = MTypeInf::TypeInferencer.new

    b = RiteSSA::Block.new(irep, nil, self)
    ti.inference_top(b)
    ti.dump_type
  end

  class TypeTupleTab
    @@id = 0

    def initialize
      @table = []
      @rev_table = []
    end

    def cmp_types(t1, t2, tup)
      if !t1.kind_of?(Array) || !t2.kind_of?(Array) then
        return t1 == t2
      end

      if t1.size != t2.size then
        return false
      end

      t1.size.times do |i|
        if t1[i] != t2[i] or !t1[i].type_equal(t2[i], tup) then
          return false
        end
      end

      return true
    end

    def get_tupple_id(types, tup)
      node = @table
      types = types.dup
      types.push nil
      types.each_with_index do |ty, i|
        cn = node.find {|te| cmp_types(te[0], ty, tup)}
        if cn.nil? then
          onn = nn = []
          types[i..-1].each do |nty|
            nn.push [nty, []]
            nn = nn[0][1]
          end
          nn[1] = @@id
          @rev_table[@@id] = types
          @@id = @@id + 1
          node.push onn[0]

          return nn[1]
        end
        node = cn[1]
      end

      node[1]
    end

    attr :rev_table
  end

  class TypeInferencer
    @@ruletab ||= {}
    @@methodtab ||= {}
    def initialize
      @typetupletab = TypeTupleTab.new
      @callstack = []
    end

    attr :typetupletab
    attr :callstack

    def dump_type
      RiteSSA::ClassSSA.all_classssa.each do |cls, clsobj|
        print "Class #{cls}\n"
        print " Instance variables\n"
        clsobj.iv.each do |iv, reg|
          types =reg.flush_type_alltup(0)[0] || []
          type = types.map {|ele| ele.class_object.inspect}.join('|')
          print "  #{iv}: #{type}\n"
        end
        print "\n methodes \n"
        clsobj.method.each do |name, node|
          types = node.retreg.type
          types.each do |arg, types|
            args = @typetupletab.rev_table[arg]
            args =  args[0..-2]
            args = args.map {|tys|
              tys.map {|ele| ele.class_object.inspect}.join('|')
            }.join(', ')
            type = types.map {|ele| ele.class_object.inspect}
            type = type.join('|')
            print "  #{name}: (#{args}) -> #{type} \n"
          end
        end
        print "\n"
      end
    end

    def inference_top(saairep)
      topobj = TOP_SELF.class
      ty = TypeTable[topobj] = UserDefinedType.new(topobj)
      intype = [[ty]]
      tup = @typetupletab.get_tupple_id(intype, 0)
      inference_block(saairep, intype, tup)
    end

    def inference_block(saairep, intype, tup)
      if saairep.argtab[tup] then
        return
      end

      saairep.argtab[tup] = true
      @callstack.push [saairep, tup]
      intype.each_with_index do |tys, i|
        if tys then
          tys.each do |ty|
            saairep.nodes[0].enter_reg[i].add_type(ty, tup)
          end
        end
      end

      inference_node(saairep.nodes[0], tup, saairep.nodes[0].enter_reg, {})
      inference_node(saairep.nodes[0], tup, saairep.nodes[0].enter_reg, {})
#      while saairep.retreg.type.size == 0
#        p saairep.retreg.type
        inference_node(saairep.nodes[0], tup, saairep.nodes[0].enter_reg, {})
        inference_node(saairep.nodes[0], tup, saairep.nodes[0].enter_reg, {})
        inference_node(saairep.nodes[0], tup, saairep.nodes[0].enter_reg, {})
#      end

      @callstack.pop
    end

    def inference_node(node, tup, in_reg, history)
      in_reg.each_with_index do |ireg, i|
        node.enter_reg[i].add_same ireg
      end

      node.ext_iseq.each do |ins|
        # p ins.op #for debug
        rc = @@ruletab[:OP][ins.op].call(self, ins, node, tup, history)
        if rc then
          # escape for customized contination (see OP_JMPNOT)
          return
        end
      end

      history[node] ||= []
      node.exit_link.each do |nd|
        if history[node].index(nd) == nil then
          history[node].push nd
          inference_node(nd, tup, node.exit_reg, history)
        end
      end
    end
  end
end

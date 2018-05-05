TOP_SELF = self
module MTypeInf
  class TypeTupleTab
    @@id = 0

    def initialize
      @table = []
      @rev_table = []
    end

    def get_tupple_id(types)
      node = @table
      types = types.dup
      types.push nil
      types.each_with_index do |ty, i|
        cn = node.find {|te| te[0] == ty}
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

    def inference_top(saairep)
      topobj = TOP_SELF
      ty = TypeTable[topobj] = UserDefinedType.new(topobj)
      intype = [[ty]]
      tup = @typetupletab.get_tupple_id(intype)
      inference_block(saairep, intype, tup)
    end

    def inference_block(saairep, intype, tup)
      if saairep.retreg.flush_type(tup)[tup] then
        return
      end

      @callstack.push [saairep, tup]
      intype.each_with_index do |tys, i|
        tys.each do |ty|
          saairep.nodes[0].enter_reg[i].add_type(ty, tup)
        end
      end

      inference_node(saairep.nodes[0], tup, saairep.nodes[0].enter_reg, {})
      @callstack.pop
    end

    def inference_node(node, tup, in_reg, history)
      in_reg.each_with_index do |ireg, i|
        node.enter_reg[i].add_same ireg
      end
      inference_iseq(node, node.ext_iseq, tup)

      history[node] ||= []
      node.exit_link.each do |nd|
        if history[node].index(nd) == nil then
          history[node].push nd
          inference_node(nd, tup, node.exit_reg, history)
        end
      end
    end

    def inference_iseq(node, iseq, tup)
      iseq.each do |ins|
        p ins.op
        @@ruletab[:OP][ins.op].call(self, ins, node, tup)
      end
    end
  end
end

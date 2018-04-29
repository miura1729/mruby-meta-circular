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
    def initialize
      @typetupletab = TypeTupleTab.new
    end

    def inference_top(saairep)
      topobj = TOP_SELF
      p topobj
      ty = TypeTable[topobj] = UserDefinedType.new(topobj)
      inference_block(saairep, [ty])
    end

    def inference_block(saairep, intype)
      tup = @typetupletab.get_tupple_id(intype)
      intype.each_with_index do |ty, i|
        saairep.regtab[i].add_type(ty, tup)
      end

      inference_node(saairep.nodes[0], tup, saairep.regtab[0, intype.size])
    end

    def inference_node(node, tup, in_reg)
      in_reg.each_with_index do |ireg, i|
        node.enter_reg[i].add_same ireg
      end

      while node
        node = inference_iseq(node, node.ext_iseq, tup)
      end
    end

    def inference_iseq(node, iseq, tup)
      iseq.each do |ins|
        @@ruletab[:OP][ins.op].call(ins, node, tup)
      end
    end
  end
end

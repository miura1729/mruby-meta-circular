module MTypeInf
  class TypeTupleTab
    @@id = 0
    def initialize
      @table = []
    end

    def get_id(types)
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
          @@id = @@id + 1
          node.push onn[0]

          return nn[1]
        end
        node = cn[1]
      end

      return node[1]
    end
  end

  class TypeInferencer
    def initialize
      @typetupletab = TypeTupleTab.new
    end

    def inference_top(saairep)
      topobj = Object.class.new
      TypeTable[topobj] = UserDefinedType.new(topobj)
      inference_block(saairep, [topobj])
    end

    def inference_block(saairep, inregtypes)
      i = 0
      inregtypes.each do |ty|
        saairep.regtab[i].same ty
      end
    end
  end
end

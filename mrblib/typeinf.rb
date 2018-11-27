TOP_SELF = Object.new

module MTypeInf
  def self.inference_main(&b)
    irep = Irep::get_proc_irep(b)
    ti = TypeInferencer.new

    b = RiteSSA::Block.new(irep, nil, TOP_SELF.class, true)
    bproc = ti.inference_top(b)
    ti.messages.each do |message, cnt|
      print message
    end

    typemess = ti.dump_type
    print typemess
    cgen = CodeGenC::CodeGen.new
    cgen.ccode << "/*\n#{typemess}*/\n"
    cgen.code_gen(bproc, ti)
    print cgen.ccode
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

    def get_tupple_id(types, proc, tup)
      node = @table
      types = types.dup
      types.push proc
      types.push nil
      types.each_with_index do |ty, i|
#        cn = node.find {|te| cmp_types(te[0], ty, tup)}
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
      @messages = {}
      @step = 1
      @exception = []
    end

    attr :typetupletab
    attr :callstack
    attr :messages
    attr :exception

    def dump_method(name, node)
      mess = ""
      node.retreg.flush_type_alltup(0)[0]
      types = node.retreg.type
      node.export_exception.flush_type_alltup(0)[0]
      exceptions = node.export_exception.type
      exfmt = []
      exceptions.each do |arg, types|
        exfmt.push types.map {|ele| ele.inspect}.join(' ,')
      end
      exfmt.uniq!
      if types.size == 0 then
        exceptions.each do |arg, types|
          args = @typetupletab.rev_table[arg]
          args =  args[0..-3]
          if args.size == 1 then
            next
          end
          args = args.map {|tys|
            tys.map {|ele| ele.inspect}.join('|')
          }.join(', ')
          if exfmt.size != 0 then
            mess << "  #{name}: (#{args}) ->  (throws #{exfmt.join(',')}) \n"
          else
            mess << "  #{name}: (#{args}) ->  \n"
          end
        end
      else
        types.each do |arg, types|
          args = @typetupletab.rev_table[arg]
          args =  args[0..-3]
          if args.size == 1 then
            next
          end
          args = args.map {|tys|
            tys.map {|ele| ele.inspect}.join('|')
          }.join(', ')
          type = types.map {|ele| ele.inspect}
          type = type.join('|')
          if exfmt.size != 0 then
            mess << "  #{name}: (#{args}) -> #{type} throws #{exfmt.join(',')} \n"
          else
            mess << "  #{name}: (#{args}) -> #{type} \n"
          end
        end
      end

      node.reps.each do |blk|
        mess << dump_method("#{name} block", blk)
      end

      mess
    end

    def dump_type
      mess = ""
      RiteSSA::ClassSSA.all_classssa.each do |cls, clsobj|
        mess << "Class #{cls}\n"
        mess << " Instance variables\n"
        clsobj.iv.each do |iv, reg|
          types =reg.flush_type_alltup(0)[0] || []
          type = types.map {|ele| ele.inspect}.join('|')
          mess << "  #{iv}: #{type}\n"
        end
        mess << "\n methodes \n"
        clsobj.method.each do |name, node|
          mess << dump_method(name, node)
        end
        mess << "\n"
      end

      mess
    end

    def inference_top(saairep)
      topobj = TOP_SELF
      ty = TypeTable[topobj] = LiteralType.new(topobj.class, topobj)
      intype = [[ty]]
      tup = @typetupletab.get_tupple_id(intype, PrimitiveType.new(NilClass), 0)
      bproc = ProcType.new(Proc, saairep, ty, [], [])
      bproc.place[true] = true
      inference_block(saairep, intype, tup, 2, bproc)
#      @step += 1
#      inference_block(saairep, intype, tup, 2, nil)
      bproc
    end

    def inference_block(saairep, intype, tup, argc, proc)
      fixp = true
      intype.each_with_index do |tys, i|
        if tys then
          tys.each do |ty|
            if saairep.nodes[0].enter_reg[i].add_type(ty, tup) then
              fixp = false
            end
          end
        end
      end

      if fixp then
        saairep.argtab[tup] ||= 0
        saairep.argtab[tup] += 1
      end

      if fixp and saairep.argtab[tup] > @step then
        exexp = saairep.export_exception
        if exexp and @exception.size == 0 then
          reg = RiteSSA::Reg.new(nil)
          @exception.push reg
          reg.add_same exexp
        end

        return
      end

      # clear all regs
      saairep.allreg.each do |reg|
        reg.type.delete(tup)
      end
      intype.each_with_index do |tys, i|
        if tys then
          tys.each do |ty|
            saairep.nodes[0].enter_reg[i].add_type(ty, tup)
          end
        end
      end

      @callstack.push [saairep, tup, argc, proc]
      inference_node(saairep.nodes[0], tup, saairep.nodes[0].enter_reg, {})
      i = 0
      oldtype = true
      while saairep.retreg.type != oldtype and i < 5
        oldtype = saairep.retreg.type.dup
        #p saairep.retreg.type
        i += 1
        inference_node(saairep.nodes[0], tup, saairep.nodes[0].enter_reg, {})
      end

      @callstack.pop
      if @callstack.size > 0 then
        proc = @callstack[-1][3]
        saairep.retreg.type.keys.each do |tup|
          saairep.retreg.type[tup].each do |ty|
            ty.place[proc] = true
          end
        end
      end
    end

    def inference_node(node, tup, in_reg, history)
      in_reg.each_with_index do |ireg, i|
        node.enter_reg[i].add_same ireg
        node.enter_reg[i].negative_list = ireg.negative_list.clone
        node.enter_reg[i].positive_list = ireg.positive_list.clone
      end

      node.ext_iseq.each do |ins|
        #p ins.line
        #p ins.filename
        #p ins.op #for debug
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

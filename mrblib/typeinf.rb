TOP_SELF = Object.new

module MTypeInf
  DEFAULT_OPTION = {
    :dump_level => 1
  }
  def self.inference_main(option = DEFAULT_OPTION, &b)
    irep = Irep::get_proc_irep(b)
    ti = TypeInferencer.new(option)

    b = RiteSSA::Block.new(irep, nil, TOP_SELF.class, true)
    bproc = ti.inference_top(b)
    #UserDefinedType.reset_hometown
    ti.messages.each do |message, cnt|
      print message
    end

    cgen = CodeGenC::CodeGen.new
    cgen.code_gen(bproc, ti)
    typemess = ti.dump_type
    cgen.ccode << "/*\n#{typemess}*/\n"
    print cgen.ccode
    #print typemess
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
        if !t1[i].type_equal(t2[i], tup) then
          return false
        end
      end

      return true
    end

    def get_tupple_id(types, proc, tup, ext = true)
      node = @table
      types = types.map {|nty|
        if nty.is_a?(Array) then
          nty.map {|e| e = e.dup; e.place = e.place.dup; e}
        else
          nty = nty.dup
          nty.place = nty.place.dup
          nty
        end
      }

      types.push proc
      types.push nil

      types.each_with_index do |ty, i|
        if ext then
          cn = node.find {|te|
            te[0] == ty
          }
        else
          cn = node.find {|te|
            cmp_types(te[0], ty, tup)
          }
        end

        #        cn = node.find {|te| te[0] == ty}
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
        types[i] = cn[0]
        node = cn[1]
      end

      node[1]
    end

    attr :rev_table
  end

  class TypeInferencer
    @@ruletab ||= {}
    @@methodtab ||= {}
    def initialize(option)
      @option = option
      @typetupletab = TypeTupleTab.new
      @callstack = []
      @messages = {}
      @step = 1
      @exception = []
      @fiber = nil
      @allocate_object = []
    end

    attr :option
    attr :typetupletab
    attr :callstack
    attr :messages
    attr :exception
    attr_accessor :fiber

    def dump_method(name, node)
      level = @option[:dump_level]
      mess = []
      messes = ""
      node.retreg.flush_type_alltup(0)[0]
      types = node.retreg.type
      node.export_exception.flush_type_alltup(0)[0]
      exceptions = node.export_exception.type
      exfmt = []
      exceptions.each do |arg, types|
        exfmt.push types.map {|ele| ele.inspect(level)}.join(' ,')
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
            tys.map {|ele| ele.inspect(level)}.join('|')
          }.join(', ')
          if exfmt.size != 0 then
            mess << "#{name} (#{args}) ->  (throws #{exfmt.join(',')})"
          else
            mess << "#{name} (#{args}) ->  "
          end
        end
      else
        types.each do |arg, types|
#           mess << "(#{arg}) " #Raw tup for debug
          args = @typetupletab.rev_table[arg]
          args =  args[0..-3]
          if args.size == 1 then
            next
          end
          args = args.map {|tys|
            tys.map {|ele|
              if ele.is_a?(ProcType) and name != "" then
                dump_method("", ele.irep)
              else
                ele.inspect(level)
              end
            }.join('|')
          }.join(', ')
          type = types.map {|ele| ele.inspect(level)}
          type = type.join('|')
          if exfmt.size != 0 then
            mess << "#{name} (#{args}) -> #{type} throws #{exfmt.join(',')} "
          else
            mess << "#{name} (#{args}) -> #{type} "
          end
        end
      end

      if name != "" and false then
        node.reps.each do |blk|
          messes << dump_method("#{name} block", blk)
        end
      end
      mess.uniq.join("\n  ")
    end

    def dump_type
      mess = ""
      level = @option[:dump_level]
      RiteSSA::ClassSSA.all_classssa.each do |cls, clsobj|
        if cls == TypeVariable then
          acls = cls.ancestors[1]
          mblk = clsobj.method.values[0]
          sreg = mblk.regtab[0]
          slftype = sreg.type.values[0][0]
          tvpara = slftype.sub_type_var.map {|name, ty| ty }
          interface = slftype.using_method.map {|name, ty|
            name.val
          }
          mess << "#{acls.class} #{acls} #{tvpara} #{interface} \n"
        else
          mess << "#{cls.class} #{cls}\n"
        end
        mess << " Instance variables\n"
        clsobj.iv.each do |iv, reg|
          types =reg.flush_type_alltup(0)[0] || []
          type = types.map {|ele| ele.inspect(level)}.join('|')
          mess << "  #{iv}: #{type}\n"
        end
        mess << "\n methodes \n"
        clsobj.method.each do |name, node|
          mess << "  " + dump_method(name, node)
          mess << "\n"
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
      bproc = ProcType.new(Proc, saairep, ty, nil,  [], [], nil)
      #bproc.place[true] = true
      inference_block(saairep, intype, tup, 2, bproc)
       @step += 1
      inference_block(saairep, intype, tup, 2, bproc)
      bproc
    end

    def inference_block(saairep, intype, tup, argc, proc)
      if !saairep.nodes[0] then
        return
      end
      fixp = true
      intype.each_with_index do |tys, i|
        if tys then
          if !saairep.nodes[0].enter_reg[i] then
            saairep.nodes[0].enter_reg[i] = RiteSSA::ParmReg.new(i)
          end
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

      # last element (nil) is current caller instruction for using SUPER
      @callstack.push [saairep, tup, argc, proc, nil]
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

      # Check return value is escape data
      if @callstack.size > 0 then
        proc = @callstack[-1][3]
        rets = saairep.retreg.refpoint
        noesc = rets.all? {|rins|
          rreg = rins.inreg[0]

          rreg.is_a?(RiteSSA::ParmReg) or
          rreg.genpoint.op == :GETUPVAR or
          rreg.genpoint.op == :LOADSELF or
          rreg.genpoint.op == :ENTER
        }

        ivvar = rets.all? {|rins|
          rreg = rins.inreg[0]
          if rreg.genpoint.is_a?(RiteSSA::Inst) then
            rreg.genpoint.op == :GETIV
          else
            false
          end
        }

        if !noesc then
          saairep.retreg.type.each do |tup, types|
            types.each do |ty|
              if ivvar then
                ty.place[:returniv] = [intype[0][0],
                  saairep.nodes[0].ext_iseq[0].line]
              else
                cpos = saairep.nodes[0].ext_iseq[0].line
                if ty.hometown and saairep.irep == ty.hometown.irep then
                  ty.place[:return_fst] ||= {}
                  ty.place[:return_fst][ty.hometown.irep] = cpos
                else
                  ty.place[:return] ||= {}
                  ty.place[:return][ty.hometown] = cpos
                end
              end
            end
          end
        end
      end
    end

    def inference_node(node, tup, in_reg, history)
      in_reg.each_with_index do |ireg, i|
        if i > node.irep.nlocals then
          break
        end
        if ireg then
          if !node.enter_reg[i] then
            node.enter_reg[i] = RiteSSA::ParmReg.new(i)
          end
          node.enter_reg[i].add_same ireg
          node.enter_reg[i].flush_type(tup)
          node.enter_reg[i].negative_list = ireg.negative_list.clone
          node.enter_reg[i].positive_list = ireg.positive_list.clone
        end
      end

      rc = nil
      node.ext_iseq.each do |ins|
        #p ins.line
        #p ins.filename
        #p "#{ins.line} #{ins.op} #{ins.para[0]}" #for debug
        begin
          rc = @@ruletab[:OP][ins.op].call(self, ins, node, tup, history)
        rescue Object => e
          p "#{ins.op} #{ins.filename}##{ins.line}"
          raise e
        end
        if rc then
          break
        end
      end

      if rc then
        return
      end

      history[node] ||= []
      history[nil] ||= []
      history[nil].push node
      node.exit_link.each do |nd|
        if history[node].index(nd) == nil then
          history[node].push nd
          inference_node(nd, tup, node.exit_reg, history)
        end
      end
      history[nil].pop
    end
  end
end

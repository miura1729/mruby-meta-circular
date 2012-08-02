module VmOpt
  include InstUtil
  def disasm(n)
    irep = Irep.get_irep_by_no(n)
    iseq = irep.iseq
    iseq.each do |code|
      print get_opcode(code)
      print "\n"
    end
  end
end  

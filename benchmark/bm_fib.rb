
def fib n
  return n if n < 2
  fib(n-2) + fib(n-1)
end

puts fib(39)

puts fib(39.0)

include RiteOpcodeUtil
irep = Irep::get_irep(self, :fib)
#irep = Irep::get_irep(Vec.new(-2.0, 0.0, -3.5), :vdot)
irep.iseq.each_with_index {|ele, i|
  print "#{Irep::disasm(ele, irep)} \n"
  irep.reg_type(i).each do |ele2|
    print " \t #{ele2}\n"
  end
}

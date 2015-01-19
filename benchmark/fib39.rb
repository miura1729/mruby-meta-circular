# Fib 39

def fib n
  return n if n < 2
  fib(n-2) + fib(n-1)
end

puts fib(39)

include RiteOpcodeUtil
irep = Irep::get_irep(self, :fib)
#irep = Irep::get_irep(Vec.new(-2.0, 0.0, -3.5), :vdot)
irep.iseq.each_with_index {|ele, i|
  print "#{Irep::disasm(ele, irep)} \t #{irep.reg_class(i)[0]}\n"
}

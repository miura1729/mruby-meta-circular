#a = Irep::get_irep_by_no(2)
a = Irep::get_irep([], :each)
#a = Irep::get_irep(1..3, :each)
p a
p a.pool
p a.syms
p Irep::OPTABLE_SYM
a.iseq.each do |n|
  p Irep::OPTABLE[n & 0x7f]
end

def foo
  i = 400
  j = 0
  while i != 0
    j = j + i
    i = i - 1
  end
  j + 3
end

def factt
  fact(5)
end

def fact(n)
  if n == 0 then
    1
  else
    fact(n - 1) * n
  end
end

def fib(n)
  if n == 1 then
    1
  elsif n == 0 then
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end

def fibt
  fib(20)
end

#a = Irep::get_irep(self, :foo)
#a = Irep::get_irep(self, :factt)
#a = Irep::get_irep(self, :fibt)
a.iseq.each do |n|
  p Irep::OPTABLE[n & 0x7f]
end
vm = RiteVM.new
p vm.to_relocate_iseq(Irep::get_irep(self, :fib))
p vm.eval(a)
#p foo
#p factt
p fibt



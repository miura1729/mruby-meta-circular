#a = Irep::get_irep_by_no(2)
a = Irep::get_irep([], :each)
#a = Irep::get_irep(1..3, :each)
p a
p a.pool
p a.syms
p Irep::OPTABLE_SYM
a.iseq.each do |n|
#  p Irep::OPTABLE[n & 0x7f]
end

def foo
  i = 4
  i + 3
end

a = Irep::get_irep(self, :foo)
a.iseq.each do |n|
  p Irep::OPTABLE[n & 0x7f]
end
p a.eval




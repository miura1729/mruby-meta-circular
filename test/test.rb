#a = Irep::get_irep_by_no(2)
#a = Irep::get_irep(Array, :each)
a = Irep::get_irep(1..3, :each)
p a
p a.iseq
p a.pool
p a.syms

def foo(x)
  a = 5
  b = 10
  c = 32
  d = 65
  e = 69

  t_print  Env.get_current_env(0).to_a
  t_print Env.get_current_env(1).to_a
end

def bar
  k = 3
  a = Proc.new { b = -1}
  a.call
  t_print Env.get_proc_env(a).to_a
end

y = -1
x = 0.1
foo(1)
bar


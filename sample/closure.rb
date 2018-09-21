MTypeInf::inference_main {
  def f(x)
    x
  end

  def foo
    x = 42
    $f1 = lambda { x = "str" }
    $f2 = lambda { f(x) }
  end

  foo
  if rand < 0.5
    $f1.call
  else
    $f2.call
  end
}

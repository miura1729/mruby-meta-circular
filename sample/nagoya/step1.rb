def fib(x)
  if x < 2 then
    1
  else
    fib(x - 1.0) + fib(x - 2.0)
  end
end

MTypeInf::inference_main {
  fib(42.0)
}

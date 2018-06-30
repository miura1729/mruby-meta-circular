def foo(x)
  x + x
end

MTypeInf::inference_main {
  foo(1)
  foo(1.9)
  foo("a")
  foo(:a)
}

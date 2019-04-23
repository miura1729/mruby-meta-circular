def foo(*a)
  a
end

def bar(*a)
  a[2]
end

MTypeInf::inference_main {
  foo(1, 2, "a")
  bar(1, 2, "a")
}


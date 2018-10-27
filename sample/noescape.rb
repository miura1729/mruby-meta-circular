def foo
  a = [1, 2, 3]
  a[0] + a[1] + a[2]
end

MTypeInf::inference_main {
  foo
}

# main#foo: (main, NilClass) -> Array|NilClass
# main#bottom_up_tree: (main, Fixnum, Fixnum, NilClass) -> Array
# main#bar: (main, NilClass) -> Fixnum


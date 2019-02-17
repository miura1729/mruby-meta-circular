def foo
  a = []
  a[0] = "ab"
  a[1] = 3.0
  a[2] = :b
  i = 3
  [:a, [], "abc", 1].each do |x|
    a[i] = x
    i = i + 1
  end

  a[0]
end

MTypeInf::inference_main {
  foo
}

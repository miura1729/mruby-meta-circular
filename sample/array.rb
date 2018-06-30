def foo
  a = []
  a[0] = "ab"
  a[1] = 3.0
  a[2] = :a
  i = 3
  [:a, 1, "abc", 1].each do |x|
    a[i] = x
    i = i + 1
  end

  a[5]
end

MTypeInf::inference_main {
  foo
}

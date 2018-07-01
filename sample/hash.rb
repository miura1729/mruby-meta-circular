def foo
  a = {}
#  a[1] = "ab"
#  a[:ab] = 3.0
#  a["ab"] = :a
  i = 0
  [[:a, 1], ["abc", :foo], [:a, [1]]].each do |x|
    a[x[0]] = x[1]
    $foo = x
#    i = i + 1
  end

  a
end

MTypeInf::inference_main {
  foo
}

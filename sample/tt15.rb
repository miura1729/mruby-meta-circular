MTypeInf::inference_main {
  def foo(n)
    ["foo1", "bar"].each do |sym|
      s = "def #{sym}; end"
      eval(s)
    end
  end

  foo
  foo1
  bar
}

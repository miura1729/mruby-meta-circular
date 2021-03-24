MTypeInf::inference_main {
  def foo(n)
    ["foo", "bar"].each do |sym|
      $sym = sym
      eval("def #{sym}; end")
    end
  end

  foo
}

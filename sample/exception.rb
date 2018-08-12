MTypeInf::inference_main {
  def bar
    raise
  end
  
  def foo
    begin
      $kkk = 1
      bar
    rescue
      p "foo"
    rescue ArgumentError
      p "bar"
    end
  end
  
  foo
}

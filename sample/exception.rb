MTypeInf::inference_main {
  def bar
    raise
  end

  def bazr
    bar
  end

  def foo
    begin
      bazr
    rescue
      $kkk = 1
    end
  end

  foo
}

MTypeInf::inference_main {
  def bar
    raise
  end

  def bazr
    if random
      bar
    else
      1.0
    end
  end

  def bazrr
    if random
      raise
    else
      1.0
    end
  end

  def foo
    begin
      bazr
    rescue
      bazr
      "a"
    end
  end

  foo
}

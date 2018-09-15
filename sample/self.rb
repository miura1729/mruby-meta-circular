MTypeInf::inference_main {
  def foo
  end


  $foo = MTypeInf
  MTypeInf::inference_main {
      foo
  }
}

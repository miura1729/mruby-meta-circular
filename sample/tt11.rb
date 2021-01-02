MTypeInf::inference_main {
  class Fixnum
    def method_missing(mn)
      Object::eval("class Fixnum;def #{mn};self * 60 * 60 * 24;end;end")
    end
  end

  2.days
}

MTypeInf::inference_main {
  class Fixnum
    def method_missing(mn)
      eval("class Fixnum;def #{mn};self * 60 * 60 * 24;end;end")
    end
  end

  p 2.days
}

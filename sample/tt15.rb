MTypeInf::inference_main {
  def foo(n)
    ["foo1", "bar"].each do |sym|
      s = "def #{sym};:#{sym} end"
      eval(s)
    end
  end

  foo
  foo1
  bar

  class Object
    def my_attr(*n)
#      $foo = n
      n.each do |sym|
        eval("class Foo;def #{sym}; @#{sym} end;end")
      end
      nil
    end
  end

  class Foo
    def initialize
      @a = 1
      @b = "a"
      @c = 1.1
    end
    my_attr :a, :b, :c
  end

  aa = Foo.new
  aa.a
  aa.b
  aa.c
}

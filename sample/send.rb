MTypeInf::inference_main {
class Foo
  def top
    [:a, :b, :c, :d].map do |m|
      send(m, 1)
    end
  end

  def initialize
  end

  def a(x)
    x
  end

  def b(x)
    x.to_f
  end

  def c(x)
    :a
  end

  def d(x)
    3.134
  end
end

  #def method_missing(n, *m)
  #  m
  #end

pp Foo.new.top
}

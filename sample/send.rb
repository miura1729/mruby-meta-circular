MTypeInf::inference_main {
class Foo
  def top
    [:abaa, :bcaa, :cdaa, :dddd].map do |m|
      send(m, 1)
    end
  end

  def initialize
  end

  def abaa(x)
    x
  end

  def bcaa(x)
    x.to_f
  end

  def cdaa(x)
    :a
  end

#  def dddd(x)
#    3.134
#  end

  def method_missing(n, m)
    n
  end
end

pp Foo.new.top
}

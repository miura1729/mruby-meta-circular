def top
  [[:a, [1]], [:b, ["a"]], [:c, [1, 2, 3]]].map do |m, a|
    send(m, a)
  end
end

def a(x)
  x
end

def b(x)
  x
end

def d(x)
  3.134
end

def method_missing(n, *m)
  m
end


MTypeInf::inference_main {
  top
}

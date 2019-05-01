def top
  [:a, :b, :c, :e].map do |m|
    send(m, 1)
  end
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

def method_missing(n, *m)
  m
end


MTypeInf::inference_main {
  top
}

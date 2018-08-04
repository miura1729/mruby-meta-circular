def foo(x)
  x
end

def bar
x = 42
while rand < 0.5
  if x.kind_of?(Array)
    if x[0].kind_of?(Array)
      x = 42
    end
  end
  $bar = x
  foo(x)
  x = [x]
end
end

MTypeInf::inference_main {
bar
}


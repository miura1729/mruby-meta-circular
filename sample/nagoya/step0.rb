def foo(x)
  x.to_i
end

def bar(x)
  if !x.kind_of?(Symbol)
    x.to_i
  end
end

MTypeInf::inference_main {
  if rand < 0.5 then
    x = :a
  else
    x = 1
  end

  foo(x)
  bar(x)
}

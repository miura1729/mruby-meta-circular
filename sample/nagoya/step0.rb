def foo
  if rand < 0.5 then
    x = 1
  else
    x = :a
  end

  x.to_i
end

def bar
  if rand < 0.5 then
    x = :a
  else
    x = 1
  end

  if !x.kind_of?(Symbol)
    x.to_i
  end
end

MTypeInf::inference_main {
  foo
  bar
}

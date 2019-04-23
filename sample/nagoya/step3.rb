def foo
  if rand < 0.4 then
    1
  else
    [foo]
  end
end

MTypeInf::inference_main {
  foo
}


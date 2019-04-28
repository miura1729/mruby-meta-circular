def baz
  raise
end
def bar
  if rand < 0.5 then
    baz
  else
    1
  end
end

def foo
  begin
    bar
  rescue
    2
  end
end

MTypeInf::inference_main {
  foo
}


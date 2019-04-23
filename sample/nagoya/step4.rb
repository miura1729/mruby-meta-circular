def foo
  hash = {1 => "abc", 5 => 1.3, 2 => 2}
  keys = []
  vals = []
  hash.each do |key, val|
    keys.push key
    vals.push val
  end

  [keys, vals]
end

MTypeInf::inference_main {
  foo
}


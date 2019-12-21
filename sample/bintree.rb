def bottom_up_tree(item, depth)
  if depth > 0
    item_item = 2 * item
    depth -= 1
    [bottom_up_tree(item_item - 1, depth), item, bottom_up_tree(item_item,  depth)]
  else
    [nil, item, nil]
  end
end

def id(n)
  n
end

def foo
  a = bottom_up_tree(0, 20)
  b = a[0]
  if !b.nil? then
    b
  else
    [1]
  end
end

def bar
  a = bottom_up_tree(0, 20)
end

#MTypeInf::inference_main {
  foo
  bar
#}

# main#foo: (main, NilClass) -> Array|NilClass
# main#bottom_up_tree: (main, Fixnum, Fixnum, NilClass) -> Array
# main#bar: (main, NilClass) -> Fixnum


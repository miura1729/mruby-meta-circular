MTypeInf::inference_main {
def foo
  a = [[1]]
  b = a
  a[0][0] = "str"
  b
end

foo
}


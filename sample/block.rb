# main#b: (main) -> Fixnum|Float
def b
  a = [1, 2, 1.0]
  a[c]
end

# main#c: () -> Fixnum
def c
  b = fib(5)
  if b == 10
    1
  else
    3
  end
end

# main#d: () -> Float
def d
  a = [1, 2, 1.0]
  a[2]
end

# main#e: () -> Fixnum
def e
  a = [1, 2, 1.0]
  a[0]
end

# main#a: (main, NilClass) -> Fixnum|Float
def a
  b = fib(5)
  if b == 10
    1
  else
    3.0
  end
end

# main#fib: (main, Fixnum, NilClass) -> Fixnum
def fib(n)
  if n == 1 then
    10
  elsif n == 0 then
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end

# main#foo: (main, Proc) -> Fixnum
# main#foo: (main, Proc) -> Float
def foo(&b)
  b.call
end

def f
  a = nil
  if fib(3) != 30 then
    a = Float
  else
    a = Array
  end
  a.new
end

# main#bar: () -> Fixnum
def bar
  a = 1
  foo {
    a
  }
end

# main#baz: () -> Float
def baz
  bar
  a = 1.1
  foo {
    a + 1.5
  }
end

def top
  a
  b
  d
  e
  f
  fib(5)
  baz
end

MTypeInf::inference_main {
  top
}



class Foo
  def initialize(x, y, z)
    "#{x} #{y} #{z}"
  end
end

def fib(i)
  if i < 2 then
    1
  else
    fib(i - 1) + fib(i - 2)
  end
end

def fact(i)
 if i == 1 then
   1
 else
   fact(i - 1) * i
 end
end

p fib(34)
i = 1100
#while i > 0
#  Foo.new(1, 3, 2)
#  i= i - 1
#  p bar(i)
#  p fact(20)
#end


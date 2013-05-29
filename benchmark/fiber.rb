100.times { |i|
  a = []
  f1 = Fiber.new{
    [1,2,3].each{|x| Fiber.yield(x)}
  }
  f2 = Fiber.new{
    [9,8,7].each{|x| Fiber.yield(x)}
  }
  3.times {
    a << f1.resume
    a << f2.resume
  }
}

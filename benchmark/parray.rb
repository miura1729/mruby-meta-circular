a = PArray::PVector4.new(1.9, 2.0, 3.9, 4.9)
b = PArray::PVector4.new(3.0, 2.0, 23.0, 4.0)
100.times do 
  p a + b
  p a - b
end


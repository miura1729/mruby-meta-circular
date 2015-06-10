a = PArray::PVector4[1.9, 2.1, 3.9, 4.9]
b = PArray::PVector4[3.2, 2.2, 23.2, 4.1]
100.times do 
  c =  a + b
  p c
  c.move
  c = a - b
  p c
  c.move
end


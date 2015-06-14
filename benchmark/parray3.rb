a = PArray::PVector4[1.9, 2.1, 3.9, 4.9]
b = PArray::PVector4[3.2, 2.2, 23.2, 4.1]
1000000.times do |i|
  c = a + b
  d = c + b
  e = d + b
  f = e + b
  c.move
  d.move
  e.move
  f.move
# p i
end


class Array
  def add(other)
    Array[
    self[0] + other[0],
    self[1] + other[1],
    self[2] + other[2],
    self[3] + other[3]]
  end

  def sub(other)
    Array[
    self[0] - other[0],
    self[1] - other[1],
    self[2] - other[2],
    self[3] - other[3]]
  end
end

a = Array[1.9, 2.1, 3.9, 4.9]
b = Array[3.2, 2.2, 23.2, 4.1]

1000000.times do 
  c =  a.add(b)
  c =  a.add(b)
end

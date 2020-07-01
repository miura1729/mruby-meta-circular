MTypeInf::inference_main {
class Array
  def each2(&block)
    idx = 0
    while idx < length
      $aaa = self
      block.call(self[idx])
      idx += 1
    end
    self
  end
end

class PQueue
  def initialize
    @array = []
  end

  def len
    @array.size
  end

  def heappush(val)
    @array.push val
    i = @array.size
    while i >= 2
      i2 = i / 2
      if @array[i2 - 1][0] > @array[i - 1][0] then
        tmp = @array[i2 - 1]
        @array[i2 - 1] = @array[i - 1]
        @array[i - 1] = tmp
      end
      i = i2
    end
  end

  def heappop
    if @array.size < 2 then
      return @array.pop
    end

    res = @array[0]
    @array[0] = @array.pop
    i = 1
    max = @array.size / 2
    while i <= max
      i2 = i * 2
      if @array[i2] and @array[i2 - 1][0] > @array[i2][0] then
        i2 = i2 + 1
      end
      if @array[i - 1][0]  > @array[i2 - 1][0] then
        tmp = @array[i2 - 1]
        @array[i2 - 1] = @array[i - 1]
        @array[i - 1] = tmp
      end
      i = i2
    end

    res
  end
end

if nil
q = PQueue.new
q.heappush([2, 1])
q.heappush([4, 1])
q.heappush([3, 1])
p q.heappop
q.heappush([6, 1])
p q.heappop
q.heappush([4, 1])
q.heappush([1, 1])
p q.heappop
p q.heappop
q.heappush([1, 1])
p q.heappop
p q.heappop
exit
end

class G
  def initialize
    @id2idx = {}
    @idx2id = [0]
    @idx = 1
    @edge = [[]]
  end

  attr :id2idx
  attr :idx2id
  attr_accessor :idx
  attr :edge
end

DISTANCE_MULTIPLE = 100

def get_idx(id, g)
  i = g.id2idx[id]
  if i.nil? then
    j = g.idx
    g.id2idx[id] = j
    g.idx2id.append(id)
    g.edge.append([])
    g.idx = g.idx + 1
    j
  else
    i
  end
end

def add_edge(start, ed, distance, g)
  s = get_idx(start, g)
  e = get_idx(ed, g)
  g.edge[s].append([e, distance])
end

# 123.45 -> 12345
# 123.4599 -> 12345
# 123 -> 12300
# 123.4 -> 12340
# .5 -> 50

def stof100(s)
  result = 0
  place = 0
  s.each_byte do |ch|
    if ch == '.'.ord then
      place = 1
      next
    end
    result *= 10
    result += ch - '0'.ord
    if  place > 0 then
      place += 1
      if place >= 3 then
        break
      end
    end
  end
  while place < 3
    result *= 10
    place += 1
  end
  return result
end

def load(g)
  STDIN.readline
  STDIN.readlines.each do |line|
    line = line.strip()
    data = line.split(",")
    # println(data)
    s = data[2].to_i
    e = data[3].to_i
    d = stof100(data[5])
    if true then
      print("line: #{line} s: #{s} e: #{e} D: #{d} \n")
    end
    add_edge(s, e, d, g)
  end
end

def dijkstra(start, ed, g)
  s = get_idx(start, g)
  e = get_idx(ed, g)

  size = g.idx
  d = []
  prev = []
  size.times do |i|
    d[i] = 0x7fffffff
    prev[i] = 0
  end

  queue = PQueue.new
  queue.heappush([0, s])

  visited = 0
  while queue.len > 0
    distance, here = queue.heappop
    if distance > d[here] then
      next
    end
    visited = visited + 1
    if true then #debug
      print("visiting: #{here} distance: #{distance} #{queue.len}\n")
    end

    g.edge[here].each do |arr|
      to = arr[0]
      weight = arr[1]
      w = distance + weight
      if w < d[to] then
        prev[to] = here
        d[to] = w
        queue.heappush([w, to])
      end
    end
  end
  print("visited:", visited)

  n = e
  result = [g.idx2id[n]]

  while d[n] != 0x7fffffff and n != s and n
    n = prev[n]
    result.append(g.idx2id[n])
  end

  return (d[e] / DISTANCE_MULTIPLE).to_i, result
end

def main()
  g = G.new
  count = 2

  load(g)
  print("loaded nodes:", g.idx)

  route = []
  (1..count+1).each do |i|
    s = g.idx2id[i*1000]
    res = dijkstra(s, g.idx2id[1], g)
    distance = res[0]
    route = res[1]
    print("distance:", distance)

    result = "route: "
#    route.each2 do |id|
#      result = result + id.to_s + " "
#      print(result)
#    end
    max = route.size
    i = 0
    while i < max
      result = result + route[i].to_s + " "
      print(result)
    end
  end
end

main()
}

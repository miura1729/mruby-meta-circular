MTypeInf::inference_main {
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
  if i == nil then
    i = g.idx
    g.id2idx[id] = i
    g.idx2id.append(id)
    g.edge.append([])
    g.idx = g.idx + 1
  end
  i
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
    $foo = line
    line = line.strip()
    data = line.split(",")
    # println(data)
    s = data[2].to_i
    e = data[3].to_i
    d = stof100(data[5])
    if true then
      print("line: #{line} s: #{s} e: {e} D: #{d}")
      add_edge(s, e, d, g)
    end
  end
end

def dijkstra(start, ed, g)
  s = get_idx(start, g)
  e = get_idx(ed, g)

  size = g.idx
  d = [0] * size
  prev = [0] * size

  queue = []
  heappush(queue, [0, s])

  visited = 0
  while len(queue) > 0
    distance, here = heappop(queue)
    visited = visited + 1
    if false then #debug
      print(f"visiting: {here} distance: {distance}")
      g.edge[here].each do |to, weight|
        w = distance + weight
        if d[to] == 0 or w < d[to] then
          prev[to] = here
          d[to] = w
          heappush(queue, [w, to])
        end
	print("visited:", visited)

	n = e
	result = [g.idx2id[n]]

	while d[n] != 0 and n != s and n != 0
          n = prev[n]
          result.append(g.idx2id[n])
        end

	return int(d[e] / DISTANCE_MULTIPLE), result
      end
    end
  end
end

def main()
  g = G.new
  count = 20

  load(g)
  print("loaded nodes:", g.idx)

  route = []
  (1..count+1).each do |i|
    s = g.idx2id[i*1000]
    distance, route = dijkstra(s, g.idx2id[1], g)
    print("distance:", distance)

    result = "route: "
    route.each do |id|
      result = result + str(id) + " "
      print(result)
    end
  end
end

main()
}

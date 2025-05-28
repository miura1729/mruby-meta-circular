# The Computer Language Shootout
# http://shootout.alioth.debian.org
#
# Optimized for Ruby by Jesse Millikan
# From version ported by Michael Neumann from the C gcc version,
# which was written by Christoph Bauer.

#SOLAR_MASS = 4 * Math::PI**2
SOLAR_MASS = 4 * Math::PI*Math::PI
DAYS_PER_YEAR = 365.24

##
# Range
#
# ISO 15.2.14
class Range

  ##
  # Calls the given block for each element of +self+
  # and pass the respective element.
  #
  # ISO 15.2.14.4.4
  def each(&block)
    return to_enum :each unless block

    val = self.first
    last = self.last

    if val.kind_of?(Fixnum) && last.kind_of?(Fixnum) # fixnums are special
      lim = last
      lim += 1 unless exclude_end?
      i = val
      while i < lim
        block.call(i)
        i += 1
      end
      return self
    end

    if val.kind_of?(String) && last.kind_of?(String) # fixnums are special
      if val.respond_to? :upto
        return val.upto(last, exclude_end?, &block)
      else
        str_each = true
      end
    end

    raise TypeError, "can't iterate" unless val.respond_to? :succ

    return self if (val <=> last) > 0

    while (val <=> last) < 0
      block.call(val)
      val = val.succ
      if str_each
        break if val.size > last.size
      end
    end

    block.call(val) if !exclude_end? && (val <=> last) == 0
    self
  end

  # redefine #hash 15.3.1.3.15
  def hash
    h = first.hash ^ last.hash
    h += 1 if self.exclude_end?
    h
  end
end

##
# Range is enumerable
#
# ISO 15.2.14.3
class Range
  include Enumerable
end

#def _puts *args
#end

MTypeInf::inference_main({:dump_level => 0}) {
class Planet
  attr_accessor :x, :y, :z, :vx, :vy, :vz, :mass

 def initialize(x, y, z, vx, vy, vz, mass)
  @x, @y, @z = x, y, z
  @vx, @vy, @vz = vx * DAYS_PER_YEAR, vy * DAYS_PER_YEAR, vz * DAYS_PER_YEAR
  @mass = mass * SOLAR_MASS
 end

 def move_from_i(bodies, nbodies, dt, i)
  while i < nbodies
   b2 = bodies[i]
   dx = @x - b2.x
   dy = @y - b2.y
   dz = @z - b2.z

   distance2 = dx * dx + dy * dy + dz * dz
   distance = Math.sqrt(distance2)
   mag = dt / (distance2 * distance2)
   b_mass_mag, b2_mass_mag = @mass * mag * distance, b2.mass * distance * mag

   @vx -= dx * b2_mass_mag
   @vy -= dy * b2_mass_mag
   @vz -= dz * b2_mass_mag
   b2.vx += dx * b_mass_mag
   b2.vy += dy * b_mass_mag
   b2.vz += dz * b_mass_mag
   i += 1
  end

  @x += dt * @vx
  @y += dt * @vy
  @z += dt * @vz
 end
end

def energy(bodies)
  e = 0.0
  nbodies = bodies.size

  for i in 0 ... nbodies
    b = bodies[i]
    e += 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz)
    rng = (i + 1) ... nbodies
    for j in rng
      b2 = bodies[j]
      dx = b.x - b2.x
      dy = b.y - b2.y
      dz = b.z - b2.z
      distance = Math.sqrt(dx * dx + dy * dy + dz * dz)
      e -= (b.mass * b2.mass) / distance
    end
  end
  e
end

def offset_momentum(bodies)
  px, py, pz = 0.0, 0.0, 0.0

  for b in bodies
    m = b.mass
    px += b.vx * m
    py += b.vy * m
    pz += b.vz * m
  end

  b = bodies[0]
  b.vx = - px / SOLAR_MASS
  b.vy = - py / SOLAR_MASS
  b.vz = - pz / SOLAR_MASS
end

def top
bodies = [
  # sun
  Planet.new(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),

  # jupiter
  Planet.new(
    4.84143144246472090e+00,
    -1.16032004402742839e+00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03,
    7.69901118419740425e-03,
    -6.90460016972063023e-05,
    9.54791938424326609e-04),

  # saturn
  Planet.new(
    8.34336671824457987e+00,
    4.12479856412430479e+00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03,
    4.99852801234917238e-03,
    2.30417297573763929e-05,
    2.85885980666130812e-04),

  # uranus
  Planet.new(
    1.28943695621391310e+01,
    -1.51111514016986312e+01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03,
    2.37847173959480950e-03,
    -2.96589568540237556e-05,
    4.36624404335156298e-05),

  # neptune
  Planet.new(
    1.53796971148509165e+01,
    -2.59193146099879641e+01,
    1.79258772950371181e-01,
    2.68067772490389322e-03,
    1.62824170038242295e-03,
    -9.51592254519715870e-05,
    5.15138902046611451e-05)
]

init = 50_000_000 # ARGV[0]
#n = Integer(init)
n = init

offset_momentum(bodies)

puts "%.9f" % energy(bodies)

nbodies = bodies.size
dt = 0.01

n.times do
  i = 0
  while i < nbodies
    b = bodies[i]
    b.move_from_i(bodies, nbodies, dt, i + 1)
    i += 1
  end
end

puts "%.9f" % energy(bodies)
end

 top
}


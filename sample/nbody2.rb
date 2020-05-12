# The Computer Language Shootout
# http://shootout.alioth.debian.org
#
# Optimized for Ruby by Jesse Millikan
# From version ported by Michael Neumann from the C gcc version,
# which was written by Christoph Bauer.

#SOLAR_MASS = 4 * Math::PI**2
SOLAR_MASS = 4 * Math::PI*Math::PI
DAYS_PER_YEAR = 365.24

#def _puts *args
#end

MTypeInf::inference_main({:dump_level => 0}) {
class Planet
  attr_accessor :po, :v, :mass

 def initialize(x, y, z, vx, vy, vz, mass)
  @po = [x, y, z]
  @v = [vx * DAYS_PER_YEAR, vy * DAYS_PER_YEAR, vz * DAYS_PER_YEAR]
  @mass = mass * SOLAR_MASS
 end

 def move_from_i(bodies, nbodies, dt, i)
  while i < nbodies
   b2 = bodies[i]
   dx = @po[0] - b2.po[0]
   dy = @po[1] - b2.po[1]
   dz = @po[2] - b2.po[2]

   distance = Math.sqrt(dx * dx + dy * dy + dz * dz)
   mag = dt / (distance * distance * distance)
   b_mass_mag, b2_mass_mag = @mass * mag, b2.mass * mag

   @v[0] -= dx * b2_mass_mag
   @v[1] -= dy * b2_mass_mag
   @v[2] -= dz * b2_mass_mag
   b2.v[0] += dx * b_mass_mag
   b2.v[1] += dy * b_mass_mag
   b2.v[2] += dz * b_mass_mag
   i += 1
  end

  @po[0] += dt * @v[0]
  @po[1] += dt * @v[1]
  @po[2] += dt * @v[2]
 end
end

def energy(bodies)
  e = 0.0
  nbodies = bodies.size

  for i in 0 ... nbodies
    b = bodies[i]
    e += 0.5 * b.mass * (b.v[0] * b.v[0] + b.v[1] * b.v[1] + b.v[2] * b.v[2])
    for j in (i + 1) ... nbodies
      b2 = bodies[j]
      dx = b.po[0] - b2.po[0]
      dy = b.po[1] - b2.po[1]
      dz = b.po[2] - b2.po[2]
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
    px += b.v[0] * m
    py += b.v[1] * m
    pz += b.v[2] * m
  end

  b = bodies[0]
  b.v[0] = - px / SOLAR_MASS
  b.v[1] = - py / SOLAR_MASS
  b.v[2] = - pz / SOLAR_MASS
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

a = bodies[1].po
#init = 200_000 # ARGV[0]
init = 50_000_000 # ARGV[0]
#n = Integer(init)
n = init

offset_momentum(bodies)

#puts "%.9f" % energy(bodies)
printf "%.9f\n", energy(bodies)

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

printf "%.9f\n", energy(bodies)
nil
#puts "%.9f" % energy(bodies)
end

 top
}

# AO render benchmark
# Original program (C) Syoyo Fujita in Javascript (and other languages)
#      http://lucille.atso-net.jp/blog/?p=642
#      http://lucille.atso-net.jp/blog/?p=711
# Ruby(yarv2llvm) version by Hideki Miura
# mruby version by Hideki Miura
#

IMAGE_WIDTH = 256
IMAGE_HEIGHT = 256
NSUBSAMPLES = 2
NAO_SAMPLES = 8

$rand = Random.new
$rand.srand(100)
module Rand
  # Use xorshift
  @@x = 123456789
  @@y = 362436069
  @@z = 521288629
  @@w = 88675123
  BNUM = 1 << 29
  BNUMF = BNUM.to_f
  def self.rand
    x = @@x
    t = x ^ ((x & 0xfffff) << 11)
    w = @@w
    @@x, @@y, @@z = @@y, @@z, w
    w = @@w = (w ^ (w >> 19) ^ (t ^ (t >> 8)))
    (w % BNUM) / BNUMF
  end
end

class Sphere
  def initialize(center, radius)
    @center = center
    @radius = radius
  end

  def center; @center; end
  def radius; @radius; end

  def intersect(ray, isect)
    rs = ray.org - @center
    b = rs.dot(ray.dir)
    c = rs.dot(rs) - (@radius * @radius)
    rs.move
    d = b * b - c
    if d > 0.0 then
      t = - b - Math.sqrt(d)

      if t > 0.0 and t < isect.t then
        isect.t = t
        isect.hit = true
        isect.pl.move
        isect.pl = PArray::PVector4[ray.org[0] + ray.dir[0] * t,
                          ray.org[1] + ray.dir[1] * t,
                          ray.org[2] + ray.dir[2] * t, 0.0]
        n = isect.pl - @center
        isect.n = n.normalize
      end
    end
  end
end

class Plane
  def initialize(p, n)
    @p = p
    @n = n
  end

  def intersect(ray, isect)
    d = -@p.dot(@n)
    v = ray.dir.dot(@n)
    v0 = v
    if v < 0.0 then
      v0 = -v
    end
    if v0 < 1.0e-17 then
      return
    end

    t = -(ray.org.dot(@n) + d) / v

    if t > 0.0 and t < isect.t then
      isect.hit = true
      isect.t = t
      isect.n = @n
      isect.pl.move
      isect.pl = PArray::PVector4[ray.org[0] + t * ray.dir[0],
                        ray.org[1] + t * ray.dir[1],
                         ray.org[2] + t * ray.dir[2], 0.0]
    end
  end
end

class Ray
  include  MMM

  def initialize(org, dir)
    @org = org
    @dir = dir
  end

  def org; @org; end
  def org=(v); @org = v; end
  def dir; @dir; end
  def dir=(v); @dir = v; end
end

class Isect
  include  MMM

  def initialize
    @t = 10000000.0
    @hit = false
    @pl = PArray::PVector4[0.0, 0.0, 0.0, 0.0]
    @n = PArray::PVector4[0.0, 0.0, 0.0, 0.0]
  end

  def t; @t; end
  def t=(v); @t = v; end
  def hit; @hit; end
  def hit=(v); @hit = v; end
  def pl; @pl; end
  def pl=(v); @pl = v; end
  def n; @n; end
  def n=(v); @n = v; end
end

def clamp(f)
  i = f * 255.5
  if i > 255.0 then
    i = 255.0
  end
  if i < 0.0 then
    i = 0.0
  end
  i.to_i
end

def otherBasis(basis, n)
  basis[2] = PArray::PVector4[n[0], n[1], n[2], 0.0]
  basis[1] = PArray::PVector4[0.0, 0.0, 0.0, 0.0]

  if n[0] < 0.6 and n[0] > -0.6 then
    basis[1][0] = 1.0
  elsif n[1] < 0.6 and n[1] > -0.6 then
    basis[1][1] = 1.0
  elsif n[2] < 0.6 and n[2] > -0.6 then
    basis[1][2] = 1.0
  else
    basis[1][0] = 1.0
  end

  basis[0] = basis[1].cross(basis[2])
  a = basis[0]
  basis[0] = basis[0].normalize
  a.move

  basis[1] = basis[2].cross(basis[0])
  a = basis[1]
  basis[1] = basis[1].normalize
  a.move
end

class Scene
  def initialize
    @spheres = Array.new
    @spheres[0] = Sphere.new(PArray::PVector4[-2.0, 0.0, -3.5, 0.0], 0.5)
    @spheres[1] = Sphere.new(PArray::PVector4[-0.5, 0.0, -3.0, 0.0], 0.5)
    @spheres[2] = Sphere.new(PArray::PVector4[1.0, 0.0, -2.2, 0.0], 0.5)
    @plane = Plane.new(PArray::PVector4[0.0, -0.5, 0.0, 0.0], PArray::PVector4[0.0, 1.0, 0.0, 0.0])
  end

  def ambient_occlusion(isect)
    basis = PArray::PVector4[0.0, 0.0, 0.0, 0.0]
    otherBasis(basis, isect.n)

    ntheta    = NAO_SAMPLES
    nphi      = NAO_SAMPLES
    eps       = 0.0001
    occlusion = 0.0

    p0 = PArray::PVector4[isect.pl[0] + eps * isect.n[0],
                isect.pl[1] + eps * isect.n[1],
                isect.pl[2] + eps * isect.n[2], 0.0]
    nphi.times do |j|
      ntheta.times do |i|
#        r = Rand::rand
        r = $rand.rand
#        phi = 2.0 * 3.14159265 * Rand::rand
        phi = 2.0 * 3.14159265 * $rand.rand
        x = Math.cos(phi) * Math.sqrt(1.0 - r)
        y = Math.sin(phi) * Math.sqrt(1.0 - r)
        z = Math.sqrt(r)

        rx = x * basis[0][0] + y * basis[1][0] + z * basis[2][0]
        ry = x * basis[0][1] + y * basis[1][1] + z * basis[2][1]
        rz = x * basis[0][2] + y * basis[1][2] + z * basis[2][2]

        raydir = PArray::PVector4[rx, ry, rz, 0.0]
        ray = Ray.new(p0, raydir)

        occisect = Isect.new
        @spheres[0].intersect(ray, occisect)
        @spheres[1].intersect(ray, occisect)
        @spheres[2].intersect(ray, occisect)
        @plane.intersect(ray, occisect)
        ray.move
        raydir.move
        if occisect.hit then
          occlusion = occlusion + 1.0
        else
          0.0
        end
        occisect.move
      end
    end

    occlusion = (ntheta.to_f * nphi.to_f - occlusion) / (ntheta.to_f * nphi.to_f)
    PArray::PVector4[occlusion, occlusion, occlusion, 0.0]
  end

  def render(w, h, nsubsamples)
    cnt = 0
    nsf = nsubsamples.to_f
    h.times do |y|
      w.times do |x|
        rad = PArray::PVector4[0.0, 0.0, 0.0, 0.0]

        # Subsmpling
        nsubsamples.times do |v|
          nsubsamples.times do |u|
            cnt = cnt + 1
            wf = w.to_f
            hf = h.to_f
            xf = x.to_f
            yf = y.to_f
            uf = u.to_f
            vf = v.to_f

            px = (xf + (uf / nsf) - (wf / 2.0)) / (wf / 2.0)
            py = -(yf + (vf / nsf) - (hf / 2.0)) / (hf / 2.0)

            eye = PArray::PVector4[px, py, -1.0, 0.0].normalize

            ray = Ray.new(PArray::PVector4[0.0, 0.0, 0.0, 0.0], eye)

            isect = Isect.new
            @spheres[0].intersect(ray, isect)
            @spheres[1].intersect(ray, isect)
            @spheres[2].intersect(ray, isect)
            @plane.intersect(ray, isect)
            ray.move

            if isect.hit then
              col = ambient_occlusion(isect)
              rad[0] = rad[0] + col[0]
              rad[1] = rad[1] + col[1]
              rad[2] = rad[2] + col[2]
              col.move
            else
              0.0
            end
          end
        end

        r = rad[0] / (nsf * nsf)
        g = rad[1] / (nsf * nsf)
        b = rad[2] / (nsf * nsf)
        printf("%c", clamp(r))
        printf("%c", clamp(g))
        printf("%c", clamp(b))
        rad.move
      end
    end
  end
end

# File.open("ao.ppm", "w") do |fp|
  printf("P6\n")
  printf("%d %d\n", IMAGE_WIDTH, IMAGE_HEIGHT)
  printf("255\n", IMAGE_WIDTH, IMAGE_HEIGHT)
  Scene.new.render(IMAGE_WIDTH, IMAGE_HEIGHT, NSUBSAMPLES)
#  Scene.new.render(256, 256, 2)
# end


MTypeInf::inference_main {
class Count
#  include MMC_EXT::LockPolicy::NoLock
#  include MMC_EXT::LockPolicy::LockFree

  def initialize
    @c = 1
  end

  def cnt=(v)
    @c = v
  end

  def cnt
#    $foo = self
    @c
  end
end

def foo
  i = 0
  c = Count.new
  r1 = [0, 0, nil]
  r2 = [0, 0, nil]

  th = MMC_EXT::Thread.new(c, r1) {|cnt, res|
    n = 0
    cnt.lock do |c|
      res.lock do |r|
        c.cnt = c.cnt + 1
      end
    end
  }
  th2 = MMC_EXT::Thread.new(c, r1) {|cnt, res|
    n = 0
    res.lock do |r|
      cnt.lock do |c|
        c.cnt = c.cnt + 1
      end
    end
  }

  th3 = MMC_EXT::Thread.new(c, r1) {|cnt, res|
    n = 0
    c.cnt = c.cnt + 1
  }

  th4 = MMC_EXT::Thread.new(c, r1) {|cnt, res|
    n = 0
    c.cnt = c.cnt + 1
  }

  th.join
  th2.join
  th3.join
  th4.join
end


  foo
}


MTypeInf::inference_main {
class Count
#  include MMC_EXT::LockPolicy::NoLock
#  include MMC_EXT::LockPolicy::LockFree

  def initialize
    @c = 0
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
        pp c.cnt
      end
    end
  }
  th2 = MMC_EXT::Thread.new(c, r1) {|cnt, res|
    n = 0
    cnt.lock do |c|
      res.lock do |r|
        c.cnt = c.cnt + 1
        pp c.cnt
      end
    end
  }

  th3 = MMC_EXT::Thread.new(c, r1) {|cnt, res|
    n = 0
    cnt.cnt = cnt.cnt + 1
    pp c.cnt
  }

  th4 = MMC_EXT::Thread.new(c, r1) {|cnt, res|
    n = 0
    aaa(cnt)
    pp c.cnt
 }

  th.join
  th2.join
  th3.join
  th4.join
end

def aaa(cnt)
  cnt.cnt = cnt.cnt + 1
end
  foo
}

MTypeInf::inference_main {
class Allocator
  class Header
    def initiaize
      @size = 0
      @next = mem::static_cast(0, Header)  # NULL pointer
    end
    attr_accessor :size
    attr_accessor :next
  end

  def initialize
    cpu = HAL::CPU.instance(0)
    mem = cpu.mem
    hsize = MMC::class_sizeof(Header)

    arena = mem.static_array_allocate(Fixnum, 65536)
    header = mem::static_cast(MMC::addressof(arena), Header)
    header.size = 0
    nheader = mem::static_cast(MMC::addressof(arena) + hsize, Header)
    nheader.size = 65536 - 1
    nheader.next = mem::static_cast(0, Header)
    header.next = nheader
    @root = header
  end

  def malloc(klass)
    cpu = HAL::CPU.instance(0)
    mem = cpu.mem
    raddr = _malloc(MMC::class_sizeof(klass))
    result = mem::static_cast(raddr, klass)
    result.initialize
    result
  end

  def _malloc(size)
    cpu = HAL::CPU.instance(0)
    mem = cpu.mem

    hsize = MMC::class_sizeof(Header)
    size = size + hsize

    cchunk = @root
    pchunk = cchunk
    while cchunk != mem::static_cast(0, Header) and cchunk.size < size
      pchunk = cchunk
      cchunk = cchunk.next
    end

#    if cchunk == mem::static_cast(0, Header) then
#    end

    header = mem::static_cast(cchunk, Header)
    nsize = cchunk.size - size
    if nsize > MMC::class_sizeof(Header) then
      header.size = size
      nchunk = mem::static_cast(MMC::addressof(cchunk) + size, Header)
      nchunk.size = nsize
      nchunk.next = cchunk.next
      pchunk.next = nchunk
    else
      pchunk.next = cchunk.next
    end

    MMC::addressof(cchunk) + hsize
  end

  def free(obj)
    _free(MMC::addressof(obj))
  end

  def _free(oaddr)
    cpu = HAL::CPU.instance(0)
    mem = cpu.mem
    hsize = MMC::class_sizeof(Header)
    haddr = oaddr - hsize

    cchunk = @root
    pchunk = cchunk
    header = mem::static_cast(haddr, Header)
    while cchunk != mem::static_cast(0, Header) and haddr < MMC::addressof(cchunk.next)
      pchunk = cchunk
      cchunk = cchunk.next
    end

    if MMC::addressof(pchunk) + pchunk.size == oaddr then
      pchunk.size += header.size

    elsif oaddr + header.size == MMC::addressof(cchunk) then
      header.size += cchunk.size
      header.next = cchunk.next
      pchunk.next = header

    else
      header.next = cchunk
      pchunk.next = header
    end

    nil
  end
end

class Foo
  def initialize
    @a = 1
    @b = 1
  end
end

class Bar
  def initialize
    @a = 1
  end
end

a = Allocator.new
b = a.malloc(Foo)
p MMC::addressof(b)
c = a.malloc(Bar)
p MMC::addressof(c)
d = a.malloc(Foo)
p MMC::addressof(d)
a.free(c)
a.free(b)
c = a.malloc(Bar)
p MMC::addressof(c)
d = a.malloc(Foo)
p MMC::addressof(d)
}

MTypeInf::inference_main {
class Allocator
  class Header
    def initiaize
      @size = 0
      @next = 0  # NULL pointer
    end
    attr_accessor :size
    attr_accessor :next
  end

  def initialize
    cpu = HAL::CPU.new(0)
    mem = cpu.mem

    arena = mem.static_array_allocate(Fixnum, 65536)
    header = mem::static_cast(MMC::addressof(arena), Header)
    header.size = 0
    nheader = mem::static_cast(MMC::addressof(arena) + MMC::class_sizeof(Header), Header)
    nheader.size = 65536 - 1
    nheader.next = mem::static_cast(0, Header)
    header.next = nheader
    @root = header
  end

  def malloc(klass)
    cpu = HAL::CPU.new(0)
    mem = cpu.mem

    size = MMC::class_sizeof(klass)
    cchunk = @root
    pchunk = cchunk
    while cchunk != mem::static_cast(0, Header) and cchunk.size < size
      pchunk = cchunk
      cchunk = cchunk.next
    end

    result = mem::static_cast(cchunk, klass)
    nsize = cchunk.size - size
    if nsize > MMC::class_sizeof(Header) then
      nchunk = mem::static_cast(MMC::addressof(cchunk) + size, Header)
      nchunk.size = nsize
      nchunk.next = cchunk.next
      pchunk.next = nchunk
    else
      pchunk.next = cchunk.next
    end
    result
  end
end

class Foo
  def initialize
    @a = 1
    @b = 1
  end
end

Foo.new
  a = Allocator.new
  a.malloc(Foo)
  a.malloc(Foo)
  a.malloc(Foo)
}

MTypeInf::inference_main {
  class Foo
    def nnn(n)
      if random < 0.5 then
        nnn([n, 1.1])
      else
        n
      end
    end
  end

  Foo.new.nnn(1)
}

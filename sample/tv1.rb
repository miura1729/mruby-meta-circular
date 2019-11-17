class Foo<TypeVariable
  include Enumerable
end

MTypeInf::inference_main {
  Foo.new.collect(&Foo.new)
  Foo.new.any?(&Foo.new)
}




  class Foo
    class Bar
      def initialize
      end
    end

   class Baz
     def initialize
       @foo = Bar.new
     end
   end

   def initialize
     @foo = Baz.new
   end

   def foo
     @foo
   end
  end

MTypeInf::inference_main {
   Foo.new
   Foo.new.foo
}

MTypeInf::inference_main {
class A
  %w[a b].each {|str| define_method(str) { 0 } }
end

p A.new.a + A.new.b
}

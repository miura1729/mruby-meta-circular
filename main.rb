$foo = nil
level = 0
op = ARGV[0]
ARGV.shift
while op[0] == '-'
  case op[1]
  when 'l'
    level = op[2].to_i
    op = ARGV[0]
    ARGV.shift
  else
    raise "#{$0} -l[dump_level] program"
  end
end

fn =  op
prog = IO::read(fn)
b = eval("$foo = lambda { #{prog} };$foo")
MTypeInf::inference_main({:dump_level => level}, &b)

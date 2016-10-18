#!env ruby

addrtab = Hash.new(0)
File.foreach('report') do |lin|
  if /^#/ =~ lin then
    next
  end

  addr = lin.split[4]
  addrtab[addr] += 1
end

File.foreach('dis') do |lin|
  if /^([0-9a-f]+):/ =~ lin then
    addr = $1
    printf "%-5d %s", addrtab[addr], lin
  else
  end
end


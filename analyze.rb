#!env ruby

addrtab = Hash.new(0)
symtab = {}
asymtab = {}
File.foreach('report') do |lin|
  if /^#/ =~ lin then
    next
  end

  sp = lin.split
  addr = sp[4]
  addrtab[addr] += 1
  sym = sp[5] + " " + sp[6]
  symtab[addr] = sym
end

addrtab.sort_by {|e| -e[1]}.each do |addr, cnt|
  printf("%-14s    %5d     %s\n", addr, cnt, symtab[addr])
end

File.foreach('sym') do |lin|
  addr = lin.split[0]
  sym = lin.split[1]
  asymtab[addr] = sym
end

File.foreach('dis') do |lin|
  if /^([0-9a-f]+):/ =~ lin then
    addr = $1
    if asymtab[addr] then
      print asymtab[addr], "\n"
    end
    printf "%-5d %s", addrtab[addr], lin
  else
  end
end


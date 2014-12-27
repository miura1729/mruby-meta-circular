include RiteOpcodeUtil
# Make irep
iseq = [
  mkop_ABx(Irep::OPTABLE_CODE[:STRING], 1, 0),
  mkop_ABC(Irep::OPTABLE_CODE[:SEND], 0, 0, 1),
  mkop_ABx(Irep::OPTABLE_CODE[:NOP], 0, 1),
  mkop_A(Irep::OPTABLE_CODE[:RETURN], 0)
]

irep = Irep.new_irep(iseq, ["Hello World", 0, 0], [:p], 2, 1)
p = irep.to_proc

i = 0
while i < 100
  p.call
  i = i + 1
end
p irep.reg_class(0)
p irep.reg_class(1)


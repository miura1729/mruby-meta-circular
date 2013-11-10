include RiteOpcodeUtil
# Make irep
iseq = [
  mkop_ABx(Irep::OPTABLE_CODE[:STRING], 1, 0),
  mkop_ABC(Irep::OPTABLE_CODE[:SEND], 0, 0, 1),
  mk_opcode(Irep::OPTABLE_CODE[:RETURN])
]

irep = Irep.new_irep(iseq, ["Hello World"], [:p], 2, 2)
irep.to_proc.call

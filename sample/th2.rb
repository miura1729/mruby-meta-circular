MTypeInf::inference_main {
  cpu = HAL::CPU.new(0)
  regs = cpu.regs
  mem = cpu.mem
#  $foo = regs
  regs[:rbx] = 1
  regs[:rax] = regs[:rbx]
  regs[:rax] = regs[:rax] + 4
  regs[:rax] = regs[:rax] + regs[:rbx]
}

def boot
  cpu = HAL::CPU.new(0)
  regs = cpu.regs
  mem = cpu.mem
  #  $foo = regs
  cpu.jmp(:skip)

  # BIOS Parametor
  cpu.byte 0
  cpu.byte 0
  cpu.byte 0
  cpu.byte 0
  cpu.long 0xcafebab + 123

  cpu.label(:skip)
  regs[:rsp] = 0x7c00
  regs[:rax] = regs[:rbx]
  regs[:rax] = regs[:rax] + 1
  regs[:rax] = regs[:rax] + regs[:rbx]
  nil
end

MTypeInf::inference_main {
    boot
}

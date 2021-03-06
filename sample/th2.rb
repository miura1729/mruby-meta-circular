MTypeInf::inference_main {
  MMC::attribute(:section, "BOOT")
  def boot
    cpu = HAL::CPU.instance(0)
    regs = cpu.regs
    mem = cpu.mem
    #  $foo = regs
    cpu.jmp("1f")

    # BIOS Parametor
    cpu.byte 0
    cpu.byte 0
    cpu.byte 0
    cpu.byte 0
    cpu.long 0

    cpu.label(1)
    regs[:sp] = 0x7c00
    regs[:ax] = regs[:bx]
    regs[:ax] = regs[:ax] + 1
    regs[:ax] = regs[:bx] + 1
    regs[:ax] = regs[:ax] + 2
    mem[regs[:ax]] = regs[:ax]
    #  mem[regs[:ax] + 4] = regs[:ax]
    #  regs[:ax] = mem[regs[:ax] + 4]
    #  regs[:ax] = mem[regs[:ax]]
    #  regs[:ax] = mem[regs[:ax]]
    nil
  end

  boot
}

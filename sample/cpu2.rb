MTypeInf::inference_main {
      ADDRESSING_MODES = {
        :ctl =>  [:imm,   :zpg, :imm, :abs, nil,    :zpg_x, nil,    :abs_x],
        :rmw =>  [:imm,   :zpg, :imm, :abs, nil,    :zpg_y, nil,    :abs_y],
        :alu => [:ind_x, :zpg, :imm, :abs, :ind_y, :zpg_x, :abs_y, :abs_x],
        :uno => [:ind_x, :zpg, :imm, :abs, :ind_y, :zpg_y, :abs_y, :abs_y],
      }

      def op(opcodes, args)
        #$foo = args
        opcodes.each do |opcode|
          if args.kind_of?(Array) && [:r_op, :w_op, :rw_op].include?(args[0])
            kind, op, mode = args
            mode = ADDRESSING_MODES[mode][opcode >> 2 & 7]
            send_args = [kind, op, mode]
            send_args << (mode.to_s.start_with?("zpg") ? :store_zpg : :store_mem) if kind != :r_op
            #$dispatch[opcode] = send_args
          else
            #$dispatch[opcode] = [*args]
          end
        end
      end

def foo
      # load instructions
      op([0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1],       [:r_op, :_lda, :alu])
      op([0xa2, 0xa6, 0xb6, 0xae, 0xbe],                         [:r_op, :_ldx, :rmw])
      op([0xa0, 0xa4, 0xb4, 0xac, 0xbc],                         [:r_op, :_ldy, :ctl])

      # store instructions
      op([0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91],             [:w_op, :_sta, :alu])
      op([0x86, 0x96, 0x8e],                                     [:w_op, :_stx, :rmw])
      op([0x84, 0x94, 0x8c],                                     [:w_op, :_sty, :ctl])

      # transfer instructions
      op([0xaa],                                                 :_tax)
      op([0xa8],                                                 :_tay)
      op([0x8a],                                                 :_txa)
      op([0x98],                                                 :_tya)

      # flow control instructions
      op([0x4c],                                                 :_jmp_a)
      op([0x6c],                                                 :_jmp_i)
      op([0x20],                                                 :_jsr)
      op([0x60],                                                 :_rts)
      op([0x40],                                                 :_rti)
      op([0xd0],                                                 :_bne)
      op([0xf0],                                                 :_beq)
      op([0x30],                                                 :_bmi)
      op([0x10],                                                 :_bpl)
      op([0xb0],                                                 :_bcs)
      op([0x90],                                                 :_bcc)
      op([0x70],                                                 :_bvs)
      op([0x50],                                                 :_bvc)

      # math operations
      op([0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71],       [:r_op, :_adc, :alu])
      op([0xe9, 0xeb, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1], [:r_op, :_sbc, :alu])

      # logical operations
      op([0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31],       [:r_op, :_and, :alu])
      op([0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11],       [:r_op, :_ora, :alu])
      op([0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51],       [:r_op, :_eor, :alu])
      op([0x24, 0x2c],                                           [:r_op, :_bit, :alu])
      op([0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1],       [:r_op, :_cmp, :alu])
      op([0xe0, 0xe4, 0xec],                                     [:r_op, :_cpx, :rmw])
      op([0xc0, 0xc4, 0xcc],                                     [:r_op, :_cpy, :rmw])

      # shift operations
      op([0x0a],                                                 [:a_op, :_asl])
      op([0x06, 0x16, 0x0e, 0x1e],                               [:rw_op, :_asl, :alu])
      op([0x4a],                                                 [:a_op, :_lsr])
      op([0x46, 0x56, 0x4e, 0x5e],                               [:rw_op, :_lsr, :alu])
      op([0x2a],                                                 [:a_op, :_rol])
      op([0x26, 0x36, 0x2e, 0x3e],                               [:rw_op, :_rol, :alu])
      op([0x6a],                                                 [:a_op, :_ror])
      op([0x66, 0x76, 0x6e, 0x7e],                               [:rw_op, :_ror, :alu])

      # increment and decrement operations
      op([0xc6, 0xd6, 0xce, 0xde],                               [:rw_op, :_dec, :alu])
      op([0xe6, 0xf6, 0xee, 0xfe],                               [:rw_op, :_inc, :alu])
      op([0xca],                                                 :_dex)
      op([0x88],                                                 :_dey)
      op([0xe8],                                                 :_inx)
      op([0xc8],                                                 :_iny)

      # flags instructions
      op([0x18],                                                 :_clc)
      op([0x38],                                                 :_sec)
      op([0xd8],                                                 :_cld)
      op([0xf8],                                                 :_sed)
      op([0x58],                                                 :_cli)
      op([0x78],                                                 :_sei)
      op([0xb8],                                                 :_clv)

      # stack operations
      op([0x48],                                                 :_pha)
      op([0x08],                                                 :_php)
      op([0x68],                                                 :_pla)
      op([0x28],                                                 :_plp)
      op([0xba],                                                 :_tsx)
      op([0x9a],                                                 :_txs)

      # undocumented instructions, rarely used
      op([0x0b, 0x2b],                                           [:r_op, :_anc, :uno])
      op([0x8b],                                                 [:r_op, :_ane, :uno])
      op([0x6b],                                                 [:r_op, :_arr, :uno])
      op([0x4b],                                                 [:r_op, :_asr, :uno])
      op([0xc7, 0xd7, 0xc3, 0xd3, 0xcf, 0xdf, 0xdb],             [:rw_op, :_dcp, :alu])
      op([0xe7, 0xf7, 0xef, 0xff, 0xfb, 0xe3, 0xf3],             [:rw_op, :_isb, :alu])
      op([0xbb],                                                 [:r_op, :_las, :uno])
      op([0xa7, 0xb7, 0xaf, 0xbf, 0xa3, 0xb3],                   [:r_op, :_lax, :uno])
      op([0xab],                                                 [:r_op, :_lxa, :uno])
      op([0x27, 0x37, 0x2f, 0x3f, 0x3b, 0x23, 0x33],             [:rw_op, :_rla, :alu])
      op([0x67, 0x77, 0x6f, 0x7f, 0x7b, 0x63, 0x73],             [:rw_op, :_rra, :alu])
      op([0x87, 0x97, 0x8f, 0x83],                               [:w_op, :_sax, :uno])
      op([0xcb],                                                 [:r_op, :_sbx, :uno])
      op([0x9f, 0x93],                                           [:w_op, :_sha, :uno])
      op([0x9b],                                                 [:w_op, :_shs, :uno])
      op([0x9e],                                                 [:w_op, :_shx, :rmw])
      op([0x9c],                                                 [:w_op, :_shy, :ctl])
      op([0x07, 0x17, 0x0f, 0x1f, 0x1b, 0x03, 0x13],             [:rw_op, :_slo, :alu])
      op([0x47, 0x57, 0x4f, 0x5f, 0x5b, 0x43, 0x53],             [:rw_op, :_sre, :alu])

      # nops
      op([0x1a, 0x3a, 0x5a, 0x7a, 0xda, 0xea, 0xfa],             [:no_op, :_nop, 0, 2])
      op([0x80, 0x82, 0x89, 0xc2, 0xe2],                         [:no_op, :_nop, 1, 2])
      op([0x04, 0x44, 0x64],                                     [:no_op, :_nop, 1, 3])
      op([0x14, 0x34, 0x54, 0x74, 0xd4, 0xf4],                   [:no_op, :_nop, 1, 4])
      op([0x0c],                                                 [:no_op, :_nop, 2, 4])
      op([0x1c, 0x3c, 0x5c, 0x7c, 0xdc, 0xfc],                   [:r_op, :_nop, :ctl])

      # interrupts
      op([0x00],                                                 :_brk)
      op([0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xb2, 0xd2, 0xf2], :_jam)

  #$dispatch[0]
end

  #    $dispatch = []

      foo
}

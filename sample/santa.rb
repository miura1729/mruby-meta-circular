def main
  tonakaimail = []
  tonakailist = []
  27.times do |i|
    tonakailist.push MMC_EXT::Thread.new(tonakaimail, i) {|mail, id|
      #leep((rand*10).to_i)
      pp "wake up #{id}"
      reply = []
      mail.push reply
      reply.pop
      pp "Tonakai Work done #{id}"
    }
  end

  # Santa tonakai dev.
  3.times do
    reps = []
    9.times do |i|
      a = tonakaimail.pop
      reps.push a
    end
    pp "HoHo Let's send present"

    reps.each do |ele|
      ele.push 1
      nil
    end
  end

  tonakailist.each do |th|
   th.join
  end
end


MTypeInf::inference_main {
  main
}

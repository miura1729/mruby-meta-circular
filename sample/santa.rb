def main
  tonakaimail = []
  tonakailist = []
  27.times do |i|
    tonakailist.push MMC_EXT::Thread.new(tonakaimail, i) {|mail, id|
#      sleep((rand*10).to_i)
#      pp "wake up #{id}"
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

    pp reps
    reps.each do |ele|
      ele.push 1
      pp "done"
      nil
    end
  end
end


MTypeInf::inference_main {
  main
}

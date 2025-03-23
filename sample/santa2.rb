module Enumerable
  def map(&block)
    return to_enum :collect unless block

    ary = []
    self.each{|*val| ary.push(block.call(*val))}
    ary
  end
end

def main
  # Santa hobbit dev.
  hobbit = MMC_EXT::Thread.new do
    hobbitmail = []
    hobbitlist = Array.new
    9.times do |i|
      hobbitlist.push MMC_EXT::Thread.new(hobbitmail, i) {|mail, id|
        sleep((rand*10).to_i)
        pp "hobbit wake up #{id}"
        reply = [id, []]
        mail.push reply
        reply[1].pop
        pp "Hobbit Work done #{id}"
      }
    end

    3.times do
      reps = []
      3.times do |i|
        a = hobbitmail.pop
        reps.push a
      end
#      pp "HoHo Let's make present"
      pp reps.map {|i, r|
        i
      }
      pp reps.map {|n| n[0]}

      reps.each do |ele|
        ele[1].push 1
        nil
      end
    end

    hobbitlist.each do |th|
      th.join
    end
  end

  # Santa tonakai dev.
  tonakai = MMC_EXT::Thread.new do
    tonakaimail = []
    tonakailist = []
    27.times do |i|
      tonakailist.push MMC_EXT::Thread.new(tonakaimail, i) {|mail, id|
        sleep((rand*10).to_i)
        pp "tonakai wake up #{id}"
        reply = []
        mail.push reply
        reply.pop
        pp "Tonakai Work done #{id}"
      }
    end

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

  hobbit.join
  tonakai.join
  pp "OK"
  nil
end


MTypeInf::inference_main {
  main
}

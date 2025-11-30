module Enumerable
  def map(&block)
    return to_enum :collect unless block

    ary = []
    self.each{|*val| ary.push(block.call(*val))}
    ary
  end
end

def main
  pp "START"
  # Santa hobbit dev.
  m = []
  hobbit = MMC_EXT::Thread.new(m) do |main|
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
      mess =  "HoHo Let's make present\n"
      reps.each {|i, r|
        mess = mess + "#{i}\n"
      }
#      pp reps.map {|n| n[0]}

      main.unshift mess

      reps.each do |ele|
        ele[1].push 1
        nil
      end
    end
    main.unshift nil

    hobbitlist.each do |th|
      th.join
    end
  end

  # Santa tonakai dev.
  tonakai = MMC_EXT::Thread.new(m) do |main|
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
      mess = "HoHo Let's send present\n"
      main.push mess

      reps.each do |ele|
        ele.push 1
        nil
      end
    end
    main.push nil

    tonakailist.each do |th|
      th.join
    end
  end

  cnt = 2
  sleep(1)
  while cnt > 0
    mess = m.pop
    pp "foo"
    if mess then
      pp mess
    else
      cnt = cnt - 1
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

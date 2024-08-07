MTypeInf::inference_main {
# Copyright (c) Alex Gaynor and individual contributors.
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#     1. Redistributions of source code must retain the above copyright notice,
#        this list of conditions and the following disclaimer.

#     2. Redistributions in binary form must reproduce the above copyright
#        notice, this list of conditions and the following disclaimer in the
#        documentation and/or other materials provided with the distribution.

#     3. Neither the name of topaz nor the names of its contributors may be used
#        to endorse or promote products derived from this software without
#        specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
class Array
#  include Inline
#  make_inline_method  :each

  def product(other)
    res = []
    each do |x|
      other.each do |y|
        if !x.nil? then
          if !y.nil? then
            res.push [x, y]
          end
        end
      end
    end

    res
  end

  def inject(args, &block)
    result = args
    e = self.size
    i = 0
    while i < e
      result = block.call(result, self[i])
      i = i + 1
    end
    result
  end
  alias reduce inject

  def map(&block)
    ary = []
    self.each{|val| ary.push(block.call(val))}
    ary
  end
  def map2(&block)
    ary = []
    self.each{|val| ary.push(block.call(val))}
    ary
  end
  #alias map collect

  def zip(*args, &block)
    args = args.map do |a|
      if a.respond_to?(:to_ary)
        a.to_ary.to_enum(:each)
      elsif a.respond_to?(:each)
        a.to_enum(:each)
      else
        raise TypeError # , "wrong argument type #{a.class} (must respond to :each)"
      end
    end

    result = block ? nil : []

    each do |*val|
      tmp = [val.__svalue]
      args.each do |arg|
        v = if arg.nil?
              nil
        else
          begin
            arg.next
          rescue StopIteration
            0
          end
        end
        tmp.push(v)
      end
      if block
        block.call(tmp)
      else
        result.push(tmp)
      end
    end
    result
  end
end

module Enumerator
  attr_accessor :obj, :meth, :args, :fib
  def initialize(obj=nil, meth=:each, *args, &block)
    if block
      obj = Generator.new(&block)
    else
      raise ArgumentError unless obj
    end
    if @obj and !self.respond_to?(meth)
      raise NoMethodError, "undefined method #{meth}"
    end

    @obj = obj
    @meth = meth
    @args = args.dup
    @fib = nil
    @dst = nil
    @lookahead = nil
    @feedvalue = nil
    @stop_exc = false
  end

  def each(*argv, &block)
    obj = self
    if 0 < argv.length
      obj = self.dup
      args = obj.args
      if !args.empty?
        args = args.dup
        args.concat argv
      else
        args = argv.dup
      end
      obj.args = args
    end
    return obj unless block
    enumerator_block_call(&block)
  end

  def enumerator_block_call(&block)
    @obj.__send__ @meth, *@args, &block
  end

  ##
  # call-seq:
  #   e.next   -> object
  #
  # Returns the next object in the enumerator, and move the internal position
  # forward.  When the position reached at the end, StopIteration is raised.
  #
  # === Example
  #
  #   a = [1,2,3]
  #   e = a.to_enum
  #   p e.next   #=> 1
  #   p e.next   #=> 2
  #   p e.next   #=> 3
  #   p e.next   #raises StopIteration
  #
  # Note that enumeration sequence by +next+ does not affect other non-external
  # enumeration methods, unless the underlying iteration methods itself has
  # side-effect
  #
  def next
    next_values[0]
  end

  def next_values
    if @lookahead
      vs = @lookahead
      @lookahead = nil
      return vs
    end
    raise @stop_exc if @stop_exc

    curr = Fiber.current

    if !@fib ||
        !@fib.alive?
      @dst = curr
      @fib = Fiber.new do
        result = each do |*args|
          feedvalue = nil
          Fiber.yield args
          if @feedvalue
            feedvalue = @feedvalue
            @feedvalue = nil
          end
          feedvalue
        end
        @stop_exc = StopIteration.new "iteration reached an end"
        @stop_exc.result = result
        Fiber.yield nil
      end
      @lookahead = nil
    end

    vs = @fib.resume curr
    if @stop_exc
      @fib = nil
      @dst = nil
      @lookahead = nil
      @feedvalue = nil
      raise @stop_exc
    end
    vs
  end
end

class DeterministicRandom
  def initialize
    @seed = 49734321
  end

  def rand
    Random.rand
  end
end

class Synapse
  attr_accessor :source_neuron, :dest_neuron
  attr_accessor :weight, :prev_weight

  def initialize(source_neuron, dest_neuron, prng)
    @source_neuron = source_neuron
    @dest_neuron = dest_neuron
    @prev_weight = @weight = prng.rand * 2 - 1
  end
end

class Neuron

  LEARNING_RATE = 1.0
  MOMENTUM = 0.3

  attr_accessor :synapses_in, :synapses_out
  attr_accessor :threshold, :prev_threshold, :error
  attr_accessor :output

  def initialize(prng)
    @prev_threshold = @threshold = prng.rand * 2 - 1
    @synapses_in = []
    @synapses_out = []
  end

  def calculate_output
    activation = synapses_in.inject(0.0) do |sum, synapse|
      sum + synapse.weight * synapse.source_neuron.output
    end
    activation -= threshold

    self.output = 1.0 / (1.0 + Math.exp(-activation))
  end

  def derivative
    output * (1 - output)
  end

  def output_train(rate, target)
    self.error = (target - output) * derivative
    update_weights(rate)
  end

  def hidden_train(rate)
    self.error = synapses_out.inject(0.0) do |sum, synapse|
      sum + synapse.prev_weight * synapse.dest_neuron.error
    end * derivative
    update_weights(rate)
  end

  def update_weights(rate)
    synapses_in.each do |synapse|
      temp_weight = synapse.weight
      synapse.weight += (rate * LEARNING_RATE * error * synapse.source_neuron.output) + (MOMENTUM * ( synapse.weight - synapse.prev_weight))
      synapse.prev_weight = temp_weight
    end
    temp_threshold = threshold
    self.threshold += (rate * LEARNING_RATE * error * -1) + (MOMENTUM * (threshold - prev_threshold))
    self.prev_threshold = temp_threshold
  end
end

class NeuralNetwork
  attr_accessor :prng

  def initialize(inputs, hidden, outputs)
    @prng = DeterministicRandom.new

    @input_layer = (1..inputs).map do
        Neuron.new(prng)
    end
    @hidden_layer = (1..hidden).map do
        Neuron.new(prng)
    end
    a = (1..outputs).map do
        Neuron.new(prng)
    end
    @output_layer= a

    @input_layer.product(@hidden_layer).each do |source, dest|
      synapse = Synapse.new(source, dest, prng)
      source.synapses_out << synapse
      dest.synapses_in << synapse
    end
    @hidden_layer.product(@output_layer).each do |source, dest|
      synapse = Synapse.new(source, dest, prng)
      source.synapses_out << synapse
      dest.synapses_in << synapse
    end
  end

  def train(inputs, targets)
    feed_forward(inputs)

    #$foo = @output_layer.zip(targets)[0]
    #$foo = targets
    @output_layer.zip(targets).each do |neuron, target|
      #$foo = neuron
      neuron.output_train(0.3, target)
    end
    @hidden_layer.each do |neuron|
        neuron.hidden_train(0.3)
    end
  end

  def feed_forward(inputs)
    @input_layer.zip(inputs).each do |neuron, input|
      neuron.output = input
    end
    @hidden_layer.each do|neuron|
        neuron.calculate_output
    end
    @output_layer.each do |neuron|
        neuron.calculate_output
    end
  end

  def current_outputs
    @output_layer.map do |neuron|
      neuron.output
    end
  end

end

def run(n)
  xor = NeuralNetwork.new(2, 10, 1)

  n.times do
    xor.train([0, 0], [0])
    xor.train([1, 0], [1])
    xor.train([0, 1], [1])
    xor.train([1, 1], [0])
  end

  checksum = 0.0

  n.times do
    xor.feed_forward([0, 0])
    checksum += xor.current_outputs[0]
    xor.feed_forward([0, 1])
    checksum += xor.current_outputs[0]
    xor.feed_forward([1, 0])
    checksum += xor.current_outputs[0]
    xor.feed_forward([1, 1])
    checksum += xor.current_outputs[0]
  end

  checksum
end

def harness_input
  10000
end

def harness_sample(input)
  run(input)
end

def harness_verify(output)
  output - 20018.21456752439 < 0.00001
end

def foo
  input = harness_input
  actual_output = harness_sample(input)
  p actual_output
end

foo
nil
}

require_relative 'machine'
machine = Machine.new

def un(&blk)
  -> x {
    raise ArgumentError unless x.is_a? Numeric
    blk.call(x)
  }
end

def bin(&blk)
  -> x, y {
    raise ArgumentError unless [x, y].all?(Numeric)
    blk.call(x, y)
  }
end

machine.op(:add, &bin { |x, y| x + y })
machine.op(:sub, &bin { |x, y| x - y })
machine.op(:mul, &bin { |x, y| x * y })
machine.op(:div, &bin { |x, y| x / y })
machine.op(:mod, &bin { |x, y| x % y })
machine.op(:abs, &un { |x| x.abs })
machine.op(:neg, &un { |x| -x })
machine.op(:zero?, &un { |x| x.zero? })
machine.op(:dup) { |x| [x, x] }
machine.op(:pop, &un { |x| puts x; [] })

machine[true]
machine[:dup]
machine[:add]
machine[:pop]
machine[:pop]
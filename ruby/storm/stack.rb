# frozen_string_literal: true

# Stack holds multiple sequential values, LIFO.
class Stack
  def initialize
    @inner = []
  end

  def all
    @inner
  end

  def push(value)
    @inner.push(value)
  end

  def peek(count = 1)
    @inner.last(count)
  end

  def pop(count = 1)
    @inner.pop(count)
  end

  def pop2
    [@inner.pop, @inner.pop]
  end

  def length
    @inner.length
  end
end

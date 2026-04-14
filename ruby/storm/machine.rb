# frozen_string_literal: true
require_relative 'stack'

class UnderflowError < StandardError; end

Op = Struct.new(:name, :arity, :action) do
  def run(stack)
    args = arity ? stack.pop(arity) : stack.pop(stack.length)
    result = arity == 0 ? action.call : action.call(*args)
    Array(result).each { |v| stack.push(v) } unless result.nil?
  end

  def self.auto(name, &block)
    new(name, block.arity == -1 ? nil : block.arity, block)
  end
end

class Machine
  attr_reader :stack

  def initialize
    @stack = Stack.new
    @ops = {}
  end

  def assign(op) = @ops[op.name] = op

  def op(name, &block) = assign(Op.auto(name, &block))

  def invoke(name)
    op = @ops[name] or return
    raise UnderflowError if op.arity && @stack.length < op.arity
    op.run(@stack)
  end

  def [](obj)
    return invoke(obj) if obj.is_a?(Symbol)
    @stack.push(obj)
  end
end
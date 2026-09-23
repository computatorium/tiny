#!/usr/bin/env ruby
# frozen_string_literal: true

# 05-04-26 - rbi - ruby info, all constants under Ruby::
# 09-22-26 - updated - added a more robust algorithm for getting the constants, and predicates for RUBY_ENGINE
module Rbi
  Ruby.constants.reject { |c| c == :Box }.to_h { |c| [c, Ruby.const_get(c)] }.each_pair { |k, v| const_set(k, v) }

  def mri? = RUBY_ENGINE == "ruby"

  def jruby? = RUBY_ENGINE == "jruby"

  def truffleruby? = RUBY_ENGINE == "truffleruby"

  def rubinius? = RUBY_ENGINE == "rbx"
end

Rbi.constants.each { |c| puts "#{c}: #{Rbi.const_get(c)}" } if __FILE__ == $PROGRAM_NAME

#!/usr/bin/env ruby
# frozen_string_literal: true

# 05-04-26 - rbi - ruby info, all constants under Ruby::
def rbi
  Ruby.constants.reject { |c| c == :Box }.to_h { |c| [c, Ruby.const_get(c)] }
end

rbi.each_pair { |k, v| puts "#{k}: #{v}" } if __FILE__ == $PROGRAM_NAME

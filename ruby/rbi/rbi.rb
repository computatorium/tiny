#!/usr/bin/env ruby
# 05-04-26 - rbi - ruby info, prints all constants under Ruby::
Ruby.constants.each { |c| puts "#{c.to_s} - #{Ruby.const_get(c)}" }
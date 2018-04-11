#!/bin/env ruby
# -*- coding: utf-8 -*-

# Get the input
text = ARGF.read

output = []
# Words I want to remove:
wordsToRemove = ["TODO", "DONE"]

# Lines I want to remove
linesToRemove= ["Note taken", "SCHEDULED", "CLOCK"]

# Change Org headings
replaceTable = {/^\*\*\* / => "    +", /^\*\*/ => "->"}

buf = ""

text.each_line do |line|
  buf = line if !linesToRemove.any?{|x| line.include? x}
  wordsToRemove.each do |w|
    buf = buf.gsub(w,'')
  end

  replaceTable.each do |key, value|
    buf = buf.gsub(key, value)
  end

  output << buf if !buf.nil?
end

puts "Output is:\n"
puts output.uniq!.join('')

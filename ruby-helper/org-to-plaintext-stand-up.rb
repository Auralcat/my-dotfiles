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
replaceTable = {/^\*\*\* / => "    +", /^\*\*/ => "\u{1f449}"}

buf = ""

text.each_line do |line|
  # Don't process the line if it has any of the excluded words
  buf = line if !linesToRemove.any?{|x| line.include? x}

  wordsToRemove.each do |w|
    buf = buf.gsub(w,'')
  end

  replaceTable.each do |key, value|
    buf = buf.gsub(key, value)
  end

  output << buf if !buf.nil?
end

puts output.uniq!.join('')

#!/usr/bin/env ruby

require 'find'

module Find
  def populate(*paths)
    matched = {}
    find(*paths) do |path|
      if File.file?(path) && yield(path)
        matched[File.basename(path, File.extname(path))] = path
      end
    end
    matched
  end

  module_function :populate
end

def select(path, term = '')
  books = Find.populate(path) do |p|
    p =~ /#{term}/i
  end.sort

  books.each_with_index do |book, i|
    puts "\e[32m %-4d \e[33m%s" % [ i, book.flatten.first ]
  end

  print "\e[31m Select:\e[0m "
  choice = STDIN.gets
  return if choice.nil?
  choice = choice.chomp.to_i

  exec 'open', books[choice].flatten.last
end

begin
  select '/data/books', ARGV.join(' ')
rescue Interrupt
  # Catch CTRL-C.
end

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

class Books
  def initialize(path, term = '')
    @books = find(path, term)
    if @books.empty?
      puts "No matches."
      return
    end
    select
  end

  def find(path, term)
    Find.populate(path) do |p|
      p =~ /#{term}/i
    end.sort
  end

  def select
    @books.each_with_index do |book, i|
      puts "\e[32m %-4d \e[33m%s" % [ i, book.flatten.first ]
    end

    print "\e[31m Select:\e[0m "
    begin
      choice = STDIN.gets
    rescue Interrupt
      # Catch CTRL-C.
    end
    return if choice.nil?
    choice = choice.chomp.to_i

    open @books[choice].flatten.last
  end

  def open(path)
    exec 'open', path
  end
end

Books.new('/data/books', ARGV.join(' '))

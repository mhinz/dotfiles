begin
  require 'pry'
  Pry.start
  exit
rescue LoadError
  require 'irb/completion'
  IRB.conf[:PROMPT_MODE] = :SIMPLE
  warn 'not installed: pry'
end

begin
  require "pry"
  Pry.start
  exit
rescue LoadError => e
  warn "Not installed: pry"
end

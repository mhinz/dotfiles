Pry.config.prompt = proc { |obj, lvl, _| "[#{lvl}] #{obj} ❯ " }

def line(obj, depth, indent=true)
  print indent ? obj.rjust(depth.to_i + obj.length) : obj
end

def ppp(obj, depth=0, indent=true)
  if obj.is_a? Array
    line "[\n", depth, indent
    obj.each { |v| ppp v, depth+2 }
    line "],\n", depth
  elsif obj.is_a? Hash
    line "{\n", depth, indent
    obj.each do |k,v|
      line "#{k} => ", depth+2
      ppp v, depth+2, false
    end
    line "},\n", depth
  else
    line "#{':' if obj.is_a? Symbol}#{obj},\n", depth, indent
  end
end

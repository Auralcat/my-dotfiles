#!/usr/bin/env ruby

# IRB config file.
require 'rubygems'

# Credits: https://gist.github.com/jimweirich/4950443
def edit(file, line)
  `emacsclient -n +#{line} #{file}`
end

def src(object, method)
  if object.respond_to?(method)
    meth = object.method(method)
  elsif object.is_a?(Class)
    meth = object.instance_method(method)
  end
  location = meth.source_location
  edit(*location) if location
  location
rescue NameError => ex
  nil
end

# Credits: https://github.com/r00k/dotfiles/blob/master/irbrc
class Object
  # list methods which aren't in superclass
  def local_methods(obj = self)
    (obj.methods - obj.class.superclass.instance_methods).sort
  end
end

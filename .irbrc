# IRB config file.

require 'rubygems'
require 'interactive_editor'
require 'method_source'

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

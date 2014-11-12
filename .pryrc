Pry.config.editor = "emacsclient"
begin
  require 'awesome_print'
  require 'tapp'
  # require 'hirb'
  # require 'hirb-unicode'
  require 'pry-debugger'
  require 'pry-doc'
  require 'pry-stack_explorer'
rescue LoadError => err
  puts "failed to require #{err} ... :("
end

Pry::Commands.block_command "refe", "run refe" do |*args|
  run ".refe-1_9_2 #{args.join(' ')}"
end

default_command_set = Pry::CommandSet.new do
  command "caller_method" do |depth|
    depth = depth.to_i || 1
    if /^(.+?):(\d+)(?::in `(.*)')?/ =~ caller(depth+1).first
      file   = Regexp.last_match[1]
      line   = Regexp.last_match[2].to_i
      method = Regexp.last_match[3]
      output.puts [file, line, method]
    end
  end
end

Pry.config.commands.import default_command_set
Pry.config.should_load_plugins = false

# if defined? Hirb
#   # Slightly dirty hack to fully support in-session Hirb.disable/enable toggling
#   Hirb::View.instance_eval do
#     def enable_output_method
#       @output_method = true
#       @old_print = Pry.config.print
#       Pry.config.print = proc do |output, value|
#         Hirb::View.view_or_page_output(value) || @old_print.call(output, value)
#       end
#     end
#
#     def disable_output_method
#       Pry.config.print = @old_print
#       @output_method = nil
#     end
#   end
#
#   Hirb.enable
# end

#
# Prompt
#
if defined?(Rails) && Rails.env
  require "rails/console/app"
  require "rails/console/helpers"
  TOPLEVEL_BINDING.eval("self").extend ::Rails::ConsoleMethods

  Pry.config.prompt = [
    proc { |target_self, nest_level, pry|
    nested = (nest_level.zero?) ? '' : ":#{nest_level}"
    "[#{pry.input_array.size}] #{Rails.version}@#{RUBY_VERSION}-p#{RUBY_PATCHLEVEL}(#{Pry.view_clip(target_self)})#{nested}> "
    },
    proc {|target_self, nest_level, pry|
    nested = (nest_level.zero?) ?  '' : ":#{nest_level}"
    "[#{pry.input_array.size}] #{Rails.version}@#{RUBY_VERSION}-p#{RUBY_PATCHLEVEL}(#{Pry.view_clip(target_self)})#{nested}* "
    }
  ]
else
  Pry.config.prompt = [
    proc { |target_self, nest_level, pry|
    nested = (nest_level.zero?) ? '' : ":#{nest_level}"
    "[#{pry.input_array.size}] #{RUBY_VERSION}-p#{RUBY_PATCHLEVEL}(#{Pry.view_clip(target_self)})#{nested}> "
    },
    proc {|target_self, nest_level, pry|
    nested = (nest_level.zero?) ?  '' : ":#{nest_level}"
    "[#{pry.input_array.size}] #{RUBY_VERSION}-p#{RUBY_PATCHLEVEL}(#{Pry.view_clip(target_self)})#{nested}* "
    }
  ]
end

#
# Aliases
#
if defined?(PryDebugger)
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
  Pry.commands.alias_command 'f', 'finish'
  Pry.commands.alias_command 'cat', 'show-method'
  Pry.commands.alias_command 'vi', 'edit-method'
  Pry.commands.alias_command 'vim', 'edit-method'
  Pry.commands.alias_command 'pwd', 'whereami'
end

Pry.config.exception_handler = proc do |output, exception, _|
  output.puts "\e[31m#{exception.class}: #{exception.message}"
  output.puts "from #{exception.backtrace.first}\e[0m"
end

#
# Toy methods
#
def time(repetitions = 100, &block)
    require 'benchmark'
    Benchmark.bm{|b| b.report{repetitions.times(&block)}}
end

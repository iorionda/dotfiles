# vim:filetype=ruby
#
VIM = '/Applications/MacVim.app/Contents/MacOS/Vim -f'
Pry.config.editor = proc { |file, line| "#{VIM} #{file} +#{line}" }

begin
  require 'awesome_print'
  require 'tapp'
  require 'hirb'
rescue LoadError => err
  puts 'failed to require.. :('
end

Pry::Commands.block_command "refe", "run refe" do |*args|
  run ".refe-1_9_2 #{args.join(' ')}"
end

if defined? Hirb
  # Slightly dirty hack to fully support in-session Hirb.disable/enable toggling
  Hirb::View.instance_eval do
    def enable_output_method
      @output_method = true
      @old_print = Pry.config.print
      Pry.config.print = proc do |output, value|
        Hirb::View.view_or_page_output(value) || @old_print.call(output, value)
      end
    end

    def disable_output_method
      Pry.config.print = @old_print
      @output_method = nil
    end
  end

  Hirb.enable
end
#
# Aliases
#
Pry.commands.alias_command 'c', 'continue'
Pry.commands.alias_command 's', 'step'
Pry.commands.alias_command 'n', 'next'
Pry.commands.alias_command 'f', 'finish'
Pry.commands.alias_command 'cat', 'show-method'
Pry.commands.alias_command 'vi', 'edit-method'
Pry.commands.alias_command 'vim', 'edit-method'
Pry.commands.alias_command 'pwd', 'whereami'

Pry.config.exception_handler = proc do |output, exception, _|
  output.puts "\e[31m#{exception.class}: #{exception.message}"
  output.puts "from #{exception.backtrace.first}\e[0m"
end

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
# Toy methods
#
def time(repetitions = 100, &block)
    require 'benchmark'
    Benchmark.bm{|b| b.report{repetitions.times(&block)}}
end


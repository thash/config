# clipboard

def pbcopy(str)
  IO.popen('pbcopy', 'r+') {|io| io.puts str }
  output.puts "-- Copy to clipboard --\n#{str}"
end

Pry.config.commands.command "hiscopy", "History copy to clipboard" do |n|
  pbcopy _pry_.input_array[n ? n.to_i : -1]
end

Pry.config.commands.command "copy", "Copy to clipboard" do |str|
  unless str
    str = "#{_pry_.input_array[-1]}#=> #{_pry_.last_result}\n"
  end
  pbcopy str
end

Pry.config.commands.command "lastcopy", "Last result copy to clipboard" do
  pbcopy _pry_.last_result.chomp
end

Pry.config.prompt = proc { "#{Time.now.strftime("%X")}:pry % " }

cs = Pry::CommandSet.new do
  import Pry::Commands
  command "lm" "Alias for ls -m" do |args|
    run "ls", "-m #{args}"
  end
  command "lM" "Alias for ls -M" do |args|
    run "ls", "-M #{args}"
  end
  command "pwd" "Alias for whereami" do
    run "whereami"
  end
end

#Pry::Hooks.new.add_hook(:after_session, :bye_lain) do
#  puts "close."
#end


### hirb ###
# begin
#   require 'hirb'
# rescue LoadError
#   begin
#     require 'bundler/setup'
#     require 'hirb'
#   rescue LoadError
#     # Missing goodies, bummer
#   end
# end
#
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

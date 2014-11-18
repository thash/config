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

module ComposableFunction
  def self.included(klass)
    klass.send(:alias_method, :<<, :compose)
  end

  def compose(g)
    lambda {|*args| self.to_proc.call(g.to_proc.call(*args))}
  end

  def >>(g)
    g << self
  end
end

[Proc, Method, Symbol].each do |klass|
  klass.send(:include, ComposableFunction)
end

Pry.config.editor = "emacsclient"

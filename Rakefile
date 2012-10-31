# -*- ruby -*-

require 'rubygems'
require 'hoe'

Hoe.add_include_dirs("lib",
                     "../../ruby_parser/dev/lib",
                     "../../sexp_processor/dev/lib")

Hoe.plugin :seattlerb
Hoe.plugin :isolate

Hoe.spec 'ruby2ruby' do
  developer 'Ryan Davis', 'ryand-ruby@zenspider.com'

  self.rubyforge_name = 'seattlerb'

  dependency "sexp_processor", "~> 4.0"
  dependency "ruby_parser",    "~> 3.0.0.a10"
end

def process ruby, file="stdin"
  require "ruby_parser"
  require "ruby2ruby"

  parser    = RubyParser.new
  ruby2ruby = Ruby2Ruby.new

  begin
    sexp = parser.process(ruby, file)

    ruby2ruby.process(sexp)
  rescue Interrupt => e
    raise e
  end
end

task :stress do
  $: << "lib"
  $: << "../../ruby_parser/dev/lib"
  require "pp"

  files = Dir["../../*/dev/**/*.rb"]

  warn "Stress testing against #{files.size} files"

  bad = {}

  files.each do |file|
    warn file

    begin
      process File.read(file), file
    rescue Interrupt => e
      raise e
    rescue Exception => e
      bad[file] = e
    end
  end

  pp bad
end

task :debug => :isolate do
  ENV["V"] ||= "18"

  $: << "lib"
  require 'ruby_parser'

  parser = if ENV["V"] == "18" then
             Ruby18Parser.new
           else
             Ruby19Parser.new
           end

  file = ENV["F"] || ENV["FILE"]

  ruby = if file then
           File.read(file)
         else
           file = "env"
           ENV["R"] || ENV["RUBY"]
         end

  puts process(ruby, file)
end

# vim: syntax=ruby

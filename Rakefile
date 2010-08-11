# -*- ruby -*-

require 'rubygems'
require 'hoe'

Hoe.add_include_dirs("lib",
                     "../../ParseTree/dev/test",
                     "../../ruby_parser/dev/lib",
                     "../../sexp_processor/dev/lib")

Hoe.plugin :seattlerb

Hoe.spec 'ruby2ruby' do
  developer 'Ryan Davis', 'ryand-ruby@zenspider.com'

  self.rubyforge_name = 'seattlerb'

  extra_deps     << ["sexp_processor", "~> 3.0"]
  extra_deps     << ["ruby_parser",    "~> 2.0"]
  extra_dev_deps << ["ParseTree",      "~> 3.0"]
end

task :stress do
  $: << "lib"
  $: << "../../ruby_parser/dev/lib"
  require "ruby_parser"
  require "ruby2ruby"

  files = Dir["../../*/dev/**/*.rb"]

  warn "Stress testing against #{files.size} files"
  parser    = RubyParser.new
  ruby2ruby = Ruby2Ruby.new

  files.each do |file|
    warn file
    ruby = File.read(file)

    sexp = parser.process(ruby, file)

    ruby2ruby.process(sexp)
  end
end

# vim: syntax=ruby

# -*- ruby -*-

require 'rubygems'
require 'hoe'

Hoe.add_include_dirs("lib",
                     "../../ZenTest/dev/lib",
                     "../../ParseTree/dev/test",
                     "../../ParseTree/dev/lib",
                     "../../RubyInline/dev/lib",
                     "../../ruby_parser/dev/lib",
                     "../../sexp_processor/dev/lib")

require './lib/ruby2ruby.rb'

Hoe.new('ruby2ruby', RubyToRuby::VERSION) do |r2r|
  r2r.rubyforge_name = 'seattlerb'
  r2r.developer('Ryan Davis', 'ryand-ruby@zenspider.com')

  r2r.clean_globs << File.expand_path("~/.ruby_inline")
  r2r.extra_deps << "ParseTree"

  r2r.multiruby_skip << "rubinius"
end

task :test => :clean

# vim: syntax=Ruby

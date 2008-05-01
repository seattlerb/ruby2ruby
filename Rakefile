# -*- ruby -*-

dirs = (%w(lib ../../ParseTree/dev/test) +
        %w(ParseTree RubyInline ruby_parser).map { |p| "../../#{p}/dev/lib" })
$:.push(*dirs)
ENV['RUBY_FLAGS'] = "-I" + dirs.join(":")

require 'rubygems'
require 'hoe'
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

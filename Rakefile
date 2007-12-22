# -*- ruby -*-

dirs = %w(../../ParseTree/dev/lib ../../ParseTree/dev/test:lib)
$:.push(*dirs)
ENV['RUBY_FLAGS'] = "-I" + dirs.join(":")

require 'rubygems'
require 'hoe'
require './lib/ruby2ruby.rb'

Hoe.new('ruby2ruby', RubyToRuby::VERSION) do |p|
  p.rubyforge_name = 'seattlerb'
  p.summary = 'ruby2ruby provides a means of generating pure ruby code easily from ParseTree\'s Sexps.'
  p.description = p.paragraphs_of('README.txt', 2).join
  p.url = p.paragraphs_of('README.txt', 0).first.split(/\n/)[1..-1].map {|u| u.strip }
  p.changes = p.paragraphs_of('History.txt', 0..4).join("\n\n")
  p.clean_globs << File.expand_path("~/.ruby_inline")
  p.extra_deps << "ParseTree"
end

# vim: syntax=Ruby

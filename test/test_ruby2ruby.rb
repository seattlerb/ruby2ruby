#!/usr/local/bin/ruby -w

require 'test/unit'
begin require 'rubygems'; rescue LoadError; end
require 'ruby2ruby'
require 'pt_testcase'

class TestRubyToRuby < Test::Unit::TestCase
  def setup
    @processor = RubyToRuby.new
  end

  def test_rewrite_resbody
    inn = [:resbody,
           [:array, [:const, :SyntaxError]],
           [:block, [:lasgn, :e1, [:gvar, :$!]], [:lit, 2]],
           [:resbody,
            [:array, [:const, :Exception]],
            [:block, [:lasgn, :e2, [:gvar, :$!]], [:lit, 3]]]]

    out = [:resbody,
           [:array, [:const, :SyntaxError], [:lasgn, :e1, [:gvar, :$!]]],
           [:block, [:lit, 2]],
           [:resbody,
            [:array, [:const, :Exception], [:lasgn, :e2, [:gvar, :$!]]],
            [:block, [:lit, 3]]]]

    assert_equal out, @processor.rewrite_resbody(inn)
  end

  def test_rewrite_resbody_lasgn
    inn = [:resbody,
           [:array, [:const, :SyntaxError]],
           [:lasgn, :e1, [:gvar, :$!]],
           [:resbody,
            [:array, [:const, :Exception]],
            [:block, [:lasgn, :e2, [:gvar, :$!]], [:lit, 3]]]]

    out = [:resbody,
           [:array, [:const, :SyntaxError], [:lasgn, :e1, [:gvar, :$!]]],
           nil,
           [:resbody,
            [:array, [:const, :Exception], [:lasgn, :e2, [:gvar, :$!]]],
            [:block, [:lit, 3]]]]

    assert_equal out, @processor.rewrite_resbody(inn)
  end

  eval ParseTreeTestCase.testcases.map { |node, data|
    "def test_#{node}
       pt = #{data['ParseTree'].inspect}
       rb = #{(data['Ruby2Ruby'] || data['Ruby']).inspect}

       assert_not_nil pt, \"ParseTree for #{node} undefined\"
       assert_not_nil rb, \"Ruby for #{node} undefined\"

       assert_equal rb, @processor.process(pt)
     end"
  }.join("\n")
end

# Self-Translation: 1st Generation
eval RubyToRuby.translate(RubyToRuby).sub("RubyToRuby", "RubyToRuby2")

class TestRubyToRuby2 < TestRubyToRuby
  def setup
    @processor = RubyToRuby2.new
  end
end

# Self-Translation: 2nd Generation - against the tests this time
eval RubyToRuby2.translate(TestRubyToRuby).sub("TestRubyToRuby", "TestRubyToRuby3")

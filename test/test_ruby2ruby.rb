#!/usr/local/bin/ruby -w

require 'test/unit'
begin require 'rubygems'; rescue LoadError; end
require 'ruby2ruby'
require 'pt_testcase'

class TestRubyToRuby < Test::Unit::TestCase
  def setup
    @processor = RubyToRuby.new
  end

  def test_rewrite_defn_define_method
    inn = s(:defn, :splatted,
            s(:bmethod,
              s(:masgn, s(:dasgn_curr, :args)),
              s(:block,
                s(:dasgn_curr, :y),
                s(:dasgn_curr, :y, s(:call, s(:dvar, :args), :first)),
                s(:call, s(:dvar, :y), :+, s(:array, s(:lit, 42))))))
    out = s(:defn, :splatted,
            s(:args, :"*args"),
            s(:scope,
              s(:block,
                s(:dasgn_curr, :y, s(:call, s(:lvar, :args), :first)),
                s(:call, s(:lvar, :y), :+, s(:array, s(:lit, 42))))))

    assert_equal out, @processor.rewrite_defn(inn)
  end

  def test_rewrite_defn_bmethod
    inn = s(:defn, :group,
            s(:fbody,
              s(:bmethod,
                s(:masgn, s(:dasgn_curr, :params)),
                s(:block,
                  s(:dasgn_curr, :force_reload, s(:dasgn_curr, :association, s(:dasgn_curr, :retval))),
                  s(:lit, 42)))))
    out = s(:defn, :group,
            s(:args, :"*params"),
            s(:scope,
              s(:block, s(:lit, 42))))

    assert_equal out, @processor.rewrite_defn(inn)
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
ruby = RubyToRuby.translate(RubyToRuby).sub("RubyToRuby", "RubyToRuby2")
begin
  eval ruby
rescue SyntaxError => e
  puts "SyntaxError: #{e.message}"
  puts
  puts ruby
  exit 1
end

class TestRubyToRuby2 < TestRubyToRuby
  def setup
    @processor = RubyToRuby2.new
  end
end

# Self-Translation: 2nd Generation - against the tests this time
eval RubyToRuby2.translate(TestRubyToRuby).sub("TestRubyToRuby", "TestRubyToRuby3")

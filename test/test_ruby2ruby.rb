#!/usr/local/bin/ruby -w

$TESTING = true

require 'test/unit'
begin require 'rubygems'; rescue LoadError; end
require 'ruby2ruby'
require 'pt_testcase'

# TODO: rename file so autotest stops bitching

class TestRubyToRuby < Test::Unit::TestCase
  def setup
    @processor = RubyToRuby.new
  end

  def test_lit_regexp_slash
    inn = s(:lit, /blah\/blah/)
    out = '/blah\/blah/'

    assert_equal out, @processor.process(inn)

    r = eval(out)
    assert_equal(/blah\/blah/, r)
  end

  def util_thingy(type)
    s(type,
      "blah",
      s(:call, s(:lit, 1), :+, s(:array, s(:lit, 1))),
      s(:str, 'blah"blah/blah'))
  end

  def test_dregx_slash
    inn = util_thingy(:dregx)
    out = '/blah#{(1 + 1)}blah"blah\/blah/'

    assert_equal out, @processor.process(inn)

    r = eval(out)
    assert_equal(/blah2blah"blah\/blah/, r)
  end

  def test_dstr_quote
    inn = util_thingy(:dstr)
    out = '"blah#{(1 + 1)}blah\"blah/blah"'

    assert_equal out, @processor.process(inn)

    r = eval(out)
    assert_equal "blah2blah\"blah/blah", r
  end

  def test_dsym_quote
    inn = util_thingy(:dsym)
    out = ':"blah#{(1 + 1)}blah\"blah/blah"'

    assert_equal out, @processor.process(inn)

    r = eval(out)
    assert_equal :"blah2blah\"blah/blah", r
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

# # Self-Translation: 1st Generation
# ruby = RubyToRuby.translate(RubyToRuby).sub("RubyToRuby", "RubyToRuby2")
# begin
#   eval ruby
# rescue SyntaxError => e
#   puts "SyntaxError: #{e.message}"
#   puts
#   puts ruby
#   exit 1
# end

# RubyToRuby2::LINE_LENGTH = RubyToRuby::LINE_LENGTH # HACK

# class TestRubyToRuby2 < TestRubyToRuby
#   def setup
#     @processor = RubyToRuby2.new
#   end
# end

# # Self-Translation: 2nd Generation - against the tests this time
# eval RubyToRuby2.translate(TestRubyToRuby).sub("TestRubyToRuby", "TestRubyToRuby3")

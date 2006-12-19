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


  ParseTreeTestCase.testcases.each do |node, data|
    define_method :"test_#{node}" do
      pt = data['ParseTree'].deep_clone
      rb = (data['Ruby2Ruby'] || data['Ruby']).deep_clone

      result = @processor.process(pt)

      assert_not_nil pt, "ParseTree for #{node} undefined"
      assert_not_nil rb, "Ruby for #{node} undefined"
      assert_equal rb, result
    end
  end

  def test_self_translation
    r2r2r = RubyToRuby.translate(RubyToRuby).sub("RubyToRuby","RubyToRubyToRuby")

    begin
      Object.class_eval r2r2r
    rescue SyntaxError => e
      $stderr.puts r2r2r
      flunk "syntax error, see above (#{e.inspect})"
    end

    r2r2r2 = RubyToRubyToRuby.translate(RubyToRuby).sub("RubyToRuby","RubyToRubyToRuby")
    r2r2r2r = RubyToRubyToRuby.translate(RubyToRubyToRuby)
    assert_equal(r2r2r, r2r2r2, "first generation must equal second generation")
    assert_equal(r2r2r, r2r2r2r, "first generation must equal third generation")
  end

  def hairy_method(z,x=10,y=20*z.abs,&blok)
    n = 1 + 2 * 40.0 / (z - 2)
    retried = false
    begin
      raise ArgumentError, n if retried
      n -= yield x,y,z,[z,x,y].map(&blok)
      n = n / 1.1 until n < 500 # TODO: translated isn't respecting post iter
      n = "hop hop #{"%.4f" % n}"
      raise n
    rescue RuntimeError => e
      raise if n != e.message
      n = lambda do |i|
        lambda do |j|
          "#{i} #{z+2*2} #{j.message.reverse}"
        end
      end[n].call(e)
      unless retried
        retried = true
        retry
      end
    rescue ArgumentError => e
      e.message
    rescue
    end
  ensure
    x << "ensure a-working"
  end

  def foobar a, &block; block.call(a) end
  def k; foobar [1,2,3].each { |x| x*2 } do |x| x*2 end end

  def test_block_precedence_escape
    eval RubyToRuby.translate(self.class, :k).sub(" k"," l")
    assert_equal(k, l)
  end

  def test_hairy_method
    src = RubyToRuby.translate(self.class, :hairy_method).sub(" h", " f")

    eval src

    blk = lambda{|x,y,z,arr|
      unless y
        x.to_i*2
      else
        x.to_i*y*z*arr.inject(1){|s,i| s+i}
      end
    }
    x1 = ""
    x2 = ""
    res = [hairy_method(-5,x1,&blk), fairy_method(-5,x2,&blk)]
    assert_equal(res.first, res.last)
    assert_equal(x1, x2)
    assert_equal("ensure a-working", x1)
  end
end

# TODO: pass your tests through yourself and run them again
# r2r2 = RubyToRuby.translate(RubyToRuby).sub("RubyToRuby","RubyToRuby2")
# begin
#   Object.class_eval r2r2
# rescue SyntaxError => e
#   $stderr.puts r2r2
#   flunk "syntax error, see above (#{e.inspect})"
# end
# class TestRubyToRubyToRuby < TestRubyToRuby
#   def setup
#     @processor = RubyToRuby2.new
#   end
# end

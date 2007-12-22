#!/usr/local/bin/ruby -w

$TESTING = true

$: << 'lib'

require 'test/unit'
require 'ruby2ruby'
require 'pt_testcase'

class TestRuby2Ruby < Test::Unit::TestCase
  def setup
    @processor = Ruby2Ruby.new
  end

  def test_proc_to_ruby
    block = proc { puts "something" }
    assert_equal %Q|proc {\n  puts("something")\n}|, block.to_ruby
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
      'blah"blah',
      s(:call, s(:lit, 1), :+, s(:array, s(:lit, 1))),
      s(:str, 'blah"blah/blah'))
  end

  def test_dregx_slash
    inn = util_thingy(:dregx)
    out = "/blah\\\"blah#\{(1 + 1)}blah\\\"blah\\/blah/"

    assert_equal out, @processor.process(inn)

    r = eval(out)
    assert_equal(/blah\"blah2blah\"blah\/blah/, r)
  end

  def test_dstr_quote
    inn = util_thingy(:dstr)
    out = "\"blah\\\"blah#\{(1 + 1)}blah\\\"blah/blah\""

    assert_equal out, @processor.process(inn)

    r = eval(out)
    assert_equal "blah\"blah2blah\"blah/blah", r
  end

  def test_dsym_quote
    inn = util_thingy(:dsym)
    out = ":\"blah\\\"blah#\{(1 + 1)}blah\\\"blah/blah\""

    assert_equal out, @processor.process(inn)

    r = eval(out)
    assert_equal :"blah\"blah2blah\"blah/blah", r
  end

  def test_proc_to_sexp
    p = proc { 1 + 1 }
    s = [:proc, nil, [:call, [:lit, 1], :+, [:array, [:lit, 1]]]]
    assert_equal s, p.to_sexp
  end

  def test_unbound_method_to_ruby
    r = "proc { ||\n  p = proc { (1 + 1) }\n  s = [:proc, nil, [:call, [:lit, 1], :+, [:array, [:lit, 1]]]]\n  assert_equal(s, p.to_sexp)\n}"
    m = self.class.instance_method(:test_proc_to_sexp)

    assert_equal r, m.to_ruby
  end

  eval ParseTreeTestCase.testcases.map { |node, data|
    next if node == "vcall" # HACK

    "def test_#{node}
       pt = #{data['ParseTree'].inspect}
       rb = #{(data['Ruby2Ruby'] || data['Ruby']).inspect}

       assert_not_nil pt, \"ParseTree for #{node} undefined\"
       assert_not_nil rb, \"Ruby for #{node} undefined\"

       assert_equal rb, @processor.process(pt)
     end"
  }.join("\n")
end

##
# Converts a +target+ using a +processor+ and converts +target+ name
# in the source adding +gen+ to allow easy renaming.

def morph_and_eval(processor, target, gen, n)
  begin
    old_name = target.name
    new_name = target.name.sub(/\d*$/, gen.to_s)
    ruby = processor.translate(target).sub(old_name, new_name)

    eval ruby
    target.constants.each do |constant|
      eval "#{new_name}::#{constant} = #{old_name}::#{constant}"
    end
  rescue SyntaxError => e
    warn "Self-Translation Generation #{n} failed:"
    warn "#{e.class}: #{e.message}"
    warn ""
    warn ruby
    warn ""
  rescue => e
    warn "Self-Translation Generation #{n} failed:"
    warn "#{e.class}: #{e.message}"
    warn ""
    warn ruby
    warn ""
  else
    begin
      yield if block_given?
    rescue
      # probably already handled
    end
  end
end

####################
#         impl
#         old  new
#
# t  old    0    1
# e
# s
# t  new    2    3

# Self-Translation: 1st Generation - morph Ruby2Ruby using Ruby2Ruby
morph_and_eval Ruby2Ruby, Ruby2Ruby, 2, 1 do
  class TestRuby2Ruby1 < TestRuby2Ruby
    def setup
      @processor = Ruby2Ruby2.new
    end
  end
end

# Self-Translation: 2nd Generation - morph TestRuby2Ruby using Ruby2Ruby
morph_and_eval Ruby2Ruby, TestRuby2Ruby, 2, 2 do
  # Self-Translation: 3rd Generation - test Ruby2Ruby2 with TestRuby2Ruby1
  class TestRuby2Ruby3 < TestRuby2Ruby2
    def setup
      @processor = Ruby2Ruby2.new
    end
  end
end

# Self-Translation: 4th (and final) Generation - fully circular
morph_and_eval(Ruby2Ruby2, Ruby2Ruby2, 3, 4) do
  class TestRuby2Ruby4 < TestRuby2Ruby3
    def setup
      @processor = Ruby2Ruby3.new
    end
  end
end rescue nil # for Ruby2Ruby2 at the top

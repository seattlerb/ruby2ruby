#!/usr/local/bin/ruby -w

$TESTING = true

$: << 'lib'

require 'minitest/autorun'
require 'ruby2ruby'
require 'pt_testcase'
require 'fileutils'
require 'tmpdir'
require 'ruby_parser' if ENV["CHECK_SEXPS"]

class R2RTestCase < ParseTreeTestCase
  def self.previous key
    "ParseTree"
  end

  def self.generate_test klass, node, data, input_name, output_name
    output_name = data.has_key?('Ruby2Ruby') ? 'Ruby2Ruby' : 'Ruby'

    return if node.to_s =~ /(str_question|not|bang).*_(19|20|21|22)$/

    klass.class_eval <<-EOM
      def test_#{node}
        pt = #{data[input_name].inspect}
        rb = #{data[output_name].inspect}

        refute_nil pt, \"ParseTree for #{node} undefined\"
        refute_nil rb, \"Ruby for #{node} undefined\"

        assert_equal rb, @processor.process(pt)
      end
    EOM
  end
end

start = __LINE__

class TestRuby2Ruby < R2RTestCase
  def setup
    super
    @check_sexp = ENV["CHECK_SEXPS"]
    @processor = Ruby2Ruby.new
  end

  def do_not_check_sexp!
    @check_sexp = false
  end

  def test_util_dthing_dregx
    inn = util_thingy(:dregx)
    inn.shift
    out = '/a"b#{(1 + 1)}c"d\/e/'
    exp = /a"b2c"d\/e/

    assert_equal exp, eval(out)

    assert_equal out[1..-2], @processor.util_dthing(:dregx, inn)
  end

  def test_util_dthing_dstr
    inn = util_thingy(:dstr)
    inn.shift
    out = '"a\"b#{(1 + 1)}c\"d/e"'
    exp = 'a"b2c"d/e'

    assert_equal exp, eval(out)

    assert_equal out[1..-2], @processor.util_dthing(:dstr, inn)
  end

  def test_util_dthing_dregx_bug?
    inn = s(:dregx, '[\/\"]', s(:evstr, s(:lit, 42)))
    inn.shift
    out = '/[\/\"]#{42}/'
    exp =  /[\/\"]42/

    assert_equal out[1..-2], @processor.util_dthing(:dregx, inn)
    assert_equal exp, eval(out)
  end

  def test_hash_parens_str
    inn = s(:hash, s(:lit, :k), s(:str, "banana"))
    out = '{ :k => "banana" }'
    util_compare inn, out
  end

  def test_hash_parens_lit
    inn = s(:hash, s(:lit, :k), s(:lit, 0.07))
    out = "{ :k => 0.07 }"
    util_compare inn, out
  end

  def test_hash_parens_bool
    inn = s(:hash, s(:lit, :k), s(:true))
    out = "{ :k => true }"
    util_compare inn, out
  end

  def test_hash_parens_nil
    inn = s(:hash, s(:lit, :k), s(:nil))
    out = "{ :k => nil }"
    util_compare inn, out
  end

  def test_hash_parens_lvar
    inn = s(:hash, s(:lit, :k), s(:lvar, :x))
    out = "{ :k => x }"
    util_compare inn, out
  end

  def test_hash_parens_call
    inn = s(:hash, s(:lit, :k), s(:call, nil, :foo, s(:lit, :bar)))
    out = "{ :k => foo(:bar) }"
    util_compare inn, out
  end

  def test_hash_parens_iter
    iter = s(:iter, s(:call, nil, :foo), 0, s(:str, "bar"))
    inn = s(:hash, s(:lit, :k), iter)
    out = '{ :k => (foo { "bar" }) }'
    util_compare inn, out
  end

  def test_and_alias
    inn = s(:and, s(:true), s(:alias, s(:lit, :a), s(:lit, :b)))
    out = "true and (alias :a :b)"
    util_compare inn, out
  end

  def test_attr_reader_diff
    inn = s(:defn, :same, s(:args), s(:ivar, :@diff))
    out = "def same\n  @diff\nend"
    util_compare inn, out
  end

  def test_attr_reader_same
    do_not_check_sexp!

    inn = s(:defn, :same, s(:args), s(:ivar, :@same))
    out = "attr_reader :same"
    util_compare inn, out
  end

  def test_attr_reader_double
    inn = s(:defn, :same, s(:args), s(:ivar, :@same), s(:ivar, :@diff))
    out = "def same\n  @same\n  @diff\nend"
    util_compare inn, out
  end

  def test_attr_reader_same_name_diff_body
    do_not_check_sexp!

    inn = s(:defn, :same, s(:args), s(:not, s(:ivar, :@same)))
    out = "def same\n  (not @same)\nend"
    util_compare inn, out
  end

  def test_attr_writer_diff
    inn = s(:defn, :same=, s(:args, :o), s(:iasgn, :@diff, s(:lvar, :o)))
    out = "def same=(o)\n  @diff = o\nend"
    util_compare inn, out
  end

  def test_attr_writer_double
    inn = s(:defn, :same=, s(:args, :o),
            s(:iasgn, :@same, s(:lvar, :o)), s(:iasgn, :@diff, s(:lvar, :o)))
    out = "def same=(o)\n  @same = o\n  @diff = o\nend"
    util_compare inn, out
  end

  def test_attr_writer_same_name_diff_body
    inn = s(:defn, :same=, s(:args, :o), s(:iasgn, :@same, s(:lit, 42)))
    out = "def same=(o)\n  @same = 42\nend"
    util_compare inn, out
  end

  def test_attr_writer_same
    do_not_check_sexp!

    inn = s(:defn, :same=, s(:args, :o), s(:iasgn, :@same , s(:lvar, :o)))
    out = "attr_writer :same"
    util_compare inn, out
  end

  def test_dregx_slash
    do_not_check_sexp!

    inn = util_thingy(:dregx)
    out = '/a"b#{(1 + 1)}c"d\/e/'
    util_compare inn, out, /a"b2c"d\/e/
  end

  def test_dstr_quote
    inn = util_thingy(:dstr)
    out = '"a\"b#{(1 + 1)}c\"d/e"'
    util_compare inn, out, 'a"b2c"d/e'
  end

  def test_dsym_quote
    inn = util_thingy(:dsym)
    out = ':"a\"b#{(1 + 1)}c\"d/e"'
    util_compare inn, out, :'a"b2c"d/e'
  end

  def test_lit_regexp_slash
    do_not_check_sexp! # dunno why on this one

    util_compare s(:lit, /blah\/blah/), '/blah\/blah/', /blah\/blah/
  end

  def test_call_kwsplat
    inn = s(:call, nil, :test_splat, s(:hash, s(:kwsplat, s(:call, nil, :testing))))
    out = "test_splat(**testing)"

    util_compare inn, out
  end

  def test_call_arg_assoc_kwsplat
    inn = s(:call, nil, :f,
           s(:lit, 1),
           s(:hash, s(:lit, :kw), s(:lit, 2), s(:kwsplat, s(:lit, 3))))
    out = "f(1, :kw => 2, **3)"

    util_compare inn, out
  end

  def test_call_kwsplat_x
    inn = s(:call, nil, :a, s(:hash, s(:kwsplat, s(:lit, 1))))
    out = "a(**1)"

    util_compare inn, out
  end

  def test_defn_kwargs
    inn = s(:defn, :initialize,
            s(:args, :arg, s(:kwarg, :keyword, s(:nil)), :"**args"),
            s(:nil))
    out = "def initialize(arg, keyword: nil, **args)\n  # do nothing\nend"

    util_compare inn, out
  end

  def test_defn_kwargs2
    inn = s(:defn, :initialize,
            s(:args, :arg,
              s(:kwarg, :kw1, s(:nil)),
              s(:kwarg, :kw2, s(:nil)),
              :"**args"),
            s(:nil))
    out = "def initialize(arg, kw1: nil, kw2: nil, **args)\n  # do nothing\nend"

    util_compare inn, out
  end

  def test_call_self_index
    util_compare s(:call, nil, :[], s(:lit, 42)), "self[42]"
  end

  def test_call_self_index_equals
    util_compare(s(:attrasgn, s(:self), :[]=, s(:lit, 42), s(:lit, 24)),
                 "self[42] = 24")
  end

  def test_call_self_index_equals_array
    util_compare(s(:attrasgn, s(:self), :[]=, s(:lit, 1), s(:lit, 2), s(:lit, 3)),
                 "self[1, 2] = 3")
  end

  def test_call_arglist_hash_first
    inn = s(:call, nil, :method,
            s(:hash, s(:lit, :a), s(:lit, 1)),
            s(:call, nil, :b))
    out = "method({ :a => 1 }, b)"

    util_compare inn, out
  end

  def test_call_arglist_hash_first_last
    inn = s(:call, nil, :method,
            s(:hash, s(:lit, :a), s(:lit, 1)),
            s(:call, nil, :b),
            s(:hash, s(:lit, :c), s(:lit, 1)))
    out = "method({ :a => 1 }, b, :c => 1)"

    util_compare inn, out
  end

  def test_call_arglist_hash_last
    inn = s(:call, nil, :method,
            s(:call, nil, :b),
            s(:hash, s(:lit, :a), s(:lit, 1)))
    out = "method(b, :a => 1)"

    util_compare inn, out
  end

  def test_call_arglist_if
    inn = s(:call,
            s(:call, nil, :a),
            :+,
            s(:if,
              s(:call, nil, :b),
              s(:call, nil, :c),
              s(:call, nil, :d)))

    out = "(a + (b ? (c) : (d)))"
    util_compare inn, out
  end

  def test_defn_kwsplat
    inn = s(:defn, :test, s(:args, :"**testing"), s(:nil))
    out = "def test(**testing)\n  # do nothing\nend"
    assert_parse inn, out
  end

  def test_defn_rescue_return
    inn = s(:defn, :blah, s(:args),
           s(:rescue,
             s(:lasgn, :a, s(:lit, 1)),
             s(:resbody, s(:array), s(:return, s(:str, "a")))))
    out = "def blah\n  a = 1\nrescue\n  return \"a\"\nend"

    assert_parse inn, out
  end

  def test_masgn_block_arg
    inn = s(:iter,
            s(:call,
              s(:nil),
              :x),
            s(:args, s(:masgn, :a, :b)),
            s(:dstr, "",
              s(:evstr, s(:lvar, :a)),
              s(:str, "="),
              s(:evstr, s(:lvar, :b))))
    out = 'nil.x { |(a, b)| "#{a}=#{b}" }'

    util_compare inn, out
  end

  def test_masgn_wtf
    inn = s(:block,
            s(:masgn,
              s(:array, s(:lasgn, :k), s(:lasgn, :v)),
              s(:splat,
                s(:call,
                  s(:call, nil, :line),
                  :split,
                  s(:lit, /\=/), s(:lit, 2)))),
            s(:attrasgn,
              s(:self),
              :[]=,
              s(:lvar, :k),
              s(:call, s(:lvar, :v), :strip)))

    out = "k, v = *line.split(/\\=/, 2)\nself[k] = v.strip\n"

    util_compare inn, out
  end

  def test_masgn_splat_wtf
    inn = s(:masgn,
            s(:array, s(:lasgn, :k), s(:lasgn, :v)),
            s(:splat,
              s(:call,
                s(:call, nil, :line),
                :split,
                s(:lit, /\=/), s(:lit, 2))))
    out = 'k, v = *line.split(/\\=/, 2)'
    util_compare inn, out
  end

  def test_match3_asgn
    inn = s(:match3, s(:lit, //), s(:lasgn, :y, s(:call, nil, :x)))
    out = "(y = x) =~ //"
    # "y = x =~ //", which matches on x and assigns to y (not what sexp says).
    util_compare inn, out
  end

  def test_safe_attrasgn
    inn = s(:safe_attrasgn,
            s(:call, nil, :x),
            :y=,
            s(:lit, 1))

    out = "x&.y = 1"
  end

  def test_splat_call
    inn = s(:call, nil, :x,
            s(:splat,
              s(:call,
                s(:call, nil, :line),
                :split,
                s(:lit, /\=/), s(:lit, 2))))

    out = 'x(*line.split(/\=/, 2))'
    util_compare inn, out
  end

  def test_resbody_block
    inn = s(:rescue,
            s(:call, nil, :x1),
            s(:resbody,
              s(:array),
              s(:call, nil, :x2),
              s(:call, nil, :x3)))

    out = "begin\n  x1\nrescue\n  x2\n  x3\nend"
    util_compare inn, out
  end

  def test_resbody_short_with_begin_end
    # "begin; blah; rescue; []; end"
    inn = s(:rescue,
            s(:call, nil, :blah),
            s(:resbody, s(:array), s(:array)))
    out = "blah rescue []"
    util_compare inn, out
  end

  def test_resbody_short_with_begin_end_multiple
    # "begin; blah; rescue; []; end"
    inn = s(:rescue,
            s(:call, nil, :blah),
            s(:resbody, s(:array),
              s(:call, nil, :log),
              s(:call, nil, :raise)))
    out = "begin\n  blah\nrescue\n  log\n  raise\nend"
    util_compare inn, out
  end

  def test_resbody_short_with_defn_multiple
    inn = s(:defn,
            :foo,
            s(:args),
            s(:rescue,
              s(:lasgn, :a, s(:lit, 1)),
              s(:resbody,
                s(:array),
                s(:call, nil, :log),
                s(:call, nil, :raise))))
    out = "def foo\n  a = 1\nrescue\n  log\n  raise\nend"
    util_compare inn, out
  end

  def test_regexp_options
    inn = s(:match3,
            s(:dregx,
              "abc",
              s(:evstr, s(:call, nil, :x)),
              s(:str, "def"),
              4),
            s(:str, "a"))
    out = '"a" =~ /abc#{x}def/m'
    util_compare inn, out
  end

  def test_resbody_short_with_rescue_args
    inn = s(:rescue,
            s(:call, nil, :blah),
            s(:resbody, s(:array, s(:const, :A), s(:const, :B)), s(:array)))
    out = "begin\n  blah\nrescue A, B\n  []\nend"
    util_compare inn, out
  end

  def test_call_binary_call_with_hash_arg
    # if 42
    #   args << {:key => 24}
    # end

    inn = s(:if, s(:lit, 42),
            s(:call, s(:call, nil, :args),
              :<<,
              s(:hash, s(:lit, :key), s(:lit, 24))),
            nil)

    out = "(args << { :key => 24 }) if 42"

    util_compare inn, out
  end

  def test_binary_operators
    # (1 > 2)
    Ruby2Ruby::BINARY.each do |op|
      inn = s(:call, s(:lit, 1), op, s(:lit, 2))
      out = "(1 #{op} 2)"
      util_compare inn, out
    end
  end

  def test_call_empty_hash
    inn = s(:call, nil, :foo, s(:hash))
    out = "foo({})"
    util_compare inn, out
  end

  def test_if_empty
    inn = s(:if, s(:call, nil, :x), nil, nil)
    out = "if x then\n  # do nothing\nend"
    util_compare inn, out
  end

  def test_interpolation_and_escapes
    # log_entry = "  \e[#{message_color}m#{message}\e[0m   "
    inn = s(:lasgn, :log_entry,
            s(:dstr, "  \e[",
              s(:evstr, s(:call, nil, :message_color)),
              s(:str, "m"),
              s(:evstr, s(:call, nil, :message)),
              s(:str, "\e[0m   ")))
    out = "log_entry = \"  \e[#\{message_color}m#\{message}\e[0m   \""

    util_compare inn, out
  end

  def test_class_comments
    inn = s(:class, :Z, nil)
    inn.comments = "# x\n# y\n"
    out = "# x\n# y\nclass Z\nend"
    util_compare inn, out
  end

  def test_module_comments
    inn = s(:module, :Z)
    inn.comments = "# x\n# y\n"
    out = "# x\n# y\nmodule Z\nend"
    util_compare inn, out
  end

  def test_method_comments
    inn = s(:defn, :z, s(:args), s(:nil))
    inn.comments = "# x\n# y\n"
    out = "# x\n# y\ndef z\n  # do nothing\nend"
    util_compare inn, out
  end

  def test_basic_ensure
    inn = s(:ensure, s(:lit, 1), s(:lit, 2))
    out = "begin\n  1\nensure\n  2\nend"
    util_compare inn, out
  end

  def test_nested_ensure
    inn = s(:ensure, s(:lit, 1), s(:ensure, s(:lit, 2), s(:lit, 3)))
    out = "begin\n  1\nensure\n  begin\n    2\n  ensure\n    3\n  end\nend"
    util_compare inn, out
  end

  def test_nested_rescue
    inn = s(:ensure, s(:lit, 1), s(:rescue, s(:lit, 2), s(:resbody, s(:array), s(:lit, 3))))
    out = "begin\n  1\nensure\n  2 rescue 3\nend"
    util_compare inn, out
  end

  def test_nested_rescue_exception
    inn = s(:ensure, s(:lit, 1), s(:rescue, s(:lit, 2), s(:resbody, s(:array, s(:const, :Exception)), s(:lit, 3))))
    out = "begin\n  1\nensure\n  begin\n    2\n  rescue Exception\n    3\n  end\nend"
    util_compare inn, out
  end

  def test_nested_rescue_exception2
    inn = s(:ensure, s(:rescue, s(:lit, 2), s(:resbody, s(:array, s(:const, :Exception)), s(:lit, 3))), s(:lit, 1))
    out = "begin\n  2\nrescue Exception\n  3\nensure\n  1\nend"
    util_compare inn, out
  end

  def test_rescue_block
    inn = s(:rescue,
            s(:call, nil, :alpha),
            s(:resbody, s(:array),
              s(:call, nil, :beta),
              s(:call, nil, :gamma)))
    out = "begin\n  alpha\nrescue\n  beta\n  gamma\nend"
    util_compare inn, out
  end

  def test_array_adds_parens_around_rescue
    inn = s(:array,
            s(:call, nil, :a),
            s(:rescue, s(:call, nil, :b), s(:resbody, s(:array), s(:call, nil, :c))))
    out = "[a, (b rescue c)]"

    util_compare inn, out
  end

  def test_call_arglist_rescue
    inn = s(:call,
            nil,
            :method,
            s(:rescue,
              s(:call, nil, :a),
              s(:resbody, s(:array), s(:call, nil, :b))))
    out = "method((a rescue b))"
    util_compare inn, out
  end

  def test_unless_vs_if_not
    rb1 = "a unless b"
    rb2 = "a if (not b)"
    rb3 = "a if ! b"

    util_compare Ruby18Parser.new.parse(rb1), rb1
    util_compare Ruby19Parser.new.parse(rb1), rb1

    util_compare Ruby18Parser.new.parse(rb2), rb1
    util_compare Ruby19Parser.new.parse(rb2), rb2

    util_compare Ruby18Parser.new.parse(rb3), rb1
    util_compare Ruby19Parser.new.parse(rb3), rb2
  end

  def assert_parse sexp, expected_ruby, expected_eval = nil
    assert_equal sexp, RubyParser.new.process(expected_ruby), "ruby -> sexp" if
      @check_sexp

    assert_equal expected_ruby, @processor.process(sexp), "sexp -> ruby"
    assert_equal expected_eval, eval(expected_ruby) if expected_eval
  end
  alias util_compare assert_parse

  def util_thingy(type)
    s(type,
      'a"b',
      s(:evstr, s(:call, s(:lit, 1), :+, s(:lit, 1))),
      s(:str, 'c"d/e'))
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

tr2r = File.read(__FILE__).split(/\n/)[start+1..__LINE__-2].join("\n")
ir2r = File.read("lib/ruby2ruby.rb")

require 'ruby_parser'

def silent_eval ruby
  old, $-w = $-w, nil
  eval ruby
  $-w = old
end

def morph_and_eval src, from, to, processor
  parser = RubyParser.for_current_ruby rescue RubyParser.new
  new_src = processor.new.process(parser.process(src.sub(from, to)))

  silent_eval new_src
  new_src
end

unless ENV["SIMPLE"] then
  ____ = morph_and_eval tr2r, /TestRuby2Ruby/, 'TestRuby2Ruby2', Ruby2Ruby
  ruby = morph_and_eval ir2r, /Ruby2Ruby/,     'Ruby2Ruby2',     Ruby2Ruby
  ____ = morph_and_eval ruby, /Ruby2Ruby2/,    'Ruby2Ruby3',     Ruby2Ruby2

  class TestRuby2Ruby1 < TestRuby2Ruby
    def setup
      super
      @processor = Ruby2Ruby2.new
    end
  end

  class TestRuby2Ruby3 < TestRuby2Ruby2
    def setup
      super
      @processor = Ruby2Ruby2.new
    end
  end

  class TestRuby2Ruby4 < TestRuby2Ruby2
    def setup
      super
      @processor = Ruby2Ruby3.new
    end
  end
end

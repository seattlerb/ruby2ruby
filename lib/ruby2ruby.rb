require 'parse_tree'
require 'environment'
require 'sexp_processor'

class Object
  def parse_tree(method_name=nil)
    if method_name
      m = method(method_name)
      klass = m.defined_in
      method_name = m.name
      return ParseTree.new.parse_tree_for_method(klass, method_name)
    elsif is_a?(Class)
      klass = self
    else
      klass = self.class
    end
    ParseTree.new.parse_tree(klass).first
  end

  def source(method_name=nil)
    RubyToRuby.new.process(parse_tree(method_name))
  end
end

class Method
  def defined_in
    full_name = to_s.split(" ").last.chop
    klass_name = full_name.split(/[\#\.]/).first
    if klass_name.include?("(")
      klass_name = klass_name.split("(").last.chop
    end
    klass = klass_name.split("::").inject(Object){|o,n| o.const_get(n)}
    klass
  end

  def name
    full_name = to_s.split(" ").last.chop
    full_name.split(/[\#\.]/).last
  end
end

class RubyToRuby < SexpProcessor
  def self.translate(klass, method=nil)
    unless method.nil? then
      self.new.process(ParseTree.new(false).parse_tree_for_method(klass, method))
    else
      self.new.process(ParseTree.new(false).parse_tree(klass).first) # huh? why is the :class node wrapped?
    end
  end

  def initialize
    super
    @env = Environment.new
    @indent = "  "
    self.auto_shift_type = true
    self.strict = true
    self.expected = String

    # TODO: reduce this
    self.unsupported.push(:alias, :alloca, :argscat, :argspush, :attrset, :back_ref, :bmethod, :break, :cdecl, :cfunc, :colon3, :cref, :cvar, :cvasgn, :cvdecl, :dasgn, :defined, :defs, :dmethod, :dregx, :dregx_once, :dsym, :dxstr, :evstr, :fbody, :flip2, :flip3, :gasgn, :hash, :ifunc, :last, :match, :match2, :match3, :memo, :method, :newline, :next, :nth_ref, :op_asgn1, :op_asgn2, :op_asgn_and, :op_asgn_or, :opt_n, :postexe, :redo, :sclass, :undef, :valias, :xstr)
  end
  
  ############################################################
  # Processors (rewriters at bottom)

  def process_and(exp)
    "(#{process exp.shift} and #{process exp.shift})"
  end
  
  def process_args(exp)
    args = []

    until exp.empty? do
      arg = exp.shift
      case arg
      when Symbol then
        args << arg
      when Array then
        case arg.first
        when :block then
          args[-(arg.size-1)..-1] = arg[1..-1].map{|a| process a}
        when :block_arg then
          args << "&#{arg.last}"
        else
          raise "unknown arg type #{arg.first.inspect}"
        end
      else
        raise "unknown arg type #{arg.inspect}"
      end
    end

    return "(#{args.join ', '})"
  end
  
  def process_arglist(exp) # custom made node
    args = []

    until exp.empty? do
      args << process(exp.shift)
    end

    return "(#{args.join ', '})"
  end
  
  def process_array(exp)
    "[#{arg_list(exp)}]"
  end

  def process_attrasgn(exp)
    process_call(exp)
  end

  def process_begin(exp)
    code = []
    code << process(exp.shift) until exp.empty?
    return code.join("\n")
  end

  def process_block(exp)
    code = []
    if true then
      until exp.empty? do
        found = exp.first.first == :block_arg rescue false
        if found then
          code[-1] = code[-1][0..-2] + ", #{process(exp.shift)})"
        else
          code << process(exp.shift)
        end
      end
    else
      # TODO REVIEW:
      catch_block_arg = false
      until exp.empty? do
        if catch_block_arg
          if exp.first == :block_arg
            code[-1] = code[-1][0..-2] + ", #{process(exp.shift)})"
          end
          catch_block_arg = false
        else
          if exp.first.first == :args
            catch_block_arg = true
          end
          if [:ensure, :rescue].include? exp.first.first 
            code << process(exp.shift)
          else
            code << indent(process(exp.shift))
          end
        end
      end
    end
    body = code.join("\n")
    body += "\n"

    return body
  end

  def process_block_arg(exp)
    "&#{exp.shift}"
  end
  
  def process_block_pass(exp)
    bname = process(exp.shift)
    fcall = process(exp.shift)
    if fcall[-1,1] == ')'
      "#{fcall[0..-2]}, &(#{bname}))"
    else
      "#{fcall}(&#{bname})"
    end
  end
  
  def process_call(exp)
    receiver = process exp.shift
    name = exp.shift
    args_exp = exp.shift
    if args_exp && args_exp.first == :array
      args = "#{process(args_exp)[1..-2]}"
    else
      args = process args_exp
    end

    case name
    when :<=>, :==, :<, :>, :<=, :>=, :-, :+, :*, :/, :% then #
      "(#{receiver} #{name} #{args})"
    when :[] then
      "#{receiver}[#{args}]"
    else
      unless receiver.nil? then
        "#{receiver}.#{name}#{args ? "(#{args})" : args}"
      else
        "#{name}#{args ? "(#{args})" : args}"
      end
    end
  end
  
  def process_case(exp)
    s = "case #{process exp.shift}\n"
    until exp.empty?
      pt = exp.shift
      if pt and pt.first == :when
        s << "#{process(pt)}\n"
      else
        s << "else\n#{indent(process(pt))}\n"
      end
    end
    s + "\nend"
  end
  
  def process_class(exp)
    s = "class #{exp.shift}"
    superk = exp.shift
    if superk == :Object
      s << "\n"
    else
      s << " < #{superk}\n"
    end
    body = ""
    body << "#{process exp.shift}\n\n" until exp.empty?
    s + indent(body) + "end"
  end
  
  def process_colon2(exp)
    "#{process(exp.shift)}::#{exp.shift}"
  end
  
  def process_const(exp)
    exp.shift.to_s
  end
  
  def process_dasgn_curr(exp)
    s = exp.shift.to_s 
    s += "=" + process(exp.shift) unless exp.empty?
    s
  end

  def process_defn(exp)
    if exp[1].first == :cfunc
      s = "# method '#{exp.shift}' defined in a C function"
      exp.shift
      return s
    else
      name = exp.shift
      args = process(exp.shift)
      body = indent(process(exp.shift))
      return "def #{name}#{args}\n#{body}end".gsub(/\n\s*\n+/, "\n")
    end
  end
  
  def process_dot2(exp)
    "(#{process exp.shift}..#{process exp.shift})"
  end

  def process_dot3(exp)
    "(#{process exp.shift}...#{process exp.shift})"
  end
  
  def process_dstr(exp)
    s = exp.shift.dump[0..-2]
    until exp.empty?
      pt = exp.shift
      if pt.first == :str
        s << process(pt)[1..-2]
      else
        s << '#{' + process(pt) + '}'
      end
    end
    s + '"'
  end

  def process_dvar(exp)
    exp.shift.to_s
  end

  def process_ensure(exp)
    body = process exp.shift
    ens  = process exp.shift
    return "#{body}\nensure\n#{indent ens}"
  end

  def process_false(exp)
    "false"
  end
  
  def process_fcall(exp)
    exp_orig = exp.deep_clone
    # [:fcall, :puts, [:array, [:str, "This is a weird loop"]]]
    name = exp.shift.to_s
    args = exp.shift
    code = []
    unless args.nil? then
      assert_type args, :array
      args.shift # :array
      until args.empty? do
        code << process(args.shift)
      end
    end
    return "#{name}(#{code.join(', ')})"
  end
  
  def process_for(exp)
    recv = process exp.shift
    iter = process exp.shift
    body = process exp.shift
    return "for #{iter} in #{recv}\n#{indent body}\nend\n"
  end
  
  def process_gvar(exp)
    return exp.shift.to_s
  end

  def process_iasgn(exp)
    "#{exp.shift} = #{process exp.shift}"
  end
  
  def cond_indent_process(pt)
    (pt and pt.first == :block) ? process(pt) : indent(process(pt))
  end
  
  def process_if(exp)
    s = ["if (#{process exp.shift})"]
    s << "#{cond_indent_process(exp.shift)}"
    s << "else\n#{cond_indent_process(exp.shift)}" until exp.empty?
    s << "end"
    s.join("\n")
  end
  
  def process_iter(exp)
    "#{process exp.shift} {|#{process exp.shift}|\n" +
    indent("#{process exp.shift}\n") +
    "}"
  end
  
  def process_ivar(exp)
    exp.shift.to_s
  end
  
  def process_lasgn(exp)
    s = "#{exp.shift}" 
    s += " = #{process exp.shift}" unless exp.empty?
    s
  end
  
  def process_lit(exp)
    obj = exp.shift
    if obj.is_a? Range # to get around how parsed ranges turn into lits and lose parens
      "(" + obj.inspect + ")"
    else
      obj.inspect
    end
  end
  
  def process_lvar(exp)
    exp.shift.to_s
  end
  
  def process_masgn(exp)
    lhs = exp.shift
    rhs = exp.shift

    assert_type lhs, :array
    assert_type rhs, :array

    lhs.shift
    rhs.shift

    lhs = lhs.map { |l| process(l) }
    rhs = rhs.map { |r| process(r) }

    return "#{lhs.join(", ")} = #{rhs.join(", ")}"
  end

  def process_module(exp)
    s = "module #{exp.shift}\n"
    body = ""
    body << "#{process exp.shift}\n\n" until exp.empty?
    s + indent(body) + "end"
  end
  
  def process_nil(exp)
    "nil"
  end
  
  def process_not(exp)
    "(not #{process exp.shift})"
  end
  
  def process_or(exp)
    "(#{process exp.shift} or #{process exp.shift})"
  end
  
  require 'pp'
  def process_resbody(exp)
    code = []
    until exp.empty?
      exceptions = exp.shift
      unless exceptions.nil? then
        case exceptions.first
        when :array then
          assert_type exceptions, :array
          exceptions.shift
          exceptions = exceptions.map { |o| process o }.join ", "
          code << "rescue #{exceptions}"
          code << indent(process(exp.shift))
        when :resbody then
          code << process(exceptions)
        else
          code << "rescue"
          code << indent(process(exceptions))
          # raise "unknown resbody #{exceptions.inspect}"
        end
      end
    end

    body = code.join("\n")
    body += "\n"

    return body
  end

  def process_rescue(exp)
    # TODO: proper formatting depends on knowing the context
    #
    # a = b rescue c            =>                [lasgn a [rescue b c]]
    # begin; a = b; rescue c    => [begin [rescue [lasgn a b] c]]
    body = process exp.shift
    resbody = process exp.shift
    els = process exp.shift
    code = "begin\n#{indent body}\n#{resbody}"
    code += "else\n#{indent els}" if els
    code += "end\n"
    return code
  end
  
  def process_retry(exp)
    "retry"
  end
  
  def process_return(exp)
    return "return #{process exp.shift}"
  end

  def process_scope(exp)
    return process(exp.shift)
  end

  def process_self(exp)
    "self"
  end

  def process_splat(exp)
    "*#{process(exp.shift)}"
  end

  def process_str(exp)
    return exp.shift.dump
  end

  def process_super(exp)
    "super(#{process(exp.shift)})"
  end
  
  def process_svalue(exp)
    arg_list(exp.shift.sexp_body)
  end
  
  def process_to_ary(exp)
    process(exp.shift)
  end

  def process_true(exp)
    "true"
  end

  def process_until(exp)
    cond_loop(exp, 'until')
  end
  
  def process_vcall(exp)
    return exp.shift.to_s
  end
  
  def process_when(exp)
    "when #{process(exp.shift).to_s[1..-2]}\n#{indent(process(exp.shift))}"
  end

  def process_while(exp)
    cond_loop(exp, 'while')
  end
  
  def process_yield(exp)
    body = process(exp.shift)[1..-2] unless exp.empty?
    "yield#{body and "(#{body})"}"
  end
  
  def process_zarray(exp)
    "[]"
  end

  def process_zsuper(exp)
    "super"
  end
  
  def cond_loop(exp, name)
    cond = process(exp.shift)
    body = cond_indent_process(exp.shift)
    head_controlled = exp.empty? ? false : exp.shift

    code = []
    if head_controlled then
      code << "#{name} (#{cond}) do"
      code << body
      code << "end"
    else
      code << "begin"
      code << body
      code << "end #{name} (#{cond})"
    end
    code.join("\n")
  end

  ############################################################  
  # Rewriters

  def rewrite_defn(exp)
    exp.shift # :defn
    name = exp.shift
    args = s(:args)
    body = Sexp.from_array exp.shift

    case body.first
    when :args then # already normalized
      args = body
      body = exp.shift
      assert_type args, :args
      assert_type body, :scope
      assert_type body[1], :block
    when :scope, :fbody then
      body = body[1] if body.first == :fbody
      args = body.last.delete_at 1
      assert_type args, :args
      assert_type body, :scope
      assert_type body[1], :block

      if body[1][1].first == :block_arg then
        block_arg = body[1].delete_at 1
        args << block_arg
      end
    when :bmethod then
      body.shift # :bmethod
      if body.first.first == :dasgn_curr then
        # WARN: there are some implications here of having an empty
        # :args below namely, "proc { || " does not allow extra args
        # passed in.
        dasgn = body.shift
        assert_type dasgn, :dasgn_curr
        dasgn.shift # type
        args.push(*dasgn)
        body.find_and_replace_all(:dvar, :lvar)
      end
      if body.first.first == :block then
        body = s(:scope, body.shift)
      else
        body = s(:scope, s(:block, body.shift)) # single statement
      end
    when :dmethod
      # BEFORE: [:defn, :dmethod_added, [:dmethod, :bmethod_maker, ...]]
      # AFTER:  [:defn, :dmethod_added, ...]
      body = body[2][1][2] # UGH! FIX
      args = body[1]
      body.delete_at 1
      body = s(:scope, body)
    when :ivar then
      body = s(:scope, s(:block, s(:return, body)))
    when :attrset then
      argname = body.last
      args << :arg
      body = s(:scope, s(:block, s(:return, s(:iasgn, argname, s(:lvar, :arg)))))
    else
      raise "Unknown :defn format: #{name.inspect} #{args.inspect} #{body.inspect}"
    end
    
    return s(:defn, name, args, body)
  end

  ############################################################
  # Utility Methods:

  def arg_list(exp, delim = ", ")
    code = []
    while !exp.empty?
      code << process(exp.shift)
    end
    code.join delim
  end

  def indent(s)
    s.to_s.map{|line| @indent + line}.join
  end
end

if __FILE__ == $0 then
  require 'test/unit'

  class R2RTest < Test::Unit::TestCase
    def test_self_translation
      r2r2r = RubyToRuby.translate(RubyToRuby).sub("RubyToRuby","RubyToRubyToRuby")
      Object.class_eval r2r2r

      r2r2r2 = RubyToRubyToRuby.translate(RubyToRuby).sub("RubyToRuby","RubyToRubyToRuby")
      r2r2r2r = RubyToRubyToRuby.translate(RubyToRubyToRuby)
      assert_equal(r2r2r,r2r2r2)
      assert_equal(r2r2r,r2r2r2r)
    end
    
    def hairy_method(z,x=10,y=20*z.abs,&blok)
      n = 1 + 2 * 40.0 / (z - 2)
      retried = false
      begin
        raise ArgumentError, n if retried
        n -= yield x,y,z,[z,x,y].map(&blok)
        n /= 1.1 until n < 500 # TODO: translated isn't respecting post iter
        n = "hop hop #{"%.4f" % n}"
        raise n
      rescue RuntimeError => e
        raise if n != e.message
        n = lambda{|i| 
          lambda{|j| "#{i} #{z+2*2} #{j.message.reverse}"
          }
        }[n].call(e)
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
end

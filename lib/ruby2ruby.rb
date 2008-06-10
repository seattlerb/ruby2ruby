#!/usr/bin/env ruby -w

begin require 'rubygems'; rescue LoadError; end
require 'sexp_processor'
require 'unified_ruby'

class Ruby2Ruby < SexpProcessor
  include UnifiedRuby

  VERSION = '1.1.9'
  LINE_LENGTH = 78

  ##
  # Nodes that represent assignment and probably need () around them.

  ASSIGN_NODES = [
                  :dasgn,
                  :flip2,
                  :flip3,
                  :lasgn,
                  :masgn,
                  :op_asgn1,
                  :op_asgn2,
                  :op_asgn_and,
                  :op_asgn_or,
                  :return,
                 ]

  def self.translate(klass_or_str, method = nil)
    require 'parse_tree'
    self.new.process(ParseTree.translate(klass_or_str, method))
  end

  def initialize
    super
    @indent = "  "
    self.auto_shift_type = true
    self.strict = true
    self.expected = String

    # self.debug[:defn] = /zsuper/
  end

  def process exp
    exp = Sexp.from_array(exp) if Array === exp unless Sexp === exp
    super exp
  end

  ############################################################
  # Processors

  def process_alias(exp)
    "alias_method #{process(exp.shift)}, #{process(exp.shift)}"
  end

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
          asgns = {}
          arg[1..-1].each do |lasgn|
            asgns[lasgn[1]] = process(lasgn)
          end

          args.each_with_index do |name, index|
            args[index] = asgns[name] if asgns.has_key? name
          end
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
    code = []
    until exp.empty? do
      code << process(exp.shift)
    end
    code.join ', '
  end

  def process_argscat(exp)
    args = []

    ary = exp.shift
    ary.shift # :array
    until ary.empty? do
      args << process(ary.shift)
    end

    args << "*#{process(exp.shift)}"
    args << process(exp.shift) unless exp.empty? # optional block arg

    args.join ', '
  end

  def process_argspush(exp)
    process_arglist(exp)
  end

  def process_array(exp)
    "[#{process_arglist(exp)}]"
  end

  def process_attrasgn(exp)
    receiver = process exp.shift
    name = exp.shift
    args = exp.empty? ? nil : exp.shift

    case name
    when :[]= then
      rhs = process args.pop
      args[0] = :arglist if args[0] == :array
      "#{receiver}[#{process(args)}] = #{rhs}"
    else
      if args then
        "#{receiver}.#{name.to_s[0..-2]} = #{process(args)[1..-2]}"
      else
        "#{receiver}.#{name.to_s[0..-2]}"
      end
    end
  end

  def process_back_ref(exp)
    "$#{exp.shift}"
  end

  def process_begin(exp)
    is_rescue = exp.first.first == :rescue rescue false
    code = []
    code << "begin"
    until exp.empty?
      src = process(exp.shift)
      src = indent(src) unless src =~ /(^|\n)rescue/ # ensures no level 0 rescues
      code << src
    end
    code << "end" unless is_rescue
    return code.join("\n")
  end

  def process_block(exp)
    result = []

    exp << nil if exp.empty?
    until exp.empty? do
      code = exp.shift
      if code.nil? or code.first == :nil then
        result << "# do nothing"
      else
        result << process(code)
      end
    end

    result = result.join "\n"

    result = case self.context[1]
             when nil, :scope, :if, :iter, :resbody, :when, :while then
               result + "\n"
             else
               "(#{result})"
             end

    return result
  end

  def process_block_arg(exp)
    "&#{exp.shift}"
  end

  def process_block_pass(exp)
    bname = s(:block_arg, process(exp.shift)) # FIX
    call = exp.shift

    if Array === call.last then # HACK - I _really_ need rewrites to happen first
      case call.last.first
      when :splat then
        call << [:array, call.pop]
      when :array then
        # do nothing
      else
        has_args = Array === call.last and call.last.first == :array
        call << [:array] unless has_args
      end
      call.last << bname
    else
      call << [:array, bname]
    end

    process(call)
  end

  def process_break(exp)
    val = exp.empty? ? nil : process(exp.shift)
    # HACK "break" + (val ? " #{val}" : "")
    if val then
      "break #{val}"
    else
      "break"
    end
  end

  def process_call(exp)
    receiver_node_type = exp.first.nil? ? nil : exp.first.first
    receiver = process exp.shift

    receiver = "(#{receiver})" if
      Ruby2Ruby::ASSIGN_NODES.include? receiver_node_type

    name = exp.shift
    args_exp = exp.shift rescue nil
    if args_exp && args_exp.first == :array # FIX
      args = "#{process(args_exp)[1..-2]}"
    else
      args = process args_exp
      args = nil if args.empty?
    end

    case name
    when :<=>, :==, :<, :>, :<=, :>=, :-, :+, :*, :/, :%, :<<, :>>, :** then
      "(#{receiver} #{name} #{args})"
    when :[] then
      "#{receiver}[#{args}]"
    when :"-@" then
      "-#{receiver}"
    when :"+@" then
      "+#{receiver}"
    else
      unless receiver.nil? then
        "#{receiver}.#{name}#{args ? "(#{args})" : args}"
      else
        "#{name}#{args ? "(#{args})" : args}"
      end
    end
  end

  def process_case(exp)
    result = []
    expr = process exp.shift
    if expr then
      result << "case #{expr}"
    else
      result << "case"
    end
    until exp.empty?
      pt = exp.shift
      if pt and pt.first == :when
        result << "#{process(pt)}"
      else
        code = indent(process(pt))
        code = indent("# do nothing") if code =~ /^\s*$/
        result << "else\n#{code}"
      end
    end
    result << "end"
    result.join("\n")
  end

  def process_cdecl(exp)
    lhs = exp.shift
    lhs = process lhs if Sexp === lhs
    rhs = process exp.shift
    "#{lhs} = #{rhs}"
  end

  def process_class(exp)
    "class #{util_module_or_class(exp, true)}"
  end

  def process_colon2(exp)
    "#{process(exp.shift)}::#{exp.shift}"
  end

  def process_colon3(exp)
    "::#{exp.shift}"
  end

  def process_const(exp)
    exp.shift.to_s
  end

  def process_cvar(exp)
    "#{exp.shift}"
  end

  def process_cvasgn(exp)
    "#{exp.shift} = #{process(exp.shift)}"
  end

  def process_cvdecl(exp)
    "#{exp.shift} = #{process(exp.shift)}"
  end

  # (a, lit1)      => "a = 1"
  # (a, (b, lit2)) => "a = b = 2"
  # (a, (b))       => ""

  def process_dasgn_curr(exp)
    lhs = exp.shift.to_s
    rhs = (exp.empty? ? nil : exp.shift)
    if rhs.nil? then
      if self.context[1] == :block then
        return ''
      end

      return lhs
    end
    return "#{lhs} = #{process rhs}" unless rhs.first == :dasgn_curr

    # keep recursing. ensure that the leaf node assigns to _something_
    "#{lhs} = #{process rhs}"
  end

  def process_dasgn(exp)
    if exp.size == 1 then
      exp.shift.to_s
    else
      "#{exp.shift} = #{process(exp.shift)}"
    end
  end

  def process_defined(exp)
    "defined? #{process(exp.shift)}"
  end

  def process_defn(exp)
    type1 = exp[1].first
    type2 = exp[2].first rescue nil

    if type1 == :args and [:ivar, :attrset].include? type2 then
      name = exp.shift
      case type2
      when :ivar then
        exp.clear
        return "attr_reader #{name.inspect}"
      when :attrset then
        exp.clear
        return "attr_writer :#{name.to_s[0..-2]}"
      else
        raise "Unknown defn type: #{exp.inspect}"
      end
    end

    case type1
    when :scope, :args then
      name = exp.shift
      args = process(exp.shift)
      args = "" if args == "()"
      body = indent(process(exp.shift))
      return "def #{name}#{args}\n#{body}\nend".gsub(/\n\s*\n+/, "\n")
    else
      raise "Unknown defn type: #{type1} for #{exp.inspect}"
    end
  end

  def process_defs(exp)
    exp.unshift "#{process(exp.shift)}.#{exp.shift}"
    process_defn(exp)
  end

  def process_dot2(exp)
    "(#{process exp.shift}..#{process exp.shift})"
  end

  def process_dot3(exp)
    "(#{process exp.shift}...#{process exp.shift})"
  end

  def process_dregx(exp)
    "/" << util_dthing(exp, true) << "/"
  end

  def process_dregx_once(exp)
    process_dregx(exp) + "o"
  end

  def process_dstr(exp)
    "\"#{util_dthing(exp)}\""
  end

  def process_dsym(exp)
    ":#{process_dstr(exp)}"
  end

  def process_dvar(exp)
    exp.shift.to_s
  end

  def process_dxstr(exp)
    "`#{process_dstr(exp)[1..-2]}`"
  end

  def process_ensure(exp)
    body = process exp.shift
    ens  = process exp.shift
    return "#{body}\nensure\n#{indent ens}"
  end

  def process_evstr(exp)
    process exp.shift
  end

  def process_false(exp)
    "false"
  end

  # TODO: remove for unified
  def process_fcall(exp)
    recv = exp.shift unless Symbol === exp.first # HACK conditional - some not getting rewritten?
    name = exp.shift.to_s
    args = exp.shift
    code = []
    unless args.nil? then
      args[0] = :arglist if args.first == :array
      code << process(args)
    end
    return code.empty? ? name : "#{name}(#{code.join(', ')})"
  end

  def process_flip2(exp)
    "#{process(exp.shift)}..#{process(exp.shift)}"
  end

  def process_flip3(exp)
    "#{process(exp.shift)}...#{process(exp.shift)}"
  end

  def process_for(exp)
    recv = process exp.shift
    iter = process exp.shift
    body = exp.empty? ? nil : process(exp.shift)

    result = ["for #{iter} in #{recv} do"]
    result << indent(body ? body : "# do nothing")
    result << "end"

    result.join("\n")
  end

  def process_gasgn(exp)
    process_iasgn(exp)
  end

  def process_gvar(exp)
    return exp.shift.to_s
  end

  def process_hash(exp)
    result = []
    until exp.empty?
      result << "#{process(exp.shift)} => #{process(exp.shift)}"
    end

    case self.context[1]
    when :arglist, :argscat then
      return "#{result.join(', ')}" # HACK - this will break w/ 2 hashes as args
    else
      return "{ #{result.join(', ')} }"
    end
  end

  def process_iasgn(exp)
    lhs = exp.shift
    if exp.empty? then # part of an masgn
      lhs.to_s
    else
      "#{lhs} = #{process exp.shift}"
    end
  end

  def process_if(exp)
    expand = Ruby2Ruby::ASSIGN_NODES.include? exp.first.first
    c = process exp.shift
    t = process exp.shift
    f = process exp.shift

    c = "(#{c.chomp})" if c =~ /\n/

    if t then
      unless expand then
        if f then
          r = "#{c} ? (#{t}) : (#{f})"
          r = nil if r =~ /return/ # HACK - need contextual awareness or something
        else
          r = "#{t} if #{c}"
        end
        return r if r and (@indent+r).size < LINE_LENGTH and r !~ /\n/
      end

      r = "if #{c} then\n#{indent(t)}\n"
      r << "else\n#{indent(f)}\n" if f
      r << "end"

      r
    else
      unless expand then
        r = "#{f} unless #{c}"
        return r if (@indent+r).size < LINE_LENGTH and r !~ /\n/
      end
      "unless #{c} then\n#{indent(f)}\nend"
    end
  end

  def process_iter(exp)
    iter = process exp.shift
    args = exp.shift
    args = (args == 0) ? '' : process(args)
    body = exp.empty? ? nil : process(exp.shift)

    b, e = if iter == "END" then
             [ "{", "}" ]
           else
             [ "do", "end" ]
           end

    iter.sub!(/\(\)$/, '')

    # REFACTOR: ugh
    result = []
    result << "#{iter} {"
    result << " |#{args}|" if args
    if body then
      result << " #{body.strip} "
    else
      result << ' '
    end
    result << "}"
    result = result.join
    return result if result !~ /\n/ and result.size < LINE_LENGTH

    result = []
    result << "#{iter} #{b}"
    result << " |#{args}|" if args
    result << "\n"
    result << indent(body.strip)
    result << "\n"
    result << e
    result.join
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
    case obj
    when Range then
      "(#{obj.inspect})"
    else
      obj.inspect
    end
  end

  def process_lvar(exp)
    exp.shift.to_s
  end

  def splat(sym)
    :"*#{sym}"
  end

  def process_masgn(exp)
    lhs = exp.shift
    rhs = exp.empty? ? nil : exp.shift

    unless exp.empty? then
      rhs[-1] = splat(rhs[-1]) unless rhs == s(:splat)
      lhs << rhs
      rhs = exp.shift
    end

    case lhs.first
    when :array then
      lhs.shift
      lhs = lhs.map do |l|
        case l.first
        when :masgn then
          "(#{process(l)})"
        else
          process(l)
        end
      end
    when :dasgn_curr then
      lhs = [ splat(lhs.last) ]
    when :splat then
      lhs = [ :"*" ]
    else
      raise "no clue: #{lhs.inspect}"
    end

    if context[1] == :iter and rhs then
      lhs << splat(rhs.last)
      rhs = nil
    end

    unless rhs.nil? then
      t = rhs.first
      rhs = if t == :argscat then
              rhs.shift
              process_argscat(rhs)
            else
              r = process(rhs)
              r = r[1..-2] if t != :to_ary
              r
            end
      return "#{lhs.join(", ")} = #{rhs}"
    else
      return lhs.join(", ")
    end

  end

  def process_match(exp)
    "#{process(exp.shift)}"
  end

  def process_match2(exp)
    lhs = process(exp.shift)
    rhs = process(exp.shift)
    "#{lhs} =~ #{rhs}"
  end

  def process_match3(exp)
    rhs = process(exp.shift)
    lhs = process(exp.shift)
    "#{lhs} =~ #{rhs}"
  end

  def process_module(exp)
    "module #{util_module_or_class(exp)}"
  end

  def process_next(exp)
    val = exp.empty? ? nil : process(exp.shift)
    if val then
      "next #{val}"
    else
      "next"
    end
  end

  def process_nil(exp)
    "nil"
  end

  def process_not(exp)
    "(not #{process exp.shift})"
  end

  def process_nth_ref(exp)
    "$#{exp.shift}"
  end

  def process_op_asgn1(exp)
    # [[:lvar, :b], [:array, [:lit, 1]], :"||", [:lit, 10]]
    lhs = process(exp.shift)
    index = process(exp.shift)
    msg = exp.shift
    rhs = process(exp.shift)

    "#{lhs}#{index} #{msg}= #{rhs}"
  end

  def process_op_asgn2(exp)
    # [[:lvar, :c], :var=, :"||", [:lit, 20]]
    lhs = process(exp.shift)
    index = exp.shift.to_s[0..-2]
    msg = exp.shift

    rhs = process(exp.shift)

    "#{lhs}.#{index} #{msg}= #{rhs}"
  end

  def process_op_asgn_or(exp)
    # a ||= 1
    # [[:lvar, :a], [:lasgn, :a, [:lit, 1]]]
    exp.shift
    process(exp.shift).sub(/=/, '||=')
  end

  def process_op_asgn_and(exp)
    # a &&= 1
    # [[:lvar, :a], [:lasgn, :a, [:lit, 1]]]
    exp.shift
    process(exp.shift).sub(/=/, '&&=')
  end

  def process_or(exp)
    "(#{process exp.shift} or #{process exp.shift})"
  end

  def process_postexe(exp)
    "END"
  end

  def process_redo(exp)
    "redo"
  end

  def process_resbody(exp) # TODO: rewrite this fucker
    code = []

    sexp = exp
    until exp.empty? and (sexp.nil? or sexp.empty?)
      list = sexp.shift
      body = sexp.shift

      var = if list and
                list.size > 1 and
                [:lasgn, :dasgn, :dasgn_curr].include? list.last.first then
              list.pop[1]
            else
              nil
            end

      # FIX: omg this is horrid. I should be punished
      var = body.delete_at(1)[1] if
        [:dasgn_curr, :dasgn].include? body[1][0] unless
        var or body.nil? rescue nil

      if list and list.size > 1 then
        list[0] = :arglist
        code << "rescue #{process(list)}"
      else
        code << "rescue"
      end

      code.last << " => #{var}" if var

      if body then
        code << indent(process(body)).chomp
      else
        code << indent("# do nothing")
      end

      unless exp.empty? then
        sexp = exp.shift
        assert_type sexp, :resbody
        sexp.shift
      end
    end

    code.join("\n")
  end

  def process_rescue(exp)
    # TODO: rewrite this
    #
    # a = b rescue c            =>                [lasgn a [rescue b c]]
    # begin; a = b; rescue c    => [begin [rescue [lasgn a b] c]]

    current = self.context[1]
    case current
    when :begin, :ensure, :block then
      body = (exp.first.first == :resbody) ? nil : process(exp.shift)
      resbody = exp.empty? ? '' : process(exp.shift)
      els = exp.empty? ? nil : process(exp.shift)

      code = []
      code << indent(body) if body
      code << resbody
      if els then
        code << "else"
        code << indent(els)
      else
        unless [:block].include? current then
          code << "end\n" unless current == :ensure
        else
          r = [body, resbody.gsub(/rescue\n\s+/, 'rescue ')].compact.join(' ')
          code = [r] if (@indent+r).size < LINE_LENGTH and r !~ /\n/
        end
      end

      code.join("\n").chomp
    else # a rescue b and others
      body = process exp.shift
      assert_type exp.first, :resbody
      resbody = exp.shift
      resbody.shift # resbody
      resbody.shift # nil (no types for expression form)
      resbody = resbody.shift # actual code

      resbody = process resbody
      code = "#{body} rescue #{resbody}"
      case current
      when :hash then # HACK move to process_hash
        "(#{code})"
      else
        code
      end
    end
  end

  def process_retry(exp)
    "retry"
  end

  def process_return(exp)
    # HACK return "return" + (exp.empty? ? "" : " #{process exp.shift}")

    if exp.empty? then
      return "return"
    else
      return "return #{process exp.shift}"
    end
  end

  def process_sclass(exp)
    "class << #{process(exp.shift)}\n#{indent(process(exp.shift))}\nend"
  end

  def process_scope(exp)
    exp.empty? ? "" : process(exp.shift)
  end

  def process_self(exp)
    "self"
  end

  def process_splat(exp)
    if exp.empty? then
      "*"
    else
      "*#{process(exp.shift)}"
    end
  end

  def process_str(exp)
    return exp.shift.dump
  end

  def process_super(exp)
    args = exp.shift
    args[0] = :arglist if args[0] == :array
    "super(#{process(args)})"
  end

  def process_svalue(exp)
    process(exp.shift)
  end

  def process_to_ary(exp)
    process(exp.shift)
  end

  def process_true(exp)
    "true"
  end

  def process_undef(exp)
    "undef #{process(exp.shift)}"
  end

  def process_until(exp)
    cond_loop(exp, 'until')
  end

  def process_valias(exp)
    "alias #{exp.shift} #{exp.shift}"
  end

  def process_when(exp)
    src = []

    if self.context[1] == :array then # ugh. matz! why not an argscat?!?
      val = process(exp.shift)
      exp.shift # empty body
      return "*#{val}"
    end

    until exp.empty?
      cond = process(exp.shift).to_s[1..-2]
      code = indent(process(exp.shift))
      code = indent "# do nothing" if code =~ /\A\s*\Z/
      src << "when #{cond} then\n#{code.chomp}"
    end

    src.join("\n")
  end

  def process_while(exp)
    cond_loop(exp, 'while')
  end

  def process_xstr(exp)
    "`#{process_str(exp)[1..-2]}`"
  end

  def process_yield(exp)
    args = exp.empty? ? nil : exp.shift
    if args then
      args[0] = :arglist if args.first == :array
      args = process(args)
    end

    # "yield" + (args ? "(#{args})" : "")
    if args then
      "yield(#{args})"
    else
      "yield"
    end
  end

  def process_zarray(exp)
    "[]"
  end

  def process_zsuper(exp)
    "super"
  end

  def cond_loop(exp, name)
    cond = process(exp.shift)
    body = process(exp.shift)
    head_controlled = exp.shift

    body = indent(body).chomp if body

    code = []
    if head_controlled then
      code << "#{name} #{cond} do"
      code << body if body
      code << "end"
    else
      code << "begin"
      code << body if body
      code << "end #{name} #{cond}"
    end
    code.join("\n")
  end

  ############################################################
  # Rewriters:

  def rewrite_rescue exp
    exp = s(:begin, exp) if
      context[1] == :block unless
      context[2] == :scope and [:defn, :defs].include? context[3]
    exp
  end

  ############################################################
  # Utility Methods:

  def util_dthing(exp, regx = false)
    s = []
    suck = true
    if suck then
      x = exp.shift.gsub(/"/, '\"').gsub(/\n/, '\n')
    else
      x = exp.shift.dump[1..-2]
    end
    x.gsub!(/\//, '\/') if regx

    s << x
    until exp.empty?
      pt = exp.shift
      case pt
      when Sexp then
        case pt.first
        when :str then
          if suck then
            x = pt.last.gsub(/"/, '\"').gsub(/\n/, '\n')
          else
            x = pt.last.dump[1..-2]
          end
          x.gsub!(/\//, '\/') if regx
          s << x
        else
          s << '#{' << process(pt) << '}' # do not use interpolation here
        end
      else
        # do nothing - yet
      end
    end

    s.join
  end

  def util_module_or_class(exp, is_class=false)
    result = []

    name = exp.shift
    name = process name if Sexp === name

    result << name

    if is_class then
      superk = process(exp.shift)
      result << " < #{superk}" if superk
    end

    result << "\n"

    body = []
    begin
      code = process(exp.shift).chomp
      body << code unless code.nil? or code.empty?
    end until exp.empty?

    unless body.empty? then
      body = indent(body.join("\n\n")) + "\n"
    else
      body = ""
    end
    result << body
    result << "end"

    result.join
  end

  def indent(s)
    s.to_s.split(/\n/).map{|line| @indent + line}.join("\n")
  end
end

RubyToRuby = Ruby2Ruby # For backwards compatibilty... TODO: remove 2008-03-28

class Method
  def with_class_and_method_name
    if self.inspect =~ /<Method: (.*)\#(.*)>/ then
      klass = eval $1
      method  = $2.intern
      raise "Couldn't determine class from #{self.inspect}" if klass.nil?
      return yield(klass, method)
    else
      raise "Can't parse signature: #{self.inspect}"
    end
  end

  def to_sexp
    require 'parse_tree'
    parser = ParseTree.new(false)
    with_class_and_method_name do |klass, method|
      parser.parse_tree_for_method(klass, method)
    end
  end

  def to_ruby
    Ruby2Ruby.new.process(self.to_sexp)
  end
end

class ProcStoreTmp
  @@n = 0
  def self.new_name
    @@n += 1
    return :"myproc#{@@n}"
  end
end

class UnboundMethod
  def to_ruby
    name = ProcStoreTmp.new_name
    ProcStoreTmp.send(:define_method, name, self)
    m = ProcStoreTmp.new.method(name)
    result = m.to_ruby.sub(/def #{name}(?:\(([^\)]*)\))?/,
                           'proc { |\1|').sub(/end\Z/, '}')
    return result
  end
end

class Proc
  def to_method
    name = ProcStoreTmp.new_name
    ProcStoreTmp.send(:define_method, name, self)
    ProcStoreTmp.new.method(name)
  end

  def to_sexp
    sexp = self.to_method.to_sexp
    body = sexp[2]
    body[0] = :block
    args = body.delete_at 1
    body = body[1] if body.size == 2

    [:iter, [:fcall, :proc], args, body]
  end

  def to_ruby
    Ruby2Ruby.new.process(self.to_sexp).sub(/^\Aproc do/, 'proc {').sub(/end\Z/, '}')
  end
end

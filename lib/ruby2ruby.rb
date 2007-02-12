#!/usr/bin/env ruby -w

begin require 'rubygems'; rescue LoadError; end
require 'parse_tree'
require 'sexp_processor'

class NilClass # Objective-C trick
  def method_missing(msg, *args, &block)
    nil
  end
end

class RubyToRuby < SexpProcessor
  VERSION = '1.1.5'
  LINE_LENGTH = 78

  def self.translate(klass_or_str, method = nil)
    self.new.process(ParseTree.translate(klass_or_str, method))
  end

  def initialize
    super
    @indent = "  "
    self.auto_shift_type = true
    self.strict = true
    self.expected = String
  end

  ############################################################
  # Processors (rewriters at bottom)

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
        when :array then
          names = arg
          vals  = exp.shift
          names.shift
          vals.shift
          v_size = vals.size

          args << process(names.shift) until names.size == v_size
          names.zip(vals) do |name, val|
            args << "#{process name} = #{process val}"
          end
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
    args.join ', '
  end

  def process_argspush(exp)
    args = []

    until exp.empty? do
      args << process(exp.shift)
    end

    "#{args.join ', '}"
  end

  def process_array(exp)
    "[#{process_arglist(exp)}]"
  end

  def process_attrasgn(exp)
    receiver = process exp.shift
    name = exp.shift
    args = exp.shift

    case name
    when :[]= then
      rhs = process args.pop
      args[0] = :arglist
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
      src = indent(src) unless src =~ /\nrescue/ # ensures no level 0 rescues
      code << src
    end
    code << "end" unless is_rescue
    return code.join("\n")
  end

  def process_block(exp)
    result = []

    until exp.empty? do
      code = exp.shift
      if code.nil? or code.first == :nil then
        result << "# do nothing"
      else
        result << process(code)
      end
    end

    return result.join("\n") + "\n"
  end

  def process_block_arg(exp)
    "&#{exp.shift}"
  end

  def process_block_pass(exp)
    bname = [:block_arg, process(exp.shift)]
    call = exp.shift

    if Array === call.last then # HACK - I _really_ need rewrites to happen first
      case call.last.first
      when :splat then
        call << [:array, call.pop]
      when :array then
        # do nothing
      else
        call << [:array] unless has_args
      end
      call.last << bname
    else
      call << [:array, bname]
    end

    process(call)
  end

  def process_break(exp)
    val = process(exp.shift)
    "break" + (val ? " #{val}" : "")
  end

  def process_call(exp)
    receiver = process exp.shift
    name = exp.shift
    args_exp = exp.shift rescue nil
    if args_exp && args_exp.first == :array
      args = "#{process(args_exp)[1..-2]}"
    else
      args = process args_exp
    end

    case name
    when :<=>, :==, :<, :>, :<=, :>=, :-, :+, :*, :/, :%, :<<, :>> then #
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
    "#{exp.shift} = #{process(exp.shift)}"
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
    rhs = exp.shift
    return lhs if rhs.nil?
    return "#{lhs} = #{process rhs}" unless rhs.first == :dasgn_curr

    # keep recursing. ensure that the leaf node assigns to _something_
    rhs = process rhs
    if rhs =~ /=/ then
      "#{lhs} = #{rhs}"
    else
      ""
    end
  end

  def process_dasgn(exp)
    "#{exp.shift.to_s} = #{process(exp.shift)}"
  end

  def process_defined(exp)
    "defined? #{process(exp.shift)}"
  end

  def process_defs(exp)
    process_defn(exp)
  end

  def process_defn(exp)
    t = exp[1].first
    t2 = exp[2].first rescue nil

    if t == :args and [:ivar, :attrset].include? t2 then
      name = exp.shift
      case t2
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

    case t
    when :cfunc then
      s = "# method '#{exp.shift}' defined in a C function"
      exp.shift
      return s
    when :scope, :args then
      name = exp.shift
      args = process(exp.shift)
      args = "" if args == "()"
      body = indent(process(exp.shift))
      return "def #{name}#{args}\n#{body}\nend".gsub(/\n\s*\n+/, "\n")
    when :fcall then
      # skip the fcall (to define_method and such) and grab the body
      name = exp.shift
      exp.shift # :fcall to define_method
      body = process(exp.shift)
      raise "no"
    else
      raise "Unknown defn type: #{t} for #{exp.inspect}"
    end
  end

  def process_defx(exp) # custom node type - TODO why in r2r?
    name = exp.shift
    args = process(exp.shift)
    body = indent(process(exp.shift))
    return "defx #{name}#{args}\n#{body}end".gsub(/\n\s*\n+/, "\n")
  end

  def process_dot2(exp)
    "(#{process exp.shift}..#{process exp.shift})"
  end

  def process_dot3(exp)
    "(#{process exp.shift}...#{process exp.shift})"
  end

  def process_dregx(exp)
    "/#{process_dstr(exp)[1..-2]}/"
  end

  def process_dregx_once(exp)
    process_dregx(exp) + "o"
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

  def process_dsym(exp)
    ":" + process_dstr(exp)
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

  def process_false(exp)
    "false"
  end

  def process_fcall(exp)
    exp_orig = exp.deep_clone
    name = exp.shift.to_s
    args = exp.shift
    code = []
    unless args.nil? then
      args[0] = :arglist if args.first == :array
      code << process(args)
    end
    return "#{name}(#{code.join(', ')})"
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
    body = process exp.shift
    return "for #{iter} in #{recv}\n#{indent body}\nend\n"
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
    return "{ #{result.join(', ')} }"
  end

  def process_iasgn(exp)
    lhs = exp.shift
    if exp.empty? then # part of an masgn
      lhs.to_s
    else
      "#{lhs} = #{process exp.shift}"
    end
  end

  def cond_indent_process(pt)
    (pt and pt.first == :block) ? process(pt) : indent(process(pt))
  end

  def process_if(exp)
    c = process exp.shift
    t = process exp.shift
    f = process exp.shift

    if t then
      unless f then
        r = "#{t} if #{c}"
        return r if (@indent+r).size < LINE_LENGTH and r !~ /\n/
      end
      r = "if #{c} then\n#{indent(t)}\n"
      r << "else\n#{indent(f)}\n" if f
      r << "end"
      r
    else
      r = "#{f} unless #{c}"
      return r if (@indent+r).size < LINE_LENGTH and r !~ /\n/
      "unless #{c} then\n#{indent(f)}\nend"
    end
  end

  def process_iter(exp)
    iter = process exp.shift
    args = process exp.shift
    body = process exp.shift

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
    if body then
      result << "\n"
      result << indent(body.strip)
      result << "\n"
    else
      result << ' '
    end
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
    if obj.is_a? Range # to get around how parsed ranges turn into lits and lose parens
      "(" + obj.inspect + ")"
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
      rhs[-1] = splat(rhs[-1])
      lhs << rhs
      rhs = exp.shift
    end

    case lhs.first
    when :array then
      lhs.shift
      lhs = lhs.map { |l| process(l) }
    when :dasgn_curr then
      lhs = [ splat(lhs.last) ]
    else
      raise "no clue: #{lhs.inspect}"
    end

    unless rhs.nil? then
      # HACK - but seems to work (see to_ary test)      assert_type rhs, :array
      rhs.shift
      rhs = rhs.map { |r| process(r) }
      return "#{lhs.join(", ")} = #{rhs.join(", ")}"
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
    "next"
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

      var = if list and list.last.first == :lasgn then
              list.pop[1]
            else
              nil
            end

      if list then
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

      sexp = exp.shift
      if sexp then
        assert_type sexp, :resbody
        sexp.shift
      end
    end

    code.join("\n")
  end

  def process_rescue(exp)
    # TODO: rewrite this
    # TODO: proper formatting depends on knowing the context
    #
    # a = b rescue c            =>                [lasgn a [rescue b c]]
    # begin; a = b; rescue c    => [begin [rescue [lasgn a b] c]]
    stack = caller.map { |s| s[/process_\w+/] }.compact

    case stack.first
    when "process_begin", "process_ensure", "process_block" then
      body = process exp.shift
      resbody = process exp.shift
      els = process exp.shift

      code = []
      code << indent(body)
      code << resbody
      if els then
        code << "else"
        code << indent(els)
      else
        unless stack.first == "process_block" then
          code << "end\n"
        else
          r = [body, resbody.gsub(/rescue\n\s+/, 'rescue ')].join(' ')
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
      "#{body} rescue #{resbody}"
    end
  end

  def process_retry(exp)
    "retry"
  end

  def process_return(exp)
    return "return" + (exp.empty? ? "" : " #{process exp.shift}")
  end

  def process_sclass(exp)
    "class << #{process(exp.shift)}\n#{indent(process(exp.shift))}\nend"
  end

  def process_scope(exp)
    return process(exp.shift) || ""
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
    args = exp.shift
    args[0] = :arglist
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

  def process_vcall(exp)
    return exp.shift.to_s
  end

  def process_when(exp)
    src = []

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
    args = exp.shift
    if args then
      args[0] = :arglist if args.first == :array
      args = process(args)
    end

    "yield" + (args ? "(#{args})" : "")
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
  # Rewriters

  ##
  # defn: [:defn, :name, [:args...], [:scope, [:block, ...]]]

  def rewrite_defs(exp)
    target = exp.delete_at 1
    exp[1] = :"#{target}.#{exp[1]}"
    rewrite_defn(exp)
  end

  # s(:defn, :name, s(:scope, s(:block, s(:args, ...), ...)))
  # s(:defn, :name, s(:bmethod, s(:masgn, s(:dasgn_curr, :args)), s(:block, ...)))
  # s(:defn, :name, s(:fbody, s(:bmethod, s(:masgn, s(:dasgn_curr, :params)), s(:block, ...))))
  # =>
  # s(:defn, :name, s(:args, ...), s(:scope, s:(block, ...)))

  def rewrite_defn(exp)
    # REFACTOR this needs help now
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
      body = body.pop if body.first == :fbody
      case body.first
      when :scope then
        args = body.block.args(true)
        assert_type body, :scope
        assert_type body[1], :block

        if body[1][1].first == :block_arg then
          block_arg = body[1].delete_at 1
          args << block_arg
        end
      when :bmethod then
        body[0] = :scope
        body.block.delete_at(1) # nuke the decl # REFACTOR
        masgn = body.masgn(true)
        if masgn then
          splat = self.splat(masgn[-1][-1])
          args.push(splat)
        else
          dasgn_curr = body.dasgn_curr(true)
          if dasgn_curr then
            arg = self.splat(dasgn_curr[-1])
            args.push(arg)
          end
        end
        body.find_and_replace_all(:dvar, :lvar)
      else
        raise "no: #{body.first} / #{body.inspect}"
      end
    when :bmethod then
      body.shift # :bmethod
      case body.first.first
      when :dasgn_curr then
        # WARN: there are some implications here of having an empty
        # :args below namely, "proc { || " does not allow extra args
        # passed in.
        dasgn = body.shift
        assert_type dasgn, :dasgn_curr
        dasgn.shift # type
        args.push(*dasgn)
        body.find_and_replace_all(:dvar, :lvar)
      when :masgn then
        dasgn = body.masgn(true)
        # DAMNIT body.block.dasgn_curr(true) - multiple values so can't use
        body.block.delete_at(1) # nuke the decl
        splat = self.splat(dasgn[-1][-1])
        args.push(splat)
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
      body[0] = :scope
      body.delete_at 1 # method name
      args = body.scope.block.args(true)
    when :ivar, :attrset then
      # do nothing
    else
      raise "Unknown :defn format: #{name.inspect} #{args.inspect} #{body.inspect}"
    end

    return s(:defn, name, args, body)
  end

  def rewrite_resbody(exp)
    result = []

    code = result
    while exp and exp.first == :resbody do
      code << exp.shift
      list = exp.shift
      body = exp.shift
      exp  = exp.shift # either another resbody or nil

      # code may be nil, :lasgn, or :block
      case body.first
      when nil then
        # do nothing
      when :lasgn then
        # TODO: check that it is assigning $!
        list << body
        body = nil
      when :block then
        # TODO: check that it is assigning $!
        list << body.delete_at(1) if body[1].first == :lasgn
      else
        # do nothing (expression form)
      end

      code << list << body
      if exp then
        code = []
        result << code
      end
    end

    result
  end

  ############################################################
  # Utility Methods:

  def util_module_or_class(exp, is_class=false)
    s = "#{exp.shift}"

    if is_class then
      superk = process(exp.shift)
      s << " < #{superk}" if superk
    end

    s << "\n"

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
    s + body + "end"
  end

  def indent(s)
    s.to_s.map{|line| @indent + line}.join
  end
end

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
    with_class_and_method_name do |klass, method|
      ParseTree.new(false).parse_tree_for_method(klass, method)
    end
  end

  def to_ruby
    RubyToRuby.new.process(self.to_sexp)
  end
end

class ProcStoreTmp
  @@n = 0
  def self.name
    @@n += 1
    return :"myproc#{@@n}"
  end
end

class UnboundMethod
  def to_ruby
    name = ProcStoreTmp.name
    ProcStoreTmp.send(:define_method, name, self)
    m = ProcStoreTmp.new.method(name)
    result = m.to_ruby.sub(/def #{name}\(([^\)]*)\)/,
                           'proc { |\1|').sub(/end\Z/, '}')
    return result
  end
end

class Proc
  def to_method
    name = ProcStoreTmp.name
    ProcStoreTmp.send(:define_method, name, self)
    ProcStoreTmp.new.method(name)
  end

  def to_sexp
    body = self.to_method.to_sexp[2][1..-1]
    [:proc, *body]
  end

  def to_ruby
    ruby = self.to_method.to_ruby
    ruby.sub!(/\A(def \S+)\(([^\)]*)\)/, '\1 |\2|')     # move args
    ruby.sub!(/\Adef[^\n\|]+/, 'proc { ')               # strip def name
    ruby.sub!(/end\Z/, '}')                             # strip end
    ruby.gsub!(/\s+$/, '')                              # trailing WS bugs me
    ruby
  end
end

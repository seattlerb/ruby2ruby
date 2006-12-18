#!/usr/local/bin/ruby -w

require 'rubygems'
require 'ruby2ruby'

# [:defn, :relative_url_root, [:scope, [:block, [:args], [:op_asgn_or, [:cvar, :@@relative_url_root], [:cvasgn, :@@relative_url_root, [:when, [:array, [:call, [:ivar, :@env], :[], [:array, [:str, "uO=::j\rMyi3P}\\{C\n\004Z|ET`2N\037\004\025NW&_5$U!EgWfs=L\025D\020TWT"]]]], [:call, [:ivar, :@env], :[], [:array, [:str, "RAILS_RELATIVE_URL_ROOT"]]]]]]]]]

# Produces:

# def relative_url_root
#   @@relative_url_root ||= when @env["uO=::j\rMyi3P}\\{C\n\004Z|ET`2N\037\004\025NW&_5$U!EgWfs=L\025D\020TWT"]
# then
#     @env["RAILS_RELATIVE_URL_ROOT"]
# end

# The original tree

# Produces:

# def relative_url_root
#   @@relative_url_root ||= when @env["RAILS_RELATIVE_URL_ROOT"] then
#     @env["RAILS_RELATIVE_URL_ROOT"]
# end


# The original code

class X
  def relative_url_root
    @@relative_url_root ||= case
                            when @env["RAILS_RELATIVE_URL_ROOT"]
                              @env["RAILS_RELATIVE_URL_ROOT"]
                            when server_software == 'apache'
                              @env["SCRIPT_NAME"].to_s.sub(/\/dispatch\.(fcgi|rb|cgi)$/, '')
                            else
                              ''
                            end
  end
  def simple
    case
    when 1 then
      :a
    when 2 then
      :b
    else
      :c
    end
  end
end

puts RubyToRuby.translate(X, :relative_url_root)


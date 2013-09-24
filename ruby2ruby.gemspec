# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = "ruby2ruby"
  s.version = "2.0.6"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Ryan Davis"]
  s.date = "2013-06-21"
  s.description = "ruby2ruby provides a means of generating pure ruby code easily from\nRubyParser compatible Sexps. This makes making dynamic language\nprocessors in ruby easier than ever!"
  s.email = ["ryand-ruby@zenspider.com"]
  s.executables = ["r2r_show"]
  s.extra_rdoc_files = ["History.txt", "Manifest.txt", "README.txt"]
  s.files = ["bin/r2r_show", "History.txt", "Manifest.txt", "README.txt"]
  s.homepage = "https://github.com/seattlerb/ruby2ruby"
  s.rdoc_options = ["--main", "README.txt"]
  s.require_paths = ["lib"]
  s.rubyforge_project = "seattlerb"
  s.rubygems_version = "2.0.3"
  s.summary = "ruby2ruby provides a means of generating pure ruby code easily from RubyParser compatible Sexps"

  if s.respond_to? :specification_version then
    s.specification_version = 3

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<sexp_processor>, ["~> 4.0"])
      s.add_runtime_dependency(%q<ruby_parser>, ["~> 3.1"])
      s.add_development_dependency(%q<minitest>, ["~> 5.0"])
      s.add_development_dependency(%q<rdoc>, ["~> 4.0"])
      s.add_development_dependency(%q<hoe>, ["~> 3.6"])
    else
      s.add_dependency(%q<sexp_processor>, ["~> 4.0"])
      s.add_dependency(%q<ruby_parser>, ["~> 3.1"])
      s.add_dependency(%q<minitest>, ["~> 5.0"])
      s.add_dependency(%q<rdoc>, ["~> 4.0"])
      s.add_dependency(%q<hoe>, ["~> 3.6"])
    end
  else
    s.add_dependency(%q<sexp_processor>, ["~> 4.0"])
    s.add_dependency(%q<ruby_parser>, ["~> 3.1"])
    s.add_dependency(%q<minitest>, ["~> 5.0"])
    s.add_dependency(%q<rdoc>, ["~> 4.0"])
    s.add_dependency(%q<hoe>, ["~> 3.6"])
  end
end

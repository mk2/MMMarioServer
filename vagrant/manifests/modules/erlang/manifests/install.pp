# Class: erlang::install
#
#
class erlang::install {

  include apt

  apt::source { "erlang_solutions":
    comment     => "Latest erlang packages.",
    location    => "http://packages.erlang-solutions.com/debian",
    repos       => "contrib",
    release     => "wheezy",
    include_deb => true,
    include_src => true,
    key         => "A14F4FCA",
    key_server  => "keyserver.ubuntu.com",
  }

  package { "erlang":
    ensure  => installed,
    require => Apt::Source["erlang_solutions"],
  }

}

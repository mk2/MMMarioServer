# Class: nginx::install
#
#
class nginx::install {

  include apt

  apt::source { "nginx_official":
    comment     => "Latest nginx packages.",
    location    => "http://nginx.org/packages/debian/",
    repos       => "nginx",
    release     => "wheezy",
    include_deb => true,
    key         => "7BD9BF62",
    key_server  => "keyserver.ubuntu.com",
  }

  package { "nginx":
    ensure  => installed,
    require => Apt::Source["nginx_official"],
  }

}
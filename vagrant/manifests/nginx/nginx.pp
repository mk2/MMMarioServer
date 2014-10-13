# 最新版のnginxを入れて、websocketの設定をする

include apt

apt::source { "nginx_official":
    comment     => "Latest nginx packages.",
    location    => "http://nginx.org/packages/debian/",
    repos       => "nginx",
    release     => "wheezy",
    include_deb => true,
    key         => "",
    key_server  => "http://nginx.org/keys/nginx_signing.key",
}

package { "nginx":
    ensure => installed,
    require => Apt::Source["nginx_official"],
}

$docRoot = "/opt/htdocs"

file { "nignx_doc_root":
    ensure => directory,
    path => $docRoot,
}

file { "nginx_default_index_html":
    ensure => present,
    path => "${docRoot}/index.html",
    content => template("index.html.erb"),
    require => File["nignx_doc_root"],
}

file { "nginx_config":
    ensure => present,
    path => "/etc/nginx/conf.d/default.conf",
    content => template("default.conf.erb"),
    require => [Package["nginx"],
                File["nginx_default_index_html"]],
}

service { "nginx":
    enable      => true,
    ensure      => running,
    hasrestart => true,
    require    => File["nginx_config"],
}
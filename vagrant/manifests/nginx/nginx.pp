# 最新版のnginxを入れて、websocketの設定をする

include apt

apt::source { 'nginx_official':
    comment     => 'Latest nginx packages.',
    location    => 'http://nginx.org/packages/debian/',
    repos       => 'nginx',
    release     => 'wheezy',
    include_deb => true,
    key         => '',
    key_server  => 'http://nginx.org/keys/nginx_signing.key',
}

package { 'nginx':
    ensure => installed,
    require => Apt::Source['nginx_official'],
}

file { 'nginx_config':
    path => '',
    ensure => present,
    require => Package['nginx'],
}

service { 'nginx':
    enable      => true,
    ensure      => running,
    hasrestart => true,
    require    => File["nginx_config"],
}
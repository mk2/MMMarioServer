
# Class: nginx::config
#
#
class nginx::config {

    $docRoot = "/opt/htdocs"

    file { "nignx_doc_root":
        ensure => directory,
        path => $docRoot,
    }

    file { "nginx_default_index_html":
        ensure => present,
        path => "${docRoot}/index.html",
        content => template("nginx/index.html.erb"),
        require => File["nignx_doc_root"],
    }

    file { "nginx_config":
        ensure => present,
        path => "/etc/nginx/conf.d/default.conf",
        content => template("nginx/default.conf.erb"),
        require => [Package["nginx"],
                    File["nginx_default_index_html"]],
    }

}
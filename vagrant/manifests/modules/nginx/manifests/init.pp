# 最新版のnginxを入れて、websocketの設定をする

# Class: nginx
#
#
class nginx {

    include nginx::install
    include nginx::config
    include nginx::service

    Class["nginx::install"]
    -> Class["nginx::config"]
    ~> Class["nginx::service"]
}

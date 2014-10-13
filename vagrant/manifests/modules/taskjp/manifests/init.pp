
# Class: taskjp
#
#
class taskjp {

    include taskjp::install
    include taskjp::config

    Class['taskjp::install']
    -> Class['taskjp::config']
}


# Class: taskjp::install
#
#
class taskjp::install {

    package { 'task-japanese':
        ensure => installed,
    }

}
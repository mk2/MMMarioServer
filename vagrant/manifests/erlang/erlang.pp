# 最新版のErlangを入れる

include apt

apt::source { 'erlang_solutions':
    comment     => 'Latest erlang packages.',
    location    => 'http://packages.erlang-solutions.com/debian',
    repos       => 'contrib',
    release     => 'wheezy',
    include_deb => true,
    key         => '',
    key_server  => 'http://packages.erlang-solutions.com/debian/erlang_solutions.asc',
}

package { 'erlang':
    ensure  => installed,
    require => Apt::Source['erlang_solutions'],
}

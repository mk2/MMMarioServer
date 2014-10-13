# パペットマニフェスト

exec { 'install_puppetlabs-apt':
    command      => 'puppet module install puppetlabs-apt',
    path        => '/usr/bin:/usr/sbin:/bin:/usr/local/bin',
}
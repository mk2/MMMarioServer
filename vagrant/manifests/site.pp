# taskjp : 日本語化
# erlang : erlangインストール & 設定
# nginx  : nginxインストール & 設定
node default {

  include taskjp
  include erlang
  include nginx

}

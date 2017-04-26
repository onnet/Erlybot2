erlybot
=====

Telegram bot example on Erlang

Build
-----

    $ rebar3 compile

For using this bot example, you will need to set your own Telegram API Token and IP address:port in erlybot.app.src file. In my test lab this bot was behind nginx, that's why there's no SSL - nginx take care about SSL, and proxied all requests to https://external IP:443 onto http://localhost:7770.

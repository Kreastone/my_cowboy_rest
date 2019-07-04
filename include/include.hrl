-define(APPLICATION, test_zimad).
-define(HTTP_LISTEN, my_http).

-define(txt(Format, Args), iolist_to_binary(io_lib:fwrite(Format, Args))).

-define(certfile, "").
-define(keyfile, "").

-record(login, {
  auth_token :: binary(),
  uid :: binary(),
  ttl = 100 :: integer()
}).

-record(profile, {
  uid :: binary(),
  nickname :: binary(),
  coins = 100 :: integer(),
  stars = 0 :: integer(),
  level = 0 :: integer()
}).
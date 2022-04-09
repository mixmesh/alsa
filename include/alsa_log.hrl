-ifndef(__ALSA_LOG_HRL__).
-define(__ALSA_LOG_HRL__, true).

-define(dbg(F,A), ok).
%%-define(dbg(F,A), io:format((F),(A))).
-define(error(F), io:format((F))).
-define(error(F,A), io:format((F),(A))).
-define(warning(F), io:format((F))).
-define(warning(F,A), io:format((F),(A))).
-define(info(F), io:format((F))).
-define(info(F,A), io:format((F),(A))).

-endif.

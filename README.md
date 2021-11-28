# alsa

An Erlang binding to the Advanced Linux Sound Architecture
([ALSA](https://www.alsa-project.org/wiki/Main_Page)), i.e. if you feel
the need to build ALSA based applications in Erlang you have come to
the right place.

The API can be found in
[alsa.erl](https://github.com/mixmesh/alsa/blob/main/src/alsa.erl) and
the examples in
[playback.erl](https://github.com/mixmesh/alsa/blob/main/src/playback.erl)
and
[capture.erl](https://github.com/mixmesh/alsa/blob/main/src/capture.erl)
is sort of self explanatory. No documentation needed. :-)

The API is not a complete Erlang adaption of the [ALSA C
library](https://www.alsa-project.org/alsa-doc/alsa-lib/) but rather a
high level ditto which only implements what you need(TM). :-)

Before you begin I strongly recommend that you read
[Frames&amp;Periods](https://www.alsa-project.org/wiki/FramesPeriods). You
will thank me.


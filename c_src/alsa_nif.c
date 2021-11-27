#include <stdbool.h>
#include <erl_nif.h>
#include <alsa/asoundlib.h>
#include "uthash/uthash.h"

// #define DEBUGF(f,a...) fprintf(stderr, f "\r\n", a)
#define DEBUGF(f,a...)

#define ATOM(name) atm_##name

#define DECL_ATOM(name) ERL_NIF_TERM atm_##name = 0

#define LOAD_ATOM(name) \
    do { \
        if (!enif_make_existing_atom(env, #name, &atm_##name, ERL_NIF_LATIN1)) \
            return -1; \
    } while (0)

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(no_such_handle);

DECL_ATOM(playback);
DECL_ATOM(capture);

DECL_ATOM(bad_param);
DECL_ATOM(format);
DECL_ATOM(channels);
DECL_ATOM(rate);
DECL_ATOM(period_size);
DECL_ATOM(buffer_size);

DECL_ATOM(start_threshold);

DECL_ATOM(too_little_data);
DECL_ATOM(underrun);
DECL_ATOM(overrun);
DECL_ATOM(suspend_event);

typedef struct {
    int handle;
    snd_pcm_t *pcm_handle;
    UT_hash_handle hh;
} session_t;

session_t *sessions = NULL;

void add_session(session_t *session) {
    HASH_ADD_INT(sessions, handle, session);
}

session_t *find_session(int handle) {
    session_t *session = NULL;
    HASH_FIND_INT(sessions, &handle, session);
    return session;
}

void delete_session(session_t *session) {
    HASH_DEL(sessions, session);
    free(session);
}

uint32_t next_handle = 0;

ERL_NIF_TERM get_hw_params_map(ErlNifEnv* env, snd_pcm_t *pcm_handle,
                               ERL_NIF_TERM *hw_params_map) {
    int err;

    snd_pcm_hw_params_t *hw_params;
    if ((err = snd_pcm_hw_params_malloc(&hw_params)) < 0) {
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_hw_params_current(pcm_handle, hw_params)) < 0) {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_int(env, err);
    }

    snd_pcm_format_t format;
    snd_pcm_hw_params_get_format(hw_params, &format);
    unsigned int channels;
    snd_pcm_hw_params_get_channels(hw_params, &channels);
    unsigned int rate;
    int dir;
    snd_pcm_hw_params_get_rate(hw_params, &rate, &dir);
    snd_pcm_uframes_t period_size;
    snd_pcm_hw_params_get_period_size(hw_params, &period_size, &dir);
    snd_pcm_uframes_t buffer_size;
    snd_pcm_hw_params_get_buffer_size(hw_params, &buffer_size);

    ERL_NIF_TERM hw_params_keys[5] =
        {
         ATOM(format),
         ATOM(channels),
         ATOM(rate),
         ATOM(period_size),
         ATOM(buffer_size)
        };

    ERL_NIF_TERM hw_params_values[5] =
        {
         enif_make_uint(env, format),
         enif_make_uint(env, channels),
         enif_make_uint(env, rate),
         enif_make_uint(env, period_size),
         enif_make_uint(env, buffer_size)
        };

    enif_make_map_from_arrays(env, hw_params_keys, hw_params_values, 5,
                              hw_params_map);

    return 0;
}

ERL_NIF_TERM set_hw_params_map(ErlNifEnv* env, snd_pcm_t *pcm_handle,
                               ERL_NIF_TERM hw_params_map) {
    int err;

    snd_pcm_hw_params_t *hw_params;
    if ((err = snd_pcm_hw_params_malloc(&hw_params)) < 0) {
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_hw_params_any(pcm_handle, hw_params)) < 0) {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_hw_params_set_access(pcm_handle, hw_params,
                                            SND_PCM_ACCESS_RW_INTERLEAVED)) <
        0) {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_int(env, err);
    }

    size_t size;
    if (enif_get_map_size(env, hw_params_map, &size)) {
        if (size > 0) {
            ErlNifMapIterator iter;
            if (enif_map_iterator_create(env, hw_params_map, &iter,
                                         ERL_NIF_MAP_ITERATOR_FIRST)) {
                ERL_NIF_TERM key, value, reason = 0;
                bool badarg = false;
                while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
                    if (key == ATOM(format)) {
                        snd_pcm_format_t format;
                        if (enif_get_int(env, value, &format)) {
			    DEBUGF("format=%d", format);
                            if ((err = snd_pcm_hw_params_set_format(
                                           pcm_handle, hw_params,
                                           format)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(format),
                                                          value,
                                                          enif_make_int(env,
                                                                        err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(channels)) {
                        unsigned int channels;
                        if (enif_get_uint(env, value, &channels)) {
			    DEBUGF("channels=%d", channels);
                            if ((err = snd_pcm_hw_params_set_channels(
                                           pcm_handle, hw_params,
                                           channels)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(channels),
                                                          value,
                                                          enif_make_int(env,
                                                                        err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(rate)) {
                        unsigned int rate;
                        int dir;
                        if (enif_get_uint(env, value, &rate)) {
			    DEBUGF("rate=%u", rate);
                            if ((err = snd_pcm_hw_params_set_rate_near(
                                           pcm_handle, hw_params, &rate, &dir)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(rate),
                                                          value,
                                                          enif_make_int(env,
                                                                        err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(period_size)) {
                        snd_pcm_uframes_t period_size;
                        int dir;
                        if (enif_get_ulong(env, value, &period_size)) {
			    DEBUGF("period_size=%lu", period_size);
                            if ((err = snd_pcm_hw_params_set_period_size_near(
                                           pcm_handle, hw_params,
                                           &period_size, &dir)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(period_size),
                                                          value,
                                                          enif_make_int(env,
                                                                        err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(buffer_size)) {
                        snd_pcm_uframes_t buffer_size;
                        if (enif_get_ulong(env, value, &buffer_size)) {
			    DEBUGF("buffer_size=%lu", buffer_size);
                            if ((err = snd_pcm_hw_params_set_buffer_size_near(
                                           pcm_handle, hw_params,
                                           &buffer_size)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(buffer_size),
                                                          value,
                                                          enif_make_int(env,
                                                                        err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else {
                        badarg = true;
                        break;
                    }
                    enif_map_iterator_next(env, &iter);
                }
                enif_map_iterator_destroy(env, &iter);

                if (badarg) {
                    snd_pcm_hw_params_free(hw_params);
                    return enif_make_badarg(env);
                } else if (reason != 0) {
                    snd_pcm_hw_params_free(hw_params);
                    return reason;
                } else {
                    if ((err = snd_pcm_hw_params(pcm_handle,
                                                 hw_params)) < 0) {
                        snd_pcm_hw_params_free(hw_params);
                        return enif_make_int(env, err);
                    }

                    snd_pcm_hw_params_free(hw_params);
                    return 0;
                }
            } else {
                snd_pcm_hw_params_free(hw_params);
                return enif_make_badarg(env);
            }
        }
    } else {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_badarg(env);
    }

    return 0;
}

ERL_NIF_TERM get_sw_params_map(ErlNifEnv* env, snd_pcm_t *pcm_handle,
                               ERL_NIF_TERM *sw_params_map) {
    int err;

    snd_pcm_sw_params_t *sw_params;
    if ((err = snd_pcm_sw_params_malloc(&sw_params)) < 0) {
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_sw_params_current(pcm_handle, sw_params)) < 0) {
        snd_pcm_sw_params_free(sw_params);
        return enif_make_int(env, err);
    }

    snd_pcm_uframes_t start_threshold;
    snd_pcm_sw_params_get_start_threshold(sw_params, &start_threshold);

    ERL_NIF_TERM sw_params_keys[1] = {ATOM(start_threshold)};
    ERL_NIF_TERM sw_params_values[1] = {enif_make_uint(env, start_threshold)};

    enif_make_map_from_arrays(env, sw_params_keys, sw_params_values, 1,
                              sw_params_map);
    return 0;
}

ERL_NIF_TERM set_sw_params_map(ErlNifEnv* env, snd_pcm_t *pcm_handle,
                               ERL_NIF_TERM sw_params_map) {
    int err;

    snd_pcm_sw_params_t *sw_params;
    if ((err = snd_pcm_sw_params_malloc(&sw_params)) < 0) {
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_sw_params_current(pcm_handle, sw_params)) < 0) {
        snd_pcm_sw_params_free(sw_params);
        return enif_make_int(env, err);
    }

    size_t size;
    if (enif_get_map_size(env, sw_params_map, &size)) {
        if (size > 0) {
            ErlNifMapIterator iter;
            if (enif_map_iterator_create(env, sw_params_map, &iter,
                                         ERL_NIF_MAP_ITERATOR_FIRST)) {
                ERL_NIF_TERM key, value, reason = 0;
                bool badarg = false;
                while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
                    if (key == ATOM(start_threshold)) {
                        snd_pcm_uframes_t start_threshold;
                        if (enif_get_ulong(env, value, &start_threshold)) {
                            if ((err = snd_pcm_sw_params_set_start_threshold(
                                           pcm_handle, sw_params,
                                           start_threshold)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(start_threshold),
                                                          value,
                                                          enif_make_int(env,
                                                                        err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else {
                        badarg = true;
                        break;
                    }
                    enif_map_iterator_next(env, &iter);
                }
                enif_map_iterator_destroy(env, &iter);

                if (badarg) {
                    snd_pcm_sw_params_free(sw_params);
                    return enif_make_badarg(env);
                } else if (reason != 0) {
                    snd_pcm_sw_params_free(sw_params);
                    return reason;
                } else {
                    if ((err = snd_pcm_sw_params(pcm_handle,
                                                 sw_params)) < 0) {
                        snd_pcm_sw_params_free(sw_params);
                        return enif_make_int(env, err);
                    }

                    snd_pcm_sw_params_free(sw_params);
                    return 0;
                }
            } else {
                snd_pcm_sw_params_free(sw_params);
                return enif_make_badarg(env);
            }
        }
    } else {
        snd_pcm_sw_params_free(sw_params);
        return enif_make_badarg(env);
    }

    return 0;
}

/*
 * open
 */

#define MAX_DEVICE_NAME_LEN 64

static ERL_NIF_TERM _open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int err;

    char device_name[MAX_DEVICE_NAME_LEN];
    if (enif_get_string(env, argv[0], device_name, MAX_DEVICE_NAME_LEN,
                        ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }

    snd_pcm_stream_t stream;
    if (argv[1] == ATOM(playback)) {
        stream = SND_PCM_STREAM_PLAYBACK;
    } else if (argv[1] == ATOM(capture)) {
        stream = SND_PCM_STREAM_CAPTURE;
    } else {
        return enif_make_badarg(env);
    }

    session_t *session = malloc(sizeof(session_t));
    session->handle = next_handle;

    if ((err = snd_pcm_open(&session->pcm_handle, device_name, stream,
                            0)) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    ERL_NIF_TERM reason;
    if ((reason = set_hw_params_map(env, session->pcm_handle, argv[2])) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    ERL_NIF_TERM hw_params_map;
    if ((reason = get_hw_params_map(env, session->pcm_handle,
                                    &hw_params_map)) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    if ((reason = set_sw_params_map(env, session->pcm_handle, argv[3])) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    ERL_NIF_TERM sw_params_map;
    if ((reason = get_sw_params_map(env, session->pcm_handle,
                                    &sw_params_map)) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    add_session(session);

    if ((err = snd_pcm_prepare(session->pcm_handle)) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    return enif_make_tuple4(env, ATOM(ok),
                            enif_make_uint(env, next_handle++),
                            hw_params_map,
                            sw_params_map);
}

/*
 * close
 */

static ERL_NIF_TERM _close(ErlNifEnv* env, int argc,
                           const ERL_NIF_TERM argv[]) {
    int handle;
    if (!enif_get_int(env, argv[0], &handle)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }
    session_t *session = find_session(handle);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }
    snd_pcm_drain(session->pcm_handle);
    snd_pcm_close(session->pcm_handle);
    delete_session(session);

    return ATOM(ok);
}

/*
 * strerror
 */

#define MAX_STRERROR_LEN 256
static char strerror_buf[MAX_STRERROR_LEN];

static ERL_NIF_TERM _strerror(ErlNifEnv* env, int argc,
                              const ERL_NIF_TERM argv[]) {
    int err, arity;
    const ERL_NIF_TERM *terms;
    if (enif_get_int(env, argv[0], &err)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN, snd_strerror(err));
    } else if (argv[0] == ATOM(no_such_handle)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN, "Alsa handle is stale");
    } else if (argv[0] == ATOM(underrun)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "Failed to recover from underrun");
    } else if (argv[0] == ATOM(overrun)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "Failed to recover from overrun");
    } else if (argv[0] == ATOM(underrun)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "Failed to recover from suspend event");
    } else if (enif_get_tuple(env, argv[0], &arity, &terms)) {
        if (terms[0] == ATOM(bad_param) && arity == 4) {
            int err;
            enif_get_int(env, terms[3], &err);
            enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                          "%T cannot be %T (%s)", terms[1], terms[2],
                          snd_strerror(err));
        } else {
            enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                          "Unknown error: %T", argv[0]);
        }
    } else {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "Unknown error: %T", argv[0]);
    }

    return enif_make_string(env, strerror_buf, ERL_NIF_LATIN1);
}

/*
 * get_hw_params
 */

static ERL_NIF_TERM _get_hw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    int handle;
    if (!enif_get_int(env, argv[0], &handle)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }
    session_t *session = find_session(handle);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM hw_params_map, reason;
    if ((reason = get_hw_params_map(env, session->pcm_handle,
                                    &hw_params_map)) < 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    return enif_make_tuple2(env, ATOM(ok), hw_params_map);
}

/*
 * set_hw_params
 */

static ERL_NIF_TERM _set_hw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    int handle;
    if (!enif_get_int(env, argv[0], &handle)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }
    session_t *session = find_session(handle);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM reason;
    if ((reason = set_hw_params_map(env, session->pcm_handle, argv[1])) < 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    ERL_NIF_TERM hw_params_map;
    if ((reason = get_hw_params_map(env, session->pcm_handle,
                                    &hw_params_map)) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    return enif_make_tuple2(env, ATOM(ok), hw_params_map);
}

/*
 * get_sw_params
 */

static ERL_NIF_TERM _get_sw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    int handle;
    if (!enif_get_int(env, argv[0], &handle)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }
    session_t *session = find_session(handle);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM sw_params_map, reason;
    if ((reason = get_sw_params_map(env, session->pcm_handle,
                                    &sw_params_map)) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    return enif_make_tuple2(env, ATOM(ok), sw_params_map);
}

/*
 * set_sw_params
 */

static ERL_NIF_TERM _set_sw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    int handle;
    if (!enif_get_int(env, argv[0], &handle)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }
    session_t *session = find_session(handle);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM reason;
    if ((reason = set_sw_params_map(env, session->pcm_handle, argv[1])) < 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    ERL_NIF_TERM sw_params_map;
    if ((reason = get_sw_params_map(env, session->pcm_handle,
                                    &sw_params_map)) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    return enif_make_tuple2(env, ATOM(ok), sw_params_map);
}

/*
 * read
 */

static ERL_NIF_TERM _read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int handle;
    if (!enif_get_int(env, argv[0], &handle)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }
    session_t *session = find_session(handle);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    snd_pcm_uframes_t frames;
    if (!enif_get_ulong(env, argv[1], &frames)) {
        return enif_make_badarg(env);
    }

    ssize_t binlen = snd_pcm_frames_to_bytes(session->pcm_handle, frames);
    ErlNifBinary bin;
    if (!enif_alloc_binary(binlen, &bin)) {
        return enif_make_badarg(env);
    }

    snd_pcm_sframes_t read_frames =
        snd_pcm_readi(session->pcm_handle, bin.data, frames);

    if (read_frames == -EPIPE) {
        if (snd_pcm_recover(session->pcm_handle, read_frames, 0) < 0) {
            enif_release_binary(&bin);
            return enif_make_tuple2(env, ATOM(error), ATOM(overrun));
        }
        enif_release_binary(&bin);
        return enif_make_tuple2(env, ATOM(ok), ATOM(overrun));
    } else if (read_frames == -ESTRPIPE) {
        if (snd_pcm_recover(session->pcm_handle, read_frames, 0) < 0) {
            enif_release_binary(&bin);
            return enif_make_tuple2(env, ATOM(error), ATOM(suspend_event));
        }
        enif_release_binary(&bin);
        return enif_make_tuple2(env, ATOM(ok), ATOM(suspend_event));
    } else if (read_frames < 0) {
        enif_release_binary(&bin);
        return enif_make_tuple2(env, ATOM(error),
                                enif_make_int(env, read_frames));
    } else {
        return enif_make_tuple2(env, ATOM(ok), enif_make_binary(env, &bin));
    }
}

/*
 * write
 */

static ERL_NIF_TERM _write(ErlNifEnv* env, int argc,
                           const ERL_NIF_TERM argv[]) {
    int handle;
    if (!enif_get_int(env, argv[0], &handle)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }
    session_t *session = find_session(handle);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ErlNifBinary bin;
    if (enif_inspect_binary(env, argv[1], &bin)) {
        snd_pcm_uframes_t frames =
            snd_pcm_bytes_to_frames(session->pcm_handle, bin.size);
        if (frames == 0) {
            return enif_make_tuple2(env, ATOM(error), ATOM(too_little_data));
        }
        snd_pcm_sframes_t written_frames =
            snd_pcm_writei(session->pcm_handle, bin.data, frames);
        if (written_frames == -EPIPE) {
            if (snd_pcm_recover(session->pcm_handle, written_frames, 0) < 0) {
                return enif_make_tuple2(env, ATOM(error), ATOM(underrun));
            }
            return enif_make_tuple2(env, ATOM(ok), ATOM(underrun));
        } else if (written_frames == -ESTRPIPE) {
            if (snd_pcm_recover(session->pcm_handle, written_frames, 0) < 0) {
                return enif_make_tuple2(env, ATOM(error), ATOM(suspend_event));
            }
            return enif_make_tuple2(env, ATOM(ok), ATOM(suspend_event));
        } else if (written_frames < 0) {
            return enif_make_tuple2(env, ATOM(error),
                                    enif_make_int(env, written_frames));
        } else {
            return enif_make_tuple2(env, ATOM(ok),
                                    enif_make_int(env, written_frames));
        }
    } else {
        return enif_make_badarg(env);
    }
}

/*
 * load
 */

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(no_such_handle);

    LOAD_ATOM(playback);
    LOAD_ATOM(capture);

    LOAD_ATOM(bad_param);
    LOAD_ATOM(format);
    LOAD_ATOM(format);
    LOAD_ATOM(channels);
    LOAD_ATOM(rate);
    LOAD_ATOM(period_size);
    LOAD_ATOM(buffer_size);

    LOAD_ATOM(start_threshold);

    LOAD_ATOM(too_little_data);
    LOAD_ATOM(underrun);
    LOAD_ATOM(overrun);
    LOAD_ATOM(suspend_event);

    return 0;
}

/*
 * unload
 */

static void unload(ErlNifEnv* env, void* priv_data) {
}

/*
 * NIF functions registration
 */

static ErlNifFunc nif_funcs[] =
    {
     {"open", 4, _open, 0},
     {"close", 1, _close, 0},
     {"strerror", 1, _strerror, 0},
     {"get_hw_params", 1, _get_hw_params, 0},
     {"set_hw_params", 2, _set_hw_params, 0},
     {"get_sw_params", 1, _get_sw_params, 0},
     {"set_sw_params", 2, _set_sw_params, 0},
     {"read", 2, _read, ERL_NIF_DIRTY_JOB_IO_BOUND},
     {"write", 2, _write, ERL_NIF_DIRTY_JOB_IO_BOUND}
    };

ERL_NIF_INIT(alsa, nif_funcs, load, NULL, NULL, unload);

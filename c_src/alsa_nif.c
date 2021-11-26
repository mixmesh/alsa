#include <stdbool.h>
#include <erl_nif.h>
#include <alsa/asoundlib.h>
#include "uthash/uthash.h"

#define ATOM(name) atm_##name

#define DECL_ATOM(name) ERL_NIF_TERM atm_##name = 0

// FIXME: Use enif_make_existing_atom(env, #name) (return -1 if it fails)
#define LOAD_ATOM(name) atm_##name = enif_make_atom(env, #name)

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
    //fprintf(stderr, "get_hw_params: %p:%d:%d:%d:%ld:%ld\n",
    //        pcm_handle, format, channels, rate, period_size, buffer_size);

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
                            if ((err = snd_pcm_hw_params_set_format(
                                           pcm_handle, hw_params,
                                           format)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(format),
                                                          value,
                                                          enif_make_int(env, err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(channels)) {
                        unsigned int channels;
                        if (enif_get_uint(env, value, &channels)) {
                            if ((err = snd_pcm_hw_params_set_channels(
                                           pcm_handle, hw_params,
                                           channels)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(channels),
                                                          value,
                                                          enif_make_int(env, err));
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
                            if ((err = snd_pcm_hw_params_set_rate_near(
                                           pcm_handle, hw_params, &rate, &dir)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(rate),
                                                          value,
                                                          enif_make_int(env, err));
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
                            if ((err = snd_pcm_hw_params_set_period_size_near(
                                           pcm_handle, hw_params,
                                           &period_size, &dir)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(period_size),
                                                          value,
                                                          enif_make_int(env, err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(buffer_size)) {
                        snd_pcm_uframes_t buffer_size;
                        if (enif_get_ulong(env, value, &buffer_size)) {
                            if ((err = snd_pcm_hw_params_set_buffer_size_near(
                                           pcm_handle, hw_params,
                                           &buffer_size)) < 0) {
                                reason = enif_make_tuple4(env,
                                                          ATOM(bad_param),
                                                          ATOM(buffer_size),
                                                          value,
                                                          enif_make_int(env, err));
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
                                                          enif_make_int(env, err));
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

    if ((err = snd_pcm_open(&session->pcm_handle, device_name, stream, 0)) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    ERL_NIF_TERM reason;
    if ((reason = set_hw_params_map(env, session->pcm_handle, argv[2])) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    ERL_NIF_TERM hw_params_map;
    if ((reason = get_hw_params_map(env, session->pcm_handle, &hw_params_map)) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    if ((reason = set_sw_params_map(env, session->pcm_handle, argv[3])) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    ERL_NIF_TERM sw_params_map;
    if ((reason = get_sw_params_map(env, session->pcm_handle, &sw_params_map)) != 0) {
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
 * get_hw_params
 */

static ERL_NIF_TERM _get_hw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    session_t *session = find_session(argv[0]);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM hw_params_map, reason;
    if ((reason = get_hw_params_map(env, session->pcm_handle, &hw_params_map)) < 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    return enif_make_tuple2(env, ATOM(ok), hw_params_map);
}

/*
 * set_hw_params
 */

static ERL_NIF_TERM _set_hw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    session_t *session = find_session(argv[0]);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM reason;
    if ((reason = set_hw_params_map(env, session->pcm_handle, argv[1])) < 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    ERL_NIF_TERM hw_params_map;
    if ((reason = get_hw_params_map(env, session->pcm_handle, &hw_params_map)) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    return enif_make_tuple2(env, ATOM(ok), hw_params_map);
}

/*
 * get_sw_params
 */

static ERL_NIF_TERM _get_sw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    session_t *session = find_session(argv[0]);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM sw_params_map, reason;
    if ((reason = get_sw_params_map(env, session->pcm_handle, &sw_params_map)) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    return enif_make_tuple2(env, ATOM(ok), sw_params_map);
}

/*
 * set_sw_params
 */

static ERL_NIF_TERM _set_sw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    session_t *session = find_session(argv[0]);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM reason;
    if ((reason = set_sw_params_map(env, session->pcm_handle, argv[1])) < 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    ERL_NIF_TERM sw_params_map;
    if ((reason = get_sw_params_map(env, session->pcm_handle, &sw_params_map)) != 0) {
        return enif_make_tuple2(env, ATOM(error), reason);
    }

    return enif_make_tuple2(env, ATOM(ok), sw_params_map);
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
    } else if (enif_get_tuple(env, argv[0], &arity, &terms)) {
        if (terms[0] == ATOM(bad_param) && arity == 4) {
            int err;
            enif_get_int(env, terms[3], &err);
            enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                          "%T cannot be %T (%s)", terms[1], terms[2],
                          snd_strerror(err));
        } else {
            enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                          "Unknown error1: %T %T %d", argv[0], terms[0], arity);
        }
    } else {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "Unknown error2: %T", argv[0]);
    }

    return enif_make_string(env, strerror_buf, ERL_NIF_LATIN1);
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
     {"get_hw_params", 1, _get_hw_params, 0},
     {"set_hw_params", 2, _set_hw_params, 0},
     {"get_sw_params", 1, _get_sw_params, 0},
     {"set_sw_params", 2, _set_sw_params, 0},
     {"strerror", 1, _strerror, 0},
     /*
     {"close", 1, _close, 0},
     {"strerror", 1, _strerror, 0},
     {"get_hw_params", 1, _get_hw_params, 0},
     {"set_hw_params", 2, _set_hw_params, 0},
     {"get_sw_params", 1, _get_sw_params, 0},

     {"read", 2, _read, ERL_NIF_DIRTY_JOB_IO_BOUND},
     {"write", 2, _write, ERL_NIF_DIRTY_JOB_IO_BOUND},
     {"prepare", 1, _prepare, 0},
     {"recover", 3, _recover, 0},
     {"drain", 1, _drain, 0}
     */
    };

ERL_NIF_INIT(alsa, nif_funcs, load, NULL, NULL, unload);

/*
strerror(overrun) ->
    "An overrun occurred";
strerror(waiting_for_recovery) ->
    "Stream is suspended and waiting for an application recovery";
strerror(not_prepared_nor_running) ->
    "Stream not prepared nor running";
*/

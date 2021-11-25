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

DECL_ATOM(format);
DECL_ATOM(channels);
DECL_ATOM(rate);
DECL_ATOM(period_size);
DECL_ATOM(buffer_size);

DECL_ATOM(start_threshold);

typedef struct {
    int handle;
    snd_pcm_t *pcm;
    UT_hash_handle hh;
} session_t;

session_t *sessions = NULL;

void add_session(session_t *session) {
    HASH_ADD_INT(sessions, handle, session);
}

session_t *find_session(int handle) {
    session_t *session;
    HASH_FIND_INT(sessions, &handle, session);
    return session;
}

uint32_t next_handle = 0;

int get_hw_params_map(ErlNifEnv* env, snd_pcm_t *pcm,
                      ERL_NIF_TERM *hw_params_map) {
    int err;

    snd_pcm_hw_params_t *hw_params;
    if ((err = snd_pcm_hw_params_malloc(&hw_params)) < 0) {
        return err;
    }

    if ((err = snd_pcm_hw_params_any(pcm, hw_params)) < 0) {
        snd_pcm_hw_params_free(hw_params);
        return err;
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

    ERL_NIF_TERM hw_params_keys =
        enif_make_list5(env,
                        ATOM(format),
                        ATOM(channels),
                        ATOM(rate),
                        ATOM(period_size),
                        ATOM(buffer_size));
    ERL_NIF_TERM hw_params_values =
        enif_make_list5(env,
                        enif_make_uint(env, format),
                        enif_make_uint(env, channels),
                        enif_make_uint(env, rate),
                        enif_make_uint(env, period_size),
                        enif_make_uint(env, buffer_size));

    enif_make_map_from_arrays(env, &hw_params_keys, &hw_params_values, 5,
                              hw_params_map);
    return 0;
}

int get_sw_params_map(ErlNifEnv* env, snd_pcm_t *pcm,
                      ERL_NIF_TERM *sw_params_map) {
    int err;

    snd_pcm_sw_params_t *sw_params;
    if ((err = snd_pcm_sw_params_malloc(&sw_params)) < 0) {
        return err;
    }

    if ((err = snd_pcm_sw_params_current(pcm, sw_params)) < 0) {
        snd_pcm_sw_params_free(sw_params);
        return err;
    }

    snd_pcm_uframes_t start_threshold;
    snd_pcm_sw_params_get_start_threshold(sw_params, &start_threshold);

    ERL_NIF_TERM sw_params_keys = enif_make_list1(env, ATOM(start_threshold));
    ERL_NIF_TERM sw_params_values =
        enif_make_list1(env, enif_make_uint(env, start_threshold));

    enif_make_map_from_arrays(env, &sw_params_keys, &sw_params_values, 5,
                              sw_params_map);
    return 0;
}

/*
 * open
 */

static ERL_NIF_TERM _open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int err;

    char device_name[64];
    if (enif_get_string(env, argv[0], device_name, 64, ERL_NIF_LATIN1) < 0) {
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

    // Add a new session
    session_t *session = malloc(sizeof(session_t));
    session->handle = next_handle;

    // Open audio device
    if ((err = snd_pcm_open(&session->pcm, device_name, stream, 0)) < 0) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM *hw_params_map;
    if ((err = get_hw_params_map(env, session->pcm, hw_params_map)) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    ERL_NIF_TERM *sw_params_map;
    if ((err = get_sw_params_map(env, session->pcm, sw_params_map)) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    add_session(session);

    return enif_make_tuple4(env, ATOM(ok),
                            enif_make_uint(env, next_handle++),
                            *hw_params_map,
                            *sw_params_map);
}

/*
 * get_hw_params
 */

static ERL_NIF_TERM _get_hw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    int err;

    session_t *session;
    find_session(argv[0]);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM *hw_params_map;
    if ((err = get_hw_params_map(env, session->pcm, hw_params_map)) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    return enif_make_tuple2(env, ATOM(ok), *hw_params_map);
}

/*
 * set_hw_params
 */

static ERL_NIF_TERM _set_hw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    int err = 0;

    session_t *session;
    find_session(argv[0]);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    snd_pcm_hw_params_t *hw_params;
    if ((err = snd_pcm_hw_params_malloc(&hw_params)) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    if ((err = snd_pcm_hw_params_any(session->pcm, hw_params)) < 0) {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    size_t size;
    if (enif_get_map_size(env, argv[1], &size)) {
        if (size > 0) {
            ErlNifMapIterator iter;
            if (enif_map_iterator_create(env, argv[1], &iter,
                                         ERL_NIF_MAP_ITERATOR_FIRST)) {
                ERL_NIF_TERM key, value;
                bool badarg = false;
                while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
                    if (key == ATOM(format)) {
                        snd_pcm_format_t format;
                        if (enif_get_int(env, value, &format)) {
                            if ((err = snd_pcm_hw_params_set_format(
                                           session->pcm, hw_params,
                                           format)) < 0) {
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
                                           session->pcm, hw_params,
                                           channels)) < 0) {
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(rate)) {
                        unsigned int rate;
                        if (enif_get_uint(env, value, &rate)) {
                            if ((err = snd_pcm_hw_params_set_rate(
                                           session->pcm, hw_params, rate,
                                           0)) < 0) {
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
                                           session->pcm, hw_params,
                                           &period_size, &dir)) < 0) {
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
                                           session->pcm, hw_params,
                                           &buffer_size)) < 0) {
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
                } else if (err != 0) {
                    snd_pcm_hw_params_free(hw_params);
                    return enif_make_tuple2(env,
                                            ATOM(error),
                                            enif_make_int(env, err));
                } else {
                    if ((err = snd_pcm_hw_params(session->pcm,
                                                 hw_params)) < 0) {
                        snd_pcm_hw_params_free(hw_params);
                        return enif_make_tuple2(env,
                                                ATOM(error),
                                                enif_make_int(env, err));
                    }

                    snd_pcm_hw_params_free(hw_params);

                    ERL_NIF_TERM *hw_params_map;
                    if ((err = get_hw_params_map(env, session->pcm,
                                                 hw_params_map)) < 0) {
                        return enif_make_tuple2(env, ATOM(error),
                                                enif_make_int(env, err));
                    }

                    return enif_make_tuple2(env, ATOM(ok), *hw_params_map);
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
}


/*
 * get_sw_params
 */

static ERL_NIF_TERM _get_sw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    int err;

    session_t *session;
    find_session(argv[0]);
    if (session == NULL) {
        return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
    }

    ERL_NIF_TERM *sw_params_map;
    if ((err = get_sw_params_map(env, session->pcm, sw_params_map)) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    return enif_make_tuple2(env, ATOM(ok), *sw_params_map);
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
     {"open", 2, _open, 0},
     {"get_hw_params", 1, _get_hw_params, 0},
     {"set_hw_params", 2, _set_hw_params, 0},
     {"get_sw_params", 1, _get_hw_params, 0},
     /*
     {"close", 1, _close, 0},
     {"strerror", 1, _strerror, 0},
     {"get_hw_params", 1, _get_hw_params, 0},
     {"set_hw_params", 2, _set_hw_params, 0},
     {"get_sw_params", 1, _get_sw_params, 0},
     {"set_sw_params", 2, _set_sw_params, 0},
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

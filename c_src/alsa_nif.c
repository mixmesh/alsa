#include <erl_nif.h>
#include <alsa/asoundlib.h>
#include "uthash/uthash.h"

#define ATOM(name) atm_##name

#define DECL_ATOM(name)                         \
    ERL_NIF_TERM atm_##name = 0

// FIXME: Use enif_make_existing_atom(env, #name) (return -1 if it fails)
#define LOAD_ATOM(name)                                 \
    atm_##name = enif_make_atom(env, #name)

DECL_ATOM(playback);
DECL_ATOM(capture);

DECL_ATOM(format);
DECL_ATOM(channels);
DECL_ATOM(rate);
DECL_ATOM(period_size);
DECL_ATOM(buffer_size);

DECL_ATOM(s8);
DECL_ATOM(u8);
DECL_ATOM(s16_le);
DECL_ATOM(s16_be);
DECL_ATOM(u16_le);
DECL_ATOM(u16_be);
DECL_ATOM(s24_le);
DECL_ATOM(s24_be);
DECL_ATOM(u24_le);
DECL_ATOM(u24_be);
DECL_ATOM(s32_le);
DECL_ATOM(s32_be);
DECL_ATOM(u32_le);
DECL_ATOM(u32_be);
DECL_ATOM(float_le);
DECL_ATOM(float_be);
DECL_ATOM(float64_le);
DECL_ATOM(float64_be);
DECL_ATOM(iec958_subframe_le);
DECL_ATOM(iec958_subframe_be);
DECL_ATOM(mu_law);
DECL_ATOM(a_law);
DECL_ATOM(ima_adpcm);
DECL_ATOM(mpeg);
DECL_ATOM(gsm);
DECL_ATOM(s20_le);
DECL_ATOM(s20_be);
DECL_ATOM(u20_le);
DECL_ATOM(u20_be);
DECL_ATOM(special);
DECL_ATOM(s24_3le);
DECL_ATOM(s24_3be);
DECL_ATOM(u24_3le);
DECL_ATOM(u24_3be);
DECL_ATOM(s20_3le);
DECL_ATOM(s20_3be);
DECL_ATOM(u20_3le);
DECL_ATOM(u20_3be);
DECL_ATOM(s18_3le);
DECL_ATOM(s18_3be);
DECL_ATOM(u18_3le);
DECL_ATOM(u18_3be);

typedef struct {
    uint16_t handle;
    snd_pcm_t *pcm;
    UT_hash_handle hh;
} session_t;

session_t *sessions = NULL;

void add_session(session_t *session) {
    HASH_ADD_INT(sessions, handle, session);
}

uint32_t next_free_handle = 0;

/*
 * open
 */

static ERL_NIF_TERM _open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int err;

    // Extract parameters
    char device_name[64];
    if (enif_get_string(env, argv[0], device_name, 64, ERL_NIF_LATIN1) < 0) {
        return enif_make_badarg(env);
    }

    snd_pcm_stream_t stream;
    if (ATOM(playback) == argv[1]) {
        stream = SND_PCM_STREAM_PLAYBACK;
    } else if (argv[1] == ATOM(capture)) {
        stream = SND_PCM_STREAM_CAPTURE;
    } else {
        return enif_make_badarg(env);
    }


    // Add a new session
    session_t *session = malloc(sizeof(session_t));
    session->handle = next_free_handle;
    add_session(session);

    // Open audio device
    if ((err = snd_pcm_open(&session->pcm, device_name, stream, 0)) < 0) {
        return enif_make_badarg(env);
    }

    // Set hardware parameters
    snd_pcm_hw_params_t *hw_params;
    if (snd_pcm_hw_params_malloc(&hw_params) < 0) {
        return enif_make_badarg(env);
    }
    if (snd_pcm_hw_params_any(session->pcm, hw_params) < 0) {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_badarg(env);
    }

    size_t size;
    if (enif_get_map_size(env, argv[2], &size)) {
        if (size > 0) {
            ErlNifMapIterator iter;
            if (enif_map_iterator_create(env, argv[2], &iter,
                                         ERL_NIF_MAP_ITERATOR_FIRST)) {
                ERL_NIF_TERM key, value;
                while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
                    if (key == ATOM(format)) {
                        snd_pcm_format_t format;
                        if (value == ATOM(s8)) {
                            format = SND_PCM_FORMAT_S8;
                        } else if (value == ATOM(u8)) {
                            format = SND_PCM_FORMAT_U8;
                        } else if (value == ATOM(s16_le)) {
                            format = SND_PCM_FORMAT_S16_LE;
                        } else if (value == ATOM(s16_be)) {
                            format = SND_PCM_FORMAT_S16_BE;
                        } else if (value == ATOM(u16_le)) {
                            format = SND_PCM_FORMAT_U16_LE;
                        } else if (value == ATOM(u16_be)) {
                            format = SND_PCM_FORMAT_U16_BE;
                        } else if (value == ATOM(s24_le)) {
                            format = SND_PCM_FORMAT_S24_LE;
                        } else if (value == ATOM(s24_be)) {
                            format = SND_PCM_FORMAT_S24_BE;
                        } else if (value == ATOM(u24_le)) {
                            format = SND_PCM_FORMAT_U24_LE;
                        } else if (value == ATOM(u24_be)) {
                            format = SND_PCM_FORMAT_U24_BE;
                        } else if (value == ATOM(s32_le)) {
                            format = SND_PCM_FORMAT_S32_LE;
                        } else if (value == ATOM(s32_be)) {
                            format = SND_PCM_FORMAT_S32_BE;
                        } else if (value == ATOM(u32_le)) {
                            format = SND_PCM_FORMAT_U32_LE;
                        } else if (value == ATOM(u32_be)) {
                            format = SND_PCM_FORMAT_U32_BE;
                        } else if (value == ATOM(float_le)) {
                            format = SND_PCM_FORMAT_FLOAT_LE;
                        } else if (value == ATOM(float_be)) {
                            format = SND_PCM_FORMAT_FLOAT_BE;
                        } else if (value == ATOM(float64_le)) {
                            format = SND_PCM_FORMAT_FLOAT_LE;
                        } else if (value == ATOM(float64_be)) {
                            format = SND_PCM_FORMAT_FLOAT64_BE;
                        } else if (value == ATOM(iec958_subframe_le)) {
                            format = SND_PCM_FORMAT_FLOAT64_BE;
                        } else if (value == ATOM(iec958_subframe_le)) {
                            format = SND_PCM_FORMAT_IEC958_SUBFRAME_LE;
                        } else if (value == ATOM(iec958_subframe_be)) {
                            format = SND_PCM_FORMAT_IEC958_SUBFRAME_BE;
                        } else if (value == ATOM(mu_law)) {
                            format = SND_PCM_FORMAT_MU_LAW;
                        } else if (value == ATOM(a_law)) {
                            format = SND_PCM_FORMAT_A_LAW;
                        } else if (value == ATOM(ima_adpcm)) {
                            format = SND_PCM_FORMAT_IMA_ADPCM;
                        } else if (value == ATOM(mpeg)) {
                            format = SND_PCM_FORMAT_MPEG;
                        } else if (value == ATOM(gsm)) {
                            format = SND_PCM_FORMAT_GSM;
                        } else if (value == ATOM(s20_le)) {
                            format = SND_PCM_FORMAT_S20_LE;
                        } else if (value == ATOM(s20_be)) {
                            format = SND_PCM_FORMAT_S20_BE;
                        } else if (value == ATOM(u20_le)) {
                            format = SND_PCM_FORMAT_U20_LE;
                        } else if (value == ATOM(u20_be)) {
                            format = SND_PCM_FORMAT_U20_BE;
                        } else if (value == ATOM(special)) {
                            format = SND_PCM_FORMAT_SPECIAL;
                        } else if (value == ATOM(s24_3le)) {
                            format = SND_PCM_FORMAT_S24_3LE;
                        } else if (value == ATOM(s24_3be)) {
                            format = SND_PCM_FORMAT_S24_3BE;
                        } else if (value == ATOM(u24_3le)) {
                            format = SND_PCM_FORMAT_U24_3LE;
                        } else if (value == ATOM(u24_3be)) {
                            format = SND_PCM_FORMAT_U24_3BE;
                        } else if (value == ATOM(s20_3le)) {
                            format = SND_PCM_FORMAT_S20_3LE;
                        } else if (value == ATOM(s20_3be)) {
                            format = SND_PCM_FORMAT_S20_3BE;
                        } else if (value == ATOM(u20_3le)) {
                            format = SND_PCM_FORMAT_U20_3LE;
                        } else if (value == ATOM(u20_3be)) {
                            format = SND_PCM_FORMAT_U20_3BE;
                        } else if (value == ATOM(s18_3le)) {
                            format = SND_PCM_FORMAT_S18_3LE;
                        } else if (value == ATOM(s18_3be)) {
                            format = SND_PCM_FORMAT_S18_3BE;
                        } else if (value == ATOM(u18_3le)) {
                            format = SND_PCM_FORMAT_U18_3LE;
                        } else if (value == ATOM(u18_3be)) {
                            format = SND_PCM_FORMAT_U18_3BE;
                        } else {
                            snd_pcm_hw_params_free(hw_params);
                            enif_map_iterator_destroy(env, &iter);
                            return enif_make_badarg(env);
                        }
                        if (snd_pcm_hw_params_set_format(session->pcm, hw_params,
                                                         format) < 0) {
                            snd_pcm_hw_params_free(hw_params);
                            enif_map_iterator_destroy(env, &iter);
                            return enif_make_badarg(env);
                        }
                    } else if (key == ATOM(channels)) {
                        int channels;
                        if (enif_get_int(env, value, &channels)) {
                            if (snd_pcm_hw_params_set_channels(session->pcm,
                                                               hw_params,
                                                               channels) < 0) {
                                snd_pcm_hw_params_free(hw_params);
                                enif_map_iterator_destroy(env, &iter);
                            }
                        } else {
                            snd_pcm_hw_params_free(hw_params);
                            enif_map_iterator_destroy(env, &iter);
                        }
                    } else if (key == ATOM(rate)) {
                        int rate;
                        if (enif_get_int(env, value, &rate)) {
                            if (snd_pcm_hw_params_set_rate(session->pcm,
                                                           hw_params,
                                                           rate,
                                                           0) < 0) {
                                snd_pcm_hw_params_free(hw_params);
                                enif_map_iterator_destroy(env, &iter);
                            }
                        } else {
                            snd_pcm_hw_params_free(hw_params);
                            enif_map_iterator_destroy(env, &iter);
                        }
                    } else if (key == ATOM(period_size)) {
                        int period_size;
                        if (enif_get_int(env, value, &period_size)) {
                            if (snd_pcm_hw_params_set_period_size(
                                  session->pcm, hw_params, period_size,
                                  0) < 0) {
                                snd_pcm_hw_params_free(hw_params);
                                enif_map_iterator_destroy(env, &iter);
                            }
                        } else {
                            snd_pcm_hw_params_free(hw_params);
                            enif_map_iterator_destroy(env, &iter);
                        }
                    } else if (key == ATOM(buffer_size)) {
                        int buffer_size;
                        if (enif_get_int(env, value, &buffer_size)) {
                            if (snd_pcm_hw_params_set_buffer_size(
                                    session->pcm, hw_params, buffer_size) < 0) {
                                snd_pcm_hw_params_free(hw_params);
                                enif_map_iterator_destroy(env, &iter);
                            }
                        } else {
                            snd_pcm_hw_params_free(hw_params);
                            enif_map_iterator_destroy(env, &iter);
                        }
                    } else {
                        snd_pcm_hw_params_free(hw_params);
                        enif_map_iterator_destroy(env, &iter);
                        return enif_make_badarg(env);
                    }
                    enif_map_iterator_next(env, &iter);
                }
                enif_map_iterator_destroy(env, &iter);

                if ((err = snd_pcm_hw_params(session->pcm, hw_params)) < 0) {
                    snd_pcm_hw_params_free(hw_params);
                    return enif_make_badarg(env);
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

    return enif_make_badarg(env);
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    LOAD_ATOM(playback);
    LOAD_ATOM(capture);

    LOAD_ATOM(format);
    LOAD_ATOM(format);
    LOAD_ATOM(channels);
    LOAD_ATOM(rate);
    LOAD_ATOM(period_size);
    LOAD_ATOM(buffer_size);

    LOAD_ATOM(s8);
    LOAD_ATOM(u8);
    LOAD_ATOM(s16_le);
    LOAD_ATOM(s16_be);
    LOAD_ATOM(u16_le);
    LOAD_ATOM(u16_be);
    LOAD_ATOM(s24_le);
    LOAD_ATOM(s24_be);
    LOAD_ATOM(u24_le);
    LOAD_ATOM(u24_be);
    LOAD_ATOM(s32_le);
    LOAD_ATOM(s32_be);
    LOAD_ATOM(u32_le);
    LOAD_ATOM(u32_be);
    LOAD_ATOM(float_le);
    LOAD_ATOM(float_be);
    LOAD_ATOM(float64_le);
    LOAD_ATOM(float64_be);
    LOAD_ATOM(iec958_subframe_le);
    LOAD_ATOM(iec958_subframe_be);
    LOAD_ATOM(mu_law);
    LOAD_ATOM(a_law);
    LOAD_ATOM(ima_adpcm);
    LOAD_ATOM(mpeg);
    LOAD_ATOM(gsm);
    LOAD_ATOM(s20_le);
    LOAD_ATOM(s20_be);
    LOAD_ATOM(u20_le);
    LOAD_ATOM(u20_be);
    LOAD_ATOM(special);
    LOAD_ATOM(s24_3le);
    LOAD_ATOM(s24_3be);
    LOAD_ATOM(u24_3le);
    LOAD_ATOM(u24_3be);
    LOAD_ATOM(s20_3le);
    LOAD_ATOM(s20_3be);
    LOAD_ATOM(u20_3le);
    LOAD_ATOM(u20_3be);
    LOAD_ATOM(s18_3le);
    LOAD_ATOM(s18_3be);
    LOAD_ATOM(u18_3le);
    LOAD_ATOM(u18_3be);
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
}

static ErlNifFunc nif_funcs[] =
    {
     {"open", 4, _open, 0},
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

ERL_NIF_INIT(malsa, nif_funcs, load, NULL, NULL, unload);

/*
strerror(overrun) ->
    "An overrun occurred";
strerror(waiting_for_recovery) ->
    "Stream is suspended and waiting for an application recovery";
strerror(not_prepared_nor_running) ->
    "Stream not prepared nor running";
*/

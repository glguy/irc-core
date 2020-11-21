#ifndef GLIRC_API
#define GLIRC_API

#include <stdlib.h>

struct glirc;

enum message_code {
        NORMAL_MESSAGE = 0,
        ERROR_MESSAGE  = 1
};

enum process_result {
        PASS_MESSAGE = 0,
        DROP_MESSAGE = 1
};

struct glirc_string {
        const char *str;
        size_t len;
};

struct glirc_message {
        struct glirc_string network;
        struct glirc_string prefix_nick;
        struct glirc_string prefix_user;
        struct glirc_string prefix_host;
        struct glirc_string command;
        const struct glirc_string *params;
        size_t params_n;
        const struct glirc_string *tagkeys;
        const struct glirc_string *tagvals;
        size_t tags_n;
};

struct glirc_chat {
        struct glirc_string network;
        struct glirc_string target;
        struct glirc_string message;
};

struct glirc_command {
        struct glirc_string command;
};

typedef void *start_type         (struct glirc *G, const char *path, const struct glirc_string *args, size_t args_len);
typedef void stop_type           (void *S);
typedef enum process_result process_message_type(void *S, const struct glirc_message *);
typedef enum process_result process_chat_type(void *S, const struct glirc_chat *);
typedef void process_command_type(void *S, const struct glirc_command *);
typedef void process_thread_join(void *result);

typedef long timer_id;
typedef void timer_callback(void *dat, timer_id);

struct glirc_extension {
        const char *name;
        int major_version, minor_version;
        start_type           *start;
        stop_type            *stop;
        process_message_type *process_message;
        process_command_type *process_command;
        process_chat_type    *process_chat;
        process_thread_join  *process_thread_join;
};

int glirc_send_message(struct glirc *G, const struct glirc_message *);
int glirc_print(struct glirc *G, enum message_code, const char *msg, size_t msglen);
int glirc_inject_chat(struct glirc *G,
                const char* net, size_t netLen,
                const char* src, size_t srcLen,
                const char* tgt, size_t tgtLen,
                const char* msg, size_t msgLen);
char ** glirc_list_networks(struct glirc *G);
char ** glirc_list_channels(struct glirc *G, const char *net, size_t netlen);
char ** glirc_list_channel_users(struct glirc *G, const char *net, size_t net_len, const char *chan, size_t chan_len);
void glirc_current_focus(struct glirc *G, char **net, size_t *netlen, char **tgt , size_t *tgtlen);
void glirc_set_focus(struct glirc *G, const char *net, size_t netlen, const char *tgt , size_t tgtlen);
char * glirc_my_nick(struct glirc *G, const char *net, size_t netlen);
char * glirc_user_account(struct glirc *G, const char *net, size_t netlen, const char *nick, size_t nicklen);
char * glirc_user_channel_modes(struct glirc *G, const char *net, size_t netlen, const char *chan, size_t chanlen, const char *nick, size_t nicklen);
char ** glirc_channel_modes(struct glirc *G, const char *net, size_t netlen, const char *chan, size_t chanlen);
char ** glirc_channel_masks(struct glirc *G, const char *net, size_t netlen, const char *chan, size_t chanlen, char mode);
void glirc_mark_seen(struct glirc *G, const char *net, size_t net_len, const char *chan, size_t chan_len);
void glirc_clear_window(struct glirc *G, const char *net, size_t net_len, const char *chan, size_t chan_len);
int glirc_identifier_cmp(const char *s, size_t s_len, const char *t, size_t t_len);
int glirc_is_channel(struct glirc *G, const char *net, size_t netlen,
                                      const char *tgt, size_t tgtlen);
int glirc_is_logged_on(struct glirc *G, const char *net, size_t netlen,
                                        const char *tgt, size_t tgtlen);
char * glirc_resolve_path(struct glirc *G, const char *path, size_t path_len);
timer_id glirc_set_timer(struct glirc *G, unsigned long millis, timer_callback *cb, void *dat);
void *glirc_cancel_timer(struct glirc *G, timer_id tid);
char ** glirc_window_lines(struct glirc *G, const char *net, size_t netlen, const char *tgt, size_t tgtlen, int filtered);
void glirc_thread(struct glirc *G, void *(*start)(void *), void *arg);

void glirc_free_string(char *);
void glirc_free_strings(char **);

#endif

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

struct glirc_command {
        const struct glirc_string *params;
        size_t params_n;
};

typedef void *start_type         (struct glirc *G, const char *path);
typedef void stop_type           (struct glirc *G, void *S);
typedef enum process_result process_message_type(struct glirc *G, void *S, const struct glirc_message *);
typedef void process_command_type(struct glirc *G, void *S, const struct glirc_command *);

struct glirc_extension {
        const char *name;
        int major_version, minor_version;
        start_type           *start;
        stop_type            *stop;
        process_message_type *process_message;
        process_command_type *process_command;
};

int glirc_send_message(struct glirc *G, const struct glirc_message *);
int glirc_print(struct glirc *G, enum message_code, struct glirc_string msg);
int glirc_inject_chat(struct glirc *G,
                const char* net, size_t netLen,
                const char* src, size_t srcLen,
                const char* tgt, size_t tgtLen,
                const char* msg, size_t msgLen);
char ** glirc_list_networks(struct glirc *G);
char ** glirc_list_channels(struct glirc *G, struct glirc_string network);
char ** glirc_list_channel_users(struct glirc *G, struct glirc_string network, struct glirc_string channel);
char * glirc_my_nick(struct glirc *G, struct glirc_string network);
void glirc_mark_seen(struct glirc *G, struct glirc_string network, struct glirc_string channel);
void glirc_clear_window(struct glirc *G, struct glirc_string network, struct glirc_string channel);
int glirc_identifier_cmp(struct glirc_string s, struct glirc_string t);

void glirc_free_string(char *);
void glirc_free_strings(char **);

#endif

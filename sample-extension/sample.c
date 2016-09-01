#include <stdio.h>

#include "glirc-api.h"

static void *start(void *glirc, const char *path ) {
        FILE *file = fopen("sample-output.txt", "w");
        return file;
}

static void stop(void *glirc, void * S) {
        FILE *file = S;
        fclose(file);
}

static enum process_result process_message(void *glirc, void *S, const struct glirc_message *msg) {
        FILE *file = S;
        fwrite(msg->command.str, 1, msg->command.len, file);
        fputs("\n",file);
        fflush(file);
        return PASS_MESSAGE;
}

struct glirc_extension extension = {
        .name            = "sample",
        .major_version   = 1,
        .minor_version   = 0,
        .start           = start,
        .stop            = stop,
        .process_message = process_message,
        .process_command = NULL
};

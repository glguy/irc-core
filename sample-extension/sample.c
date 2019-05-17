/*
 * A simple extension that writes messages received to a file.
 */
#include <stdio.h>

#include "glirc-api.h"

static start_type start;
static stop_type stop;
static process_message_type process_message;
struct glirc_extension extension;

static void *start
  (struct glirc *glirc,
   const char *path,
   const struct glirc_string *args,
   size_t args_len)
{
  FILE *file = fopen("sample-output.txt", "w");
  return file;
}

static void stop(void *S) {
  FILE *file = S;
  fclose(file);
}

static enum process_result process_message
  (void *S,
   const struct glirc_message *msg)
{
  FILE *file = S;
  fwrite(msg->command.str, 1, msg->command.len, file);
  fputs("\n", file);
  fflush(file);
  return PASS_MESSAGE;
}

struct glirc_extension extension =
  {.name = "sample",
   .major_version = 1,
   .minor_version = 0,
   .start = start,
   .stop = stop,
   .process_message = process_message,
   .process_command = NULL};

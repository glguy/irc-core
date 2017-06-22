#define _GNU_SOURCE
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <libotr/proto.h>
#include <libotr/message.h>
#include <libotr/privkey.h>

#include "glirc-api.h"

#define NAME "OTR"
#define PLUGIN_USER "* OTR *"
#define MAJOR 1
#define MINOR 0

// IRC formatting escape sequences
#define PLAIN    "\17"
#define BOLD(x)  "\002"   x "\002"
#define GREEN(x) "\00303" x PLAIN
#define RED(x)   "\00304" x PLAIN

#define QUERY_TEXT "?OTRv23? This message is attempting to initiate an encrypted" \
                   " session, but your client doesn't support this protocol."

// These macros try to make it less error prone to marshal data
// into and out of the void *opdata parameters
#define GET_G      struct glirc * const G = ((struct my_opdata *)opdata)->G
#define GET_us     OtrlUserState const us = ((struct my_opdata *)opdata)->us
#define GET_opdata struct my_opdata opdata = { .G = G, .us = us }

// It is useful to store a copy of the user state in the opdata
// because some callback forget to provide it
struct my_opdata {
    struct glirc *G;
    OtrlUserState us;
};

// IRC identifiers use a Swedish character encoding. The important
// distinction from normal ASCII is that "{|}~" are the lowercased
// forms of "[\]^". We normalize account names according to this
// convention so that OTR account names align with the meaning of
// IRC nicknames.
static void normalizeCase(char *str)
{
  const char *casemap =
    "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f"
    "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f"
    " !\"#$%&'()*+,-./0123456789:;<=>?"
    "@abcdefghijklmnopqrstuvwxyz{|}~_"
    "`abcdefghijklmnopqrstuvwxyz{|}~\x7f"
    "\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f"
    "\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f"
    "\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf"
    "\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf"
    "\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf"
    "\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf"
    "\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef"
    "\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff";

  while (*str) {
    *str = casemap[*(unsigned char*)str];
    str++;
  }
}

void glirc_vprintf (struct glirc *G, const char *net, const char *src, const char *tgt,
                    const char *fmt, va_list ap)
{
  char *msg = NULL;
  int len = vasprintf(&msg, fmt, ap);

  if (0 > len || !msg) abort();

  glirc_inject_chat
    (G, net, strlen(net),
     src, strlen(src),
     tgt, strlen(tgt),
     msg, len);

  free(msg);
}

static void glirc_printf(struct glirc *, const char *, const char *, const char *, const char *, ...)
__attribute__ ((format (printf, 5, 6)));

void glirc_printf (struct glirc *G, const char *net, const char *src,
                   const char *tgt, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  glirc_vprintf(G, net, src, tgt, fmt, ap);
  va_end(ap);
}

static void print_status(struct glirc *, ConnContext *, const char *, ...)
__attribute__ ((format (printf, 3, 4)));

void print_status(struct glirc *G, ConnContext *context, const char *fmt, ...)
{
  const char *net = context->protocol;
  const char *src = PLUGIN_USER;
  const char *tgt = context->username;

  va_list ap;
  va_start(ap, fmt);
  glirc_vprintf(G, net, src, tgt, fmt, ap);
  va_end(ap);
}


// Returns true if the two strings match, false otherwise
static inline bool match_string(const char *x, struct glirc_string y)
{
  return !strcmp(x, y.str);
}

static char *state_path(const char *what)
{
  const char *home = getenv("HOME");
  char *res = NULL;
  if (home) {
    asprintf(&res, "%s/.config/glirc/otr-%s.txt", home, what);
  }
  return res;
}

static OtrlPolicy op_policy(void *opdata, ConnContext *context)
{
  (void)opdata; (void)context;

  return OTRL_POLICY_DEFAULT & ~OTRL_POLICY_SEND_WHITESPACE_TAG;
}

static void handle_smp_event
  (void *opdata, OtrlSMPEvent smp_event, ConnContext *context,
   unsigned short progress_percent, char *question)
{
  (void)progress_percent;
  GET_G;

  if (smp_event >= 9) return;

  const char *messages[9] = {
    [OTRL_SMPEVENT_NONE          ] = BOLD("none"),
    [OTRL_SMPEVENT_ERROR         ] = RED("error"),
    [OTRL_SMPEVENT_ABORT         ] = RED("abort"),
    [OTRL_SMPEVENT_CHEATED       ] = RED("cheated"),
    [OTRL_SMPEVENT_ASK_FOR_ANSWER] = BOLD("question"),
    [OTRL_SMPEVENT_ASK_FOR_SECRET] = BOLD("secret?"),
    [OTRL_SMPEVENT_IN_PROGRESS   ] = BOLD("in progress"),
    [OTRL_SMPEVENT_SUCCESS       ] = GREEN("success"),
    [OTRL_SMPEVENT_FAILURE       ] = RED("failure"),
  };

  const char *message = messages[smp_event];

  if (question) {
    print_status(G, context, "Peer verification [%s] [%s]", message, question);
  } else {
    print_status(G, context, "Peer verification [%s]", message);
  }

  if (smp_event == OTRL_SMPEVENT_ASK_FOR_ANSWER || smp_event == OTRL_SMPEVENT_ASK_FOR_SECRET) {
    print_status(G, context, "Reply with: /extension " NAME " secret <answer>");
  }
}

static void
inject_message
  (void *opdata, const char *accountname,
  const char *protocol, const char *recipient, const char *message)
{
  (void)accountname;
  GET_G;

  OtrlMessageType msgtype = otrl_proto_message_type(message);

  // The default query message contains HTML and newlines!
  if (msgtype == OTRL_MSGTYPE_QUERY) {
    message = QUERY_TEXT;
  }

  struct glirc_string params[2] = {
    { .str = recipient, .len = strlen(recipient) },
    { .str = message  , .len = strlen(message)   },
  };

  struct glirc_message m = {
    .network  = { .str = protocol, .len  = strlen(protocol) } ,
    .command  = { .str = "PRIVMSG", .len = strlen("PRIVMSG") },
    .params   = params,
    .params_n = 2,
  };

  glirc_send_message(G, &m);
}

static void
handle_msg_event
  (void *opdata, OtrlMessageEvent msg_event, ConnContext *context,
   const char *message, gcry_error_t err)
{
  (void)err;

  GET_G;

  const char *messages[16] = {
    [OTRL_MSGEVENT_NONE                      ] = "None",
    [OTRL_MSGEVENT_ENCRYPTION_REQUIRED       ] = "Encryption required, message not sent",
    [OTRL_MSGEVENT_ENCRYPTION_ERROR          ] = "Encryption error, message not sent",
    [OTRL_MSGEVENT_CONNECTION_ENDED          ] = "Connection ended, message not sent",
    [OTRL_MSGEVENT_SETUP_ERROR               ] = "Setup error",
    [OTRL_MSGEVENT_MSG_REFLECTED             ] = "Message reflected",
    [OTRL_MSGEVENT_MSG_RESENT                ] = "Previous message resent",
    [OTRL_MSGEVENT_RCVDMSG_NOT_IN_PRIVATE    ] = "Received unexpected encrypted message",
    [OTRL_MSGEVENT_RCVDMSG_UNREADABLE        ] = "Received message unreadable",
    [OTRL_MSGEVENT_RCVDMSG_MALFORMED         ] = "Received message malformed",
    [OTRL_MSGEVENT_LOG_HEARTBEAT_RCVD        ] = NULL,
    [OTRL_MSGEVENT_LOG_HEARTBEAT_SENT        ] = NULL,
    [OTRL_MSGEVENT_RCVDMSG_GENERAL_ERR       ] = "Received general error",
    [OTRL_MSGEVENT_RCVDMSG_UNENCRYPTED       ] = "Received message unencrypted",
    [OTRL_MSGEVENT_RCVDMSG_UNRECOGNIZED      ] = "Received message unrecognizable",
    [OTRL_MSGEVENT_RCVDMSG_FOR_OTHER_INSTANCE] = "Received message for other instance",
  };

  const char *desc = messages[msg_event];

  if (desc) {
    if (message) {
      print_status(G, context, "%s [%s]", desc, message);
    } else {
      print_status(G, context, "%s", desc);
    }
  }
}

static int max_message_size(void *opdata, ConnContext *context)
{
  (void)opdata, (void)context;

  return 400; // pessimistic
}

static int is_logged_in
  (void *opdata, const char *accountname, const char *protocol,
   const char *recipient)
{
  (void)accountname;
  GET_G;

  int seen = glirc_is_logged_on(G, protocol,  strlen(protocol),
                                   recipient, strlen(recipient));

  return seen ? 1 : -1; // not seen just means we might not share a channel
}

static void gone_secure(void *opdata, ConnContext *context)
{
  GET_G;

  if (otrl_context_is_fingerprint_trusted(context->active_fingerprint)) {
    print_status(G, context, "Connection secured [" GREEN("trusted") "]");
  } else {
    print_status(G, context, "Connection secured [" RED("untrusted") "]");
  }
}

static void still_secure(void *opdata, ConnContext *context, int is_reply)
{
  (void)is_reply;
  GET_G;

  if (otrl_context_is_fingerprint_trusted(context->active_fingerprint)) {
    print_status(G, context, "Connection refreshed [" GREEN("trusted") "]");
  } else {
    print_status(G, context, "Connection refreshed [" RED("untrusted") "]");
  }
}

static void
new_fingerprint
  (void *opdata, OtrlUserState us, const char *accountname, const char *net,
   const char *tgt, unsigned char fp[20])
{
  (void)opdata, (void)us, (void)accountname;
  GET_G;

  char human[OTRL_PRIVKEY_FPRINT_HUMAN_LEN];
  otrl_privkey_hash_to_human(human, fp);

  glirc_printf(G, net, PLUGIN_USER, tgt, "New fingerprint: [" BOLD("%s") "]", human);
}

static void write_fingerprints(void *opdata)
{
  GET_us;

  char *path = state_path("fingerprints");
  if (path) otrl_privkey_write_fingerprints(us, path);
  free(path);
}

static void create_privkey(void *opdata, const char *accountname,
                           const char *protocol)
{
    GET_us;

    char *path = state_path("keys");
    if (path) otrl_privkey_generate(us, path, accountname, protocol);
    free(path);
}


static void create_instag(void *opdata, const char *accountname,
                          const char *protocol)
{
    GET_us;

    char *path = state_path("instags");
    if (path) otrl_instag_generate(us, path, accountname, protocol);
    free(path);
}

static OtrlMessageAppOps ops = {
    .policy            = op_policy,
    .inject_message    = inject_message,
    .max_message_size  = max_message_size,
    .handle_msg_event  = handle_msg_event,
    .is_logged_in      = is_logged_in,

    .gone_secure       = gone_secure,
    .still_secure      = still_secure,
    .handle_smp_event  = handle_smp_event,

    .write_fingerprints = write_fingerprints,
    .new_fingerprint   = new_fingerprint,
    .create_privkey    = create_privkey,
    .create_instag     = create_instag,
};

static void *start_entrypoint(struct glirc *G, const char *libpath)
{
  (void)G, (void)libpath;

  OTRL_INIT;
  OtrlUserState us = otrl_userstate_create();

  char *path = state_path("keys");
  if (path) otrl_privkey_read(us, path);
  free(path);

  path = state_path("fingerprints");
  if (path) otrl_privkey_read_fingerprints(us, path, NULL, NULL);
  free(path);

  path = state_path("instags");
  if (path) otrl_instag_read(us, path);
  free(path);

  return us;
}

static void stop_entrypoint(struct glirc *G, void *L)
{
  (void)G;
  OtrlUserState us = L;
  otrl_userstate_free(us);
}


// Rebuild the userinfo "nick!user@host" from the prefix
// fields of a glirc message
static char *
rebuild_userinfo(const struct glirc_message *msg)
{
    char *userinfo = malloc(msg->prefix_nick.len +
                            msg->prefix_user.len +
                            msg->prefix_host.len +
                            3);
    if (!userinfo) abort();

    strcpy(userinfo, msg->prefix_nick.str);
    if (msg->prefix_user.len > 0) {
        strcat(userinfo, "!");
        strcat(userinfo, msg->prefix_user.str);
    }
    if (msg->prefix_host.len > 0) {
        strcat(userinfo, "@");
        strcat(userinfo, msg->prefix_host.str);
    }

    return userinfo;
}


static enum process_result
message_entrypoint(struct glirc *G, void *L, const struct glirc_message *msg)
{
    OtrlUserState us = L;
    GET_opdata;

    if (!match_string("PRIVMSG", msg->command) || msg->params_n != 2) {
        return PASS_MESSAGE;
    }

    const char *message = msg->params[1].str;
    const char *net     = msg->network.str;

    if (glirc_is_channel(G, net, msg->network.len, msg->params[0].str, msg->params[0].len)) {
        return PASS_MESSAGE;
    }

    char *newmessage = NULL;
    char *sender = strdup(msg->prefix_nick.str);
    char *target = strdup(msg->params[0].str);

    if (!sender || !target) abort();
    normalizeCase(sender);
    normalizeCase(target);

    int internal = otrl_message_receiving(us, &ops, &opdata, target, net, sender,
                      message, &newmessage, NULL, NULL, NULL, NULL);

    if (!internal && newmessage) {
        char *userinfo = rebuild_userinfo(msg);
        glirc_inject_chat(G, msg->network.str    , msg->network.len,
                             userinfo            , strlen(userinfo),
                             msg->prefix_nick.str, msg->prefix_nick.len,
                             newmessage          , strlen(newmessage));
        free(userinfo);
    }

    otrl_message_free(newmessage);
    free(sender);
    free(target);

    return (internal || newmessage) ? DROP_MESSAGE : PASS_MESSAGE;
}


static enum process_result chat_entrypoint(struct glirc *G, void *L, const struct glirc_chat *chat)
{
    OtrlUserState us = L;
    GET_opdata;

    char * newmsg = NULL;
    gcry_error_t err = 1; // default to error unless sending runs and succeeds
    const char * net = chat->network.str;
    const char * msg = chat->message.str;

    if (glirc_is_channel(G, net, chat->network.len, chat->target.str, chat->target.len)) {
        return PASS_MESSAGE;
    }

    char * me = glirc_my_nick(G, chat->network.str, chat->network.len);
    if (!me) return PASS_MESSAGE;
    normalizeCase(me);

    char * tgt = strdup(chat->target.str);
    if (!tgt) abort();
    normalizeCase(tgt);

    err = otrl_message_sending
      (us, &ops, &opdata, me, net, tgt, OTRL_INSTAG_BEST, msg,
       NULL, &newmsg, OTRL_FRAGMENT_SEND_ALL, NULL, NULL, NULL);

    if (err) {
        glirc_printf(G, net, PLUGIN_USER, tgt, "PANIC: OTR encryption error");
    }

    otrl_message_free(newmsg);
    glirc_free_string(me);
    free(tgt);
    return err || newmsg ? DROP_MESSAGE : PASS_MESSAGE;
}

static void cmd_end (struct glirc *G, OtrlUserState us,
                     const struct glirc_string *params)
{
  (void)params;
  GET_opdata;

  char *net = NULL; size_t netlen = 0;
  char *tgt = NULL;
  char *me  = NULL;

  glirc_current_focus(G, &net, &netlen, &tgt, NULL);
  if (!net || !tgt) goto cmd_end_done;
  normalizeCase(tgt);

  me = glirc_my_nick(G, net, netlen);
  if (!me) goto cmd_end_done;
  normalizeCase(me);

  otrl_message_disconnect_all_instances(us, &ops, &opdata, me, net, tgt);

  const char *src = PLUGIN_USER;
  const char *msg = RED("Session terminated");

  glirc_inject_chat
    (G, net, strlen(net),
        src, strlen(src),
        tgt, strlen(tgt),
        msg, strlen(msg));

cmd_end_done:
  glirc_free_string(me);
  glirc_free_string(net);
  glirc_free_string(tgt);
}


static ConnContext *
get_current_context(struct glirc *G, OtrlUserState us)
{
    char *net = NULL; size_t netlen = 0;
    char *tgt = NULL;
    char *me  = NULL;
    ConnContext *context = NULL;

    glirc_current_focus(G, &net, &netlen, &tgt, NULL);
    if (!net || !tgt) goto get_current_context_done;
    normalizeCase(tgt);

    me = glirc_my_nick(G, net, netlen);
    if (!me) goto get_current_context_done;
    normalizeCase(me);

    context = otrl_context_find(us, tgt, me, net, OTRL_INSTAG_BEST, 0, NULL, NULL, NULL);

get_current_context_done:
    glirc_free_string(me);
    glirc_free_string(net);
    glirc_free_string(tgt);

    return context;
}


static void cmd_ask (struct glirc *G, OtrlUserState us,
                     const struct glirc_string *params)
{
    GET_opdata;

    ConnContext *context = get_current_context(G, us);

    if (context) {
        otrl_message_initiate_smp
            (us, &ops, &opdata, context,
             (unsigned char *)params[1].str, params[1].len);
    }
}


static void cmd_secret (struct glirc *G, OtrlUserState us,
                        const struct glirc_string *params)
{
    GET_opdata;

    ConnContext *context = get_current_context(G, us);

    if (context) {
        otrl_message_respond_smp
            (us, &ops, &opdata, context,
             (unsigned char *)params[1].str, params[1].len);
    }
}


static void cmd_poll (struct glirc *G, OtrlUserState us,
                      const struct glirc_string *params)
{
  (void)params;
  GET_opdata;
  otrl_message_poll(us, &ops, &opdata);
}


/*
 * Manually mark the fingerprint associated with the current window trusted.
 */
static void cmd_trust (struct glirc *G, OtrlUserState us,
                       const struct glirc_string *params)
{
  (void)params;
  ConnContext *context = get_current_context(G, us);
  if (!context || !context->active_fingerprint) return;

  otrl_context_set_trust(context->active_fingerprint, "manual");

  char *path = state_path("fingerprints");
  if (path) otrl_privkey_write_fingerprints(us, path);
  free(path);

  char human[OTRL_PRIVKEY_FPRINT_HUMAN_LEN];
  otrl_privkey_hash_to_human(human, context->active_fingerprint->fingerprint);
  print_status(G, context, "Fingerprint trusted [" BOLD("%s") "]", human);
}

/*
 * Manually mark the fingerprint associated with the current window untrusted.
 */
static void cmd_untrust (struct glirc *G, OtrlUserState us,
                         const struct glirc_string *params)
{
  (void)params;

  ConnContext *context = get_current_context(G, us);
  if (!context || !context->active_fingerprint) return;

  otrl_context_set_trust(context->active_fingerprint, "");

  char *path = state_path("fingerprints");
  if (path) otrl_privkey_write_fingerprints(us, path);
  free(path);

  char human[OTRL_PRIVKEY_FPRINT_HUMAN_LEN];
  otrl_privkey_hash_to_human(human, context->active_fingerprint->fingerprint);
  print_status(G, context, "Fingerprint untrusted [" BOLD("%s") "]", human);
}

/*
 * Print status information for the current context to the chat window
 */
static void cmd_status (struct glirc *G, OtrlUserState us,
                        const struct glirc_string *params)
{
  (void)params;

  ConnContext *context = get_current_context(G, us);
  if (!context) return;

  char human[OTRL_PRIVKEY_FPRINT_HUMAN_LEN];
  const char *myfp =
          otrl_privkey_fingerprint(us, human, context->accountname, context->protocol);

  if (myfp) {
    print_status(G, context, "Local  fingerprint [" BOLD("%s") "]", myfp);
  }

  Fingerprint *fp = context->active_fingerprint;
  if (fp) {
    otrl_privkey_hash_to_human(human, fp->fingerprint);

    if (otrl_context_is_fingerprint_trusted(fp)) {
      print_status(G, context, "Remote fingerprint [" BOLD("%s") "] [" GREEN("%s") "]", human, fp->trust);
    } else {
      print_status(G, context, "Remote fingerprint [" BOLD("%s") "] [" RED("untrusted") "]", human);
    }
  }

  print_status(G, context,
      "Local instance [" BOLD("%08X") "] Remote instance [" BOLD("%08X") "] Protocol [" BOLD("%u") "]",
      context->our_instance, context->their_instance, context->protocol_version);

  const char *statuses[] = {
    [OTRL_MSGSTATE_PLAINTEXT] = RED  ("plaintext"),
    [OTRL_MSGSTATE_ENCRYPTED] = GREEN("encrypted"),
    [OTRL_MSGSTATE_FINISHED ] = RED  ("finished" ),
  };

  print_status(G, context, "Connection state [%s]", statuses[context->msgstate]);
}

// Command metadata
struct cmd_impl {
  const char *name; // Name of command
  void (*func)(struct glirc *, OtrlUserState, const struct glirc_string *); // Implementation
  unsigned params; // Expected number of parameters
  const char *doc; // Documentation string
};

static void cmd_help(struct glirc*, OtrlUserState, const struct glirc_string*);

static struct cmd_impl cmd_impls[] = {
  { "status" , cmd_status , 0, "Display the current window's OTR context"              },
  { "secret" , cmd_secret , 1, "Reply to a peer verification request (1 argument)"     },
  { "ask"    , cmd_ask    , 1, "Send a peer verification request (1 argument)"         },
  { "end"    , cmd_end    , 0, "Close the current window's OTR context"                },
  { "trust"  , cmd_trust  , 0, "Trust the current remote user's fingerprint"           },
  { "untrust", cmd_untrust, 0, "Revoke trust in the current remote user's fingerprint" },
  { "poll"   , cmd_poll   , 0, "Manually trigger an OTR poll event"                    },
  { "help"   , cmd_help   , 0, "Show available commands"                               },
  { NULL     , NULL       , 0, NULL                                                    },
};

static void cmd_help(struct glirc *G, OtrlUserState us,
                     const struct glirc_string* params)
{
  (void)us, (void)params;

  for (struct cmd_impl *c = cmd_impls; c->name; c++) {
    char *msg = NULL;
    int len = asprintf(&msg, "OTR: %s - %s", c->name, c->doc);
    if (len < 0 || !msg) abort();

    glirc_print(G, NORMAL_MESSAGE, msg, len);

    free(msg);
  }
}

static void command_entrypoint
  (struct glirc *G, void *L, const struct glirc_command *cmd)
{
  OtrlUserState us = L;
  const char *errmsg = "OTR: Unknown command";

  if (cmd->params_n > 0) {
    for (struct cmd_impl *c = cmd_impls; c->name; c++) {
      if (match_string(c->name, cmd->params[0])) {
        if (cmd->params_n == c->params + 1) {
          c->func(G, us, cmd->params);
          return;
        } else {
          errmsg = "OTR: Bad parameters";
        }
      }
    }
  }

  glirc_print(G, ERROR_MESSAGE, errmsg, strlen(errmsg));
}

struct glirc_extension extension = {
        .name            = NAME,
        .major_version   = MAJOR,
        .minor_version   = MINOR,
        .start           = start_entrypoint,
        .stop            = stop_entrypoint,
        .process_message = message_entrypoint,
        .process_chat    = chat_entrypoint,
        .process_command = command_entrypoint,
};

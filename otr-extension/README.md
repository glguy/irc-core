# OTR Extension

This extension allows you to use Off-the-record (OTR) messaging from glirc.


## Building

This project uses the [Meson](https://mesonbuild.com) build system. You
will need `meson` and `ninja` for the default configuration.

```
$ meson builddir
$ cd builddir
$ ninja
$ cp glirc-otr.bundle ~/.config/glirc/ # use .so for Linux
```

## Configuration

Add the extension to your configuration file:

```
extensions:
  * "glirc-otr.bundle" -- use .so for Linux
```

You can make it easier to use this extension with the following client macro

```
macros:
  * name     : "otr"
    arguments: "args*"
    commands : "extension OTR $0"
```

This allows `/otr help` instead of `/extension OTR help`

## Extension usage

To start an OTR session, send the message `?OTRv3?`.

Available commands:

```
/extension OTR status         - Display the current window's OTR context
/extension OTR secret SECRET  - Reply to a peer verification request (1 argument)
/extension OTR ask    SECRET  - Send a peer verification request (1 argument)
/extension OTR end            - Close the current window's OTR context
/extension OTR trust          - Trust the current remote user's fingerprint
/extension OTR untrust        - Revoke trust in the current remote user's fingerprint
/extension OTR help           - Show available commands
```

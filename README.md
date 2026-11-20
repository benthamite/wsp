# `wsp`: WhatsApp client for Emacs

Read and answer your WhatsApp chats from Emacs, with full history, search and notifications.

## Overview

`wsp` lets you use WhatsApp from Emacs. You can read your chats, reply, react, edit, delete, send files, search, and get notified when messages arrive.

`wsp` does not connect to WhatsApp itself. That is the job of [`wa-archive`](https://github.com/benthamite/wa-archive), a small daemon built on the [whatsmeow](https://github.com/tulir/whatsmeow) library. It links to your phone as a companion device, the way WhatsApp Web does, and keeps every message in a local SQLite store. `wsp` reads that store directly and read-only. To send a message or act on one, it goes through the daemon's local API, a Unix socket. `wsp` and `wa-archive` work together the way `signal-cli` and a Signal client do.

What you get:

- **A chat list** with unread counts, the time of the latest message and a preview of it. The list redraws itself as messages arrive.
- **Chat buffers** that group messages by sender. Each message shows the message it replies to, any edits or deletions, its reactions and its attachment. You can load older messages back to the start of the archive.
- **Messaging:** send, reply, edit, delete for everyone, react with an emoji, and send files. Every message you send shows whether it was delivered and read.
- **Attachments:** download them and open them from the chat. Unsafe file types ask before opening.
- **Search** across all chats or within one chat.
- **Read state:** you can send read receipts and typing and online status, or turn them off.
- **Desktop notifications** and an unread count in the mode line.
- **Opening a chat at a given message**, so other programs can link to a message.

`wsp` is an unofficial client and is not affiliated with WhatsApp or Meta.

## Installation

**Requirements**: Emacs 31.1 or later, compiled with SQLite support. You also need [`wa-archive`](https://github.com/benthamite/wa-archive) running and linked to your phone; its README explains how to set it up. Desktop notifications use `osascript` on macOS and `notifications.el` on GNU/Linux.

`wsp` uses only built-in Emacs libraries. Install it with any one of the following.

### package-vc

```emacs-lisp
(use-package wsp
  :vc (:url "https://github.com/benthamite/wsp"))
```

### Elpaca

```emacs-lisp
(use-package wsp
  :ensure (:host github :repo "benthamite/wsp"))
```

### straight.el

```emacs-lisp
(use-package wsp
  :straight (:host github :repo "benthamite/wsp"))
```

## Quick start

With `wa-archive` running and linked, using its default directories:

1. `M-x wsp`: opens the chat list and connects to `wa-archive`.
2. `RET` on a chat opens it. `M-x wsp-chat` opens a chat by name, or by phone number if you start with `+`.
3. Type in the input area at the bottom and press `RET` to send.
4. On a message, press `r` to reply, `+` to react, `e` to edit or `d` to delete.

If `wa-archive` keeps its files somewhere other than its defaults, set `wsp-data-directory` and `wsp-state-directory` to match.

## Documentation

The [manual](https://stafforini.com/notes/wsp/) describes every user option, command and function. The API that `wsp` and `wa-archive` share is in [`docs/api.md`](docs/api.md).

## License

`wsp` is licensed under the [GNU General Public License v3](LICENSE).

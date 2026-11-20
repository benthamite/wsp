# wa-archive API contract (as wsp uses it)

The contract between wa-archive (backend) and wsp (client). wa-archive
implements it; wsp's tests fake it. Changing it means changing both.

## Transport

- Unix socket `$XDG_DATA_HOME/wa-archive/api.sock` (default
  `~/.local/share/wa-archive/api.sock`), mode 0600, created by `wa-archive
  run`, removed on exit. A stale socket file is replaced on start.
- Framing: one JSON object per line, UTF-8, terminated by `\n`.
- Any number of clients may connect; each gets every event.

## Requests and responses

```json
{"id": 7, "method": "send", "params": {...}}
{"id": 7, "result": {...}}
{"id": 7, "error": {"code": "not_connected", "message": "..."}}
```

`id` is a client-chosen integer echoed back. Error codes: `bad_request`,
`not_found`, `not_connected`, `failed` (nothing went out), `uncertain` (it may
have gone out: the op is journaled as pending or the commit failed after
sending), `internal`.

An acting method answers only after its result is committed to the store,
so the client can redraw from the store as soon as it gets the answer.

`chat` parameters take a canonical chat key or an alias; the daemon resolves
aliases and validates the JID as the outbox does (`@s.whatsapp.net`, `@g.us`,
`@lid`).

## Methods

| Method | Params | Result |
|---|---|---|
| `status` | — | `{"connected": bool, "logged_in": bool, "version": str, "read_receipts": "all"\|"none"\|"unknown", "self": jid}` |
| `send` | `op, chat, text, reply_to?` | `{"op", "message_id", "ts"}` |
| `send_file` | `op, chat, path, caption?, reply_to?` | `{"op", "message_id", "ts"}` |
| `edit` | `op, chat, id, text` | `{"op", "message_id"}` (own messages only) |
| `delete` | `op, chat, id` | `{"op"}` (for everyone; own messages only) |
| `react` | `op, chat, id, emoji` | `{"op"}` (`""` removes your reaction) |
| `mark_read` | `chat` | `{"marked": n, "skipped": m}` |
| `fetch` | `chat, id` | `{"path": str}` (waits up to 120 s for the download) |
| `typing` | `chat, state` (`composing`\|`paused`) | `{}` |
| `presence` | `state` (`available`\|`unavailable`) | `{}` |
| `op_status` | `op` | `{"state": "pending"\|"sent"\|"failed", "message_id"?, "error"?}` |

`op` is a client-generated string unique per action (wsp uses a UUID). A
repeated `op` returns the journaled outcome instead of acting again, so a
client that lost an answer can ask again safely.

`mark_read` sends read receipts for the chat's unread incoming messages
(above the watermark and not in `read_messages`), grouped by sender, records
their ids in `read_messages`, and never sends receipts for messages at or
below the watermark. It sends `read` when your read-receipts setting is
on and `read-self` when it is off; when the setting cannot be read it
answers `failed` and sends and records nothing. Group messages whose sender
cannot be addressed (no sender, a group WhatsApp refuses to describe, no LID
mapping) are recorded read without a receipt and counted in `skipped`. If
receipts went out but could not be recorded, the answer is `uncertain` and
the daemon exits, as for sends.

Nothing sends presence or chat state except `typing` and `presence`. A
presence of `available` set through the API is withdrawn (`unavailable`)
when the last client disconnects or the daemon stops.

## Events

```json
{"event": "changed", "chats": ["123@s.whatsapp.net", "..."]}
{"event": "status", "connected": true, "logged_in": true}
```

`changed` follows every commit that touches the listed chats: messages,
edits, deletions, reactions, receipts, read state, names, downloaded media.

## Store additions (read by wsp)

```sql
CREATE TABLE reactions (
  chat_key TEXT NOT NULL, target_id TEXT NOT NULL, sender_key TEXT NOT NULL,
  from_me INTEGER NOT NULL DEFAULT 0, emoji TEXT NOT NULL, ts INTEGER NOT NULL,
  PRIMARY KEY (chat_key, target_id, sender_key));  -- emoji '' = removed
CREATE TABLE receipts (
  chat_key TEXT NOT NULL, message_id TEXT NOT NULL, reader_key TEXT NOT NULL,
  kind TEXT NOT NULL CHECK (kind IN ('delivered', 'read', 'played')),
  ts INTEGER NOT NULL, PRIMARY KEY (chat_key, message_id, reader_key, kind));
CREATE TABLE chat_reads (chat_key TEXT PRIMARY KEY, watermark_ts INTEGER NOT NULL);
CREATE TABLE read_messages (chat_key TEXT NOT NULL, id TEXT NOT NULL,
  PRIMARY KEY (chat_key, id));
CREATE TABLE api_ops (
  op_id TEXT PRIMARY KEY, method TEXT NOT NULL, chat_key TEXT NOT NULL,
  message_id TEXT NOT NULL DEFAULT '',
  state TEXT NOT NULL CHECK (state IN ('pending', 'sent', 'failed')),
  error TEXT NOT NULL DEFAULT '', ts INTEGER NOT NULL);
```

`receipts` covers your own messages only. An incoming message is unread
when `ts > chat_reads.watermark_ts` (0 when the chat has no row) and its id
is not in `read_messages`.

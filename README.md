# exsqlain

**Status: archived (2026-06-30). A finished thought, not a maintained tool.**

## The idea

A dev-loop tool for Haskell + Postgres work. The library:

1. Runs `EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON)` against a connection,
2. Pushes the resulting plan over Server-Sent Events to an embedded
   [pev2](https://github.com/dalibo/pev2) (Dalibo's Postgres plan visualizer) frontend,
3. Uses persistent global channels (`unsafePerformIO`) specifically so that reloading
   your hspec specs under `ghcid` keeps the browser connection alive.

The payoff was a tight loop: edit a query in your test suite, and the EXPLAIN ANALYZE
plan re-renders live in the browser on every `ghcid` reload — *see the plan of the query
under your cursor as you tweak it.*

## Why it's archived

- **The product angle was always thin.** The hard part is visualizing the plan, and that
  is pev2 — which this just embeds. Pasting a plan into the free hosted
  [explain.dalibo.com](https://explain.dalibo.com) or explain.depesz.com takes two seconds.
- **The live-loop angle is now gone too.** The premise was a human in a tight edit/look
  loop. Agentic coding changes the loop: an agent edits the query and reads textual/JSON
  output — it gets nothing from a rendered browser visualization. That inverts which half
  of this project mattered: the interesting engineering (SSE plumbing, the Vue app, the
  persistent-channel trick) is exactly the part that no longer has an audience.
- **What survives is trivial.** The only durable bit is "given a connstr + query, return
  the `EXPLAIN ... FORMAT JSON` plan as structured data" (see `runQuery` in
  `src/Database/PostgreSQL/Exsqlain.hs`). That's ~20 lines and needs no library; an agent
  just runs the EXPLAIN itself.

If the value ever migrates anywhere, it's to a language-agnostic agent-facing EXPLAIN
tool (an MCP server: "analyze this query, here's the plan + the bottleneck"). That would
reuse nothing here except the one trivial line, so it's a start-fresh idea, not a revive.

## State of the code

This never got past a 2020 prototype: ~131 lines of Haskell, a ~90-line Vue 2 app
(Vue 2 is itself EOL), placeholder package metadata, and a couple of known rough edges
(`runQuery` uses `EXPLAIN ANALYZE`, which *executes* the statement — fine for `SELECT`,
not for writes). Left as-is for reference.

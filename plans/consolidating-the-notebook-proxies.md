# Plan: a shared core library for the four notebook language servers

rust-, cpp-, go- and haskell-notebook-language-server stay in their own repos, keep their own
versions, CI and releases. A fifth repo holds the plumbing all four copy today, and each pins
it as a git dependency.

## Why

The copying costs real time. The last two shared changes were each made four times:

* **Inlay hints** — the request reached the server with the notebook's URI. rust#2, cpp#1,
  go#1, haskell#2.
* **Code action edits** — the response named the shadow file, in the shadow file's
  coordinates. rust#3, cpp#2, go#2, haskell#3. The broken handler was byte-identical in all
  four: an `isInternalReferringCodeAction` whose two clauses both returned `False`.

Neither port was mechanical. The second needed `DynFlags` threaded through by hand in haskell
and different imports in three of the four, and nothing checked afterwards that the four
results were equivalent.

Measured against cpp, these modules are identical in go after normalising the language name —
`Transform/Util.hs` (198 lines), `Language/LSP/Transformer.hs` (123), `Transform/Common.hs`
(103), `Transform/ServerRsp/Hover.hs` (82), `Transform/ServerNot.hs` (37),
`Transform/ClientNot/DidSave.hs` (30), `Transform/ClientRsp.hs` (29),
`Transform/ServerReq.hs` (9). Another three are near-identical: `ClientNot.hs` (13 differing
of 140), `ServerRsp.hs` (12 of 138), `ClientReq.hs` (25 of 96). In `app/`, `Parsing.hs` (88),
`Process.hs` (38) and `RequestMap.hs` (37) are identical across all four.

That is ~800 lines in four copies.

### The drift is already here

`class Transformer` has diverged three ways, which is what decides this:

```haskell
-- rust
getParams  :: a -> Params a
project    :: Params a -> Doc -> (Doc, a)
unproject  :: Params a -> a -> Doc -> Maybe Doc

-- cpp, go
getParams  :: a -> Params a
project    :: MonadIO m => Params a -> Doc -> m (Doc, a, Either Text ())

-- haskell
project    :: MonadIO m => Params a -> Doc -> m (Doc, a)
```

Three shapes of `project`. `unproject` only in rust. `getParams` missing from haskell. Nobody
decided any of this. It has a visible consequence: `unproject` is what maps a formatter's
output back into a cell, so rust can format a notebook cell and the other three cannot.

A fourth strand, since resolved in hnls: `transformPosition` and `untransformPosition` carried
a `Params a` argument no transformer read, and because haskell had no `getParams` the only way
to supply it was to rebuild the params from `DynFlags`. See step 2.

---

# Status

| | |
| --- | --- |
| Code action fix | **parked** as drafts: rust#3, cpp#2, go#2, haskell#3. Same fix written four times, which is the duplication this plan removes. Reapply once after the class is reconciled. |
| Drop `Params` from position mapping | done in all four: rust#4, cpp#3, go#3, haskell#4. Tests green (rust 19, cpp 21, go 40, haskell 20). |
| Class split | **prototyped and green in rust** (rust#5, stacked on #4). Not yet ported to cpp, go, haskell. |
| Releases | not cut, and not needed until the reconciliation lands. |
| Integration test | codedown-languages branch `rust-code-action-test`, pushed, no PR — goes back with the code action fix. |

# The plan

## 1. Park the code-action fix — done

rust#3, cpp#2, go#2, haskell#3 are drafts. The fix is correct and tested, but it is the same
change four times; reapplying it once over a reconciled class is the point of the exercise.
The codedown-languages integration test on `rust-code-action-test` goes back with it.

## 2. Port the `Params` drop to rust, cpp and go — done

`transformPosition` and `untransformPosition` took a `Params a` that no transformer ever read.
Three PRs mirroring haskell#4. Mechanical: every leaf already discards the argument, only the
`:>` instance threads it.

What it bought in hnls, and why it matters more there than elsewhere: with no `getParams` in
the class, the only way to build a `Params HaskellNotebookTransformer` was
`transformerParams flags`, so every caller needed `DynFlags`. Dropping the argument took
`Transform/Common.hs` from 9 `DynFlags` mentions to 0 (51 to 29 across `src/`), and made its
helper signatures character-for-character identical to rust's and cpp's. It also stopped
rebuilding the seven-element `:>` chain on every position mapped.

One wrinkle to expect: `ExpressionToDeclaration` *did* read `numberPadding` from its params,
via RecordWildCards in the body rather than the pattern. It now carries the prefix width it
added, which also collapsed a constant that was written two ways (`prefixLen = 4 +
numberPadding + 3` in `project`, `numberPadding + 7` in the position functions). Check each
repo's leaves for the same shape rather than trusting a grep of the pattern-match line.

## 3. Settle the rest of the class

One library means one class, so all four instances have to agree. Three decisions:

**`project` takes cpp/go's signature:**

```haskell
project :: MonadIO m => Params a -> Doc -> m (Doc, a, Either Text ())
```

It is the superset and both additions are load-bearing. `MonadIO` is there because cpp's
`DeclarationSifter` shells out to `cling-parser` and go's to its own parser; rust needs no
parser and haskell parses in-process, which is why theirs drifted. `Either Text ()` is that
parser's failure channel — `DeclarationSifter` returns `Left "Failed to parse C++: …"`, falls
back to `mkIdentitySifter`, and `ClientNot.hs` logs it. Dropping it loses the diagnostic
silently.

Migration is cheap: rust wraps its pure result in `pure (…, Right ())`, haskell adds
`Right ()`.

**Split the class so pure transformers stay pure:**

```haskell
class Transformer a where
  type Params a
  getParams           :: a -> Params a
  transformPosition   :: a -> Position -> Maybe Position
  untransformPosition :: a -> Position -> Maybe Position
  unproject           :: Params a -> a -> Doc -> Maybe Doc
  unproject _ _ _ = Nothing

class Transformer a => ProjectIn m a where
  project         :: Params a -> Doc -> m (Doc, a, Either Text ())
  handleDiff      :: Params a -> Doc -> TextDocumentContentChangeEvent -> a -> m ([TextDocumentContentChangeEvent], a)
  handleDiffMulti :: Params a -> Doc -> [TextDocumentContentChangeEvent] -> a -> m ([TextDocumentContentChangeEvent], a)
```

A single `class Transformer m a` doesn't work: `getParams` and `unproject` don't mention `m`,
so they become ambiguous, and that is ~25 call sites in rust, 20 in cpp, 20 in go and 36 in
haskell. A fundep `a -> m` fixes the ambiguity but pins each transformer to one monad, which
makes a mixed stack like `HeadTailTransformer :> DeclarationSifter` impossible.

With the split, `instance Monad m => ProjectIn m HeadTailTransformer` is usable at `Identity`
*and* at `IO`, so it still stacks with an IO transformer, and rust's four pure test call sites
become `runIdentity $ project …` rather than IO tests. `:>` splits cleanly:

```haskell
instance (Transformer x, Transformer y)               => Transformer   (x :> y)
instance (ProjectIn m x, ProjectIn m y, Monad m)      => ProjectIn m   (x :> y)
```

**`getParams` into haskell, and `unproject` into all four.** haskell needs `getParams` only
for `unproject`, which genuinely reads params — rust's `HeadTailTransformer` does
`unproject (initialLines, finalLines) _ doc`. The position methods no longer need it after
step 2.

**Proven in rust** (rust#5): the split compiles, all 19 tests pass, and they still run purely
as `runIdentity $ project …` rather than being dragged into IO. The only new extension needed
is `MultiParamTypeClasses` — checked explicitly, no `AllowAmbiguousTypes`, no
`UndecidableInstances`, no `FunctionalDependencies`.

Two things the port to the others will hit. Each leaf instance splits in two, with the
`project` body moving to `instance Monad m => ProjectIn m X` (or `MonadIO m` for cpp and go's
`DeclarationSifter`). And every pure `project` call site in the tests becomes
`runIdentity $ project`, which is the ergonomics the split exists to preserve.

## 4. Reconcile across all four, still in four repos

Four PRs applying step 3. Tedious, and the last time you do this by hand.

Worth doing even if the rest of the plan is abandoned: `unproject` reaching cpp, go and
haskell is what unblocks notebook formatting for them (see `notebook-formatting-in-proxies.md`
in codedown-languages).

## 5. Standardise rust, cpp and go on one GHC

Today: rust and cpp build ghc9122, go builds ghc9124, on resolvers `nightly-2025-11-19`,
`nightly-2025-11-19` and `nightly-2026-04-19`. Pick one — 9.12.4 is fine — and move all three
onto it, with the same resolver.

This matters for step 6. Each distinct resolver among the consumers is a configuration core
has to build against, and three servers needing one between them instead of three is a third
of the matrix gone.

**Consider going further and using hnls's ghc912 resolver** (`nightly-2025-08-22`, GHC 9.12.2)
for all three. Then the set of resolvers core must support is *exactly* hnls's, and core's CI
matrix is hnls's matrix with nothing bolted on. Only do this if nothing in rust/cpp/go needs
something newer.

## 6. Stand up the core repo with CI and a stub, before moving any code

New repo `codedownio/notebook-language-server-core`, one library package, no modules yet —
just enough to depend on lsp-types and compile.

Get its CI green across **every GHC hnls builds**: ghc92, ghc94, ghc96, ghc98, ghc910, ghc912
(the `9.0.2` stack file exists but isn't in hnls's matrix). hnls needs that range because it
links GHC's API and runs in production against a range of compilers; that is not negotiable,
so core inherits it. Plus whatever rust/cpp/go settled on in step 5, if it differs.

This is the cheap early test of the whole idea, and it's why it comes before the move. Core's
entire API surface is lsp-types, and which lsp-types you get comes from the resolver — a
function that compiles against `nightly-2026-04-19` may not against `lts-20.26`. You want that
failure now, on a stub, not later in a downstream repo after moving 800 lines.

Two consequences:

* Core cannot use recent language features and cannot take tight dependency bounds. GHC 9.2 is
  the floor.
* Generate core's matrix from the four repos' stack files rather than maintaining a list by
  hand, or it will drift.

If core can't be made to build across that range, the options are raising hnls's floor
(dropping ghc92 and ghc94 would cut the matrix to four) or abandoning the shared library.
Raising the floor is a decision about which codedown Haskell kernels stay supported, not a
technical one.

## 7. Move the modules into core

```
notebook-language-server-core/src/
  Language/LSP/Transformer.hs        the class and the :> stacking combinator
  Transform/Util.hs                  DocumentState, TransformerState, lookups
  Transform/Common.hs                untransformRanged / InlayHint / WorkspaceEdit / CodeAction
  Transform/ClientNot.hs             didOpen / didChange / didClose
  Transform/ClientNot/DidSave.hs
  Transform/ClientReq.hs
  Transform/ClientRsp.hs
  Transform/ServerNot.hs
  Transform/ServerReq.hs
  Transform/ServerRsp.hs
  Transform/ServerRsp/Hover.hs
  App/Parsing.hs
  App/Process.hs
  App/RequestMap.hs
```

Not moving: the transformers (they are the per-language part), `app/Main.hs` (the one app
module that genuinely differs — 71 lines between rust and cpp, 173 between rust and haskell —
and where the transformer stack gets named), `cling-parser/` and `go-parser/`, hnls's `bench/`,
and the static-linking machinery (`assets/libffi.a`, `nix/fix-dylib.sh`). The last of those
looks shared but is platform-and-GHC specific; merging it is separate work with its own
failure modes.

Leave `DeclarationSifter` duplicated in cpp and go for now. It differs by 57 lines and the
difference is their parsers, not the sifting, so it could be core parameterized over a parser
— but two copies of one module is not what hurts.

## 8. Port one server at a time

rust first: smallest transformer set, and the only `unproject` implementation, so it exercises
the class design hardest. Then cpp, go, haskell.

Each server adds:

```yaml
# stack.yaml
extra-deps:
- git: https://github.com/codedownio/notebook-language-server-core.git
  commit: <sha>
```

```yaml
# package.yaml
dependencies:
- notebook-language-server-core
```

Core needs no version numbers and no Hackage release. A commit is the version — the same way
hnls already pins IHaskell.

## 9. Write `bump-core` before porting the second server

A core bump touches **10 stack files**: one each in rust, cpp and go, and seven in hnls
(`stack/stack-{9.0.2,9.2.8,9.4.8,9.6.7,9.8.4,9.10.2,9.12.2}.yaml`). Stack has no include
mechanism to collapse them.

One script in core that rewrites all four checkouts and opens the PRs, or `.aliases/bump-core`
in each repo. Ten files by hand once is fine; four times is how this stops getting done, and
then the repos drift again, which is the thing the plan exists to prevent.

## 10. codedown-languages needs no changes

It pins released binaries, not source — `rnls-version.nix`, `hnls-version.nix` and the cpp and
go equivalents keep working exactly as they do now. Nothing about the release story changes.

---

## What this buys, and what it doesn't

A shared change goes from four hand-ported edits to one reviewed change plus four pin bumps.
The logic is written and reviewed once, and the `Transformer` class stops being four classes
that happen to share a name. That is most of the value.

It does **not** shorten the cadence: shipping a shared change is still five PRs and four
releases. If that is the part that actually hurts rather than the duplicated logic, this shape
won't help and a monorepo is the answer — but a monorepo trades this for a versioning decision
(per-package tags, or one version bumping four pins in codedown-languages every release) and a
CI setup needing path filters to keep hnls's matrix off every rust PR. Not obviously cheaper.

Costs to accept going in:

* A few days reconciling drift (steps 2–4), which is debt already owed. Step 2 is proven —
  it's done in hnls — so the remaining unknown is the class split in step 3.
* The `Transformer`/`ProjectIn` split, which is genuine design work and could fail (step 3).
* A six-GHC matrix in core, and GHC 9.2 as a floor on what core can use (step 6).
* Ten stack files per core bump, needing a script to stay tolerable (step 9).

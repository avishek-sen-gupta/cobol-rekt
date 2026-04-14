# COBOL LSP Fork Integration

## Section 1 — Upstream base

- **Upstream repo:** https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol
- **Fork branch:** `poc` in `avishek-sen-gupta/che-che4z-lsp-for-cobol-integration`
- **Current upstream base:** 2.4.3
- **How to find the merge-base for future upgrades:** `git merge-base <upstream-tag> poc`

## Section 2 — poc patch inventory (must survive every future merge)

| File | What | Why |
|---|---|---|
| `server/common/src/.../poc/PersistentData.java` | Static store for dialect parse-tree fragments | No DI container at parse time — static lookup replaces runtime injection |
| `server/common/src/.../poc/LocalisedDialect.java` | Dialect metadata carrier | Attached to `AnnotatedParserRuleContext` nodes |
| `server/common/src/.../poc/AnnotatedParserRuleContext.java` | `ParserRuleContext` subclass with `customData` map | Stores GUID → dialect-node mappings during parse |
| `CobolParser.g4` — `dialectNodeFiller` rule + references | Placeholder rule | Marks where IDMS fragments were stripped so they can be re-injected post-parse |
| `CobolLexer.g4` — `DIALECT_MARKER`, `DIALECT_IF`, `DIALECT_SCHEMA_SECTION` | Lexer tokens | Required by `dialectNodeFiller` rule |
| `server/engine/src/.../cli/di/CliModule.java` | Guice module | Wires parse engine without LSP server runtime |
| `server/engine/src/.../cli/modules/CliClientProvider.java` | Copybook path provider | Replaces LSP workspace-based copybook resolution |

## Section 3 — How to merge a future upstream release

1. `git merge-base <new-upstream-tag> poc` → extract poc-only grammar diff
2. Merge new tag into `poc`
3. Resolve conflicts: keep ALL files in the poc patch inventory above
4. Re-apply `dialectNodeFiller` grammar insertions if upstream changed surrounding rules
5. Build + test: `mvn clean install -f server/pom.xml`
6. Update this document's "Current upstream base" line

## Section 4 — Architectural constraint

`PersistentData` is not thread-safe by design. smojol parses files sequentially. Never parallelize calls to `ParsePipeline` in the same JVM without replacing `PersistentData` with a scoped, thread-local equivalent first.

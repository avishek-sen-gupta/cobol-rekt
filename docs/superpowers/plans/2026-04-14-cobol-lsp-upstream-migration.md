# COBOL LSP Upstream Migration Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate `smojol` from its pinned fork of `che-che4z-lsp-for-cobol` (last merged at upstream `2.2.0`, partially started at `2.4.1`) to upstream `2.4.3` (confirmed latest stable as of 2026-04-14).

**Architecture:** The fork is a git submodule at `che-che4z-lsp-for-cobol-integration/` that was customised to act as an embeddable parser library. The customisations (the `poc` package, grammar modifications, `CliModule`) must be re-applied on top of each new upstream base. This is a **rebase/merge exercise**, not a simple version bump.

**Tech Stack:** Java 21, Maven multi-module, ANTLR 4.13.2, Google Guice, LSP4J 0.14.0

---

## Context

The project is a COBOL analysis toolkit (COBOL-REKT). It depends on the Eclipse Che4z COBOL Language Server as an embedded parser — **not** as a running LSP server. To do this, a fork was created (`avishek-sen-gupta/che-che4z-lsp-for-cobol-integration`, `poc` branch) which layered custom classes on top of the upstream to expose the parser as a Java library.

The fork was last merged from upstream tag `2.2.0`. A subsequent partial merge of `2.4.1` exists in the `poc` branch (commits `ede96acbf`, `0cbd6079d`, `9e6ed1546`) but the smojol root project's **submodule pointer has not been updated** to those commits. The `poc` branch tip may compile but has not been validated.

The highest known upstream tag in the fork's git history is `2.4.1`. The upstream may have released beyond `2.4.1` since then.

### The Fork's Key Customisations (must survive every upgrade)

| Location | What | Why |
|---|---|---|
| `server/common/src/.../poc/PersistentData.java` | Global static store for dialect parse tree fragments | Replaces LSP server's runtime DI with a static lookup |
| `server/common/src/.../poc/LocalisedDialect.java` | Dialect metadata carrier | Attached to `AnnotatedParserRuleContext` |
| `server/common/src/.../poc/AnnotatedParserRuleContext.java` | Subclass of `ParserRuleContext` with `customData` map | Stores dialect node annotations |
| `CobolParser.g4` — `dialectNodeFiller` rule (line 2320) | Placeholder grammar rule | Marks where dialect fragments were stripped so they can be re-injected post-parse |
| `server/engine/src/.../cli/di/CliModule.java` | Guice module | Wires the parsing engine without an LSP server runtime |
| `server/engine/src/.../cli/modules/CliClientProvider.java` | Copybook path provider | Replaces LSP workspace-based copybook resolution with file-system paths |

### Smojol Integration Points (files that must compile against the new fork)

| File | Dependency |
|---|---|
| `smojol-toolkit/.../ParsePipeline.java` | `CliModule`, `CliClientProvider`, `TrueDialectServiceImpl`, `AnalysisContext`, `Pipeline` + 6 stage classes, `BenchmarkService`, `ErrorFinalizerService`, `GrammarPreprocessor` |
| `smojol-toolkit/.../FlowchartNodeListener.java` | `CobolParserBaseListener`, `CobolParser.StatementContext`, `IfThenContext`, `IfStatementContext`, `ParagraphContext` |
| `smojol-core/.../DialectIntegratorListener.java` | `CobolParserBaseListener`, `CobolParser.DialectNodeFillerContext`, `LocalisedDialect`, `PersistentData` |
| `smojol-core/.../DialectContainerNode.java` | `CobolParser.DialectNodeFillerContext`, `CobolLexer.COMPUTATIONAL`, `LocalisedDialect` |
| `smojol-core/.../CobolEntityNavigator.java` | `CobolParser.ProcedureDivisionBodyContext`, `CobolParser.DataDivisionContext` |
| `smojol-core/.../AntlrCobolExpressionVisitor.java` (+13 subclasses) | `CobolParserBaseVisitor<T>` |
| `smojol-core/.../dialect/LanguageDialect.java` | `AnalysisConfig` (factory methods `.substitutingDefaultConfig()`, `.idmsConfig()`), `CobolParser.DialectStatementContext`, `CobolParser.DialectNodeFillerContext` |
| `smojol-toolkit/ast/` — 33 FlowNode files | Various `CobolParser.*Context` types (192 usages across 33 files — see Task 2.7) |

---

## Phase 0 — Research & Decision Gate

**Before writing any code**, answer all of these. The outcome of Task 0.1 determines which path Phase 1 takes.

### Task 0.1 — Confirm upstream does not publish embeddable Maven artifacts

**Decision already made: Path A (Fork update).** Upstream is an LSP server, not an embeddable library. Continue maintaining the fork; target is `2.4.3`.

- [ ] Quick sanity check: search Maven Central for `groupId: org.eclipse.lsp.cobol` — if embeddable JARs have appeared at `2.4.3`, escalate to Path B (out of scope for this plan). Otherwise proceed.
- [ ] `TARGET_VERSION = 2.4.3` (confirmed latest stable as of 2026-04-14)

### Task 0.2 — Assess the partial 2.4.1 work already done in `poc` (Path A only)

The `poc` branch tip already has three post-2.4.1 commits:
```
9e6ed1546 conditionalStatementCall can contain dialectStatement
0cbd6079d Making code compile          ← WARNING: only compiles, not tested
ede96acbf Temp commit after re-merging 2.4.1
```
The smojol root project's submodule pointer is currently **detached** at `7b7c08023` ("Allowing basic AND/OR in SELECT CASE conditionals for DB2 SQL"). The poc branch tip (`9e6ed1546`) is ~450 commits ahead and contains `7b7c08023` in its history — verified as a linear ancestor, not a divergent lineage. No work will be lost by switching to poc.

```bash
cd che-che4z-lsp-for-cobol-integration
git checkout poc
# Build AND test — "compiles" is insufficient given the "Temp" commit message
mvn clean install -f server/pom.xml
```

- [ ] Record which modules pass/fail.
- [ ] **Gate: all modules must pass `mvn install` (including tests) before proceeding.** If tests fail, fix them (grammar regression) before advancing.

### Task 0.3 — Scope the 2.4.1 → 2.4.3 delta

`TARGET_VERSION = 2.4.3 > 2.4.1`, so after verifying the poc branch tip, a further upstream merge is required (Phase 1, Task 1.2).

**Important:** Tag `2.4.3` does NOT exist locally — only 2.4.0 and 2.4.1 are present. Must fetch first.

```bash
cd che-che4z-lsp-for-cobol-integration
git fetch upstream --tags
# Verify the tag arrived:
git tag -l '2.4*'
# Preview what changed between 2.4.1 and 2.4.3 in grammar, engine, AND common
git diff 2.4.1..2.4.3 --stat -- server/parser/src/main/antlr4/ server/engine/src/main/java/ server/common/src/main/java/
# Also diff the CLI module and pipeline (not just grammar):
git diff 2.4.1..2.4.3 -- server/engine/src/main/java/org/eclipse/lsp/cobol/cli/ > /tmp/cli-241-to-243.diff
git diff 2.4.1..2.4.3 -- server/common/src/main/java/org/eclipse/lsp/cobol/common/pipeline/ > /tmp/pipeline-241-to-243.diff
# Check if Maven version strings changed:
git diff 2.4.1..2.4.3 -- server/pom.xml server/engine/pom.xml server/parser/pom.xml server/common/pom.xml server/dialect-idms/pom.xml
```

- [ ] Review the stat output — if grammar files changed, this is higher risk; if only Java files, likely API surface changes only.
- [ ] Review CLI and pipeline diffs — watch for changes to `CliModule` bindings, `Pipeline.add()` signature, stage class constructors.
- [ ] Review Maven version diffs — if upstream changed from `1.0-SNAPSHOT` / `1.0.0-SNAPSHOT` to something else, consumer POMs must be updated (see Task 2.9).
- [ ] Proceed to Task 0.4 to extract the poc-specific grammar patch before merging.

### Task 0.4 — Extract the poc grammar patch (correct baseline)

The naive `git diff 2.4.1 HEAD` mixes upstream-2.4.1 changes with poc customisations. Use the actual merge-base:

```bash
cd che-che4z-lsp-for-cobol-integration
# Find the actual commit where 2.4.1 was merged into poc
MERGE_BASE=$(git merge-base 2.4.1 poc)
echo "Merge base: $MERGE_BASE"

# This diff shows ONLY the poc-specific additions on top of 2.4.1
git diff ${MERGE_BASE}..poc -- server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolParser.g4 > /tmp/grammar-poc-patch.diff
git diff ${MERGE_BASE}..poc -- server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolLexer.g4 >> /tmp/grammar-poc-patch.diff
git diff ${MERGE_BASE}..poc -- server/common/src/main/java/org/eclipse/lsp/cobol/common/poc/ > /tmp/poc-classes-patch.diff
```

Review `/tmp/grammar-poc-patch.diff` — these are the additions that must survive every upstream merge:
- `dialectNodeFiller` rule body
- All grammar rules that reference `dialectNodeFiller`
- `DIALECT_MARKER`, `DIALECT_IF`, `DIALECT_SCHEMA_SECTION` tokens in the lexer

### Task 0.5 — API surface diff in `engine` and `common` modules

```bash
cd che-che4z-lsp-for-cobol-integration
# Diff from last known good base (2.2.0 is where smojol was last confirmed to compile)
git diff 2.2.0..2.4.1 -- server/engine/src/main/java/org/eclipse/lsp/cobol/cli/ > /tmp/cli-module-diff.diff
git diff 2.2.0..2.4.1 -- server/engine/src/main/java/org/eclipse/lsp/cobol/core/engine/ > /tmp/engine-api-diff.diff
git diff 2.2.0..2.4.1 -- server/common/src/main/java/org/eclipse/lsp/cobol/common/pipeline/ > /tmp/pipeline-api-diff.diff
```

- [ ] Review `cli-module-diff.diff` — watch for changes to `CliModule` constructor or bindings. **Critical:** `ParsePipeline` retrieves 8 classes via Guice DI from `CliModule`: `CliClientProvider`, `TrueDialectServiceImpl`, `BenchmarkService`, `ErrorFinalizerService`, `DialectService`, `MessageService`, `GrammarPreprocessor`, `ParseTreeListener`. If any binding was removed/renamed, runtime failure.
- [ ] Review `pipeline-api-diff.diff` — watch for changes to `Pipeline.add()`, `Pipeline.run()`, `PipelineResult.getLastStageResult()`, `StageResult.getData()`.
- [ ] Review `engine-api-diff.diff` — watch for `AnalysisContext` constructor signature changes. Current signature: `AnalysisContext(ExtendedDocument, AnalysisConfig, BenchmarkSession, String uri, String text, CobolLanguageId)`.
- [ ] Note any class renames or removed methods that `ParsePipeline.java` calls.
- [ ] Check the 6 stage class constructor signatures still match:
  - `DialectCompilerDirectiveStage(DialectService)`
  - `CompilerDirectivesStage(MessageService)`
  - `DialectProcessingStage(DialectService, CleanerPreprocessor)`
  - `PreprocessorStage(GrammarPreprocessor, CleanerPreprocessor)`
  - `ImplicitDialectProcessingStage(DialectService)`
  - `ParserStage(MessageService, ParseTreeListener)`

### Task 0.6 — Verify dialect re-injection point in the new pipeline

`DialectIntegratorListener` walks the parse tree after `pipeline.run()` completes. Check whether this post-pipeline walk is still the right place in 2.4.1:

```bash
# In upstream 2.4.1, how does the engine expose the final parse tree?
git diff 2.2.0..2.4.1 -- server/engine/src/main/java/org/eclipse/lsp/cobol/core/engine/CobolLanguageEngine.java
```

- [ ] Verify `ParserStageResult.getTree()` still returns the same `ParserRuleContext` after dialect processing (not a pre-dialect version).
- [ ] If the pipeline now applies dialect re-injection internally, `DialectIntegratorListener` may be redundant — document this.

---

## Phase 1 — Get the Fork Building at TARGET_VERSION

### Task 1.1 — Update submodule to poc branch tip

```bash
# In the smojol root
cd che-che4z-lsp-for-cobol-integration
git checkout poc
cd ..
# Update the submodule reference
git add che-che4z-lsp-for-cobol-integration
git commit -m "chore: update lsp submodule to poc branch tip (2.4.1 base)"
```

- [ ] **Gate: only proceed after Task 0.2 confirms `poc` passes all tests, not just compiles.**

### Task 1.2 — Merge upstream 2.4.3 into poc

```bash
cd che-che4z-lsp-for-cobol-integration
git fetch upstream --tags
# Verify tag arrived:
git tag -l '2.4.3'
git checkout poc
git merge 2.4.3
# Resolve merge conflicts, prioritising:
# - Keep ALL poc-specific files (PersistentData, LocalisedDialect, AnnotatedParserRuleContext, CliModule)
# - Reapply dialectNodeFiller additions to CobolParser.g4 if they were dropped
```

- [ ] Resolve conflicts in `CobolParser.g4`: upstream may have changed rule bodies that the poc patched. Re-apply the `dialectNodeFiller` insertion at the same semantic locations (each place in the grammar where a statement or identifier could be a dialect placeholder).
- [ ] Run: `mvn clean install -f server/pom.xml -DskipTests` — must pass.
- [ ] Push poc branch: `git push origin poc`
- [ ] Commit updated submodule pointer in smojol root.

### Task 1.3 — Verify the fork's own tests still pass

```bash
mvn test -f che-che4z-lsp-for-cobol-integration/server/pom.xml
```

- [ ] Run tests. Note any failures — they indicate grammar regressions from the merge.
- [ ] Fixing fork test failures is in scope; they test the grammar works for standard COBOL parsing.

---

## Phase 2 — Fix Smojol Compilation Against the Updated Fork

After the fork builds, the smojol modules (`smojol-core`, `smojol-toolkit`) will likely have compilation errors due to API changes in the fork. Fix them module by module.

### Task 2.1 — Build smojol and collect all compilation errors

```bash
mvn clean install -pl smojol-core,smojol-toolkit -am -DskipTests 2>&1 | tee /tmp/smojol-build-errors.txt
```

- [ ] Collect the full error list. Group errors by file.

### Task 2.2 — Fix ParsePipeline.java

**File:** `smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/pipeline/ParsePipeline.java`

All 27 imports from the fork that must resolve (grouped by risk):
```
# DI / Module (HIGH RISK — if CliModule bindings change, runtime failure)
org.eclipse.lsp.cobol.cli.di.CliModule
org.eclipse.lsp.cobol.cli.modules.CliClientProvider

# Pipeline framework (HIGH RISK — generic type changes break casts)
org.eclipse.lsp.cobol.common.pipeline.Pipeline
org.eclipse.lsp.cobol.common.pipeline.PipelineResult
org.eclipse.lsp.cobol.common.pipeline.StageResult
org.eclipse.lsp.cobol.core.engine.analysis.AnalysisContext

# Document / Preprocessing
org.eclipse.lsp.cobol.common.CleanerPreprocessor
org.eclipse.lsp.cobol.common.ResultWithErrors
org.eclipse.lsp.cobol.common.mapping.ExtendedDocument
org.eclipse.lsp.cobol.common.mapping.ExtendedText
org.eclipse.lsp.cobol.core.preprocessor.delegates.GrammarPreprocessor

# Services (retrieved via Guice DI from CliModule)
org.eclipse.lsp.cobol.common.benchmark.BenchmarkService
org.eclipse.lsp.cobol.common.benchmark.BenchmarkSession
org.eclipse.lsp.cobol.common.benchmark.Measurement
org.eclipse.lsp.cobol.common.message.MessageService
org.eclipse.lsp.cobol.core.engine.dialects.DialectService
org.eclipse.lsp.cobol.core.engine.errors.ErrorFinalizerService
org.eclipse.lsp.cobol.dialects.TrueDialectServiceImpl

# Error types
org.eclipse.lsp.cobol.common.error.SyntaxError
org.eclipse.lsp.cobol.common.dialects.CobolLanguageId

# 6 stage classes (constructor signatures documented in Task 0.5)
org.eclipse.lsp.cobol.dialects.ibm.DialectCompilerDirectiveStage
org.eclipse.lsp.cobol.dialects.ibm.CompilerDirectivesStage
org.eclipse.lsp.cobol.dialects.ibm.DialectProcessingStage
org.eclipse.lsp.cobol.dialects.ibm.PreprocessorStage
org.eclipse.lsp.cobol.dialects.ibm.ImplicitDialectProcessingStage
org.eclipse.lsp.cobol.dialects.ibm.ParserStage
```

- [ ] For each import that fails: use `grep -r "class CliModule" che-che4z-lsp-for-cobol-integration/` to find the new location.
- [ ] Verify `AnalysisContext` constructor signature. The current call is:
  ```java
  new AnalysisContext(
      new ExtendedDocument(resultWithErrors.getResult(), text),
      dialect.analysisConfig(dialectJarPath),
      benchmarkService.startSession(), src.toURI().toString(), text, CobolLanguageId.COBOL)
  ```
  If the constructor changed, update accordingly.
- [ ] Verify `StageResult<ParserStageResult>` — ensure `ParserStageResult` class still exists and `getData().getTree()` still returns `ParserRuleContext`.
- [ ] Run `mvn compile -pl smojol-toolkit -am` until `ParsePipeline.java` compiles clean.
- [ ] Commit:
  ```bash
  git add smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/pipeline/ParsePipeline.java
  git commit -m "fix: update ParsePipeline.java imports and API calls for LSP fork upgrade"
  ```

### Task 2.3 — Fix DialectIntegratorListener.java and DialectContainerNode.java

**Files:**
- `smojol-core/src/main/java/org/smojol/common/idms/DialectIntegratorListener.java`
- `smojol-core/src/main/java/org/smojol/common/idms/DialectContainerNode.java`

These depend on:
- `CobolParser.DialectNodeFillerContext` — verify the grammar rule `dialectNodeFiller` still exists in the updated fork's `CobolParser.g4` (it should, since it's a poc addition)
- `org.eclipse.lsp.cobol.common.poc.PersistentData` — verify the class exists in updated fork
- `org.eclipse.lsp.cobol.common.poc.LocalisedDialect` — same
- `CobolLexer.COMPUTATIONAL` token — verify it still exists in `CobolLexer.g4`

```bash
grep -n "dialectNodeFiller" che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolParser.g4
grep -rn "class PersistentData" che-che4z-lsp-for-cobol-integration/
grep -n "COMPUTATIONAL" che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolLexer.g4
```

- [ ] If `DialectNodeFillerContext` is gone (e.g., the grammar rule was renamed), update both listener and container node.
- [ ] If `PersistentData`/`LocalisedDialect` moved packages, update imports.
- [ ] Commit:
  ```bash
  git add smojol-core/src/main/java/org/smojol/common/idms/
  git commit -m "fix: update dialect listener/container imports for LSP fork upgrade"
  ```

### Task 2.4 — Fix CobolEntityNavigator.java

**File:** `smojol-core/src/main/java/org/smojol/common/navigation/CobolEntityNavigator.java`

Depends on: `CobolParser.ProcedureDivisionBodyContext`, `CobolParser.DataDivisionContext`

```bash
grep -n "procedureDivisionBody\|ProcedureDivisionBody\|DataDivision" \
  che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolParser.g4
```

- [ ] Verify both context class names still match grammar rule names in the updated grammar. Grammar rule names map to `RuleName + "Context"` — e.g., rule `procedureDivisionBody` generates `ProcedureDivisionBodyContext`.
- [ ] If renamed, update both the `instanceof` checks and cast expressions in `CobolEntityNavigator`.
- [ ] Commit:
  ```bash
  git add smojol-core/src/main/java/org/smojol/common/navigation/CobolEntityNavigator.java
  git commit -m "fix: update CobolEntityNavigator context class names for LSP fork upgrade"
  ```

### Task 2.5 — Fix FlowchartNodeListener.java

**File:** `smojol-toolkit/src/main/java/org/smojol/toolkit/ast/FlowchartNodeListener.java`

Depends on: `CobolParser.StatementContext`, `IfThenContext`, `IfStatementContext`, `ParagraphContext`

```bash
grep -n "^statement\|^ifStatement\|^ifThen\|^paragraph " \
  che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolParser.g4
```

- [ ] Verify all four grammar rules still exist with the same names. If renamed, update the `@Override` method signatures.
- [ ] Commit:
  ```bash
  git add smojol-toolkit/src/main/java/org/smojol/toolkit/ast/FlowchartNodeListener.java
  git commit -m "fix: update FlowchartNodeListener for grammar rule changes in LSP fork"
  ```

### Task 2.6 — Fix AntlrCobolExpressionVisitor and subclasses

**Files:** `smojol-core/src/main/java/org/smojol/common/vm/expression/` — 14 files

All extend `CobolParserBaseVisitor<T>` from `org.eclipse.lsp.cobol.core`. These only use the base class type parameter — the actual `visit*` methods are called via dynamic dispatch. Verify:

```bash
grep -rn "extends CobolParserBaseVisitor\|import.*CobolParserBaseVisitor" smojol-core/src/main/java/
```

- [ ] Check that `CobolParserBaseVisitor` is still in package `org.eclipse.lsp.cobol.core` (it's generated by ANTLR from `CobolParser.g4` and put into the `core` package).
- [ ] If the generated class is now in a different package (e.g., `org.eclipse.lsp.cobol.parser`), update the import in `AntlrCobolExpressionVisitor` — the change propagates to all 13 subclasses automatically since they don't re-import it.
- [ ] Check specific grammar rule methods called in the visitor subclasses by grepping for `visitArithmeticExpression\|visitCondition\|visitBasis\|visitPowerExpression` — verify the grammar rules still exist.
- [ ] Commit:
  ```bash
  git add smojol-core/src/main/java/org/smojol/common/vm/expression/
  git commit -m "fix: update CobolParserBaseVisitor imports for LSP fork upgrade"
  ```

### Task 2.7 — Fix LanguageDialect.java

**File:** `smojol-core/src/main/java/org/smojol/common/dialect/LanguageDialect.java`

Depends on:
- `org.eclipse.lsp.cobol.common.AnalysisConfig` — verify factory methods `AnalysisConfig.substitutingDefaultConfig()` and `AnalysisConfig.idmsConfig()` still exist with the same signatures
- `CobolParser.DialectStatementContext` — verify grammar rule `dialectStatement` still exists
- `CobolParser.DialectNodeFillerContext` — verify grammar rule `dialectNodeFiller` still exists (poc addition)

```bash
grep -n "^dialectStatement\|^dialectNodeFiller" \
  che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolParser.g4
grep -rn "class AnalysisConfig\|substitutingDefaultConfig\|idmsConfig" \
  che-che4z-lsp-for-cobol-integration/server/common/src/main/java/
```

- [ ] If `AnalysisConfig` factory methods changed signatures, update `LanguageDialect.analysisConfig()`.
- [ ] If `DialectStatementContext` was renamed (e.g., in upstream grammar refactor), update `verifyNoNullDialectStatements()`.
- [ ] Commit:
  ```bash
  git add smojol-core/src/main/java/org/smojol/common/dialect/LanguageDialect.java
  git commit -m "fix: update LanguageDialect AnalysisConfig and dialect context calls for LSP fork upgrade"
  ```

### Task 2.8 — Fix FlowNode files blast radius

**Scope:** `smojol-toolkit/src/main/java/org/smojol/toolkit/ast/` — 33 files, 192 `CobolParser.*Context` usages

The FlowNode files bind tightly to grammar rule names via `instanceof` checks and constructor casts. If upstream renamed any statement-level rule, multiple FlowNode files will fail together.

```bash
# First: inventory ALL CobolParser.*Context types used across the FlowNode files
grep -rh "CobolParser\.[A-Z][A-Za-z]*Context" smojol-toolkit/src/main/java/org/smojol/toolkit/ast/ \
  | grep -o "CobolParser\.[A-Z][A-Za-z]*Context" | sort -u
```

For each unique context class found, verify the corresponding grammar rule name still exists:
```bash
# Example: CobolParser.IfStatementContext → grep for rule "ifStatement"
# Convert CamelCase to camelCase rule name and search
grep -n "^ifStatement\b\|^evaluateStatement\b\|^performStatement\b\|^moveStatement\b\|^searchStatement\b\|^computeStatement\b\|^addStatement\b\|^subtractStatement\b\|^multiplyStatement\b\|^divideStatement\b\|^displayStatement\b\|^goToStatement\b\|^callStatement\b" \
  che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolParser.g4
```

- [ ] Any renamed rule breaks the corresponding FlowNode's constructor — update `instanceof` checks and casts in that FlowNode.
- [ ] `CobolFlowNodeFactory.java` (75 usages) is the highest-risk single file — fix it before the individual FlowNode files.
- [ ] Commit:
  ```bash
  git add smojol-toolkit/src/main/java/org/smojol/toolkit/ast/
  git commit -m "fix: update FlowNode context class names for grammar rule changes in LSP fork"
  ```

### Task 2.9 — Fix Maven POM versions and any remaining compilation errors

**Maven version risk:** The fork currently uses `1.0-SNAPSHOT` for most modules but `1.0.0-SNAPSHOT` for `engine`. If upstream 2.4.3 changes these version strings, all consumer POMs break silently (Maven will just say "could not resolve artifact").

- [ ] Check the fork's module versions after the merge:
  ```bash
  grep -A2 '<artifactId>engine</artifactId>' che-che4z-lsp-for-cobol-integration/server/engine/pom.xml
  grep -A2 '<artifactId>parser</artifactId>' che-che4z-lsp-for-cobol-integration/server/parser/pom.xml
  grep -A2 '<artifactId>dialect-idms</artifactId>' che-che4z-lsp-for-cobol-integration/server/dialect-idms/pom.xml
  grep -A2 '<artifactId>common</artifactId>' che-che4z-lsp-for-cobol-integration/server/common/pom.xml
  ```
- [ ] Update consumer POMs if versions changed:
  - `smojol-core/pom.xml` — depends on `parser` at `1.0-SNAPSHOT`
  - `smojol-toolkit/pom.xml` — depends on `engine` at `1.0.0-SNAPSHOT`, `dialect-idms` at `1.0-SNAPSHOT`, `parser` at `1.0-SNAPSHOT`
- [ ] Run full build to surface remaining errors:
  ```bash
  mvn clean install -pl smojol-core,smojol-toolkit,smojol-cli,smojol-api -am -DskipTests 2>&1 | grep "error:" | sort -u
  ```
- [ ] For each remaining `cannot find symbol` error, find the new location in the fork:
  ```bash
  # Replace SomeClass with the actual class name from the error message
  grep -rn "class SomeClass\b" che-che4z-lsp-for-cobol-integration/server/
  # Example: if the error mentions "AnalysisResult" not found:
  grep -rn "class AnalysisResult\b" che-che4z-lsp-for-cobol-integration/server/
  ```
- [ ] Update the import in the failing smojol file to match the new package.
- [ ] Once the full build produces 0 errors, commit:
  ```bash
  git add smojol-core/ smojol-toolkit/ smojol-cli/ smojol-api/
  git commit -m "fix: fix remaining compilation errors after LSP fork upgrade"
  ```

### Task 2.10 — Create IDMS dialect regression test

**Files:**
- Create: `smojol-toolkit/test-code/idms/idms-simple.cbl`
- Create: `smojol-toolkit/src/test/java/org/smojol/toolkit/analysis/IdmsDialectIntegrationTest.java`

- [ ] Create the synthetic IDMS COBOL test file. Use syntax confirmed in the fork's `tests/test_files/project/cobol-idms/DACOSMPL.cbl`:

  `smojol-toolkit/test-code/idms/idms-simple.cbl`:
  ```cobol
         IDENTIFICATION DIVISION.
         PROGRAM-ID. IDMS-TEST.
         ENVIRONMENT DIVISION.
         DATA DIVISION.
         WORKING-STORAGE SECTION.
         01 TBLCRI-XW5 PIC X(10).
         01 CRI-BW5    PIC X(10).
         PROCEDURE DIVISION.
         MAIN-PARA.
             ROW GET TBLCRI-XW5 ON CRI-BW5.
             STOP RUN.
  ```

- [ ] Verify the dialect JAR exists before writing the test:
  ```bash
  ls che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar
  # If not present: mvn package -f che-che4z-lsp-for-cobol-integration/server/pom.xml -DskipTests
  ```

- [ ] Create the test class:

  `smojol-toolkit/src/test/java/org/smojol/toolkit/analysis/IdmsDialectIntegrationTest.java`:
  ```java
  package org.smojol.toolkit.analysis;

  import com.mojo.algorithms.id.UUIDProvider;
  import org.antlr.v4.runtime.tree.ParseTree;
  import org.junit.jupiter.api.Test;
  import org.smojol.common.ast.CobolTreeVisualiser;
  import org.smojol.common.dependency.ComponentsBuilder;
  import org.smojol.common.dialect.LanguageDialect;
  import org.smojol.common.idms.DialectContainerNode;
  import org.smojol.common.navigation.CobolEntityNavigator;
  import org.smojol.common.navigation.EntityNavigatorBuilder;
  import org.smojol.common.resource.LocalFilesystemOperations;
  import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
  import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
  import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
  import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

  import java.io.File;
  import java.io.IOException;
  import java.nio.file.Paths;
  import java.util.List;

  import static org.junit.jupiter.api.Assertions.assertFalse;

  class IdmsDialectIntegrationTest {
      @Test
      void idmsDialectNodesAreReinjectedAfterParse() throws IOException {
          String idmsTestDir = Paths.get(System.getProperty("user.dir"), "test-code/idms").toString();
          String dialectJar = Paths.get(System.getProperty("user.dir"),
              "che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar").toString();

          SourceConfig sourceConfig = new SourceConfig(
              "idms-simple.cbl",
              idmsTestDir,
              List.of(new File(idmsTestDir)),
              dialectJar
          );
          ComponentsBuilder ops = new ComponentsBuilder(
              new CobolTreeVisualiser(),
              new EntityNavigatorBuilder(),
              new UnresolvedReferenceThrowStrategy(),
              new OccursIgnoringFormat1DataStructureBuilder(),
              new UUIDProvider(),
              new LocalFilesystemOperations()
          );
          CobolEntityNavigator navigator = new ParsePipeline(sourceConfig, ops, LanguageDialect.IDMS).parse();
          List<ParseTree> dialectNodes = navigator.findAllByCondition(
              n -> n.getClass() == DialectContainerNode.class
          );
          assertFalse(dialectNodes.isEmpty(),
              "Expected at least one IDMS DialectContainerNode to be re-injected into the parse tree");
      }
  }
  ```

- [ ] Run the test:
  ```bash
  mvn test -pl smojol-toolkit -Dtest=IdmsDialectIntegrationTest -am
  ```
  Expected: **PASS** — at least one `DialectContainerNode` found in the tree.
  If FAIL with `dialectNodes.isEmpty()`: the IDMS re-injection pipeline is broken — check `DialectIntegratorListener.enterDialectNodeFiller()` and `PersistentData.getDialectNode()`.

- [ ] Commit:
  ```bash
  git add smojol-toolkit/test-code/idms/ \
          smojol-toolkit/src/test/java/org/smojol/toolkit/analysis/IdmsDialectIntegrationTest.java
  git commit -m "test: add IDMS dialect re-injection regression test"
  ```

---

## Phase 3 — Test Verification

### Task 3.1 — Run smojol-core tests

```bash
mvn test -pl smojol-core
```

- [ ] All tests must pass. The key tests are: `DataTypesTest`, `DataLayoutBuilderTest`, `ConditionExpressionTest`.
- [ ] Failures here typically mean expression grammar rules were renamed. Check the failing visitor method name against the grammar.

### Task 3.2 — Run smojol-toolkit regression tests

Test COBOL files are present in the repo (no external setup needed):
- `smojol-toolkit/test-code/flow-ast/no-branches.cbl` — used by most regression tests
- `smojol-toolkit/test-code/structure/data-structures.cbl` — used by data structure test
- `smojol-test-code/simple-if.cbl` — used by reaching condition test

```bash
mvn test -pl smojol-toolkit
```

Key regression tests:
- `WriteFlowASTTaskRegressionTest`
- `BuildTranspilerFlowgraphTaskRegressionTest`
- `BuildProgramDependenciesTaskRegressionTest`
- `ReachingConditionTaskRegressionTest`
- `CobolDataStructureBuilderTest`
- `IdmsDialectIntegrationTest` ← new; exercises the IDMS dialect re-injection path

- [ ] All must pass. Failures in the COBOL tests mean the parse pipeline API changed or grammar rule names changed. Failure in `IdmsDialectIntegrationTest` specifically means the IDMS re-injection pipeline is broken (check `DialectIntegratorListener`, `PersistentData`, `DialectContainerNode`).

### Task 3.3 — Build smojol-api and smoke test the CLI

```bash
# Verify smojol-api compiles (no direct LSP imports, but transitive exposure via smojol-toolkit)
mvn compile -pl smojol-api -am -DskipTests

# Package and smoke-test CLI (PicoCLI multi-command app)
mvn package -pl smojol-cli -am -DskipTests
java -jar smojol-cli/target/smojol-cli.jar validate \
  -s smojol-toolkit/test-code/flow-ast \
  -cp smojol-toolkit/test-code/flow-ast \
  no-branches.cbl
```

- [ ] `smojol-api` must compile clean. (Note: verified no direct LSP imports in smojol-api — only transitive.)
- [ ] The CLI `validate` command must parse the COBOL file without errors.

---

## Phase 4 — Finalise

### Task 4.1 — Update .gitmodules submodule reference if needed

If the submodule was re-pointed to a new branch or commit:

```bash
# In smojol root
git submodule update --remote --merge
git add .gitmodules che-che4z-lsp-for-cobol-integration
git commit -m "chore: update COBOL LSP submodule to upstream 2.4.3 (poc branch)"
```

### Task 4.2 — Document the upgrade and lock in the poc patch inventory

The fork is permanently private. The only obligation is to keep the poc patch set so well-segregated and documented that any future upstream merge takes minutes to understand, not days to reconstruct.

In the repository root, update or create `COBOL-LSP-INTEGRATION.md` with the following sections:

**Section 1 — Upstream base**
- Upstream repo: `https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol`
- Fork branch: `poc` in `avishek-sen-gupta/che-che4z-lsp-for-cobol-integration`
- Current upstream base: `2.4.3`
- How to find the merge-base for future upgrades: `git merge-base <upstream-tag> poc`

**Section 2 — poc patch inventory (must survive every future merge)**

| File | What | Why |
|---|---|---|
| `server/common/src/.../poc/PersistentData.java` | Static store for dialect parse-tree fragments | No DI container at parse time — static lookup replaces runtime injection |
| `server/common/src/.../poc/LocalisedDialect.java` | Dialect metadata carrier | Attached to `AnnotatedParserRuleContext` nodes |
| `server/common/src/.../poc/AnnotatedParserRuleContext.java` | `ParserRuleContext` subclass with `customData` map | Stores GUID → dialect-node mappings during parse |
| `CobolParser.g4` — `dialectNodeFiller` rule + references | Placeholder rule | Marks where IDMS fragments were stripped so they can be re-injected post-parse |
| `CobolLexer.g4` — `DIALECT_MARKER`, `DIALECT_IF`, `DIALECT_SCHEMA_SECTION` | Lexer tokens | Required by `dialectNodeFiller` rule |
| `server/engine/src/.../cli/di/CliModule.java` | Guice module | Wires parse engine without LSP server runtime |
| `server/engine/src/.../cli/modules/CliClientProvider.java` | Copybook path provider | Replaces LSP workspace-based copybook resolution |

**Section 3 — How to merge a future upstream release**
1. `git merge-base <new-upstream-tag> poc` → extract poc-only grammar diff
2. Merge new tag into `poc`
3. Resolve conflicts: keep ALL files in the poc patch inventory above
4. Re-apply `dialectNodeFiller` grammar insertions if upstream changed surrounding rules
5. Build + test: `mvn clean install -f server/pom.xml`
6. Update this document's "Current upstream base" line

**Section 4 — Architectural constraint**
`PersistentData` is not thread-safe by design. smojol parses files sequentially. Never parallelize calls to `ParsePipeline` in the same JVM without replacing `PersistentData` with a scoped, thread-local equivalent first.

---

## Hard Constraints (must be documented in code and COBOL-LSP-INTEGRATION.md)

`PersistentData` uses unprotected static mutable state (`public static int counter`, `static List<AnnotatedParserRuleContext> trees`). smojol parses files sequentially with no plans to parallelize — this is a **deliberate architectural constraint**, not oversight. Add a `@NotThreadSafe` annotation (or a prominent Javadoc comment) to `PersistentData` and note it in `COBOL-LSP-INTEGRATION.md` so any future attempt at parallelism is caught at review time rather than surfacing as a silent correctness bug in dialect node re-injection.

---

## Critical Risks

| Risk | Severity | Mitigation |
|---|---|---|
| Grammar rules renamed between 2.2.0 and TARGET in the procedure division or expression sub-rules | HIGH | Run Phase 0 grammar diff before any code changes |
| `AnalysisContext` constructor signature changed (6 params: `ExtendedDocument, AnalysisConfig, BenchmarkSession, String, String, CobolLanguageId`) | HIGH | Inspect diff from Phase 0, fix in Task 2.2 |
| `CliModule` Guice bindings changed — 8 classes are retrieved via DI (`CliClientProvider`, `TrueDialectServiceImpl`, `BenchmarkService`, `ErrorFinalizerService`, `DialectService`, `MessageService`, `GrammarPreprocessor`, `ParseTreeListener`) | HIGH | Diff `CliModule.java` in Task 0.5; runtime failure if any binding removed/renamed |
| 6 pipeline stage constructor signatures changed | HIGH | Document exact signatures in Task 0.5 and verify after merge |
| `poc` classes (`PersistentData`, `LocalisedDialect`) conflict with upstream changes to `common` module | HIGH | Review `common` module diff in Phase 0.5; may need to update `AnnotatedParserRuleContext` if it extends a changed base class |
| "Temp commit after re-merging 2.4.1" — poc branch compiles but is untested | HIGH | Task 0.2 gate: `mvn install` (with tests) must pass before the smojol submodule pointer is updated |
| Submodule HEAD (`7b7c08023`) is detached — poc branch (`9e6ed1546`) is ~450 commits ahead | LOW | Verified: `7b7c08023` is an ancestor of poc, no divergent work. Task 1.1 just needs `git checkout poc` |
| Maven version strings change in upstream 2.4.3 (currently `1.0-SNAPSHOT` / `1.0.0-SNAPSHOT` for engine) | MEDIUM | Diff POMs in Task 0.3; update consumer POMs in Task 2.9 |
| Grammar diff baseline is wrong if using `git diff 2.4.1 HEAD` — mixes upstream and poc changes | MEDIUM | Task 0.4 uses `git merge-base` to get the clean poc-only patch |
| Upstream changed how dialect plugins are loaded (JAR discovery) in 2.3.x / 2.4.x | MEDIUM | Review `DialectService` changes in engine diff |
| Tag `2.4.3` not present locally (only 2.4.0, 2.4.1 exist) — must `git fetch upstream --tags` | LOW | Added explicit fetch step to Tasks 0.3 and 1.2 |
| Java 8 → Java 11+ target in newer upstream modules | LOW | The fork already uses Java 8 compile target; smojol uses 21; no conflict expected |

---

## File Map

| File | Change Type |
|---|---|
| `che-che4z-lsp-for-cobol-integration/` (submodule) | Update git ref to poc branch at TARGET_VERSION |
| `pom.xml` (root) | No change expected (submodule is a `<module>` reference, not versioned) |
| `smojol-core/pom.xml` | Update `parser` dependency version IF fork version strings changed |
| `smojol-toolkit/pom.xml` | Update `engine` (currently `1.0.0-SNAPSHOT`), `dialect-idms`, `parser` versions IF fork version strings changed |
| `smojol-toolkit/.../ParsePipeline.java` | Fix imports / API call sites |
| `smojol-toolkit/.../FlowchartNodeListener.java` | Fix method signatures if grammar rules renamed |
| `smojol-core/.../DialectIntegratorListener.java` | Fix if poc package moved or dialectNodeFiller renamed |
| `smojol-core/.../DialectContainerNode.java` | Fix if DialectNodeFillerContext renamed |
| `smojol-core/.../CobolEntityNavigator.java` | Fix context class names if grammar rules renamed |
| `smojol-core/.../AntlrCobolExpressionVisitor.java` (+ 13 subclasses) | Fix package import if generated class location changed |
| `smojol-core/.../dialect/LanguageDialect.java` | Fix `AnalysisConfig` factory calls; fix if `DialectStatementContext` renamed |
| `smojol-toolkit/ast/` (33 FlowNode files) | Fix `CobolParser.*Context` usages if grammar statement rules renamed; highest risk: `CobolFlowNodeFactory.java` |

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

## Section 4 — Grammar migration patterns (2.2.0 → 2.4.3)

These rule changes required updates across smojol. If a future upstream merge changes the same rules again, the same smojol files will need fixing.

| Old grammar construct | New grammar construct | Migration pattern | smojol files affected |
|---|---|---|---|
| `procedureSection` (separate rule) | `sectionOrParagraph` (combined rule; `SECTION` token distinguishes sections from paragraphs) | Replace `ProcedureSectionContext` with `SectionOrParagraphContext`. **Must check `SECTION() != null`** wherever only sections are expected. Name via `ctx.cobolWord() != null ? ctx.cobolWord().getText() : ctx.integerLiteral(0).getText()` | `CobolFlowNodeFactory`, `CobolFlowNode`, `SectionFlowNode`, `CobolEntityNavigator`, `PerSection`, `FlowchartGenerationStrategy` |
| `tableCall` → direct `arithmeticExpression` list | `tableCall` → `argument` wrapper → `arithmeticExpression` | `.argument().stream().map(ArgumentContext::arithmeticExpression).toList()` | `CobolExpressionBuilder`, `GeneralIdentifierVisitor`, `CobolReferenceBuilder` |
| `performUntil` → direct `condition` | `performUntil` → `performUntilCondition` → `condition` | Add `.performUntilCondition()` before `.condition()` | `FlowIterationBuilder` |
| `functionCall` → direct `functionName` | `functionCall` → `functionReference` → `functionName` | Add `.functionReference()` before `.functionName()` | `FunctionCallExpression`, `FunctionCallCobolReference` |
| `dataPictureClause` → `pictureString+` (list) | `dataPictureClause` → `pictureString` (singular) | Remove `.getFirst()` — access `pictureString()` directly | `CobolDataStructure`, `Format1DataStructure` |
| `CobolDataDivisionParser` (separate grammar) | Merged into `CobolParser` | Change import from `CobolDataDivisionParser` to `CobolParser` | `GenericProcessingFlowNode` |

### Checklist for future grammar upgrades

1. `grep -rn 'CobolParser\.[A-Z][A-Za-z]*Context' smojol-core/ smojol-toolkit/` — inventory all context types in use
2. For each context type, verify the grammar rule still exists: rule `fooBar` generates `FooBarContext`
3. Watch for rules that were split (new intermediate wrappers) or merged (combined contexts needing token-based discrimination)
4. Run `mvn test -pl smojol-core,smojol-toolkit` — all tests must pass before committing

## Section 5 — Architectural constraint

`PersistentData` is not thread-safe by design. smojol parses files sequentially. Never parallelize calls to `ParsePipeline` in the same JVM without replacing `PersistentData` with a scoped, thread-local equivalent first.

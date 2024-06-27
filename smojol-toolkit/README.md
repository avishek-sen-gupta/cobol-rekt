Cobol REKT (Cobol Reverse Engineering KiT)
---------------------------------------------

As of now, there are two main components:

- Program and section flowcharts based on AST
- The SMOJOL Interpreter (WIP)

SMOJOL (SMol Java-powered CobOL) Interpreter
-----------------------------------------------

- Reverse Engineering Use Cases
    - Trace flows to experiment with different conditions
    - Trace variable impact analysis
    - Serve as input for LLM to explain specific flows in the program
    - Serve as a proxy for testing behaviour at section/paragraph level if needed
    - Identify dead code?
    - Try out new rules?
    - Identify different flows in the report - use cases for forward engineering

- Capabilities
    - Support for most control constructs: IF/THEN, NEXT SENTENCE, GO TO, PERFORM, SEARCH...WHEN, IDMS ON
    - Support for expression evaluation in COMPUTE, MOVE, ADD, SUBTRACT, MULTIPLY, DIVIDE
    - Support for interactive resolution of conditions
    - Most common class comparisons supported
    - Support for abbreviated relation condition forms (IF A > 10 OR 20 AND 30...)
    - Functioning type system (supports zoned decimals and alphanumerics) with a large subset of z/OS behaviour compatibility for scenarios undefined in the Cobol standard
    - Support for fixed-size tables and subscripting
    - Support for elementary, composite, and recursive REDEFINES (REDEFINES of REDEFINES)
    - Automatic detection of types from DATA DIVISION specifications
    - Supports evaluation of level 88 variables
    - Support for tracking variable state
    - Set breakpoints based on conditions or specific AST node
    - View current stack at a breakpoint
    - View variable values at a breakpoint
    - Support for different strategies to deal with unresolved record references (ignore / throw exception)
    - Support for listeners to extract specific information about the current state of the program

- Planned
    - PERFORM VARYING
    - PERFORM INLINE...VARYING
    - Initialise values of variables from DATA DIVISION
    - Support for floating point and alphabetic
    - Support for REDEFINES larger than original record
    - Variable snapshot per stack frame
    - Evaluate IDMS expressions
    - ON clauses on common operations
    - ...


| Sym-1 / Sym-2     | S (Sign) | P (Left) | P (Right) | V (Decimal Point) | X (Alphanumeric) | 9 (Number) |
|-------------------|----------|----------|-----------|-------------------|------------------|------------|
| S (Sign)          | -        | X        | X         | X                 | -                | -          |
| P (Left)          | -        | X        | -         | X                 | -                | X          |
| P (Right)         | -        | -        | X         | -                 | -                | -          |
| V (Decimal Point) | -        | -        | X         | -                 | -                | X          |
| X (Alphanumeric)  | -        | -        | -         | -                 | X                | X          |
| 9 (Number)        | -        | -        | X         | X                 | X                | X          |

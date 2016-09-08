# About

`mosmlb` is a project tool for Moscow ML compiler suite. Its purpose
is to process ML Basis files (`.mlb`) and run Moscow ML compiler with
appropriate parameters.

# Goal and current state

The goal of the project is to create standalone Moscow MLB tool which
will implement as many as possible of standard ML Basis features.
Unfortunately it is currently unclear if it is possible to achieve without
integrating Moscow MLB into compiler as a separate frontend.

Right now Moscow MLB tool is implemented as a proof-of-concept:

1. It can read correct ML Basis (.mlb) files, integrating included
ML Basis files into a single parse tree.

2. It can run Moscow ML compiler and linker with hardcoded options
to compile all SML source files mentioned in parse tree.

3. It implements rudimentary error check, full support for
ML Basis syntax, full support for path syntax. Though all 
ML Basis control structures are correctly parsed, they are ignored.
There is no error recovery nor correct parse error report.

# Milestones

## Proof-of-concept

The goal is to implement very rudimentary, still working in ideal
conditions Moscow MLB tool. We are here - `mosmlb` can compile
mosmlb-test.mlb producing mosmlb-test program.

## Basic

Goal - robust and convenient tool, which can correctly process simple
ML Basis files (only includes files of different types, does not
contain any control structures).

TODO:

1. Error handling.
  * Scanner and parser errors.
  * Cycle, file errors.
  * Compiler errors and warnings.

2. Proper implementation of execution pipeline.

3. Correct definition of SML_LIB path variable by Moscow ML build system.

4. Command line parameters (`verbose`, `help` and etc).

5. Test suite.
  * Integration of ML Kit tests.
  * Unit tests (`mosmlb-test`). 
  * Compile MLton if possible.

## Advanced

As complete as possible implementation of Moscow MLB tool. ML kit and MLton
extentions to ML Basis format.

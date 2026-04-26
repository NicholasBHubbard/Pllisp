# Pllisp

A Lisp programming language that compiles to LLVM.

This project is currently in development.

## AI Usage Statement

This project was developed collaboratively by Nicholas B. Hubbard (student) and Claude (Anthropic's AI assistant, via Claude Code). Per course policy, the AI is treated as a team member on a group project.

### Team Member Roles

**Nicholas B. Hubbard (Student):**
- Designed the language: syntax, semantics, type system, and compilation strategy
- Wrote the initial compilation pipeline by hand (parser, type checker, closure conversion, etc)
- Made all architectural decisions (e.g. dictionary-passing for typeclasses, CL-style procedural macros, positional superclass syntax)
- Used Q/A sessions with the AI to explore design tradeoffs and refine implementation details before committing to an approach
- Directed every feature, reviewed all generated code, and debugged issues
- Defined the standard library (PRELUDE.pll) and example programs

**Claude (AI Assistant):**
- Wrote implementation code under the student's direction in an interactive pair-programming workflow
- Wrote test suites (1000+ tests across 16+ test modules)
- Applied the student's design decisions to the codebase: extending the type checker, adding FFI support, implementing higher-kinded types, macro interpreter, etc.

### How to Verify Authorship

The complete git history is included in this repository. Every commit represents a directed collaboration session where the student specified what to build and the AI produced the code. Earlier commits (the initial compilation pipeline) were written directly by the student.

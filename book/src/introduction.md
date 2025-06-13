# Introduction

Tele is an alternative syntax for the Erlang programming language.

This means that Tele code is compiled to Erlang code while being fully interoperable. There are no added semantics or extra standard library.
It's just Erlang. In fact, the syntax is a subset of Erlang's.

The design is intended to be transparent to end users. Meaning libraries or tools that one writes in Tele will be seen as Erlang to the end user.

## Who is this for?

Tele is meant to be an easy functional programming language to learn with a minimal syntax so it can be accessible for a wide audience.
The goal of the language was take the good parts of Erlang and make it feel more "modern" to appeal to people coming from Python or Javascript.

However, due to current tooling limitations Tele is probably best suited for developers who have some familiarity with Erlang.

You might like this if you are an Erlang programmer who:
- Finds it hard to convince your friends or colleagues to code in Erlang because they don't like the Prolog style syntax.
- Are annoyed by the constant syntax errors caused by missing commas, semicolons, periods, etc.

## Goals

- Easy to learn
- Enjoyable to read
- Minimize use of characters to reduce syntax errors
- Consistent but willing to sacrifice grammatical purity for user friendliness

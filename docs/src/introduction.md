# Introduction

Steel is a general purpose programming language, designed for either standalone use, or for use as an extension
language for a host application. It has roots in Scheme, and is compliant with the R5RS spec.

If you're unfamiliar with scheme, starting at the [R5RS specification](https://standards.scheme.org/official/r5rs.pdf) would
be a good start. It also takes strong inspiration from Racket; if Steel varies from the scheme spec, its probably because
Racket has similar behavior. Some deviations are intentional, such as not supporting mutable pairs natively, other deviations
that might exist may not be intentional, and I'm happy to entertain issues raised if something doesn't seem quite right.

Steel is implemented using a bytecode virtual machine, and optionally includes a JIT compiler using Cranelift.
The JIT is a relatively new addition, but can be enabled by supplying the STEEL_JIT=1 environment variable.

symbol is a compiler of a subset of clojure syntax to C++.

Currently symbol supports only longs, doubles, pointers, structs and functions, but support for other datatypes might be added later.

This work is still in alpha stage and no proper releases have been made.

**Suspended**

This project is currently suspended for the following reasons.

The following things were too big challenges for me
* support for both static and dynamic memory allocation
* efficient dynamic memory management 
* gccxml doesn't support templates

If anyone is interested to take over or copy some ideas please contact me.

symbol has working
* type inference for functions and classes
* C++ code generation
* imports

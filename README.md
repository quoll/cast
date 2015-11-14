# cast
**Concrete/Abstract Syntax Trees for Clojure**

The idea of this code is to convert Clojure into a Concrete Syntax Tree (CST) suitable for storage in a database, Datomic in this case. From there it can be loaded back into the CST, and then either emitted back to file format, or "read" into the Clojure Abstract Syntax Tree (AST). Once read, the AST should be identical to having read Clojure code from the original file.

The CST contains elements that are not in the AST, such as comments and commas. Whitespace is dropped: we're not doing XML here. So emiting a file from the CST will only match the input file if the input had minimal whitespace. Also, interpretation of certain reader macros is not done in the CST, and macro expansion has not occurred.

The idea of storing the CST is to enable an editor to modify source code in a database, rather than in a flat text file. This is an idea that keeps coming up, but a nice writeup was done by Kent Beck talking about the Prune Editor:

https://www.facebook.com/notes/kent-beck/prune-a-code-editor-that-is-not-a-text-editor/1012061842160013

This is my first attempt at using Cursive, so some files for supporting IntelliJ IDEA are also included. These may be ignored.

The code still has a long way to go, but it's doing basic things now.

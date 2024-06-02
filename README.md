# tinyGHC

```
 _   _               ___         ___ 
| |_(_)_ __  _   _  / _ \ /\  /\/ __\
| __| | '_ \| | | |/ /_\// /_/ / /   
| |_| | | | | |_| / /_\\/ __  / /___ 
 \__|_|_| |_|\__, \____/\/ /_/\____/ 
             |___/                   
```
tinyGHC is a Haskell-to-C compiler for a subset of Haskell (that I call [tinyHaskell](./docs/Formal%20Description%20of%20tinyHaskell.md)), fully written in Haskell.

The main objective of this project is to force myself to learn about or unlock many "mystery boxes" with one stone. 
These "mystery boxers" mainly are:
1. Haskell: Doing a big project in Haskell should make me better at writing Haskell code (and, maybe, though not immediately obvious, in other imperative languages as well) and see the differences between it and other languages from the other paradigms (like Java and C++).
2. Programming Languages: Understanding how programming languages work by implementing one myself—maybe even a Turing-complete one. Implementing a functional one adds more fun to it because implementing a functional language is rarely explained in the literature. Therefore, I am forced to read a few papers.
3. Lexers.
4. Parsers.
5. Type-checkers.
6. Code Generations.
7. Garbage Collection

### What subset of Haskell?
- Conditional expressions
- If then else expression
- Multiple function heads with multiple patterns
- Type Synonyms
- Strings
- Algebraic Data Types
- Where expressions
- Let expressions
- Arithmetic operations
- Arithmetic operations
- Case expressions

### TODO
1. [x] CLI Abstraction
2. [x] Lexer
3. [x] Parser
4. [x] Type-checker
5. [x] AST Simplifier
6. [x] Spinless Tagless G-machine Intermediate Representation
7. [x] C minus minus (C--) Intermediate Representation 
8. [x] C Code Generation
9. [x] Garbae Collection

### Resources
1. [Peyton Jones, S. L. (1987). The implementation of functional programming languages (prentice-hall international series in computer science). Prentice-Hall, Inc..](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf)
2. [Jones, S. L. P. (1993). Implementing lazy functional languages on stock hardware: the Spineless Tagless G-machine Version 2.5.](https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf)
3. [Jones, S. L. P., & Lester, D. R. (2000). Implementing Functional Languages: a tutorial. Department of Computer Science, University of Glasgow.](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/)
4. [Jones, S. P., Hall, C., Hammond, K., Partain, W., & Wadler, P. (1993, July). The Glasgow Haskell compiler: a technical overview. In Proc. UK Joint Framework for Information Technology (JFIT) Technical Conference (Vol. 93).](https://www.microsoft.com/en-us/research/wp-content/uploads/1993/03/grasp-jfit.pdf)
5. [Peyton Jones, S. L., & Salkild, J. (1989, November). The spineless tagless G-machine. In Proceedings of the fourth international conference on Functional programming languages and computer architecture (pp. 184-201).](https://dl.acm.org/doi/pdf/10.1145/99370.99385)
6. [Jones, S. P., Ramsey, N., & Reig, F. C--: a portable assembly language that supports garbage collection.](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/ppdp.pdf)
7. [Hutton, G., & Meijer, E. (1996). Monadic parser combinators.](https://pages.cpsc.ucalgary.ca/~robin/class/521/class-handout.pdf#page=282)


### Relevant Docs & Reports [^1]
1. [Seminar Report (for college)](/docs/Seminar%20Report.md)
2. [Seminar Presentation (for college)](/docs/Seminar%20Presentation.md)
3. [Project-Based Learning Report (still a draft)](/docs/PBL%20Report%20Draft.pdf)
[^1]: Most of them are Obsidian-flavored markdown.

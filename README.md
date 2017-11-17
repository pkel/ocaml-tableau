# Ocaml Tableau

This is a basic implementation of a free-variable first-order tableau
prover without equality reasoning. Regarding speed, it is in no way
competitive to other provers. The main purpose of the project
was to connect theoretical knowledge obtained from a lecture on
Computational Logic to practice. I publish this as a point of
orientation for other students of Logic.

The prover was implemented in a rush in order to meet a deadline. During
the process of programming, the following checklist evolved.

* ☑ Propositional w/o quantifier
* ☑ Atomic closure on the fly
* ☑ Heuristic
* ☑ Quantifier (Parts. Rest not needed for f.o. f.v. tableau)
* ☑ First-order formula representation
* ☑ Skolem introduction for delta rule
* ☑ Free variable handling
* ☑ MGU literal closure
* ☑ More tests on f.o.
* ☑ Printer
* ☐ Module for (multi)-sets (steps & literals on branch)
* ☐ Efficient literal-on-branch storage
* ☐ Parser or infix notation

Especially the last point would be of interest now, when I release my
thoughts to public. Unfortunately there is no easy user interface to my
prover and it is highly unlikely to be implemented in the future. If you
want to prove something you have to write your formula in an ocaml `.ml`
file using type constructors and then recompile the project. I did this
for a trivial example meant for manual execution during an exam and for
the popular *Who killed aunt Agatha?* problem (look
[here](src/main.ml)).


# License

This project is released under MIT License.


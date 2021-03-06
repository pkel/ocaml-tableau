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
the popular [Who killed aunt Agatha?][aga] problem (look
[here](main.ml)). The latter and Thousands other Problems for
Theorem Provers can be found in the [TPTP Library][tptp].

[tptp]: http://www.cs.miami.edu/~tptp/
[aga]: http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Problems&Domain=PUZ&File=PUZ001-3.p

# Read More

I learnt about theorem proving at the [group for computational
logic][cl] at University of Innsbruck. You can check out their teaching
material for both the [theoretical][th] and the [practical][pr] part of
the module on automated reasoning.

[cl]: http://cl-informatik.uibk.ac.at/
[th]: http://cl-informatik.uibk.ac.at/teaching/ss17/cl/overview.php
[pr]: http://cl-informatik.uibk.ac.at/teaching/ss17/autres/content.php

# License

This project is released under MIT License.


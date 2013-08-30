About
-----
Meta-optimized genetic algorithms applied to the problem of generating a keyboard layout that is as efficient as possible. This project is done as part of [LISP](http://lispinsummerprojects.org).

Keyboard Layout Optimization
----------------------------

Generating a keyboard layout that is efficient involves shuffling the standard QWERTY keyboard into a configuration which performs best for a certain set of parameters. Popular parameters include the amount of time spent on the home row (the more the better), the number of times adjacent keys have to be typed consecutively (the lower the better), and so on. The 'fitness' of a keyboard layout is some function of these parameters. What is not immediately clear is what the function should be. In other words, we don't know precisely how important each parameter is and how it should contribute to the fitness function. If we assume a certain function and then proceed to optimize, then our end result is only as good as the function we started out with. A poor fitness function might even come up with something worse than QWERTY.

So we need to optimize the fitness function itself. Corresponding to each fitness function is an optimal layout. Ideally we would like to optimize the fitness function to produce the most optimal layout for all possible kinds of fitness function. In practice, it'd be easier to optimize for a *finite* set of fitness functions.

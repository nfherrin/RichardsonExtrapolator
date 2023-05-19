# RichardsonExtrapolator
A simple code to perform 1D Richardson Extrapolation.
This solves the extrapolation to $f(x_0)$ given a set of $x_i$ and $f_i$ values.

There are two ways to use this code. First, if it is known that $(x_i-x_0)=\rho(x_{i+1}-x_0)\forall i$ (that is to say, the ratio of the points' progression to the goal point is known and constant for all $i$) then the known $1>\rho> 0$ value and a list of $f_i$ values can be provided and the extrapolation performed.
This is seen in example `equal_space_example.txt` and `es_match_gen_example.txt`, and is recommended if possible.

Alternatively, if the spacing between the points is not constant, then a list of $x_i$ and $f_i$ pairs can be given (in order of $x_i$ furthest from $x_0$ to closest to $x_0$).
Additionally, the desired $x_0$ value must be provided for this use-case.
Then an iterative solve of the Richardson extrapolation will then be attempted for the given points.
This is seen in examples `general_example.txt` and `gen_match_es_example.txt`.
This fixes failing tests on CRAN by ammending functions of the forcats package, whose API had changed.

I would also like to apologise to Dr. Brian Ripley and the rest of the CRAN team.
The failing test did not provide an informative error because an API change was not expected in an upstream package and produced a warning where I was testing for a lack of warnings or messages.

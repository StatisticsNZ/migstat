# migstat 1.0.6

All tests, including ones for 'calc_is_long_term_mig'
now passing.

Changed C code in function 'calc_is_long_term_mig' to fix 
assymmetry between arrivals and departures. The previous
version was inappropriately treating some values of 
'is_long_term_mig' for departures as NA when they
should have been 0.

Added warnings to help files about records needing
to be in correct order.

# migstat 1.0.5

No changes.

# migstat 1.0.4

No changes.

# migstat 1.0.3

No changes.

# migstat 1.0.2

Fewer iterations used for tests, so they run much faster.

# migstat 1.0.0

First version in new workflow.


# qvirus 0.0.2

* Initial CRAN submission.

* Added a NEWS.md file to track changes to the package.

## Major changes

* Updated `simulate_entanglement` function to reduce direct console output and return results as a list. Added `verbose` parameter to control message printing. Now uses `message()` instead of `cat()` for console output.

* Improved the testing script for `simulate_entanglement` to validate the function’s output without relying on console print statements. Added checks for the structure and types of results.

* Enhanced the documentation for `phen_hiv` function to include references to relevant literature. Added citation for the `qsimulatR` package used in the function.



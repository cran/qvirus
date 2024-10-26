# qvirus 0.0.3

## New Features
* **InteractionClassification**:
  - Added new functionalities to the `InteractionClassification` class, enhancing its capabilities for modeling quantum simulations. [InteractionClassification.R, create_interactions.R, mse.InteractionClassification.R, plot.InteractionClassification.R, print.InteractionClassification.R, summary.InteractionClassification.R]

* **New Functions**:
  - Introduced `nearest_payoff` and `estimate_payoffs` functions to assist in quantum simulation modeling. [nearest_payoff.R, estimate_payoffs.R]
  
* **Documentation Updates**:
  - Enhanced function documentation across various files, ensuring better clarity on usage and functionality. [pure_qubit1.R, pure_qubit2.R]
  
## Minor Updates
* **Data Handling**:
  - Improved functionalities for handling interaction class objects in `data.R`.

* **Documentation and Metadata**:
  - Updated DOI references in the `phen_hiv` function to ensure accurate citations. [phen_hiv.R]

## Bug Fixes
* **simulate_entanglement**:
  - Resolved issues for CRAN submission and improved the function’s stability and performance. [simulate_entanglement.R]

# qvirus 0.0.2

* Initial CRAN submission.

* Added a NEWS.md file to track changes to the package.

## Major changes

* Updated `simulate_entanglement` function to reduce direct console output and return results as a list. Added `verbose` parameter to control message printing. Now uses `message()` instead of `cat()` for console output.

* Improved the testing script for `simulate_entanglement` to validate the function’s output without relying on console print statements. Added checks for the structure and types of results.

* Enhanced the documentation for `phen_hiv` function to include references to relevant literature. Added citation for the `qsimulatR` package used in the function.



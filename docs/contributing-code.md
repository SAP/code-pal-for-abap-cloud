All objects belonging to Code Pal for Cloud should lie in the `/CC4A/` namespace. The basic package structure is:
 - `core` for functionality shared by more than one check
 - `check` for the actual check implementation and their "ATC Check" objects
 - `test_objects` for test objects, i.e. everything that only exists for the automated (unit) tests

All our code should conform to the recommendations of the [Clean ABAP style guide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md). In particular this means that there should be no findings from Code Pal checks on Code Pal code.

SAP's new check implementation framework via the interface `IF_CI_ATC_CHECK` is no longer based on inheritance, so we also prefer composition over inheritance: Instead of having check classes share logic by subclassing, they should delegate shared logic to other global classes. `/CC4A/CHECK_META_DATA` does this for the checks' meta data and `/CC4A/ABAP_ANALYZER` should contain all logic that analyzes ABAP code. Do not hesitate to add methods to these classes or introduce new classes for more shared functionality.
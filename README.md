# Code Pal for ABAP - Cloud Edition

## About this Project

Code Pal is a project that provides ATC checks to assist ABAP programmers in adhering to the [Clean ABAP style guide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md). This is a version of that project that provides ATC checks that can be executed in an ABAP Cloud (["Steampunk"](https://blogs.sap.com/2019/08/20/its-steampunk-now/)) environment and modern on-premise installations (`SAP_BASIS` release equal to or greater than 7.56). The legacy version for SAP_BASIS releases starting at 7.40 is [here](https://github.com/SAP/code-pal-for-abap).


## Requirements and Setup

**This will need to be written after we figure out how namespaces work with Steampunk systems**

## Features

Our main functional goals that differ from the legacy version are:

 - Providing automated quick fixes for many findings in ADT
 - Enabling all checks to run in a [remote check scenario](https://blogs.sap.com/2016/12/12/remote-code-analysis-in-atc-one-central-check-system-for-multiple-systems-on-various-releases/).
 - All code lives in the `/CC4A/` (Clean Code for ABAP) namespace so collisions with Y*/Z* objects from other projects are avoided

The [check migration list](check_migration_list.md) shows the current migration status of checks from the legacy version.

## Feedback and Support

Please submit feedback and bug reports as a [GitHub issue](https://github.com/SAP/code-pal-for-abap-cloud/issues) on this project.

## Contributing

We welcome all contributions to this project, no matter whether you fixed a typo, repaired a bug or wrote a new check. See our [contributor guide](contributing.md) for details.

## Code of Conduct

We as members, contributors, and leaders pledge to make participation in our community a harassment-free experience for everyone. By participating in this project, you agree to abide by its [Code of Conduct](CODE_OF_CONDUCT.md) at all times.

## Licensing

Copyright 2022 SAP SE or an SAP affiliate company and Code Pal for ABAP Cloud contributors. Please see our [license](licenses/Apache-2.0.txt) for copyright and license information. Detailed information including third-party components and their licensing/copyright information is available [via the REUSE tool](https://api.reuse.software/info/github.com/SAP/code-pal-for-abap-cloud).

# Code Pal for ABAP - Cloud Edition

[![REUSE status](https://api.reuse.software/badge/github.com/SAP/code-pal-for-abap-cloud)](https://api.reuse.software/info/github.com/SAP/code-pal-for-abap-cloud)

## About this Project

Code Pal is a project that provides ATC checks to assist ABAP programmers in adhering to the [Clean ABAP style guide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md). This is a version of that project that provides ATC checks that can be executed in an ABAP Cloud (["Steampunk"](https://blogs.sap.com/2019/08/20/its-steampunk-now/)) environment. The legacy version for SAP_BASIS releases starting at 7.40 is [here](https://github.com/SAP/code-pal-for-abap). 


## Requirements and Setup

Install via [abapGit Eclipse plugin](https://github.com/abapGit/ADT_Frontend) on ABAP cloud systems and [abapGit for SAPGUI](https://docs.abapgit.org/guide-online-install.html) on systems with SAP_BASIS 7.58 or higher. Since Code Pal is developed in English, logon language EN is required during installation. In a Steampunk system, the abapGit plugin may report errors related to the object types NSPC and SCO2. These are caused by differences between the on Premise and cloud ABAP environments and can be ignored as these object types are only relevant when the project is imported into an on Premise environment. 

It's recommended to install the project into a package named `/CC4A/CODE_PAL`. You can choose the superpackage of this package arbitrarily, e.g. the default root package `ZLOCAL` for custom development. The namespace `/CC4A/` comes pre-installed in all Steampunk systems. It is currently unavailable in S/4 Public Cloud systems, but SAP is working on enabling it there, too.

Compatibility of the most recent version is only guaranteed for the current version of ABAP for Cloud Development. For the SAP_BASIS 7.58-compatible version, use the `SAP_BASIS-7.58-compatible` branch.

You can only use the ATC checks in an ATC check variant maintained via ADT. The ATC checks are not visible in the check maintenance of transaction SCI. This also means that the previously manual check activation in SCI is not a required step anymore. 

## Features

Our main functional goals that differ from the legacy version are:

 - Providing automated quick fixes for many findings in ADT
 - Enabling all checks to run in a [remote check scenario](https://blogs.sap.com/2016/12/12/remote-code-analysis-in-atc-one-central-check-system-for-multiple-systems-on-various-releases/).
 - All code lives in the `/CC4A/` (Clean Code for ABAP) namespace so collisions with Y*/Z* objects from other projects are avoided. You can get namespace keys at [SAP for Me]( https://me.sap.com/namespace/opensource) to use that namespace in an on Premise customer ABAP enviroment.

The project contains an ABAP Test Cockpit variant called `/CC4A/CODE_PAL_FULL` that includes all currently available checks. For general information on how to use the ABAP Test Cockpit, see [the official SAP documentation](https://help.sap.com/docs/ABAP_PLATFORM_NEW/ba879a6e2ea04d9bb94c7ccd7cdac446/145568d2a3434ef5bf8352c74853c5eb.html?locale=en-US). 

The [check migration list](check_migration_list.md) shows the current migration status of checks from the legacy version. Use that list to navigate to the documentation of the checks.

## Feedback and Support

Please submit feedback and bug reports as a [GitHub issue](https://github.com/SAP/code-pal-for-abap-cloud/issues) on this project.

## Contributing

We welcome all contributions to this project, no matter whether you fixed a typo, repaired a bug or wrote a new check. See our [contributor guide](CONTRIBUTING.md) for details.

## Code of Conduct

We as members, contributors, and leaders pledge to make participation in our community a harassment-free experience for everyone. By participating in this project, you agree to abide by its [Code of Conduct](CODE_OF_CONDUCT.md) at all times.

## Licensing

Copyright 2022 SAP SE or an SAP affiliate company and Code Pal for ABAP Cloud contributors. Please see our [license](LICENSES/Apache-2.0.txt) for copyright and license information. Detailed information including third-party components and their licensing/copyright information is available [via the REUSE tool](https://api.reuse.software/info/github.com/SAP/code-pal-for-abap-cloud).

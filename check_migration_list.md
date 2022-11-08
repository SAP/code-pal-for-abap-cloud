Legend: 
 - TBM = To be migrated
 - TBD = To be discussed

| Legacy check | Cloud check | Quickfixes | Additional info |
| --- | --- | --- | --- |
| [Avoid default table keys](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/avoid-default-key.md) | TBM ([#3](https://github.com/SAP/code-pal-for-abap-cloud/issues/3)) | yes | |
| [Boolean input parameter](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/boolean-input-parameter.md) | TBM ([#2](https://github.com/SAP/code-pal-for-abap-cloud/issues/2)) | no | |
| [CALL METHOD usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/call-method-usage.md) | TBM ([#1](https://github.com/SAP/code-pal-for-abap-cloud/issues/1)) | yes | |
| [Chain declaration usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/chain-declaration-usage.md) | TBM ([#4](https://github.com/SAP/code-pal-for-abap-cloud/issues/4)) | yes | |
| [CHECK statement position](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/check-statement-position.md) | none | n/a | This check is a subset of the SAP-delivered Extended Program Check |
| [CHECK in LOOP](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/check-in-loop.md) | TBM ([#5](https://github.com/SAP/code-pal-for-abap-cloud/issues/5)) | yes | | 
| [COLLECT usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/collect.md) | TBM ([#6](https://github.com/SAP/code-pal-for-abap-cloud/issues/6)) | no | not relevant for ABAP Cloud (`COLLECT` is forbidden) |
| [Combination of output parameters](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/method-output-parameter.md) | TBM ([#2](https://github.com/SAP/code-pal-for-abap-cloud/issues/2)) | no | |
| [Comment position](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/comment-position.md) | TBD | no | The Cloud API for ABAP code analysis currently does not support comments |
| [Comment type](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/comment-type.md) | TBD | no | The Cloud API for ABAP code analysis currently does not support comments |
| [Comment usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/comment-usage.md) | TBD | no | The Cloud API for ABAP code analysis currently does not support comments |
| [Constant interface](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/constants-interface.md) | TBM ([#7](https://github.com/SAP/code-pal-for-abap-cloud/issues/7)) | no | |
| [Cyclomatic complexity](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/cyclomatic-complexity.md) | none | n/a | This is a subset of the SAP-delivered check "Procedural metrics" | 
| [CX_ROOT usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/cx-root-usage.md) | none | n/a | This is a subset of the SAP-delivered Extended Program Check |
| [Database access in unit tests](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/db-access-in-ut.md) | TBM ([#8](https://github.com/SAP/code-pal-for-abap-cloud/issues/8)) | perhaps | The quick fix here would be rather complex: Create the statements necessary to create an SQL test double instance for the database tables accessed |
| [Deprecated classes](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/deprecated-classes.md) | TBD | yes | Currently this check only searches for two specific classes/interfaces, and it does not follow any explicit recommendation in the style guide|
| [Deprecated key words](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/deprecated-key-word.md) | TBM ([#1](https://github.com/SAP/code-pal-for-abap-cloud/issues/1)) | yes | |
| [Empty catch](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/empty-catch.md) | none | n/a | This is a subset of the SAP-delivered Extended Program Check |
| [Empty IF branches](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/empty-if-branches.md) | none | n/a | This is a subset of the SAP-delivered Extended Program Check |
| [Empty procedure](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/empty-procedure.md) | none | n/a | This is a subset of the SAP-delivered Extended Program Check |
| [Assignment chaining](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/equals-sign-chaining.md) | TBM ([#9](https://github.com/SAP/code-pal-for-abap-cloud/issues/9)) | yes | |
| [External call in unit test](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/external-call-in-ut.md) | none | n/a | Does not correspond to any section in the style guide |
| [FORM usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/form-routine.md) | TBM ([#10](https://github.com/SAP/code-pal-for-abap-cloud/issues/10)) | no | not relevant for ABAP Cloud (`PERFORM` is forbidden) |
| [FUNCTION usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/function-routine.md) | TBM ([#10](https://github.com/SAP/code-pal-for-abap-cloud/issues/10)) | no | not relevant for ABAP Cloud (non-RFC function module cannot be used anyway) |
| [Magic numbers](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/magic-number.md) | none | n/a | This is a subset of the SAP-delivered Extended Program Check |
| [Message easy to find](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/message-easy-to-find.md) | TBM ([#11](https://github.com/SAP/code-pal-for-abap-cloud/issues/11)) | perhaps | |
| [Message translation](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/message-translation.md) | none | n/a | This is a subset of the SAP-delivered Extended Program Check |
| [Boolean method names](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/method-return-bool.md) | TBD | no | While this seems to be "in the spirit" of Clean ABAP, it does not actually correspond to any section in the style guide |
| [Missing interface](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/interface-in-class.md) | TBM ([#2](https://github.com/SAP/code-pal-for-abap-cloud/issues/2)) | no | Creating a new global interface is not currently possible via the quick fix API |
| [Nesting depth](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/maximum-nesting-depth.md) | none | n/a | Subset of SAP-delivered "Procedural metrics" check | 
| [Classic exception usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/non-class-exception.md) | none | n/a | Subset of SAP-delivered Extended Program Check |
| [Number of attributes](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/number-attributes.md) | none | n/a | Subset of SAP-delivered "Object-oriented metrics" check | 
| [Number of events](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/number-events.md) | none | n/a | Subset of SAP-delivered "Object-oriented metrics" check | 
| [Number of executable statements](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/number-executable-statements.md) | none | n/a | Subset of SAP-delivered "Procedural metrics" check | 
| [Number of interfaces](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/number-interfaces.md) | none | n/a | Subset of SAP-delivered "Object-oriented metrics" check | 
| [Number of methods](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/number-methods.md) | none | n/a | Subset of SAP-delivered "Object-oriented metrics" check | 
| [Number of public attributes](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/number-public-attributes.md) | none | n/a | Subset of SAP-delivered "Object-oriented metrics" check | 
| [Number of output parameters](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/number-output-parameter.md) | TBM ([#2](https://github.com/SAP/code-pal-for-abap-cloud/issues/2)) | no | Should be merged with "Combination of output parameters" check |
| [Prefer CASE to ELSEIF](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/prefer-case-to-elseif.md) | TBM ([#12](https://github.com/SAP/code-pal-for-abap-cloud/issues/12)) | yes | |
| [Prefer RETURNING to EXPORTING](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/prefer-returning-to-exporting.md) | TBM ([#2](https://github.com/SAP/code-pal-for-abap-cloud/issues/2)) | yes | |
| [Prefer IS NOT to NOT IS](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/prefer-is-not-to-not-is.md) | TBM ([#13](https://github.com/SAP/code-pal-for-abap-cloud/issues/13)) | yes | |
| [Prefer line_* over READ TABLE/LOOP AT ... WHERE](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/prefer-line-exists.md) | TBM ([#1](https://github.com/SAP/code-pal-for-abap-cloud/issues/1)) | yes | Consider merging into "deprecated keywords" |
| [Prefer NEW to CREATE OBJECT](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/prefer-new-to-create-object.md) | TBM ([#1](https://github.com/SAP/code-pal-for-abap-cloud/issues/1)) | yes | Consider merging into "deprecated keywords" |
| [Prefer pragmas to pseudo comments](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/prefer-pragmas-to-pseudo-comments.md) | TBD | n/a | The Cloud API for ABAP code analysis currently does not support pragmas |
| [Pseudo comment usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/pseudo-comment-usage.md) | TBD | no | The ATC already offers the option to not consider pseudo comments, so the stated purpose of this check doesn't need a check. However, one could see a use case for actually *finding* all locations where pseudo comments are used. | 
| [Omit optional EXPORTING](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/omit-optional-exporting.md) | TBM ([#1](https://github.com/SAP/code-pal-for-abap-cloud/issues/1)) | yes | | 
| [Optional parameters](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/optional-parameters.md) | TBM ([#2](https://github.com/SAP/code-pal-for-abap-cloud/issues/2)) | no | |
| [READ TABLE into field symbols](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/sub-assign-read-table.md) | TBD | no | The Extended Program Check has a similar but not identical check |
| [RECEIVING usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/receiving-usage.md) | TBM ([#1](https://github.com/SAP/code-pal-for-abap-cloud/issues/1)) | yes | |
| [Returning name](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/returning-name.md) | TBM ([#1](https://github.com/SAP/code-pal-for-abap-cloud/issues/1)) | yes | |
| [Scope of variable](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/scope-of-variable.md) | TBM ([#14](https://github.com/SAP/code-pal-for-abap-cloud/issues/14))| yes ||
| [Self-reference](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/self-reference.md) | TBM ([#15](https://github.com/SAP/code-pal-for-abap-cloud/issues/15)) | yes | |
| [TEST-SEAM usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/test-seam-usage.md) | TBM ([#16](https://github.com/SAP/code-pal-for-abap-cloud/issues/16)) | yes | |
| [Text assembly](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/text-assembly.md) | TBM ([#1](https://github.com/SAP/code-pal-for-abap-cloud/issues/1)) | yes | |
| [Unit test coverage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/unit-test-coverages.md) | none | n/a | There is no Cloud API for executing unit tests (yet) and since unit tests are often executed during an ATC run anyway, coverage should be measured in that step and not by an additional ATC check - executing unit tests twice may lead to performance degradation. |

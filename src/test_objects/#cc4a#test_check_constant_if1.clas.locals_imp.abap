*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
INTERFACE if_local_1.
  " Finding expected
  CONSTANTS:
    co_1 TYPE symsgty VALUE 'E',
    co_2 TYPE symsgty VALUE 'W',
    co_3 TYPE symsgty VALUE 'N'.
ENDINTERFACE.

CLASS cl_local_1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_local_1.
ENDCLASS.

INTERFACE if_local_2.
  " Finding expected
  CONSTANTS:
    co_1 TYPE symsgty VALUE 'E',
    co_2 TYPE symsgty VALUE 'W',
    co_3 TYPE symsgty VALUE 'N'.
ENDINTERFACE.

CLASS cl_local_2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_local_2.
ENDCLASS.

INTERFACE if_local_3.                                    "#EC CONS_INTF
  " No finding expected due to pseudo comment
  CONSTANTS:
    co_1 TYPE symsgty VALUE 'E',
    co_2 TYPE symsgty VALUE 'W',
    co_3 TYPE symsgty VALUE 'N'.
ENDINTERFACE.

CLASS cl_local_3 DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_local_3.
ENDCLASS.

INTERFACE if_local_4.
  " No finding expected due to method declaration
  CONSTANTS:
    co_1 TYPE symsgty VALUE 'E',
    co_2 TYPE symsgty VALUE 'W',
    co_3 TYPE symsgty VALUE 'N'.
  METHODS:
    a.
ENDINTERFACE.

INTERFACE if_local_5.
  " No finding expected since no constants are declared
ENDINTERFACE.

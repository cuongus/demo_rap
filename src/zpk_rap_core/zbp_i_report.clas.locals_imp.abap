CLASS lsc_zi_report DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.


ENDCLASS.

CLASS lsc_zi_report IMPLEMENTATION.


ENDCLASS.

CLASS lhc_ZI_REPORT DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_report RESULT result.

ENDCLASS.

CLASS lhc_ZI_REPORT IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.

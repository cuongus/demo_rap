CLASS lhc_zjp_c_soquytienmat DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zjp_c_soquytienmat RESULT result.

*    METHODS create FOR MODIFY
*      IMPORTING entities FOR CREATE zjp_c_soquytienmat.
*
*    METHODS update FOR MODIFY
*      IMPORTING entities FOR UPDATE zjp_c_soquytienmat.
*
*    METHODS delete FOR MODIFY
*      IMPORTING keys FOR DELETE zjp_c_soquytienmat.

    METHODS read FOR READ
      IMPORTING keys FOR READ zjp_c_soquytienmat RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zjp_c_soquytienmat.

    METHODS btnExportExcel FOR MODIFY
      IMPORTING keys FOR ACTION zjp_c_soquytienmat~btnExportExcel RESULT result.

ENDCLASS.

CLASS lhc_zjp_c_soquytienmat IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

*  METHOD create.
*  ENDMETHOD.
*
*  METHOD update.
*  ENDMETHOD.
*
*  METHOD delete.
*  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD btnExportExcel.
    zcl_jp_report_fi_export=>btnexportexcel(
        EXPORTING
        keys = keys
        CHANGING
        result = result
        mapped = mapped
        failed = failed
        reported = reported
    ).
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZJP_C_SOQUYTIENMAT DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZJP_C_SOQUYTIENMAT IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

CLASS zcl_gen_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GEN_DATA IMPLEMENTATION.


METHOD if_oo_adt_classrun~main.
    DATA: ls_report type ztb_report,
          ls_rp_item type ztb_rp_item.
    DELETE FROM ztb_report.
    DELETE FROM ztb_rp_item.

    COMMIT WORK.
    out->write( |Complete| ).
  ENDMETHOD.
ENDCLASS.

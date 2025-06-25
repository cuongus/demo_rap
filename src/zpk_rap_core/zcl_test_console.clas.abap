CLASS zcl_test_console DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TEST_CONSOLE IMPLEMENTATION.


METHOD if_oo_adt_classrun~main.
    DATA: lw_month type zde_numc2,
    lw_year type zde_numc4.

    lw_month = '02'.
    lw_year = '2025'.
     DATA(lo_date) = xco_cp=>sy->date( )->overwrite(
       iv_year  = lw_year
       iv_month = lw_month
       iv_day = 1 ).

    " 2) Compute the 1st of next month and then subtract one day:
    DATA(lv_last_day) = lo_date->overwrite( iv_day   = 1 )->add( iv_month = 1 )->subtract( iv_day   = 1 )->as( xco_cp_time=>format->abap )->value.

    out->write( lv_last_day ).
  ENDMETHOD.
ENDCLASS.

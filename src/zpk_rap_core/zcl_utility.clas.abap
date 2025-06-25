CLASS zcl_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
        LAST_DAY_OF_MONTH IMPORTING
                            i_Date TYPE D
                            EXPORTING
                            e_Date type D
                            .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_UTILITY IMPLEMENTATION.


    METHOD LAST_DAY_OF_MONTH.
        DATA: lw_year TYPE zde_numc4,
              lw_month TYPE zde_numc2.
        lw_year = i_Date(4).
        lw_month = i_Date+4(2).
*        IF lw_month >= 12.
*            lw_month = 1.
*            lw_year = lw_year + 1.
*        ELSE.
*            lw_month = lw_month + 1.
*        ENDIF.
*
*        e_Date = lw_year && lw_month && '01'.
*        e_Date = e_Date - 1.

      DATA(lo_date) = xco_cp=>sy->date( )->overwrite(
       iv_year  = lw_year
       iv_month = lw_month
       iv_day = 1 ).

    " 2) Compute the 1st of next month and then subtract one day:
    e_Date = lo_date->overwrite( iv_day   = 1 )->add( iv_month = 1 )->subtract( iv_day   = 1 )->as( xco_cp_time=>format->abap )->value.

    ENDMETHOD.
ENDCLASS.

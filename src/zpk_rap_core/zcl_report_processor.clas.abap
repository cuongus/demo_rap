CLASS zcl_report_processor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           tt_ranges TYPE TABLE OF ty_range_option
           .
    METHODS:
      get_config IMPORTING i_rp_code  TYPE zde_rp_code
                 EXPORTING et_rp_item TYPE ztt_rp_item,
      parent_row IMPORTING is_row  TYPE zst_report_data
                 EXPORTING et_rows TYPE ztt_report_Data.

    CLASS-DATA:
      "Instance Singleton
      mo_instance   TYPE REF TO zcl_report_processor,
      mt_rp_item    TYPE ztt_rp_item,
      mt_row_parent TYPE TABLE OF zst_row_parent.

    CLASS-METHODS:
      "Contructor
      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_report_processor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_REPORT_PROCESSOR IMPLEMENTATION.


  METHOD get_config.

    DATA: lw_str1 TYPE char256,
          lw_str2 TYPE char256.
    DATA: ls_row_parent LIKE LINE OF mt_row_parent.
    DATA: lw_item_code tYPE zde_numc_5.

    SELECT SINGLE * FROM ztb_report AS a
      WHERE a~rp_code = @i_rp_code
      INTO @DATA(ls_report).
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM ztb_rp_item
      WHERE rp_id = @ls_report-rp_id
      INTO TABLE @mt_rp_item.

    LOOP AT mt_rp_item INTO DATA(ls_report_row) WHERE formula IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF 'SUM(' IN ls_report_row-formula WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN ls_report_row-formula WITH ''.
      lw_str1 = ls_report_row-formula.
      DO.
        SPLIT lw_str1 AT ',' INTO lw_str1 lw_str2.

        IF lw_str1(1) = '+' OR lw_str1(1) = '-'.
          ls_row_parent-sign = lw_str1(1).
          lw_str1 = lw_str1+1.
        ELSE.
          ls_row_parent-sign = '+'.
        ENDIF.
        lw_item_code = ls_report_row-item_code1.
        ls_row_parent-item_code_p = lw_item_code.
        ls_row_parent-item_id_p = ls_report_row-item_id.
        lw_item_code = lw_str1.
        ls_row_parent-item_code = lw_item_code.

        IF ls_row_parent-item_code_p <> ls_row_parent-item_code.
          APPEND ls_row_parent TO mt_row_parent.
        ENDIF.

        lw_str1 = lw_str2.
        IF lw_str1 IS INITIAL.
          EXIT.
        ENDIF.
      ENDDO.
    ENDLOOP.

    et_rp_item = mt_rp_item.

  ENDMETHOD.


  METHOD get_instance.
    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                           THEN mo_instance
                                           ELSE NEW #( ) ).
  ENDMETHOD.


  METHOD parent_row.
    DATA: ls_data   TYPE zst_report_data,
          lt_data_p TYPE TABLE OF zst_report_data,
          ls_data_p TYPE zst_report_data.

    LOOP AT mt_row_parent INTO DATA(ls_row_parent)
      WHERE item_code = is_row-item_code.
      CLEAR lt_data_p.
      ls_data = is_row.
      ls_data-item_code = ls_row_parent-item_code_p.
      ls_data-item_id = ls_row_parent-item_id_p.
      IF ls_row_parent-sign = '-'.
        ls_data-col1 *= -1.
        ls_data-col2 *= -1.
        ls_data-col3 *= -1.
        ls_data-col4 *= -1.
        ls_data-col5 *= -1.
        ls_data-col6 *= -1.
        ls_data-col7 *= -1.
        ls_data-col8 *= -1.
        ls_data-col9 *= -1.
        ls_data-col10 *= -1.
      ENDIF.

      APPEND ls_data TO et_rows.
      CLEAR lt_data_p.

      me->parent_row( EXPORTING is_row = ls_data IMPORTING et_rows = lt_data_p  ).
      LOOP AT lt_data_p INTO ls_data_p.
        COLLECT ls_data_p INTO et_rows.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

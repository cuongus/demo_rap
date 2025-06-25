CLASS zcl_filetemp_virtual DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_sadl_exit_calc_element_read.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FILETEMP_VIRTUAL IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA: lv_index.
    lv_index = sy-index.
  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    DATA: lv_index.
    lv_index = sy-index.
  ENDMETHOD.
ENDCLASS.

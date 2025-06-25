CLASS zcl_jpw_common_core DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_inputs_cocode,
             companycode TYPE bukrs,
             addressid   TYPE ad_addrnum,
             language    TYPE zde_char1,
           END OF ty_inputs_cocode.

    TYPES: BEGIN OF ty_resp_cocode,
             include TYPE zst_companycode_info,
           END OF ty_resp_cocode,

           BEGIN OF ty_resp_addrid,
             include TYPE zst_addresid_info,
           END OF ty_resp_addrid.

    INTERFACES if_http_service_extension .

    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_jp_common_core,
      handle_unknown_case CHANGING cv_message TYPE string,
      handle_get_companycode CHANGING  cv_message  TYPE string,
      handle_get_addressid CHANGING cv_message TYPE string,
      handle_get_openingbalance CHANGING cv_message TYPE string.

    METHODS:
      handle_clear.

    CONSTANTS: c_header_content TYPE string VALUE 'content-type',
               c_content_type   TYPE string VALUE 'application/json; charset=utf-8'.

    CLASS-DATA: g_addressid    TYPE zst_companycode_info-addressid,
                g_companycode  TYPE zst_companycode_info-companycode,
                g_date         TYPE string,
                g_glaccount    TYPE hkont,
                "Response Get Companycode
                es_resp_cocode TYPE ty_resp_cocode,
                "Response Get Address ID
                es_resp_addrid TYPE ty_resp_addrid,

                g_json_string  TYPE string,

                o_jp_common    TYPE REF TO zcl_jp_common_core.

  PROTECTED      SECTION.
  PRIVATE SECTION.

    DATA: ls_req_cocode TYPE ty_inputs_cocode,
          lv_error(1)   TYPE c,
          lv_text       TYPE string.

ENDCLASS.



CLASS zcl_jpw_common_core IMPLEMENTATION.

  METHOD handle_unknown_case.
    " Xử lý khi method không tồn tại
    DATA(lv_message) =  'Invalid method or parameter'.

    cv_message = xco_cp_json=>data->from_abap( lv_message )->apply( VALUE #(
        ( xco_cp_json=>transformation->underscore_to_pascal_case )
        ) )->to_string( ).
  ENDMETHOD.

  METHOD if_http_service_extension~handle_request.

    me->get_instance( ).

    me->handle_clear( ).

    DATA: lt_parts TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA(lv_req_body) = request->get_text( ).

    DATA(lv_method) = request->get_header_field( '~request_method' ).

    DATA(lv_uri) = request->get_header_field( '~request_uri' ).

    SPLIT lv_uri AT '?' INTO DATA(lv_path) DATA(lv_query_string).

    SPLIT lv_query_string AT '&' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO DATA(lv_pair).
      SPLIT lv_pair AT '=' INTO DATA(lv_key) DATA(lv_val).

      CASE lv_key.
        WHEN 'name'.
          DATA(lv_name) = lv_val.
        WHEN 'companycode'.
          g_companycode = lv_val.
        WHEN 'addressid'.
          g_addressid = lv_val.
        WHEN 'date'.
          g_date = lv_val.
        WHEN 'glaccount'.
          g_glaccount = |{ lv_val ALPHA = IN }|.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    DATA(lv_dyn_method) = |handle_{ to_lower( lv_method ) }_{ to_lower( lv_name ) }|.
    TRANSLATE lv_dyn_method TO UPPER CASE.

    DATA: lt_parameters TYPE abap_parmbind_tab.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    lt_parameters = VALUE #(
    ( name = 'CV_MESSAGE'
      kind = cl_abap_objectdescr=>changing
      value = REF #( g_json_string ) )
    ).

    DATA(lo_self) = NEW zcl_jpw_common_core( ).
*** Call Methods:
    TRY.
        CALL METHOD lo_self->(lv_dyn_method)
          PARAMETER-TABLE lt_parameters.
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_dyn).
        " Trường hợp method không tồn tại
        CALL METHOD lo_self->('HANDLE_UNKNOWN_CASE')
          PARAMETER-TABLE lt_parameters.
    ENDTRY.

*** Response
    response->set_status('200').

*** Setup -> Response content-type json
    response->set_header_field( i_name = c_header_content
      i_value = c_content_type ).

    response->set_text( g_json_string ).

  ENDMETHOD.

  METHOD handle_get_addressid.

    o_jp_common->get_address_id_details(
                EXPORTING
                addressid = g_addressid
                IMPORTING
                wa_addressid_details = DATA(ls_addressid_details)
            ).

    cv_message = xco_cp_json=>data->from_abap( ls_addressid_details )->apply( VALUE #(
        ( xco_cp_json=>transformation->underscore_to_pascal_case )
        ) )->to_string( ).

  ENDMETHOD.

  METHOD handle_get_companycode.
    o_jp_common->get_companycode_details(
                EXPORTING
                iv_companycode = g_companycode
                IMPORTING
                wa_companycode_details = DATA(ls_companycode_details)
            ).

    cv_message = xco_cp_json=>data->from_abap( ls_companycode_details )->apply( VALUE #(
          ( xco_cp_json=>transformation->underscore_to_pascal_case )
          ) )->to_string( ).

  ENDMETHOD.

  METHOD handle_clear.
    CLEAR: g_addressid, g_companycode, g_json_string,
           g_glaccount, g_date.

    CLEAR: es_resp_addrid, es_resp_cocode.
  ENDMETHOD.

  METHOD get_instance.
    o_jp_common = ro_instance = COND #( WHEN o_jp_common IS BOUND
                                               THEN o_jp_common
                                               ELSE NEW #( ) ).
  ENDMETHOD.

  METHOD handle_get_openingbalance.
    DATA: ir_companycode TYPE o_jp_common->tt_ranges,
          ir_glaccount   TYPE o_jp_common->tt_ranges,
          ir_date        TYPE o_jp_common->tt_ranges.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = g_companycode ) TO ir_companycode.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = g_glaccount ) TO ir_glaccount.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = g_date ) TO ir_date.

    o_jp_common->get_glaccount_sodu(
       EXPORTING
        ir_companycode = ir_companycode
        ir_glaccount = ir_glaccount
        ir_date      = ir_date
       IMPORTING
        it_sodu_dk = DATA(lt_sodu_dk) ).

    READ TABLE lt_sodu_dk INDEX 1 INTO DATA(ls_sodu_dk).

    cv_message = xco_cp_json=>data->from_abap( ls_sodu_dk )->apply( VALUE #(
      ( xco_cp_json=>transformation->underscore_to_pascal_case )
      ) )->to_string( ).

  ENDMETHOD.

ENDCLASS.

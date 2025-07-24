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
      handle_get_openingbalance CHANGING cv_message TYPE string,
      handle_post_sinvoicedata IMPORTING uv_message TYPE string OPTIONAL
                               CHANGING  cv_message TYPE string.

    METHODS:
      handle_clear.

    CONSTANTS: c_header_content TYPE string VALUE 'content-type',
               c_content_type   TYPE string VALUE 'application/json; charset=utf-8'.

    CLASS-DATA: g_addressid          TYPE zst_companycode_info-addressid,
                g_companycode        TYPE zst_companycode_info-companycode,
                g_date               TYPE string,
                g_glaccount          TYPE hkont,
                "Response Get Companycode
                es_resp_cocode       TYPE ty_resp_cocode,
                "Response Get Address ID
                es_resp_addrid       TYPE ty_resp_addrid,

                g_json_string        TYPE string,

                g_accountingdocument TYPE string,
                g_fiscalyear         TYPE string,
                g_usertype           TYPE string,
                g_typeofdate         TYPE string,
                g_currencytype       TYPE string,
                g_einvoicetype       TYPE string,
                g_acctdocsource      TYPE string,
                g_fislcalyearsource  TYPE string,
                g_adjusttype         TYPE string,

                o_jp_common          TYPE REF TO zcl_jp_common_core.

  PROTECTED      SECTION.
  PRIVATE SECTION.

    DATA: ls_req_cocode TYPE ty_inputs_cocode,
          lv_error(1)   TYPE c,
          lv_text       TYPE string.

ENDCLASS.



CLASS ZCL_JPW_COMMON_CORE IMPLEMENTATION.


  METHOD get_instance.
    o_jp_common = ro_instance = COND #( WHEN o_jp_common IS BOUND
                                               THEN o_jp_common
                                               ELSE NEW #( ) ).
  ENDMETHOD.


  METHOD handle_clear.
    CLEAR: g_addressid, g_companycode, g_json_string,
           g_glaccount, g_date ,
           g_accountingdocument,
           g_fiscalyear        ,
           g_usertype          ,
           g_typeofdate        ,
           g_currencytype      ,
           g_einvoicetype      ,
           g_acctdocsource     ,
           g_fislcalyearsource ,
           g_adjusttype        .

    CLEAR: es_resp_addrid, es_resp_cocode.
  ENDMETHOD.


  METHOD handle_get_addressid.

    o_jp_common->get_address_id_details(
      EXPORTING
        addressid            = g_addressid
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
        iv_companycode         = g_companycode
      IMPORTING
        wa_companycode_details = DATA(ls_companycode_details)
    ).

    cv_message = xco_cp_json=>data->from_abap( ls_companycode_details )->apply( VALUE #(
          ( xco_cp_json=>transformation->underscore_to_pascal_case )
    ) )->to_string( ).

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
        ir_glaccount   = ir_glaccount
        ir_date        = ir_date
      IMPORTING
        it_sodu_dk     = DATA(lt_sodu_dk) ).

    READ TABLE lt_sodu_dk INDEX 1 INTO DATA(ls_sodu_dk).

    cv_message = xco_cp_json=>data->from_abap( ls_sodu_dk )->apply( VALUE #(
      ( xco_cp_json=>transformation->underscore_to_pascal_case )
    ) )->to_string( ).

  ENDMETHOD.


  METHOD handle_post_sinvoicedata.
    DATA(o_einvoice_data) =  NEW zcl_einvoice_data( ).
    DATA(o_einvoice_process) = NEW zcl_einvoice_process( ).
    DATA(o_sinvoice) = NEW zcl_manage_viettel_einvoices( ).

    TYPES: BEGIN OF lty_request,
             companycode        TYPE bukrs,
             accountingdocument TYPE belnr_d,
             fiscalyear         TYPE gjahr,
             usertype           TYPE zjp_a_hddt_h-usertype,
             currencytype       TYPE zjp_a_hddt_h-currencytype,
             typeofdate         TYPE zjp_a_hddt_h-typeofdate,
             einvoicetype       TYPE zjp_a_hddt_h-einvoicetype,
           END OF lty_request.

    DATA: ls_request TYPE lty_request.

    DATA: ls_page_info          TYPE zcl_jp_common_core=>st_page_info,

          ir_companycode        TYPE zcl_jp_common_core=>tt_ranges,
          ir_accountingdocument TYPE zcl_jp_common_core=>tt_ranges,
          ir_glaccount          TYPE zcl_jp_common_core=>tt_ranges,
          ir_fiscalyear         TYPE zcl_jp_common_core=>tt_ranges,
          ir_postingdate        TYPE zcl_jp_common_core=>tt_ranges,
          ir_documentdate       TYPE zcl_jp_common_core=>tt_ranges,
          ir_statussap          TYPE zcl_jp_common_core=>tt_ranges,
          ir_einvoicenumber     TYPE zcl_jp_common_core=>tt_ranges,
          ir_einvoicetype       TYPE zcl_jp_common_core=>tt_ranges,
          ir_currencytype       TYPE zcl_jp_common_core=>tt_ranges,
          ir_usertype           TYPE zcl_jp_common_core=>tt_ranges,
          ir_typeofdate         TYPE zcl_jp_common_core=>tt_ranges,
          ir_createdbyuser      TYPE zcl_jp_common_core=>tt_ranges,
          ir_enduser            TYPE zcl_jp_common_core=>tt_ranges,
          ir_testrun            TYPE zcl_jp_common_core=>tt_ranges
          .

    "first deserialize the request
    xco_cp_json=>data->from_string( uv_message )->apply( VALUE #(
        ( xco_cp_json=>transformation->pascal_case_to_underscore )
    ) )->write_to( REF #( ls_request ) ).

    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_request-companycode ) TO ir_companycode.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_request-accountingdocument ) TO ir_accountingdocument.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_request-fiscalyear ) TO ir_fiscalyear.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_request-usertype ) TO ir_usertype.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_request-currencytype ) TO ir_currencytype.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_request-einvoicetype ) TO ir_einvoicetype.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_request-typeofdate ) TO ir_typeofdate.

    TRY.
        o_einvoice_data->get_einvoice_data(
          EXPORTING
            ir_companycode        = ir_companycode
            ir_accountingdocument = ir_accountingdocument
            ir_fiscalyear         = ir_fiscalyear
            ir_postingdate        = ir_postingdate
            ir_documentdate       = ir_documentdate
            ir_statussap          = ir_statussap
            ir_einvoicenumber     = ir_einvoicenumber
            ir_einvoicetype       = ir_einvoicetype
            ir_currencytype       = ir_currencytype
            ir_usertype           = ir_usertype
            ir_typeofdate         = ir_typeofdate
            ir_createdbyuser      = ir_createdbyuser
            ir_enduser            = ir_enduser
            ir_testrun            = ir_testrun
          IMPORTING
            it_einvoice_header    = DATA(lt_headers)
            it_einvoice_item      = DATA(lt_items)
            it_returns            = DATA(lt_returns)
        ).

      CATCH cx_abap_context_info_error.
        "handle exception
    ENDTRY.

    LOOP AT lt_headers INTO DATA(ls_header).
      ls_header-testrun = 'X'.
      TRY.
          o_einvoice_process->get_password(
            EXPORTING
              i_document = ls_header
            IMPORTING
              e_userpass = DATA(ls_userpass)
              e_return   = DATA(ls_return)
          ).
        CATCH cx_abap_context_info_error.
          "handle exception
      ENDTRY.

      TRY.
          IF ls_header-adjusttype IS NOT INITIAL.
            o_sinvoice->adjust_sinvoices(
              EXPORTING
                i_action   = 'ADJUST'
                i_einvoice = ls_header
                i_items    = lt_items
                i_userpass = ls_userpass
              IMPORTING
*               e_status   = gs_status
*               e_docsrc   = gs_docsrc
                e_json     = DATA(lv_json)
                e_return   = ls_return
            ).
          ELSE.
            o_sinvoice->create_sinvoices(
              EXPORTING
                i_action   = 'CREATE'
                i_einvoice = ls_header
                i_items    = lt_items
                i_userpass = ls_userpass
              IMPORTING
*               e_status   = gs_status
*               e_docsrc   = gs_docsrc
                e_json     = lv_json
                e_return   = ls_return
            ).
          ENDIF.

          SELECT SINGLE url_value FROM zjp_hddt_url WHERE action = 'PreviewInvoiceDraft'
            AND id_sys = 'VIETTEL' INTO @DATA(lv_url) PRIVILEGED ACCESS.

          o_sinvoice->post_sinvoices(
            EXPORTING
              i_userpass = ls_userpass
              i_context  = lv_json
              i_prefix   = lv_url
            IMPORTING
              e_context  = DATA(lv_context)
              e_return   = ls_return ).

        CATCH cx_abap_context_info_error.
          "handle exception
      ENDTRY.
    ENDLOOP.

    IF ls_return IS INITIAL.
      cv_message = cv_message = xco_cp_json=>data->from_abap( lv_context )->apply( VALUE #(
    ( xco_cp_json=>transformation->underscore_to_pascal_case )
      ) )->to_string( ).
    ELSE.
      cv_message = xco_cp_json=>data->from_abap( ls_return )->apply( VALUE #(
    ( xco_cp_json=>transformation->underscore_to_pascal_case )
      ) )->to_string( ).
    ENDIF.
  ENDMETHOD.


  METHOD handle_unknown_case.
    " Xử lý khi method không tồn tại
    DATA(lv_message) =  'Invalid method or parameter'.

    cv_message = xco_cp_json=>data->from_abap( lv_message )->apply( VALUE #(
        ( xco_cp_json=>transformation->underscore_to_pascal_case )
    ) )->to_string( ).
  ENDMETHOD.


  METHOD if_http_service_extension~handle_request.

    DATA: lt_parameters TYPE abap_parmbind_tab.
    DATA: ls_line LIKE LINE OF lt_parameters.
    FIELD-SYMBOLS: <lv_value> TYPE any.

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

    CASE lv_name.
      WHEN 'sinvoicedata'.
        ls_line-name  = 'UV_MESSAGE' .
        ls_line-kind  = cl_abap_objectdescr=>exporting .
        ls_line-value = REF #( lv_req_body ).
        INSERT ls_line INTO TABLE lt_parameters .
      WHEN OTHERS.

    ENDCASE.

    DATA(lv_dyn_method) = |handle_{ to_lower( lv_method ) }_{ to_lower( lv_name ) }|.
    TRANSLATE lv_dyn_method TO UPPER CASE.

*    lt_parameters = VALUE #(
*    ( name  = 'CV_MESSAGE'
*      kind  = cl_abap_objectdescr=>changing
*      value = REF #( g_json_string ) )
*    ).


    ls_line-name  = 'CV_MESSAGE' .
    ls_line-kind  = cl_abap_objectdescr=>changing .
    ls_line-value = REF #( g_json_string ).
    INSERT ls_line INTO TABLE lt_parameters .

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
    response->set_status( '200' ).

*** Setup -> Response content-type json
    response->set_header_field( i_name  = c_header_content
                                i_value = c_content_type ).

    response->set_text( g_json_string ).

  ENDMETHOD.
ENDCLASS.

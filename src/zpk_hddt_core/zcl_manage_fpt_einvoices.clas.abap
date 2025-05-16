CLASS zcl_manage_fpt_einvoices DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      wa_userpass        TYPE zjp_hd_userpass,
      wa_document        TYPE zjp_a_hddt_h,
      pr_cancel_einvoice TYPE zpr_cancel_einvoice,
      tt_items           TYPE TABLE OF zjp_a_hddt_i.

    CLASS-DATA: go_fpt_einvoice TYPE REF TO zcl_manage_fpt_einvoices.

    METHODS contructor.

    CLASS-METHODS:
      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_manage_fpt_einvoices,

      post_einvoices IMPORTING i_userpass TYPE wa_userpass
                               i_context  TYPE string
                               i_prefix   TYPE zde_txt255
                     EXPORTING e_context  TYPE string
                               e_return   TYPE bapiret2
                     ,

      get_einvoices IMPORTING i_context TYPE zst_fpt_info
                              i_url     TYPE zde_txt255
                    EXPORTING e_context TYPE string
                              e_return  TYPE bapiret2,

      get_general IMPORTING i_einvoice TYPE wa_document
                            i_userpass TYPE wa_userpass
                            i_items    TYPE tt_items
                            i_type     TYPE zde_txt25
                  EXPORTING e_adjust   TYPE zst_fpt_adjust
                            e_create   TYPE zst_fpt_create
                  ,
      create_einvoices IMPORTING
                         i_action   TYPE zde_action_invoice
                         i_einvoice TYPE wa_document
                         i_items    TYPE tt_items
                         i_userpass TYPE wa_userpass
                       EXPORTING
                         e_status   TYPE wa_document
                         e_docsrc   TYPE wa_document
                         e_json     TYPE string
                         e_return   TYPE bapiret2
                       ,
      cancel_einvoices IMPORTING
                         i_action   TYPE zde_action_invoice
                         i_einvoice TYPE wa_document
                         i_userpass TYPE wa_userpass
                         i_param    TYPE pr_cancel_einvoice
                       EXPORTING
                         e_status   TYPE wa_document
                         e_json     TYPE string
                         e_return   TYPE bapiret2
                       ,
      search_einvoices IMPORTING
                         i_action   TYPE zde_action_invoice
                         i_einvoice TYPE wa_document
                         i_userpass TYPE wa_userpass  OPTIONAL
                       EXPORTING
                         e_status   TYPE wa_document
                         e_docsrc   TYPE wa_document
                         e_json     TYPE string
                         e_return   TYPE bapiret2
                       ,
      adjust_einvoices IMPORTING
                         i_action   TYPE zde_action_invoice
                         i_einvoice TYPE wa_document
                         i_items    TYPE tt_items
                         i_userpass TYPE wa_userpass
                       EXPORTING
                         e_status   TYPE wa_document
                         e_docsrc   TYPE wa_document
                         e_json     TYPE string
                         e_return   TYPE bapiret2
                       ,
      process_status IMPORTING
                       i_action   TYPE zde_action_invoice
                       i_einvoice TYPE wa_document
                       i_return   TYPE bapiret2
                       i_status   TYPE string
                     EXPORTING
                       e_header   TYPE wa_document
                       e_docsrc   TYPE wa_document,

      process_message IMPORTING
                        i_document   TYPE wa_document OPTIONAL
                        message_type TYPE wa_document-messagetype OPTIONAL
                        message_text TYPE wa_document-messagetext OPTIONAL
                        status_sap   TYPE wa_document-statussap OPTIONAL
                        icon_sap     TYPE wa_document-iconsap OPTIONAL
                      EXPORTING
                        e_header     TYPE wa_document
                      .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_manage_fpt_einvoices IMPLEMENTATION.

  METHOD contructor.
    CREATE OBJECT go_fpt_einvoice.
  ENDMETHOD.

  METHOD get_instance.
    go_fpt_einvoice = ro_instance = COND #( WHEN go_fpt_einvoice IS BOUND
                                               THEN go_fpt_einvoice
                                               ELSE NEW #( ) ).
  ENDMETHOD.

  METHOD get_einvoices.
    DATA: i_prefix TYPE string.
    i_prefix = |?stax={ i_context-stax }&sid={ i_context-sid }&type=json|.
*-- Create HTTP client ->
    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                 comm_scenario  = |Z_API_FPT_EINVOICE_CSCEN|
                                 service_id     = |Z_API_FPT_EINVOICE_OB_REST|
                               ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).
*-- Add path ->
*        lo_http_client->get_http_request( )->set_uri_path( |{ i_prefix }| ).
        lo_http_client->get_http_request( )->set_header_field( i_name = |~request_uri| i_value = |{ i_url }{ i_prefix }| ).
*-- SET HTTP Header Fields
        lo_http_client->get_http_request( )->set_header_field( i_name = |Content-Type| i_value = |application/json| ).

        lo_http_client->get_http_request( )->set_header_field( i_name = |Accept| i_value = |*/*| ).

        DATA: lv_username TYPE string,
              lv_password TYPE string.

        lv_username = i_context-username.
        lv_password = i_context-password.
*-- Passing the Accept value in header which is a mandatory field
        lo_http_client->get_http_request( )->set_header_field( i_name = |username| i_value = lv_username ).
        lo_http_client->get_http_request( )->set_header_field( i_name = |password| i_value = lv_password ).
*-- Authorization
        lo_http_client->get_http_request( )->set_authorization_basic( i_username = lv_username i_password = lv_password ).
*-- GET
        lo_http_client->execute( i_method = if_web_http_client=>get
                                 i_timeout = 60 ).

        lo_http_client->get_http_request( )->set_content_type( |application/json| ).

*-- Response ->
        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get
                                                     i_timeout = 60 ).
*-- Get the status of the response ->
        e_context = lo_response->get_text( ).
        IF lo_response->get_status( )-code NE 200.
          e_return-type = 'E'.
          e_return-message = lo_response->get_status( )-code && ` ` && lo_response->get_status( )-reason
          && ` - ` && e_context.
        ENDIF.
        IF e_context = 'Không tìm thấy hóa đơn. Vui lòng kiểm tra điều kiện tìm kiếm'.
          e_return-type = 'E'.
          e_return-message = 'Không tìm thấy hóa đơn. Vui lòng kiểm tra điều kiện tìm kiếm'.
        ENDIF.
      CATCH cx_root INTO DATA(lx_exception).

    ENDTRY.
  ENDMETHOD.

  METHOD post_einvoices.
    IF i_context IS INITIAL. RETURN. ENDIF.
*-- Create HTTP client ->
    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                 comm_scenario  = |Z_API_FPT_EINVOICE_CSCEN|
                                 service_id     = |Z_API_FPT_EINVOICE_OB_REST|
                               ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).
*-- Add path ->
*        lo_http_client->get_http_request( )->set_uri_path( |{ i_prefix }| ).
        lo_http_client->get_http_request( )->set_header_field( i_name = |~request_uri| i_value = |{ i_prefix }| ).
*-- SET HTTP Header Fields
        lo_http_client->get_http_request( )->set_header_field( i_name = |Content-Type| i_value = |application/json| ).

        lo_http_client->get_http_request( )->set_header_field( i_name = |Accept| i_value = |*/*| ).

        DATA: lv_username TYPE string,
              lv_password TYPE string.

        lv_username = i_userpass-username.
        lv_password = i_userpass-password.
*-- Passing the Accept value in header which is a mandatory field
        lo_http_client->get_http_request( )->set_header_field( i_name = |username| i_value = lv_username ).
        lo_http_client->get_http_request( )->set_header_field( i_name = |password| i_value = lv_password ).
*-- Authorization
        lo_http_client->get_http_request( )->set_authorization_basic( i_username = lv_username i_password = lv_password ).

        lo_http_client->get_http_request( )->set_content_type( |application/json| ).
*-- POST
        lo_http_client->execute( i_method = if_web_http_client=>post
                                 i_timeout = 60 ).
*-- Send request ->
        lo_http_client->get_http_request( )->set_text( i_context ).
*-- Response ->
        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>post
                                                     i_timeout = 60 ).
*-- Get the status of the response ->
        e_context = lo_response->get_text( ).
        IF lo_response->get_status( )-code NE 200.
          e_return-type = 'E'.
          e_return-message = lo_response->get_status( )-code && ` ` && lo_response->get_status( )-reason
          && ` - ` && e_context.
        ENDIF.
      CATCH cx_root INTO DATA(lx_exception).

    ENDTRY.
  ENDMETHOD.

  METHOD adjust_einvoices.

    DATA: ls_einvoice TYPE zst_fpt_adjust.

    DATA:
          lv_url  TYPE zde_txt255.

    CLEAR: e_status, e_json, e_return, e_docsrc.

    go_fpt_einvoice->get_general(
        EXPORTING
        i_einvoice = i_einvoice
        i_userpass = i_userpass
        i_items = i_items
        i_type = 'ADJUST'
        IMPORTING
        e_adjust = ls_einvoice
    ).

* Create JSON *

    /ui2/cl_json=>serialize(
        EXPORTING
        data = ls_einvoice
         pretty_name  =  /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
        r_json = DATA(lv_json_string)
    ).

* Test run *
    IF i_einvoice-testrun IS NOT INITIAL.
      e_json = lv_json_string.
      RETURN.
    ENDIF.

* CALL API OUNTBOUND *
    DATA: lv_json_results TYPE string.

    go_fpt_einvoice->post_einvoices(
      EXPORTING
        i_userpass = i_userpass
        i_context  = lv_json_string
        i_prefix   = lv_url
      IMPORTING
        e_context  = lv_json_results
        e_return   = e_return ).

*-------------------------THE--END-------------------------*
    go_fpt_einvoice->process_status(
        EXPORTING
        i_action    = i_action
        i_einvoice  = i_einvoice
        i_return    = e_return
        i_status    = lv_json_results
        IMPORTING
        e_header    = e_status
        e_docsrc    = e_docsrc
        ).

  ENDMETHOD.

  METHOD cancel_einvoices.
    DATA: ls_einvoice  TYPE zst_fpt_cancel,
          ls_can_items LIKE LINE OF ls_einvoice-wrongnotice-items.

    DATA: lv_url  TYPE zde_text255.
    CLEAR: e_status, e_json, e_return.
* Username - Password
    ls_einvoice-user-username = i_userpass-Username.
    ls_einvoice-user-password = i_userpass-Password.
*----------------------------------------------------------*
    lv_url = i_action.
* Wrongnotice *
    ls_einvoice-wrongnotice-stax = i_userpass-suppliertax.
    "Loại thông báo
    ls_einvoice-wrongnotice-noti_taxtype = i_param-noti_taxtype.
    IF ls_einvoice-wrongnotice-noti_taxtype = 2. "Hủy theo thông báo của CQT
      "Số thông báo
      ls_einvoice-wrongnotice-noti_taxnum = i_param-noti_taxnum.
    ENDIF.
    "Ngày thông báo CQT
    ls_einvoice-wrongnotice-noti_taxdt = i_param-noti_taxdt.
    "Địa danh
    ls_einvoice-wrongnotice-place = i_param-place.
*----------------------------------------------------------*
* Items *
*    SELECT SINGLE * FROM zi_rap_einv_form
*    WHERE Companycode = @i_userpass-Companycode
*      AND usertype = @i_userpass-usertype
*      INTO @DATA(ls_einv_form).

    ls_can_items-form    = i_einvoice-einvoiceForm.
    ls_can_items-serial  = i_einvoice-einvoiceSerial.
    ls_can_items-seq     = i_einvoice-einvoicenumber.
    ls_can_items-idt     = |{ i_einvoice-einvoicedatecreate+0(4) }-{ i_einvoice-einvoicedatecreate+4(2) }-{ i_einvoice-einvoicedatecreate+6(2) } |
    && |{ i_einvoice-einvoicetimecreate+0(2) }:{ i_einvoice-einvoicetimecreate+2(2) }:{ i_einvoice-einvoicetimecreate+4(2) }|.
* Loại áp dụng hóa đơn điện tử mặc định = 1
    ls_can_items-type_ref = 1.
* Tính chất thông báo
    ls_can_items-noti_type = i_param-noti_type.
* Lý do hủy
*    ls_cancel_items-rea =
    APPEND ls_can_items TO ls_einvoice-wrongnotice-items.
    CLEAR: ls_can_items.

* CREATE JSON *
*    DATA(lv_json_string) = xco_cp_json=>data->from_abap( ls_einvoice )->apply( VALUE #(
*      ( xco_cp_json=>transformation->underscore_to_camel_case )
*      ( xco_cp_json=>transformation->boolean_to_abap_bool )
*    ) )->to_string( ).
*    e_json = lv_json_string.

    /ui2/cl_json=>serialize(
        EXPORTING
        data = ls_einvoice
         pretty_name  =  /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
        r_json = DATA(lv_json_string)
    ).

* Test run *
    IF i_einvoice-testrun IS NOT INITIAL.
      e_json = lv_json_string.
      RETURN.
    ENDIF.
* CALL API OUTBOUND *
    DATA: lv_json_results TYPE string.

    go_fpt_einvoice->post_einvoices(
      EXPORTING
        i_userpass = i_userpass
        i_context  = lv_json_string
        i_prefix   = lv_url
      IMPORTING
        e_context  = lv_json_results
        e_return   = e_return ).
*-------------------------THE--END-------------------------*
    go_fpt_einvoice->process_status(
        EXPORTING
        i_action = i_action
        i_einvoice = i_einvoice
        i_return = e_return
        i_status = lv_json_results
        IMPORTING
        e_header = e_status
        ).

  ENDMETHOD.

  METHOD create_einvoices.

    DATA:
          lv_url  TYPE zde_txt255.
*
*    DATA: lt_items TYPE TABLE OF zjp_a_hddt_i.
    CLEAR: e_status, e_json, e_return, e_docsrc.
*
    DATA: ls_einvoice TYPE zst_fpt_create.

    go_fpt_einvoice->get_general(
        EXPORTING
        i_einvoice = i_einvoice
        i_userpass = i_userpass
        i_items    = i_items
        i_type     = 'CREATE'
        IMPORTING
        e_create = ls_einvoice
    ).
*----------------------------------------------------------*
* Create JSON *
    DATA: lv_json_string TYPE string.

    /ui2/cl_json=>serialize(
        EXPORTING
        data = ls_einvoice
         pretty_name  =  /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
        r_json = lv_json_string
    ).
* Test run *
    IF i_einvoice-testrun IS NOT INITIAL.
      e_json = lv_json_string.
      RETURN.
    ENDIF.

* Call API OUTBOUND *
    DATA: lv_json_results TYPE string.

    lv_url = i_action.

    go_fpt_einvoice->post_einvoices(
      EXPORTING
        i_userpass = i_userpass
        i_context  = lv_json_string
        i_prefix   = lv_url
      IMPORTING
        e_context  = lv_json_results
        e_return   = e_return ).
*-------------------------THE--END-------------------------*
    go_fpt_einvoice->process_status(
        EXPORTING
        i_action = i_action
        i_einvoice = i_einvoice
        i_return = e_return
        i_status = lv_json_results
        IMPORTING
        e_header = e_status
        ).

  ENDMETHOD.

  METHOD process_status.
    TYPES: BEGIN OF lty_cancel_response,
             errorcode   TYPE zde_txt255,
             description TYPE zde_txt255,
           END OF lty_cancel_response.

    DATA: ls_cr_response     TYPE zst_fpt_cr_response,
          lt_sr_response     TYPE TABLE OF zst_fpt_search_resp,
          ls_cancel_response TYPE lty_cancel_response.

    "Create Log
    MOVE-CORRESPONDING i_einvoice TO e_header.

    IF i_return-type NE 'E'.
      IF i_action = 'CREATE_INVOICE'
      OR i_action = 'ADJUST_INVOICE'.
*        xco_cp_json=>data->from_string( i_status )->write_to( REF #( ls_cr_response ) ).
        /ui2/cl_json=>deserialize(
         EXPORTING
           json             = i_status
*        jsonx            =
           pretty_name      = /ui2/cl_json=>pretty_mode-user
*        assoc_arrays     =
*        assoc_arrays_opt =
*        name_mappings    =
*        conversion_exits =
*        hex_as_base64    =
         CHANGING
           data             = ls_cr_response
       ).
      ELSEIF i_action = 'SEARCH_INVOICE'.
        /ui2/cl_json=>deserialize(
           EXPORTING
             json             = i_status
*                jsonx            =
             pretty_name      = /ui2/cl_json=>pretty_mode-user
*                assoc_arrays     =
*                assoc_arrays_opt =
*                name_mappings    =
*                conversion_exits =
*                hex_as_base64    =
           CHANGING
             data             = lt_sr_response
         ).
      ELSEIF i_action = 'CANCEL_INVOICE'.
*        xco_cp_json=>data->from_string( i_status )->write_to( REF #( ls_cancel_response ) ).
        /ui2/cl_json=>deserialize(
         EXPORTING
           json             = i_status
*        jsonx            =
           pretty_name      = /ui2/cl_json=>pretty_mode-user
*        assoc_arrays     =
*        assoc_arrays_opt =
*        name_mappings    =
*        conversion_exits =
*        hex_as_base64    =
         CHANGING
           data             = ls_cancel_response
       ).
      ENDIF.

      DATA: lv_date TYPE zde_txt25,
            lv_time TYPE zde_txt25.

      READ TABLE lt_sr_response INTO DATA(ls_sr_response) INDEX 1.
      IF sy-subrc NE 0.
        CLEAR: ls_sr_response.
      ENDIF.

      IF ls_cr_response IS NOT INITIAL OR ls_sr_response IS NOT INITIAL.
        IF ls_cr_response IS NOT INITIAL.
          e_header-einvoiceNumber = ls_cr_response-seq. "Số hóa đơn
          e_header-einvoiceSerial = ls_cr_response-serial. "Serial
          e_header-einvoiceForm = ls_cr_response-form. "Form
          e_header-einvoiceType = ls_cr_response-type. "Type Hóa đơn
          SPLIT ls_cr_response-idt AT '' INTO lv_date lv_time.
          IF lv_date IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF '-' IN lv_date WITH ''.
          ENDIF.
          IF lv_time IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF ':' IN lv_time WITH ''.
          ENDIF.
          e_header-einvoiceDatecreate = lv_date.
          e_header-einvoiceTimecreate = lv_time.
          e_header-Mscqt              = ls_cr_response-taxo. "Mã CQT quản lý
          e_header-Link               = ls_cr_response-link.
          e_header-statusINVres       = ls_cr_response-status.
          e_header-statusCQTres       = ls_cr_response-status_received.
          e_header-invdat             = ls_cr_response-idt.
        ELSE.
          e_header-einvoiceNumber     = ls_sr_response-doc-seq. "Số hóa đơn
          e_header-einvoiceSerial     = ls_sr_response-doc-serial. "Serial
          e_header-einvoiceForm       = ls_sr_response-doc-form. "Form
          e_header-einvoiceType       = ls_sr_response-doc-type. "Type Hóa đơn
          SPLIT ls_sr_response-doc-idt AT '' INTO lv_date lv_time.
          IF lv_date IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF '-' IN lv_date WITH ''.
          ENDIF.
          IF lv_time IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF ':' IN lv_time WITH ''.
          ENDIF.
          e_header-einvoiceDatecreate = lv_date.
          e_header-einvoiceTimecreate = lv_time.
          e_header-Mscqt              = ls_sr_response-doc-taxo. "Mã CQT quản lý
          e_header-Link               = ls_sr_response-doc-link.
          e_header-statusINVres       = ls_sr_response-doc-status.
          e_header-statusCQTres       = ls_sr_response-doc-status_received.
          e_header-invdat             = ls_sr_response-doc-idt.
        ENDIF.
      ENDIF.

      CASE i_action.
        WHEN 'CREATE_EINVOICE' "Lập hóa đơn nháp
          OR 'ADJUST_EINVOICE'. "Điều chỉnh hóa đơn

          e_header-createddate   = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value.
          e_header-createdbyuser = sy-uname.
          e_header-createdtime   = xco_cp=>sy->time( )->as( xco_cp_time=>format->abap )->value.

          IF ls_cr_response-seq IS INITIAL.
            go_fpt_einvoice->process_message(
            EXPORTING
                i_document   = e_header
                status_sap   = TEXT-s02
                message_type = 'S'
                message_text = TEXT-a02
            IMPORTING
                e_header     = e_header
            ).
          ELSE.
            go_fpt_einvoice->process_message(
            EXPORTING
                i_document   = e_header
                status_sap   = TEXT-s98
                message_type = 'S'
                message_text = TEXT-a98
            IMPORTING
                e_header     = e_header
            ).
          ENDIF.

        WHEN 'SEARCH_INVOICE'. "Tra cứu hóa đơn
          CASE ls_sr_response-doc-status. "Trạng thái hóa đơn
            WHEN '1'. "Chờ cấp số.
              go_fpt_einvoice->process_message(
                EXPORTING
                    i_document   = e_header
                    status_sap   = TEXT-s02
                    message_type = 'S'
                    message_text = TEXT-a21
                IMPORTING
                    e_header     = e_header
                ).
            WHEN '2'. "Chờ duyệt.
              go_fpt_einvoice->process_message(
                EXPORTING
                    i_document   = e_header
                    status_sap   = TEXT-s02
                    message_type = 'S'
                    message_text = TEXT-a22
                IMPORTING
                    e_header     = e_header
                ).
            WHEN '3'. "Đã Duyệt.
              CASE ls_sr_response-doc-status_received.
                WHEN '0'. "Chờ gửi CQT
                  go_fpt_einvoice->process_message(
                    EXPORTING
                        i_document   = e_header
                        status_sap   = TEXT-s98
                        message_type = 'S'
                        message_text = TEXT-a23
                    IMPORTING
                        e_header     = e_header
                    ).
                WHEN '1'. "Đã gửi CQT
                  go_fpt_einvoice->process_message(
                    EXPORTING
                        i_document   = e_header
                        status_sap   = TEXT-s98
                        message_type = 'S'
                        message_text = TEXT-a24
                    IMPORTING
                        e_header     = e_header
                    ).
                WHEN '2'. "Gửi không thành công
                  go_fpt_einvoice->process_message(
                    EXPORTING
                        i_document   = e_header
                        status_sap   = TEXT-s10
                        message_type = 'S'
                        message_text = TEXT-a25
                    IMPORTING
                        e_header     = e_header
                    ).
                WHEN '8'. "Kiểm tra hợp lệ(Hóa đơn không mã hợp lệ)
                  go_fpt_einvoice->process_message(
                    EXPORTING
                        i_document   = e_header
                        status_sap   = TEXT-s99
                        message_type = 'S'
                        message_text = TEXT-a26
                    IMPORTING
                        e_header     = e_header
                    ).
                WHEN '9'. "Kiểm tra không hợp lệ
                  go_fpt_einvoice->process_message(
                    EXPORTING
                        i_document   = e_header
                        status_sap   = TEXT-s10
                        message_type = 'S'
                        message_text = TEXT-a27
                    IMPORTING
                        e_header     = e_header
                    ).
                WHEN '10'. "Đã cấp mã(HĐ có mã hợp lệ đã được CQT cấp mã)
                  go_fpt_einvoice->process_message(
                    EXPORTING
                        i_document   = e_header
                        status_sap   = TEXT-s99
                        message_type = 'S'
                        message_text = TEXT-a26
                    IMPORTING
                        e_header     = e_header
                    ).
                WHEN OTHERS.
              ENDCASE.
            WHEN '4'. "Đã hủy.
              go_fpt_einvoice->process_message(
                EXPORTING
                    i_document   = e_header
                    status_sap   = TEXT-s04
                    message_type = 'S'
                    message_text = TEXT-a04
                IMPORTING
                    e_header     = e_header
                ).
            WHEN OTHERS.
          ENDCASE.
        WHEN 'CANCEL_INVOICE'. "Hủy hóa đơn
          IF i_status = |Thông báo: Đã hủy hóa đơn thành công và tạo, duyệt thông báo sai sót gửi Thuế|.
            e_header-einvoicedatecancel = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value.
            go_fpt_einvoice->process_message(
                EXPORTING
                    i_document   = e_header
                    status_sap   = TEXT-s04
                    message_type = 'S'
                    message_text = TEXT-a04
                IMPORTING
                    e_header     = e_header
                ).
          ELSE.
            IF ls_cancel_response-errorcode IS INITIAL.
              e_header-einvoicedatecancel = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value.
              go_fpt_einvoice->process_message(
                EXPORTING
                    i_document   = e_header
                    status_sap   = TEXT-s04
                    message_type = 'S'
                    message_text = TEXT-a04
                IMPORTING
                    e_header     = e_header
                ).
            ELSE.
              go_fpt_einvoice->process_message(
                EXPORTING
                    i_document   = e_header
                    status_sap   = TEXT-s03
                    message_type = 'E'
                    message_text = ls_cancel_response-description
                IMPORTING
                    e_header     = e_header
                ).
            ENDIF.
          ENDIF.
        WHEN OTHERS.

      ENDCASE.
      IF e_header-StatusSap = '98' OR e_header-StatusSap = '99'.
        "Hóa đơn bị điều chỉnh
        SELECT SINGLE * FROM zjp_a_hddt_h WHERE Companycode        = @i_einvoice-Companycode
                                            AND Accountingdocument = @i_einvoice-accountingdocumentsource
                                            AND fiscalyear         = @i_einvoice-fiscalyearsource
              INTO @DATA(ls_einv_header_src).
        IF sy-subrc EQ 0.
          CASE i_einvoice-adjusttype.
            WHEN '3'. "Thay thế
              ls_einv_header_src-Iconsap = '@20@'.
              ls_einv_header_src-StatusSap = '07'.
              ls_einv_header_src-messagetext = 'Hóa đơn đã bị thay thế'.
            WHEN '1' OR '2'. "Điều chỉnh tiền
              ls_einv_header_src-Iconsap = '@4K@'.
              ls_einv_header_src-StatusSap = '06'.
              ls_einv_header_src-messagetext = 'Hóa đơn đã bị điều chỉnh'.
            WHEN OTHERS.
          ENDCASE.
          MOVE-CORRESPONDING ls_einv_header_src TO e_docsrc.
        ENDIF.
      ENDIF.
    ELSE.
      e_header-statussap   = '03'.
      e_header-messagetype = i_return-type.
      e_header-messagetext = i_return-message.

      IF i_return-message = |Không tìm thấy hóa đơn. Vui lòng kiểm tra điều kiện tìm kiếm|.
        e_header-statussap      = '01'.
        e_header-einvoiceForm   = ''.
        e_header-einvoiceSerial = ''.
        e_header-usertype       = ''.
      ELSEIF i_return-message = |401 Unauthorized - FPT thông báo: Thiếu thông tin xác thực tài khoản|.
        e_header-statussap = '01'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD search_einvoices.

    DATA: ls_einvoice TYPE zst_fpt_info.

    DATA: lv_url  TYPE zde_txt255.

    CLEAR: e_status, e_json, e_return, e_docsrc.
    lv_url = i_action.
*" Username - Password
    ls_einvoice-username = i_userpass-username.
    ls_einvoice-password = i_userpass-password.

    SELECT SINGLE * FROM zjp_hd_serial WHERE companycode  = @i_einvoice-companycode
                                        AND fiscalyear    = @i_einvoice-fiscalyear
                                        AND einvoicetype  = '01GTKT'
    INTO @DATA(ls_serial).
*" Form E-Invoice
    ls_einvoice-form = i_einvoice-einvoiceform.
    IF ls_einvoice-form IS INITIAL.
      ls_einvoice-form = ls_serial-einvoiceform.
    ENDIF.
*" Serial E-Invoice
    ls_einvoice-serial = i_einvoice-einvoiceserial.
    IF ls_einvoice-serial IS INITIAL.
      ls_einvoice-serial = ls_serial-einvoiceserial.
    ENDIF.
*" MST người bán
    ls_einvoice-stax = i_userpass-suppliertax.
*" Loại dữ liệu trả về JSON
    ls_einvoice-type = '1'.
*" Key để xác định hoá đơn
    ls_einvoice-sid = i_einvoice-sid.
*" MST người mua
    ls_einvoice-btax = i_einvoice-identificationnumber.
*" Tra cứu hóa đơn tạo từ API
    ls_einvoice-api = '1'.

* Create JSON *
    DATA(lv_json_string) = xco_cp_json=>data->from_abap( ls_einvoice )->apply( VALUE #(
      ( xco_cp_json=>transformation->underscore_to_camel_case )
      ( xco_cp_json=>transformation->boolean_to_abap_bool )
    ) )->to_string( ).
    e_json = lv_json_string.

* Test run *
    IF i_einvoice-testrun IS NOT INITIAL.
      e_json = lv_json_string.
      RETURN.
    ENDIF.

* CALL API OUTBOUND *

    DATA: lv_json_results TYPE string.

    go_fpt_einvoice->get_einvoices(
      EXPORTING
        i_context = ls_einvoice
        i_url     = lv_url
      IMPORTING
        e_context = lv_json_results
        e_return  = e_return ).
*-------------------------THE--END-------------------------*
    go_fpt_einvoice->process_status(
        EXPORTING
        i_action = i_action
        i_einvoice = i_einvoice
        i_return = e_return
        i_status = lv_json_results
        IMPORTING
        e_header = e_status
        e_docsrc = e_docsrc
        ).

  ENDMETHOD.

  METHOD get_general.
    DATA: ls_einvoice TYPE zst_fpt_adjust.

    DATA: lv_date TYPE zde_einv_date,
          lv_time TYPE zde_einv_time,
          lv_url  TYPE zde_txt255.

    DATA: lt_items TYPE tt_items.

    lt_items = i_items[].

* Username - Password *
    ls_einvoice-user-username = i_userpass-username.
    ls_einvoice-user-password = i_userpass-password.
*----------------------------------------------------------*
* Key - Form - Serial Invoices
*    IF i_einvoice-zsearch IS NOT INITIAL.
*      ls_einvoice-inv-sid     = i_einvoice-zsearch.
*    ELSE.
    ls_einvoice-inv-sid     = i_einvoice-sid.
*    ENDIF.

    lv_date = i_einvoice-einvoicedatecreate.
    lv_time = i_einvoice-einvoicetimecreate.

    ls_einvoice-inv-idt     = |{ lv_date+0(4) }-{ lv_date+4(2) }-{ lv_date+6(2) } { lv_time+0(2) }:{ lv_time+2(2) }:{ lv_time+4(2) }|.
    ls_einvoice-inv-type    = i_einvoice-einvoicetype.
    ls_einvoice-inv-form    = i_einvoice-einvoiceform.
    ls_einvoice-inv-serial  = i_einvoice-einvoiceserial.

    IF i_type = 'ADJUST'.
* Adjust Tag *
      ls_einvoice-inv-adj-rdt = |{ lv_date+0(4) }-{ lv_date+4(2) }-{ lv_date+6(2) } { lv_time+0(2) }:{ lv_time+2(2) }:{ lv_time+4(2) }|.
      ls_einvoice-inv-adj-ref = i_einvoice-sid.
*
      SELECT SINGLE * FROM zjp_a_hddt_h
      WHERE Companycode        = @i_einvoice-Companycode
        AND Accountingdocument = @i_einvoice-accountingdocumentsource
        AND Fiscalyear         = @i_einvoice-fiscalyearsource
        INTO @DATA(ls_document_source).
      IF sy-subrc EQ 0.
        ls_einvoice-inv-adj-seq = |1-{ ls_document_source-einvoiceserial }-{ ls_document_source-einvoicenumber }|.
        IF i_einvoice-adjusttype = '3'. "Thay thế Hóa đơn
          ls_einvoice-inv-adj-rea = |Thay thế cho hóa đơn { ls_document_source-einvoiceserial }{ ls_document_source-einvoicenumber }|
          && | ngày { ls_document_source-einvoicedatecreate+6(2) }/{ ls_document_source-einvoicedatecreate+4(2) }/{ ls_document_source-einvoicedatecreate+0(4) }|.
        ELSE. "Điều chỉnh hóa đơn - tăng/giảm tiền
          ls_einvoice-inv-adj-rea = |Điều chỉnh cho hóa đơn { ls_document_source-einvoiceserial }{ ls_document_source-einvoicenumber }|
          && | ngày { ls_document_source-einvoicedatecreate+6(2) }/{ ls_document_source-einvoicedatecreate+4(2) }/{ ls_document_source-einvoicedatecreate+0(4) }|.
        ENDIF.
      ENDIF.

* Xác định loại điều chỉnh
      IF i_einvoice-Adjusttype = 1. "Điều chỉnh tăng
        ls_einvoice-inv-ud = '1'.
      ELSEIF i_einvoice-Adjusttype = 2. "Điều chỉnh giảm
        ls_einvoice-inv-ud = '0'.
      ELSE. "Thay thế

      ENDIF.
* Thẻ đánh dấu điều chỉnh kiểu mới:
      ls_einvoice-inv-adj_only_add = '1'.

    ENDIF.
*----------------------------------------------------------*
* Buyer Details *
    ls_einvoice-inv-bcode   = i_einvoice-Customer.
    ls_einvoice-inv-bname   = i_einvoice-customername.
*    ls_einvoice-inv-buyer =
    ls_einvoice-inv-btax    = i_einvoice-identificationnumber.
    ls_einvoice-inv-baddr   = i_einvoice-customeraddress.
    ls_einvoice-inv-btel    = i_einvoice-telephonenumber.
    ls_einvoice-inv-bmail   = i_einvoice-emailaddress.

*    ls_einvoice-inv-bacc    = i_einvoice.
*    ls_einvoice-inv-bbank   = i_einvoice.
*----------------------------------------------------------*
    ls_einvoice-inv-paym    = i_einvoice-paymentmethod.
    CASE i_einvoice-currencytype.
      WHEN '1'. "Transaction Currency
        ls_einvoice-inv-curr    = i_einvoice-transactioncurrency.
        ls_einvoice-inv-exrt    = i_einvoice-absoluteexchangerate.
* Amount *
        ls_einvoice-inv-sum     = i_einvoice-amountintransaccrcy.
        ls_einvoice-inv-vat     = i_einvoice-vatamountintransaccrcy.
        ls_einvoice-inv-total   = i_einvoice-totalamountintransaccrcy.

        ls_einvoice-inv-sumv    = i_einvoice-amountincocodecrcy.
        ls_einvoice-inv-vatv    = i_einvoice-vatamountincocodecrcy.
        ls_einvoice-inv-totalv  = i_einvoice-totalamountincocodecrcy.
      WHEN '2'. "Local Currency
        ls_einvoice-inv-curr    = i_einvoice-companycodecurrency.
        ls_einvoice-inv-exrt    = i_einvoice-absoluteexchangerate.
* Amount *
        ls_einvoice-inv-sum     = i_einvoice-amountincocodecrcy.
        ls_einvoice-inv-vat     = i_einvoice-vatamountincocodecrcy.
        ls_einvoice-inv-total   = i_einvoice-totalamountincocodecrcy.

        ls_einvoice-inv-sumv    = i_einvoice-amountincocodecrcy.
        ls_einvoice-inv-vatv    = i_einvoice-vatamountincocodecrcy.
        ls_einvoice-inv-totalv  = i_einvoice-totalamountincocodecrcy.
      WHEN OTHERS.
    ENDCASE.
*----------------------------------------------------------*
* Loại hóa đơn sử dụng mặc định theo thông tư 78
    ls_einvoice-inv-type_ref = 1.
* Supplier Taxcode
    ls_einvoice-inv-stax = i_einvoice-suppliertax.
* INV Items *
    DATA: ls_items LIKE LINE OF ls_einvoice-inv-items.
    LOOP AT lt_items INTO DATA(ls_einv_line).

      ls_items-line     = ls_einv_line-itemeinvoice.
      ls_items-type     = ''.

      IF ls_einv_line-taxcode = 'ON'.
        ls_items-vrt      = '-2'.
      ELSEIF ls_einv_line-taxcode = 'OX'.
        ls_items-vrt      = '-1'.
      ELSE.
        ls_items-vrt      = ls_einv_line-Taxpercentage.
      ENDIF.

      ls_items-code     = ls_einv_line-product.

      IF ls_einv_line-longtext IS NOT INITIAL.
        ls_items-name     = ls_einv_line-longtext.
      ELSE.
        ls_items-name     = ls_einv_line-documentitemtext.
      ENDIF.

      ls_items-unit     = ls_einv_line-unitofmeasurelongname.
      ls_items-quantity = ls_einv_line-Quantity.

      CASE i_einvoice-currencytype .
        WHEN '1'. "Transaction Currency
          ls_items-price    = ls_einv_line-priceintransaccrcy.
          ls_items-amount   = ls_einv_line-amountintransaccrcy.
          ls_items-vat      = ls_einv_line-vatamountintransaccrcy.
          ls_items-total    = ls_einv_line-totalamountintransaccrcy.

          ls_items-pricev   = ls_einv_line-priceincocodecrcy.
          ls_items-amountv  = ls_einv_line-amountincocodecrcy.
          ls_items-vatv     = ls_einv_line-vatamountincocodecrcy.
          ls_items-totalv   = ls_einv_line-totalamountincocodecrcy.
        WHEN '2'. "Local Currency
          ls_items-price    = ls_einv_line-priceincocodecrcy.
          ls_items-amount   = ls_einv_line-amountincocodecrcy.
          ls_items-vat      = ls_einv_line-vatamountincocodecrcy.
          ls_items-total    = ls_einv_line-totalamountincocodecrcy.

          ls_items-pricev   = ls_einv_line-priceincocodecrcy.
          ls_items-amountv  = ls_einv_line-amountincocodecrcy.
          ls_items-vatv     = ls_einv_line-vatamountincocodecrcy.
          ls_items-totalv   = ls_einv_line-totalamountincocodecrcy.
        WHEN OTHERS.
      ENDCASE.

      IF i_type = 'CREATE'.
        IF ls_items-amount < 0.
          ls_items-type = 'CK'.
          ls_items-amount = abs( ls_items-amount ).
          ls_items-vat    = abs( ls_items-vat ).
          ls_items-total  = abs( ls_items-total ).

          ls_items-amountv = abs( ls_items-amountv ).
          ls_items-vatv    = abs( ls_items-vatv ).
          ls_items-totalv  = abs( ls_items-totalv ).
        ENDIF.
      ENDIF.

      APPEND ls_items TO ls_einvoice-inv-items.
      CLEAR: ls_items.
    ENDLOOP.

    MOVE-CORRESPONDING ls_einvoice TO e_adjust.
    MOVE-CORRESPONDING ls_einvoice TO e_create.
  ENDMETHOD.

  METHOD process_message.

    MOVE-CORRESPONDING i_document TO e_header.

    e_header-Iconsap = icon_sap.
    e_header-StatusSap = status_sap.
    e_header-messagetype = message_type.
    e_header-messagetext = message_text.

  ENDMETHOD.

ENDCLASS.

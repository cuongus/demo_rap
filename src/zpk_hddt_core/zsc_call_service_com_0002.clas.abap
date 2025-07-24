CLASS zsc_call_service_com_0002 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tt_headers TYPE TABLE OF zjp_a_hddt_h,
           tt_items   TYPE TABLE OF zjp_a_hddt_i.

    CLASS-DATA: mo_instance TYPE REF TO zsc_call_service_com_0002.
    CLASS-METHODS:
      "Create Contructor
      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zsc_call_service_com_0002,

      change_journal_entry_http IMPORTING i_header TYPE tt_headers
                                          i_items  TYPE tt_items
                                RAISING
                                          cx_uuid_error
                                          cx_abap_context_info_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZSC_CALL_SERVICE_COM_0002 IMPLEMENTATION.


  METHOD change_journal_entry_http.

    DATA: lv_url  TYPE string, " Replace with actual URL
          lv_pref TYPE string,
          i_xml   TYPE string.
*          lv_uuid TYPE string VALUE `urn:uuid:{{$randomUUID}}`.
    TRY.
        DATA(lv_host) = cl_abap_context_info=>get_system_url( ).
        SPLIT lv_host AT '.' INTO lv_host lv_pref.
        CONCATENATE lv_host `-api` `.` lv_pref INTO lv_host.
      CATCH cx_abap_context_info_error.
        "handle exception
    ENDTRY.

    lv_url = |https://{ lv_host }/sap/bc/srt/scs_ext/sap/journalentrybulkchangerequest_|.

    DATA: lv_seq     TYPE string,
          lv_bukrs   TYPE bukrs,
          lv_belnr   TYPE belnr_d,
          lv_gjahr   TYPE gjahr,
          lv_date    TYPE string,
          lv_id      TYPE string,
          lv_date_ph TYPE zde_char20,
          lv_buzei   TYPE zde_char3.

    DATA(system_uuid) = cl_system_uuid=>create_uuid_c36_static( ).

    DATA(lv_current_date) = cl_abap_context_info=>get_system_date( ).
    DATA(lv_current_time) = cl_abap_context_info=>get_system_time( ).
    DATA(lv_timezone) = cl_abap_context_info=>get_user_time_zone( ).

    lv_id  = |MSG_{ lv_current_date+0(4) }-{ lv_current_date+4(2) }-{ lv_current_date+6(2) }_API|.
    lv_date    = |{ lv_current_date+0(4) }-{ lv_current_date+4(2) }-{ lv_current_date+6(2) }T{ lv_current_time+0(2) }:{ lv_current_time+2(2) }:{ lv_current_time+4(2) }.1234567Z|.

    READ TABLE i_header INTO DATA(ls_header) INDEX 1.
    IF sy-subrc EQ 0.
      SHIFT ls_header-einvoicenumber LEFT DELETING LEADING '0'.
      lv_seq   = |1{ ls_header-einvoiceserial }#{ ls_header-einvoicenumber }|.
      lv_bukrs = ls_header-companycode.
      lv_belnr = ls_header-accountingdocument.
      lv_gjahr = ls_header-fiscalyear.
      lv_date_ph = |{ ls_header-einvoicedatecreate+0(4) }-{ ls_header-einvoicedatecreate+4(2) }-{ ls_header-einvoicedatecreate+6(2) }|.
*      lv_buzei = ls_header-documentitem.
    ENDIF.

    CHECK lv_seq IS NOT INITIAL.

    LOOP AT i_items INTO DATA(ls_item).
      TRY.

          lv_buzei = ls_item-AccountingDocumentItem.

          " Define the SOAP request body
          i_xml = |<?xml version="1.0" encoding="utf-8"?>| &&
                  |<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sfin="http://sap.com/xi/SAPSCORE/SFIN">| &&
                  |<soapenv:Header xmlns:wsa="http://www.w3.org/2005/08/addressing"><wsa:Action>http://sap.com/xi/SAPSCORE/SFIN/JournalEntryBulkChangeRequest_In/JournalEntryBulkChangeRequest_InRequest</wsa:Action>| &&
                  |<wsa:MessageID>uuid:{ system_uuid }</wsa:MessageID>| &&
                  |</soapenv:Header>| &&
                  |<soapenv:Body>| &&
                  |<sfin:JournalEntryBulkChangeRequestMessage>| &&
                  |<MessageHeader>| &&
                      |<ID schemeID="?" schemeAgencyID="?">{ lv_id }</ID>| &&
                      |<CreationDateTime>{ lv_date }</CreationDateTime>| &&
                  |</MessageHeader>| &&
                      |<JournalEntryHeader>| &&
                  |<MessageHeader>| &&
                      |<ID schemeID="?" schemeAgencyID="?">SUB_{ lv_id }</ID>| &&
                      |<CreationDateTime>{ lv_date }</CreationDateTime>| &&
                  |</MessageHeader>| &&
                  |<HeaderKey>| &&
                      |<AccountingDocument>{ lv_belnr }</AccountingDocument>| &&
                      |<CompanyCode>{ lv_bukrs }</CompanyCode>| &&
                      |<FiscalYear>{ lv_gjahr }</FiscalYear>| &&
                  |</HeaderKey>| &&
                  |<DocumentReferenceIDChange>| &&
                      |<DocumentReferenceID>{ lv_seq }</DocumentReferenceID>| &&
                      |<FieldValueChangeIsRequested>true</FieldValueChangeIsRequested>| &&
                  |</DocumentReferenceIDChange>| &&
                  |</JournalEntryHeader>| &&
                  |<JournalEntryDebtorCreditorItem>| &&
                      |<MessageHeader>| &&
                         |<ID schemeID="?" schemeAgencyID="?">SUB2_{ lv_id }</ID>| &&
                         |<CreationDateTime>{ lv_date }</CreationDateTime>| &&
                      |</MessageHeader>| &&
                      |<ItemKey>| &&
                         |<AccountingDocument>{ lv_belnr }</AccountingDocument>| &&
                         |<CompanyCode>{ lv_bukrs }</CompanyCode>| &&
                         |<FiscalYear>{ lv_gjahr }</FiscalYear>| &&
                         |<AccountingDocumentItemID>{ lv_buzei }</AccountingDocumentItemID>| &&
                      |</ItemKey>| &&
                      |<DueCalculationBaseDateChange>| &&
                         |<DueCalculationBaseDate>{ lv_date_ph }</DueCalculationBaseDate>| &&
                         |<FieldValueChangeIsRequested>true</FieldValueChangeIsRequested>| &&
                      |</DueCalculationBaseDateChange>| &&
                  |</JournalEntryDebtorCreditorItem>| &&
                  |</sfin:JournalEntryBulkChangeRequestMessage>| &&
                  |</soapenv:Body>| &&
                  |</soapenv:Envelope>|.

          "create http destination by url; API endpoint for API sandbox
          DATA(lo_http_destination) =
            cl_http_destination_provider=>create_by_url(
            i_url = lv_url ).

          DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_http_destination ).

*-- SET HTTP Header Fields

          lo_http_client->get_http_request( )->set_header_fields( VALUE #(
              ( name = |Accept-Encoding| value = |gzip,deflate| )
              ( name = |Content-Type|    value = |text/xml;charset=UTF-8| )
              ( name = |SOAPAction|      value = |http://sap.com/xi/SAPSCORE/SFIN/JournalEntryBulkChangeRequest_In/JournalEntryBulkChangeRequest_InRequest| )
              ( name = |Host|            value = |my403229-api.s4hana.cloud.sap| )
              ( name = |Connection|      value = |Keep-Alive| )
              ( name = |User-Agent|      value = |Apache-HttpClient/4.5.5 (Java/16.0.2)| )
          ) ).

          DATA: lv_username TYPE string,
                lv_password TYPE string.

          lv_username = |INBOUND_COMM_USER_BTP_EXTENSION|.
          lv_password = `DCg=#-}.v-qnz&wFz2025`.

*-- Passing the Accept value in header which is a mandatory field
          lo_http_client->get_http_request( )->set_header_field( i_name = |username| i_value = lv_username ).
          lo_http_client->get_http_request( )->set_header_field( i_name = |password| i_value = lv_password ).
*-- Authorization
          lo_http_client->get_http_request( )->set_authorization_basic( i_username = lv_username i_password = lv_password ).
          lo_http_client->get_http_request( )->set_content_type( |text/xml;charset=UTF-8| ).

          lo_http_client->get_http_request( )->set_version( version = if_web_http_request=>co_protocol_version_1_1 ).
*-- Send request ->
          lo_http_client->get_http_request( )->set_text( i_xml ).
**-- POST
          lo_http_client->execute( i_method = if_web_http_client=>post
                                   ).
*-- Response ->
          DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>post
                                                       ).
          DATA(code) = lo_response->get_status( )-code.
          DATA(reason) = lo_response->get_status( )-reason.

        CATCH cx_root INTO DATA(lx_exception).

      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance.
    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                       THEN mo_instance
                                       ELSE NEW #( ) ).
  ENDMETHOD.
ENDCLASS.

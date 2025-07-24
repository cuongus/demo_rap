CLASS zcl_manage_viettel_einvoices DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      wa_userpass        TYPE zjp_hd_userpass,
      wa_document        TYPE zjp_a_hddt_h,
      pr_cancel_einvoice TYPE zpr_cancel_einvoice,
      tt_items           TYPE TABLE OF zjp_a_hddt_i.

    CLASS-DATA: go_viettel_sinvoice TYPE REF TO zcl_manage_viettel_einvoices,
                go_einvoice_process TYPE REF TO zcl_einvoice_process.

    METHODS contructor.

    CLASS-METHODS:

      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_manage_viettel_einvoices,

      convert_milis IMPORTING i_millis TYPE zde_numc15
                    EXPORTING e_date   TYPE zde_dats
                              e_time   TYPE zde_tims,

      get_time_milis IMPORTING i_date           TYPE zde_dats OPTIONAL
                               i_time           TYPE zde_tims OPTIONAL
                     EXPORTING e_current_millis TYPE zde_numc15
                     RAISING
                               cx_abap_context_info_error,

      cloi_put_sign_in_front IMPORTING i_input  TYPE zde_char100
                             EXPORTING o_output TYPE zde_char100,

      replace_json IMPORTING i_einvoice TYPE zst_viettel_sinvoice_json
                             i_json     TYPE zde_string
                   EXPORTING e_json     TYPE zde_string,

      post_sinvoices IMPORTING i_userpass TYPE wa_userpass
                               i_context  TYPE string
                               i_prefix   TYPE zde_txt255
                     EXPORTING e_context  TYPE string
                               e_return   TYPE bapiret2,

      get_sinvoices IMPORTING i_einvoice TYPE wa_document
                              i_userpass TYPE wa_userpass
                              i_url      TYPE zde_txt255
                    EXPORTING e_context  TYPE string
                              e_return   TYPE bapiret2,

      get_general IMPORTING i_einvoice TYPE wa_document
                            i_userpass TYPE wa_userpass
                            i_items    TYPE tt_items
                            i_type     TYPE zde_txt25
                  EXPORTING e_adjust   TYPE zst_viettel_sinvoice_json
                            e_create   TYPE zst_viettel_sinvoice_json
                  RAISING
                            cx_abap_context_info_error,

      create_sinvoices IMPORTING
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
      cancel_sinvoices IMPORTING
                         i_action   TYPE zde_action_invoice
                         i_einvoice TYPE wa_document
                         i_userpass TYPE wa_userpass
                         i_param    TYPE pr_cancel_einvoice
                       EXPORTING
                         e_status   TYPE wa_document
                         e_json     TYPE string
                         e_return   TYPE bapiret2
                       ,
      search_sinvoices IMPORTING
                         i_action   TYPE zde_action_invoice
                         i_einvoice TYPE wa_document
                         i_userpass TYPE wa_userpass  OPTIONAL
                       EXPORTING
                         e_status   TYPE wa_document
                         e_docsrc   TYPE wa_document
                         e_json     TYPE string
                         e_return   TYPE bapiret2
                       ,
      adjust_sinvoices IMPORTING
                         i_action   TYPE zde_action_invoice
                         i_einvoice TYPE wa_document
                         i_items    TYPE tt_items
                         i_userpass TYPE wa_userpass
                       EXPORTING
                         e_status   TYPE wa_document
                         e_docsrc   TYPE wa_document
                         e_json     TYPE string
                         e_return   TYPE bapiret2
                       RAISING
                         cx_abap_context_info_error
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



CLASS ZCL_MANAGE_VIETTEL_EINVOICES IMPLEMENTATION.


  METHOD adjust_sinvoices.
    DATA: ls_einvoice TYPE zst_viettel_sinvoice_json.

    DATA:
          lv_url  TYPE zde_txt255.

    CLEAR: e_status, e_json, e_return, e_docsrc.

    CREATE OBJECT: go_einvoice_process, go_viettel_sinvoice.

    go_viettel_sinvoice->get_general(
      EXPORTING
        i_einvoice = i_einvoice
        i_userpass = i_userpass
        i_items    = i_items
        i_type     = 'ADJUST'
      IMPORTING
        e_adjust   = ls_einvoice
    ).

* Create JSON *

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_einvoice
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = DATA(lv_json_string)
    ).

    go_viettel_sinvoice->replace_json(
      EXPORTING
        i_einvoice = ls_einvoice
        i_json     = lv_json_string
      IMPORTING
        e_json     = lv_json_string
    ).

* Test run *
    IF i_einvoice-testrun IS NOT INITIAL.
      e_json = lv_json_string.
      RETURN.
    ENDIF.

* CALL API OUNTBOUND *
    DATA: lv_json_results TYPE string.

    IF i_action CP '*Draft' OR i_action CP '*DRAFT'.
      SELECT SINGLE url_value FROM zjp_hddt_url WHERE action = 'CreateInvoiceDraft'
      AND id_sys = 'VIETTEL' INTO @lv_url PRIVILEGED ACCESS.
    ELSE.
      SELECT SINGLE url_value FROM zjp_hddt_url WHERE action = 'CreateInvoicePH'
      AND id_sys = 'VIETTEL' INTO @lv_url PRIVILEGED ACCESS.
    ENDIF.

    go_viettel_sinvoice->post_sinvoices(
      EXPORTING
        i_userpass = i_userpass
        i_context  = lv_json_string
        i_prefix   = lv_url
      IMPORTING
        e_context  = lv_json_results
        e_return   = e_return ).

*-------------------------THE--END-------------------------*
    go_viettel_sinvoice->process_status(
      EXPORTING
        i_action   = i_action
        i_einvoice = i_einvoice
        i_return   = e_return
        i_status   = lv_json_results
      IMPORTING
        e_header   = e_status
        e_docsrc   = e_docsrc
    ).

  ENDMETHOD.


  METHOD cancel_sinvoices.

  ENDMETHOD.


  METHOD cloi_put_sign_in_front.

    DATA: text1(1) TYPE c,
          lv_pos   TYPE sy-fdpos.

    FIND '-' IN i_input MATCH OFFSET lv_pos.

    IF sy-subrc = 0 AND lv_pos <> 0.
      SPLIT i_input AT '-' INTO o_output text1.
      CONDENSE o_output.
      CONCATENATE '-' o_output INTO o_output.
    ELSE.
      o_output = i_input.
      CONDENSE o_output.
    ENDIF.

  ENDMETHOD.


  METHOD contructor.
    go_einvoice_process = COND #( WHEN go_einvoice_process IS BOUND
                              THEN go_einvoice_process
                              ELSE NEW #( )
                              ).

    go_viettel_sinvoice = COND #( WHEN go_viettel_sinvoice IS BOUND
                                  THEN go_viettel_sinvoice
                                  ELSE NEW #( )
                                  ).
  ENDMETHOD.


  METHOD convert_milis.
    DATA: date_1970      TYPE zde_dats VALUE '19700101',
          millis_in_day  TYPE zde_numc15 VALUE '86400000',
          millis_in_hour TYPE zde_numc15 VALUE '3600000',
          millis_in_min  TYPE zde_numc15 VALUE '60000',
          millis_in_sec  TYPE zde_numc15 VALUE '1000'
          .

    DATA: lv_value         TYPE p DECIMALS 5,
          lv_hour          TYPE int4,
          lv_min           TYPE int4,
          lv_sec           TYPE int4,
          lv_utc_timestamp TYPE timestamp,
          lv_time          TYPE zde_char6.

    CHECK i_millis NE 0.

    e_date = i_millis / millis_in_day + date_1970.

*"""{ -- hour
    lv_value = ( i_millis - ( e_date - date_1970 ) * millis_in_day ) / millis_in_hour.
    lv_hour  = lv_value DIV 1.
*"""{ -- min
    lv_value = ( lv_value MOD 1 ) * 60.
    lv_min   = lv_value DIV 1.
*"""{ -- sec
    lv_value = ( lv_value MOD 1 ) * 60.
    lv_sec   = lv_value DIV 1.
    lv_time  = lv_hour && lv_min && lv_sec.

    lv_time = |{ lv_time ALPHA = IN }|.

    e_time = lv_time.

  ENDMETHOD.


  METHOD create_sinvoices.
    DATA:
          lv_url  TYPE zde_txt255.
*
*    DATA: lt_items TYPE TABLE OF zjp_a_hddt_i.
    CLEAR: e_status, e_json, e_return, e_docsrc.
*
    DATA: ls_einvoice TYPE zst_viettel_sinvoice_json.

    CREATE OBJECT: go_einvoice_process, go_viettel_sinvoice.

    TRY.
        go_viettel_sinvoice->get_general(
          EXPORTING
            i_einvoice = i_einvoice
            i_userpass = i_userpass
            i_items    = i_items
            i_type     = 'CREATE'
          IMPORTING
            e_create   = ls_einvoice
        ).
      CATCH cx_abap_context_info_error.
        "handle exception
    ENDTRY.
*----------------------------------------------------------*
* Create JSON *
    DATA: lv_json_string TYPE string.

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_einvoice
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = lv_json_string
    ).


    go_viettel_sinvoice->replace_json(
      EXPORTING
        i_einvoice = ls_einvoice
        i_json     = lv_json_string
      IMPORTING
        e_json     = lv_json_string
    ).

* Test run *
    IF i_einvoice-testrun IS NOT INITIAL.
      e_json = lv_json_string.
      RETURN.
    ENDIF.

* Call API OUTBOUND *
    DATA: lv_json_results TYPE string.

*    lv_url = i_action.

    IF i_action CP '*Draft' OR i_action CP '*DRAFT'.
      SELECT SINGLE url_value FROM zjp_hddt_url WHERE action = 'CreateInvoiceDraft'
      AND id_sys = 'VIETTEL' INTO @lv_url PRIVILEGED ACCESS.
    ELSE.
      SELECT SINGLE url_value FROM zjp_hddt_url WHERE action = 'CreateInvoicePH'
      AND id_sys = 'VIETTEL' INTO @lv_url PRIVILEGED ACCESS.
    ENDIF.

    go_viettel_sinvoice->post_sinvoices(
      EXPORTING
        i_userpass = i_userpass
        i_context  = lv_json_string
        i_prefix   = lv_url
      IMPORTING
        e_context  = lv_json_results
        e_return   = e_return ).
*-------------------------THE--END-------------------------*
    go_viettel_sinvoice->process_status(
      EXPORTING
        i_action   = i_action
        i_einvoice = i_einvoice
        i_return   = e_return
        i_status   = lv_json_results
      IMPORTING
        e_header   = e_status
    ).

  ENDMETHOD.


  METHOD get_general.
    TYPES: BEGIN OF lty_data_tax,
             amount        TYPE  zde_dmbtr,
             vatamount     TYPE  zde_dmbtr,
             taxpercentage TYPE  zde_char10,
           END OF lty_data_tax.
    DATA: lt_data_tax TYPE TABLE OF lty_data_tax,
          ls_data_tax TYPE lty_data_tax.

    DATA: ls_invoice TYPE zst_viettel_sinvoice_json.

    DATA(o_jp_common_core) = NEW zcl_jp_common_core( ).

    CREATE OBJECT: go_einvoice_process, go_viettel_sinvoice.

    CLEAR: ls_invoice, e_adjust, e_create.

****"sellerInfo":      //thông tin người mua
    ls_invoice-sellerinfo-sellertaxcode         = i_userpass-suppliertax.

    DATA: lv_companycode TYPE bukrs.
    lv_companycode = i_einvoice-companycode.

    o_jp_common_core->get_companycode_details(
      EXPORTING
        iv_companycode         = lv_companycode
      IMPORTING
        wa_companycode_details = DATA(ls_companycode_details)
    ).

    ls_invoice-sellerinfo-sellerlegalname = ls_companycode_details-companycodename.

    ls_invoice-sellerinfo-selleraddressline = ls_companycode_details-companycodeaddr.
**********************************************************************
    ls_invoice-sellerinfo-sellerphonenumber = ls_companycode_details-telephonenumber.
*
*    ls_invoice-sellerinfo-sellerfaxnumber =
*
    ls_invoice-sellerinfo-selleremail = ls_companycode_details-email.
*
*    ls_invoice-sellerinfo-sellerBankAccount =
*
*    ls_invoice-sellerinfo-sellerbankname =
***"------------------------------------------------------------------------------

****"generalInvoiceInfo": //Thông tin chung của hóa đơn
    "Mã loại hóa đơn chỉ
    ls_invoice-generalinvoiceinfo-invoicetype   = i_einvoice-einvoicetype.

    "Ký hiệu mẫu hóa đơn
    ls_invoice-generalinvoiceinfo-templatecode  = i_einvoice-einvoiceform.

    "Ký hiệu hóa đơn
    ls_invoice-generalinvoiceinfo-invoiceseries = i_einvoice-einvoiceserial.

    "transactionUuid để kiểm trùng giao dịch lập hóa đơn
    ls_invoice-generalinvoiceinfo-transactionuuid = i_einvoice-sid.

    "Ngày phát hành hóa đơn
    go_viettel_sinvoice->get_time_milis(
      EXPORTING
        i_date           = i_einvoice-einvoicedatecreate
        i_time           = i_einvoice-einvoicetimecreate
      IMPORTING
        e_current_millis = DATA(e_current_millis)
    ).

    SHIFT e_current_millis LEFT DELETING LEADING '0'.
    ls_invoice-generalinvoiceinfo-invoiceIssuedDate = e_current_millis.

*Trạng thái điều chỉnh hóa đơn:
*1: Hóa đơn gốc
*3: Hóa đơn thay thế
*5: Hóa đơn điều chỉnh
*7: Hóa đơn xóa bỏ
    IF i_type = 'ADJUST'.
      SELECT SINGLE * FROM zjp_a_hddt_h
        WHERE Companycode        = @i_einvoice-Companycode
          AND Accountingdocument = @i_einvoice-accountingdocumentsource
          AND Fiscalyear         = @i_einvoice-fiscalyearsource
          INTO @DATA(ls_document_source).
      IF sy-subrc EQ 0.

        ls_invoice-generalinvoiceinfo-originalInvoiceId = |{ ls_document_source-einvoiceserial }{ ls_document_source-einvoicenumber }|.

        ls_invoice-generalinvoiceinfo-originalinvoiceissuedate = |{ ls_document_source-invdat }|.

        IF i_einvoice-adjusttype = '3'.
          ls_invoice-generalinvoiceinfo-invoicenote = |Thay thế cho hóa đơn { ls_document_source-einvoiceserial }{ ls_document_source-einvoicenumber }|
          && | ngày { ls_document_source-einvoicedatecreate+6(2) }/{ ls_document_source-einvoicedatecreate+4(2) }/{ ls_document_source-einvoicedatecreate+0(4) }|.
        ELSE.
          ls_invoice-generalinvoiceinfo-invoicenote = |Điều chỉnh cho hóa đơn { ls_document_source-einvoiceserial }{ ls_document_source-einvoicenumber }|
          && | ngày { ls_document_source-einvoicedatecreate+6(2) }/{ ls_document_source-einvoicedatecreate+4(2) }/{ ls_document_source-einvoicedatecreate+0(4) }|.
        ENDIF.
      ENDIF.
    ELSE.

    ENDIF.

    IF i_einvoice-accountingdocumentsource IS INITIAL.
      ls_invoice-generalinvoiceinfo-adjustmenttype = '1'.
    ELSE.
      IF i_einvoice-adjusttype = '3'. "Thay thế HĐ
        ls_invoice-generalinvoiceinfo-adjustmenttype = '3'.
      ELSE. "Điều chỉnh HĐ
        ls_invoice-generalinvoiceinfo-adjustmenttype = '5'.
      ENDIF.
    ENDIF.

    "Được sử dụng như là 1 dòng ghi chú ở dưới danh sách hàng hóa
    IF ls_invoice-generalinvoiceinfo-adjustmenttype = '1'.
      ls_invoice-generalinvoiceinfo-invoicenote = i_einvoice-sid.
    ENDIF.

    "Trạng thái thanh toán
    ls_invoice-generalinvoiceinfo-paymentstatus = 'true'.

    "Cho khách hàng xem hóa đơn trong Quản lý hóa đơn
    ls_invoice-generalinvoiceinfo-cusgetinvoiceright = 'true'.

* "{ -- Currency Code
    CASE i_einvoice-currencytype.
      WHEN '1'. "Transaction Currency
        ls_invoice-generalinvoiceinfo-currencycode = i_einvoice-transactioncurrency.
      WHEN '2'. "Local Currency
        ls_invoice-generalinvoiceinfo-currencycode = i_einvoice-companycodecurrency.
      WHEN OTHERS.
    ENDCASE.

****"buyerInfo":      //thông tin người bán
    "Mã khách hàng
    ls_invoice-buyerinfo-buyercode = i_einvoice-customer.

    "Tên người mua trong trường hợp là người mua lẻ, cá nhân
*          ls_invoice-buyerinfo-buyername = .

    "Tên đơn vị (đăng ký kinh doanh trong trường hợp là doanh nghiệp) của người mua
    ls_invoice-buyerinfo-buyerlegalname = i_einvoice-customername.

    "Địa chỉ xuất hóa đơn của người mua (Bắt buộc khi buyerNotGetInvoice = 0)
    ls_invoice-buyerinfo-buyeraddressline = i_einvoice-customeraddress.

    "Mã số thuế người mua
    ls_invoice-buyerinfo-buyertaxcode = i_einvoice-identificationnumber.

*          ls_invoice-buyerinfo-buyerbankname = .
*
*          ls_invoice-buyerinfo-buyerbankaccount = .

    "Email người mua
    ls_invoice-buyerinfo-buyeremail = i_einvoice-emailaddress.

    "Số điện thoại người mua
    ls_invoice-buyerinfo-buyerphonenumber = i_einvoice-telephonenumber.

* "{ -- Buyer Detail
    IF ls_invoice-buyerinfo-buyeraddressline IS INITIAL.
      ls_invoice-buyerinfo-buyerNotGetInvoice = '1'.
    ENDIF.

    "payments": //thông tin thanh toán
    DATA: ls_payments TYPE zst_payments,
          lt_payments TYPE ztt_payments.

    ls_payments-paymentmethodname = i_einvoice-paymentmethod.
    APPEND ls_payments TO lt_payments.

    ls_invoice-payments = lt_payments.

****"itemInfo":      //thông tin hàng hóa
    DATA: ls_iteminfo TYPE zst_iteminfo.

    LOOP AT i_items INTO DATA(ls_item).

      CASE i_einvoice-currencytype.
        WHEN '1'. "Transaction Currency
          ls_iteminfo-itemtotalamountwithouttax = ls_item-amountintransaccrcy.
          ls_iteminfo-taxamount = ls_item-vatamountintransaccrcy.
          IF ls_item-priceintransaccrcy EQ 0.
            ls_iteminfo-unitprice = ''.
          ELSE.
            ls_iteminfo-unitprice = ls_item-priceintransaccrcy.
          ENDIF.
        WHEN '2'. "Local
          ls_iteminfo-itemtotalamountwithouttax = ls_item-amountincocodecrcy.
          ls_iteminfo-taxamount = ls_item-vatamountincocodecrcy.
          IF ls_item-priceincocodecrcy EQ 0.
            ls_iteminfo-unitprice = ''.
          ELSE.
            ls_iteminfo-unitprice = ls_item-priceincocodecrcy.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      IF ls_item-longtext IS NOT INITIAL.
        ls_iteminfo-itemname = ls_item-longtext.
      ELSE.
        ls_iteminfo-itemname = ls_item-documentitemtext.
      ENDIF.

      CONDENSE ls_iteminfo-taxamount NO-GAPS.
      CONDENSE ls_iteminfo-itemtotalamountwithouttax NO-GAPS.
      CONDENSE ls_iteminfo-unitprice  NO-GAPS.

      go_viettel_sinvoice->cloi_put_sign_in_front(
        EXPORTING
          i_input  = ls_iteminfo-itemtotalamountwithouttax
        IMPORTING
          o_output = ls_iteminfo-itemtotalamountwithouttax
      ).

      go_viettel_sinvoice->cloi_put_sign_in_front(
        EXPORTING
          i_input  = ls_iteminfo-taxamount
        IMPORTING
          o_output = ls_iteminfo-taxamount
      ).

      IF ls_iteminfo-unitprice IS NOT INITIAL.
        go_viettel_sinvoice->cloi_put_sign_in_front(
          EXPORTING
            i_input  = ls_iteminfo-unitprice
          IMPORTING
            o_output = ls_iteminfo-unitprice
        ).
      ENDIF.

      ls_iteminfo-taxpercentage = ls_item-taxpercentage.
      CONDENSE ls_iteminfo-taxpercentage NO-GAPS.

      IF ls_item-quantity = 0.
        ls_iteminfo-quantity = ''.
      ELSE.
        ls_iteminfo-quantity = ls_item-quantity.
      ENDIF.

      CONDENSE ls_iteminfo-quantity NO-GAPS.

      IF ls_iteminfo-quantity IS NOT INITIAL.
        go_viettel_sinvoice->cloi_put_sign_in_front(
          EXPORTING
            i_input  = ls_iteminfo-quantity
          IMPORTING
            o_output = ls_iteminfo-quantity
        ).
      ENDIF.

      CASE i_einvoice-adjusttype.
        WHEN '1'. "ĐC Tăng
          ls_iteminfo-isincreaseitem = 'true'.
        WHEN '2'. "ĐC Giảm
          ls_iteminfo-isincreaseitem = 'false'.
        WHEN OTHERS.
          ls_iteminfo-isincreaseitem = 'null'.
      ENDCASE.

      ls_iteminfo-unitcode = ls_item-baseunit.
      ls_iteminfo-unitname = ls_item-unitofmeasurelongname.
      APPEND ls_iteminfo TO ls_invoice-iteminfo.
      CLEAR: ls_iteminfo.

***" Get data for Table Taxbreakdowns
      CASE i_einvoice-currencytype.
        WHEN '1'. "Transaction Currency
          ls_data_tax-amount = ls_item-amountintransaccrcy.
          ls_data_tax-vatamount = ls_item-vatamountintransaccrcy.
        WHEN '2'. "Local
          ls_data_tax-amount = ls_item-amountincocodecrcy.
          ls_data_tax-vatamount = ls_item-vatamountincocodecrcy.
        WHEN OTHERS.
      ENDCASE.

      ls_data_tax-taxpercentage = ls_item-taxpercentage.

      CONDENSE ls_data_tax-taxpercentage NO-GAPS.

      COLLECT ls_data_tax INTO lt_data_tax.
      CLEAR: ls_data_tax.
      CLEAR: ls_item.

    ENDLOOP.

****"metadata": //thông tin trường động

****"meterReading":  //thông tin đặc biệt dành cho hóa đơn điện nước

****"summarizeInfo":    //thông tin tổng hợp tiền của hóa đơn

    CASE i_einvoice-adjusttype.
      WHEN '1'. "1-ÐC tăng
        ls_invoice-summarizeinfo-isTotalAmountPos        = 'true'.
        ls_invoice-summarizeinfo-isTotalTaxAmountPos     = 'true'.
        ls_invoice-summarizeinfo-isTotalTaxAmountPos     = 'true'.
        ls_invoice-summarizeinfo-isTotalAmtWithoutTaxPos = 'true'.
        ls_invoice-summarizeinfo-isDiscountAmtPos        = 'true'.
      WHEN '2'. "2-ÐC giảm
        ls_invoice-summarizeinfo-isTotalAmountPos        = 'true'.
        ls_invoice-summarizeinfo-isTotalTaxAmountPos     = 'true'.
        ls_invoice-summarizeinfo-isTotalTaxAmountPos     = 'true'.
        ls_invoice-summarizeinfo-isTotalAmtWithoutTaxPos = 'true'.
        ls_invoice-summarizeinfo-isDiscountAmtPos        = 'true'.
      WHEN OTHERS.
    ENDCASE.

****"taxBreakdowns":    //thông tin gom nhóm tiền hóa đơn theo thuế suất
    DATA: ls_taxbreakdowns TYPE zst_taxbreakdowns.

    LOOP AT lt_data_tax INTO ls_data_tax.
      ls_taxbreakdowns-taxamount = ls_data_tax-vatamount.
      ls_taxbreakdowns-taxableamount = ls_data_tax-amount .
      ls_taxbreakdowns-taxpercentage = ls_data_tax-taxpercentage.

      "move negative sign to the front
      go_viettel_sinvoice->cloi_put_sign_in_front(
        EXPORTING
          i_input  = ls_taxbreakdowns-taxamount
        IMPORTING
          o_output = ls_taxbreakdowns-taxamount
      ).
      "move negative sign to the front
      go_viettel_sinvoice->cloi_put_sign_in_front(
        EXPORTING
          i_input  = ls_taxbreakdowns-taxableamount
        IMPORTING
          o_output = ls_taxbreakdowns-taxableamount
      ).

      CASE i_einvoice-adjusttype.
        WHEN '1'. "ÐC tăng
          ls_taxbreakdowns-taxableAmountPos = 'true'.
          ls_taxbreakdowns-taxAmountPos = 'true'.
        WHEN '2'. "ÐC giảm
          ls_taxbreakdowns-taxableAmountPos = 'false'.
          ls_taxbreakdowns-taxAmountPos = 'false'.
        WHEN OTHERS.
      ENDCASE.
      COLLECT ls_taxbreakdowns INTO ls_invoice-taxbreakdowns.
      CLEAR: ls_taxbreakdowns.

    ENDLOOP.

    IF i_type = 'ADJUST'.
      e_adjust = ls_invoice.
    ELSE.
      e_create = ls_invoice.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.
    go_viettel_sinvoice = ro_instance = COND #( WHEN go_viettel_sinvoice IS BOUND
                                               THEN go_viettel_sinvoice
                                               ELSE NEW #( ) ).
  ENDMETHOD.


  METHOD get_sinvoices.
    DATA: i_prefix TYPE string.
    DATA: lv_stax TYPE string,
          lv_uuid TYPE string.

    CLEAR: e_context, e_return.

*-- Create HTTP client ->
    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
          comm_scenario = |Z_API_VIETTEL_EINVOICE_CSCEN|
          service_id    = |Z_API_VIETTEL_EINVOICE_OB_REST|
        ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).
*-- Add path ->
*        lo_http_client->get_http_request( )->set_uri_path( |{ i_prefix }| ).
        lo_http_client->get_http_request( )->set_header_field( i_name = |~request_uri| i_value = |{ i_url }{ i_prefix }| ).
*-- SET HTTP Header Fields
*        lo_http_client->get_http_request( )->set_header_field( i_name = |Content-Type| i_value = |application/json| ).

        lo_http_client->get_http_request( )->set_header_field( i_name = |Content-Type| i_value = |application/x-www-form-urlencoded| ).

        lo_http_client->get_http_request( )->set_header_field( i_name = |Accept| i_value = |*/*| ).

        lv_uuid = i_einvoice-sid.
        lo_http_client->get_http_request( )->set_form_field( i_name = |transactionUuid| i_value = lv_uuid ).

        lv_stax = i_einvoice-suppliertax.
        lo_http_client->get_http_request( )->set_form_field( i_name = |supplierTaxCode| i_value = lv_stax ).

        DATA: lv_username TYPE string,
              lv_password TYPE string.

        lv_username = i_userpass-username.
        lv_password = i_userpass-password.
*-- Passing the Accept value in header which is a mandatory field
        lo_http_client->get_http_request( )->set_header_field( i_name = |username| i_value = lv_username ).
        lo_http_client->get_http_request( )->set_header_field( i_name = |password| i_value = lv_password ).
*-- Authorization
        lo_http_client->get_http_request( )->set_authorization_basic( i_username = lv_username i_password = lv_password ).
*-- GET
        lo_http_client->execute( i_method  = if_web_http_client=>post
                                 i_timeout = 60 ).

        lo_http_client->get_http_request( )->set_content_type( |application/x-www-form-urlencoded| ).

*-- Response ->
        DATA(lo_response) = lo_http_client->execute( i_method  = if_web_http_client=>post
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


  METHOD get_time_milis.
    DATA: date_1970        TYPE zde_dats VALUE '19700101',
          millis_in_day    TYPE zde_numc15 VALUE '86400000',
          millis_in_hour   TYPE zde_numc15 VALUE '3600000',
          millis_in_min    TYPE zde_numc15 VALUE '60000',
          millis_in_sec    TYPE zde_numc15 VALUE '1000',
          current_date     TYPE zde_dats,
          current_ts       TYPE timestampl,
          current_ts_s(22),
          sec_fraction     TYPE f.

*  GET TIME STAMP FIELD current_ts.
    DATA(lv_tzone) = cl_abap_context_info=>get_user_time_zone( ).

    CONVERT DATE i_date TIME i_time DAYLIGHT SAVING TIME 'X'
            INTO TIME STAMP DATA(time_stamp) TIME ZONE lv_tzone.

    current_ts = time_stamp.

    current_ts_s = current_ts.
    current_date = current_ts_s(8).
    sec_fraction = current_ts_s+14(8).

    e_current_millis = ( current_date - date_1970 ) * millis_in_day +
    current_ts_s+8(2) * millis_in_hour +
    current_ts_s+10(2) * millis_in_min +
    current_ts_s+12(2) * millis_in_sec +
    sec_fraction * millis_in_sec.

  ENDMETHOD.


  METHOD post_sinvoices.
    CLEAR: e_context, e_return.

    IF i_context IS INITIAL. RETURN. ENDIF.
*-- Create HTTP client ->
    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
          comm_scenario = |Z_API_VIETTEL_EINVOICE_CSCEN|
          service_id    = |Z_API_VIETTEL_EINVOICE_OB_REST|
        ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).
*-- Add path ->
*        lo_http_client->get_http_request( )->set_uri_path( |{ i_prefix }| ).
        lo_http_client->get_http_request( )->set_header_field( i_name = |~request_uri| i_value = |{ i_prefix }{ i_userpass-suppliertax }| ).
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
        lo_http_client->execute( i_method  = if_web_http_client=>post
                                 i_timeout = 60 ).
*-- Send request ->
        lo_http_client->get_http_request( )->set_text( i_context ).
*-- Response ->
        DATA(lo_response) = lo_http_client->execute( i_method  = if_web_http_client=>post
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


  METHOD process_message.

    MOVE-CORRESPONDING i_document TO e_header.

    e_header-Iconsap = icon_sap.
    e_header-StatusSap = status_sap.
    e_header-messagetype = message_type.
    e_header-messagetext = message_text.

  ENDMETHOD.


  METHOD process_status.
    CLEAR: e_header, e_docsrc.

    CREATE OBJECT: go_einvoice_process, go_viettel_sinvoice.

    TYPES: BEGIN OF lty_message,
             errorCode   TYPE string,
             description TYPE string,
             result      TYPE string,
           END OF lty_message,

           BEGIN OF lty_imessage,
             code      TYPE string,
             message   TYPE string,
             data      TYPE string,
             errorCode TYPE string,
           END OF lty_imessage.

    DATA: ls_message     TYPE zst_vt_response_cre,
          imessage       TYPE lty_imessage,

          ls_responseinf TYPE zst_vt_response_info,

          lt_result      TYPE ztt_vt_result_info,
          ls_result      TYPE zst_vt_result_info.

    DATA: lv_invoiceno TYPE zde_char255,
          lv_millis    TYPE zde_numc15.

    IF i_return-type NE 'E'.
      CASE i_action.

        WHEN 'CREATE_INVOICE' OR 'ADJUST_INVOICE'.
          /ui2/cl_json=>deserialize(
            EXPORTING
              json        = i_status
*             jsonx       =
              pretty_name = /ui2/cl_json=>pretty_mode-none
*             assoc_arrays     =
*             assoc_arrays_opt =
*             name_mappings    =
*             conversion_exits =
*             hex_as_base64    =
            CHANGING
              data        = ls_message
          ).

          IF ls_message-errorcode IS INITIAL.

            e_header-createddate   = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value.
            e_header-createdbyuser = sy-uname.
            e_header-createdtime   = xco_cp=>sy->time( )->as( xco_cp_time=>format->abap )->value.

*            e_header-statussap = '02'.
            e_header-statussap = '98'.
            e_header-messagetype = 'S'.
            e_header-messagetext = 'Đã lập hóa đơn'.

            lv_invoiceno = ls_message-result-invoiceno.

            e_header-einvoiceform = i_einvoice-einvoiceform.
            e_header-einvoiceserial = i_einvoice-einvoiceserial.
            e_header-einvoicetype = i_einvoice-einvoicetype.

            REPLACE ALL OCCURRENCES OF i_einvoice-einvoiceserial IN lv_invoiceno WITH space.
            CONDENSE lv_invoiceno NO-GAPS.

            e_header-einvoicenumber = lv_invoiceno.

*            lv_millis = ls_message-result-issuedate.
*
*                go_viettel_sinvoice->convert_milis(
*                  EXPORTING
*                    i_millis = lv_millis
*                  IMPORTING
*                    e_date   = e_header-einvoicedatecreate
*                    e_time   = e_header-einvoicetimecreate
*                ).

          ELSE.
            e_header-statussap = '03'.
            e_header-messagetype = 'E'.
            e_header-messagetext = ls_message-description.
          ENDIF.

        WHEN 'SEARCH_INVOICE'.
          /ui2/cl_json=>deserialize(
            EXPORTING
              json        = i_status
*             jsonx       =
              pretty_name = /ui2/cl_json=>pretty_mode-user
*             assoc_arrays     =
*             assoc_arrays_opt =
*             name_mappings    =
*             conversion_exits =
*             hex_as_base64    =
            CHANGING
              data        = imessage
          ).

          IF imessage-message = 'NOT_FOUND_DATA'.
            e_header-statussap = '01'.
            e_header-messagetype = 'E'.
            e_header-messagetext = imessage-data.
          ELSE.
            /ui2/cl_json=>deserialize(
              EXPORTING
                json        = i_status
*               jsonx       =
                pretty_name = /ui2/cl_json=>pretty_mode-user
*               assoc_arrays     =
*               assoc_arrays_opt =
*               name_mappings    =
*               conversion_exits =
*               hex_as_base64    =
              CHANGING
                data        = ls_responseinf
            ).

            IF ls_responseinf-errorcode IS NOT INITIAL.
              e_header-statussap = '03'.
              e_header-messagetype = 'E'.
              e_header-messagetext = ls_message-description.
            ELSE.
              READ TABLE ls_responseinf-result INTO ls_result INDEX 1.

              IF ls_result-reservationcode IS NOT INITIAL.
                e_header-messagetype = 'S'.

                CASE ls_result-exchangestatus.
                    "CQT đã chấp nhận
                  WHEN 'INVOICE_HAS_CODE_APPROVED' OR 'INVOICE_NO_CODE_APPROVED'.
                    e_header-statussap = '99'.
                    e_header-messagetext = 'CQT đã chấp nhận'.
                    "Đã gửi CQT
                  WHEN 'INVOICE_HAS_CODE_SENT' OR 'INVOICE_NO_CODE_SENT'.
                    e_header-statussap = '98'.
                    e_header-messagetext = 'Đã gửi CQT'.
                    "CQT Từ chối
                  WHEN 'INVOICE_HAS_CODE_DIS_APPROVED' OR 'INVOICE_NO_CODE_DIS_APPROVED'.
                    e_header-statussap = '10'.
                    e_header-messagetext = 'CQT Từ chối'.
                    "Ðã phát hành
                  WHEN 'INVOICE_HAS_CODE_NOT_SENT' OR 'INVOICE_NO_CODE_NOT_SENT'.
                    e_header-statussap = '98'.
                    e_header-messagetext = 'Ðã phát hành'.
                  WHEN OTHERS.
                ENDCASE.

                CASE ls_result-status.
                  WHEN 'Hóa đơn xóa bỏ'.
                    e_header-statussap = '04'.
                    e_header-messagetext = 'Huỷ hoá đơn thông thường'.
                  WHEN 'Hóa đơn thay thế'.

                  WHEN 'Hóa đơn bị điều chỉnh tiền'
                  OR 'Hóa đơn bị điều chỉnh thông tin'.
                    e_header-statussap = '06'.
                    e_header-messagetext = ls_result-status.
                  WHEN 'Hóa đơn bị thay thế'.
                    e_header-statussap = '07'.
                    e_header-messagetext = ls_result-status.
                  WHEN OTHERS.
                ENDCASE.

                SELECT SINGLE value FROM zjp_hd_config
                WITH PRIVILEGED ACCESS
                WHERE id_sys = '001' AND id_domain = 'STATUSINV'
                AND description = @ls_result-exchangestatus
                INTO @e_header-statusinvres.

                IF sy-subrc NE 0.
                  SELECT SINGLE value FROM zjp_hd_config
                  WITH PRIVILEGED ACCESS
                  WHERE id_sys = '001' AND id_domain = 'STATUSINV'
                  AND description = @ls_result-status
                  INTO @e_header-statusinvres.
                ENDIF.

                e_header-suppliertax   = ls_result-suppliertaxcode.
                e_header-invdat        = ls_result-issuedate.
                e_header-reservationcode   = ls_result-reservationcode.

                lv_invoiceno             = ls_result-invoiceno.

                REPLACE ALL OCCURRENCES OF i_einvoice-einvoiceserial IN lv_invoiceno WITH space.
                CONDENSE lv_invoiceno NO-GAPS.

                e_header-einvoicenumber = lv_invoiceno.
                lv_millis = ls_result-issuedate.

                go_viettel_sinvoice->convert_milis(
                  EXPORTING
                    i_millis = lv_millis
                  IMPORTING
                    e_date   = e_header-einvoicedatecreate
                    e_time   = e_header-einvoicetimecreate
                ).

                e_header-mscqt = ls_result-codeoftax.

              ELSE.
                e_header-statussap = '02'.
                SELECT SINGLE value FROM zjp_hd_config
                WITH PRIVILEGED ACCESS
                WHERE id_sys = '001' AND id_domain = 'STATUSINV'
                AND description = 'Đã lập HĐ nháp'
                INTO @e_header-statusinvres.
                e_header-messagetype     = 'S'.
                e_header-messagetext    = 'Đã tích hợp hoá đơn'.
              ENDIF.

            ENDIF.

          ENDIF.

        WHEN 'CANCEL_INVOICE'.

        WHEN OTHERS.
      ENDCASE.

      IF e_header-StatusSap = '98' OR e_header-StatusSap = '99'.
        "Hóa đơn bị điều chỉnh
        SELECT SINGLE * FROM zjp_a_hddt_h
        WITH PRIVILEGED ACCESS
        WHERE Companycode        = @i_einvoice-Companycode
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

    ENDIF.
  ENDMETHOD.


  METHOD replace_json.

    DATA: lt_json TYPE TABLE OF zjp_viettel_json,
          ls_json TYPE zjp_viettel_json.

    DATA: lv_tagname TYPE zde_char255.

    e_json = i_json.

    SELECT * FROM zjp_viettel_json INTO TABLE @lt_json PRIVILEGED ACCESS.
    LOOP AT lt_json INTO ls_json.
      lv_tagname = ls_json-tagname.
      TRANSLATE lv_tagname TO LOWER CASE.
      REPLACE ALL OCCURRENCES OF |{ lv_tagname }| IN e_json WITH |{ ls_json-value }|.
    ENDLOOP.

    REPLACE ALL OCCURRENCES OF |"true"| IN e_json WITH |true|.
    REPLACE ALL OCCURRENCES OF |"false"| IN e_json WITH |false|.
    REPLACE ALL OCCURRENCES OF |"null"| IN e_json WITH 'null'.

    REPLACE ALL OCCURRENCES OF |paymentMethodname| IN e_json WITH |paymentMethodName|.

    IF i_einvoice-generalinvoiceinfo-adjustmenttype = '1'.
      REPLACE ALL OCCURRENCES OF |,"originalInvoiceId":"","originalInvoiceIssueDate":""| IN e_json WITH ``.
    ENDIF.

  ENDMETHOD.


  METHOD search_sinvoices.
    CLEAR: e_status, e_docsrc, e_return, e_json.

    CREATE OBJECT: go_einvoice_process, go_viettel_sinvoice.

    DATA: lv_url  TYPE zde_txt255.

    CLEAR: e_status, e_json, e_return, e_docsrc.
    lv_url = i_action.
*" Username - Password

** Create JSON *
*    DATA(lv_json_string) = xco_cp_json=>data->from_abap( ls_einvoice )->apply( VALUE #(
*      ( xco_cp_json=>transformation->underscore_to_camel_case )
*      ( xco_cp_json=>transformation->boolean_to_abap_bool )
*    ) )->to_string( ).
*    e_json = lv_json_string.
*
** Test run *
*    IF i_einvoice-testrun IS NOT INITIAL.
*      e_json = lv_json_string.
*      RETURN.
*    ENDIF.

* CALL API OUTBOUND *

    DATA: lv_json_results TYPE string.

    SELECT SINGLE url_value FROM zjp_hddt_url WHERE action = 'SearchInvoice'
        AND id_sys = 'VIETTEL' INTO @lv_url PRIVILEGED ACCESS.

    go_viettel_sinvoice->get_sinvoices(
      EXPORTING
        i_einvoice = i_einvoice
        i_userpass = i_userpass
        i_url      = lv_url
      IMPORTING
        e_context  = lv_json_results
        e_return   = e_return ).
*-------------------------THE--END-------------------------*
    go_viettel_sinvoice->process_status(
      EXPORTING
        i_action   = i_action
        i_einvoice = i_einvoice
        i_return   = e_return
        i_status   = lv_json_results
      IMPORTING
        e_header   = e_status
        e_docsrc   = e_docsrc
    ).

  ENDMETHOD.
ENDCLASS.

CLASS zcl_jp_get_data_report_fi DEFINITION
  PUBLIC
*  FINAL
  INHERITING FROM cx_rap_query_provider
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           tt_ranges       TYPE TABLE OF ty_range_option,

           tt_returns      TYPE TABLE OF bapiret2,

           tt_soquytienmat TYPE TABLE OF zjp_c_soquytienmat.

    "Custom Entities
    INTERFACES if_rap_query_provider.

    CLASS-DATA: gt_soquytienmat TYPE tt_soquytienmat.

    CLASS-METHODS: get_soqytienmat IMPORTING ir_compaycode         TYPE tt_ranges
                                             ir_glaccount          TYPE tt_ranges
                                             ir_accountingdocument TYPE tt_ranges OPTIONAL
                                             ir_postingdate        TYPE tt_ranges
                                             ir_fiscalyear         TYPE tt_ranges OPTIONAL
                                             ir_documentdate       TYPE tt_ranges OPTIONAL
                                             ir_businesspartner    TYPE tt_ranges OPTIONAL
                                   EXPORTING e_soquytienmat        TYPE tt_soquytienmat
                                             e_return              TYPE tt_returns .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JP_GET_DATA_REPORT_FI IMPLEMENTATION.


  METHOD get_soqytienmat.
    DATA: ls_return TYPE bapiret2.
    DATA(lo_common_app) = zcl_jp_common_core=>get_instance( ).

    SELECT
        a~companycode,
        a~accountingdocument,
        a~fiscalyear,
        a~accountingdocumentitem,
        a~postingdate,
        a~documentdate,
        a~customer,
        a~supplier,
        a~GLAccount,
        a~CompanyCodeCurrency,
        a~TransactionCurrency,
        a~DocumentItemText,
        a~DebitCreditCode,
        a~isnegativeposting,
        a~AbsoluteAmountInCoCodeCrcy,
        a~AbsoluteAmountInTransacCrcy,
        b~DocumentReferenceID,
        b~AccountingDocumentType,
        b~AccountingDocCreatedByUser,
        b~AccountingDocumentCreationDate,
        b~Creationtime
    FROM i_operationalacctgdocitem AS a INNER JOIN I_JournalEntry AS b
        ON a~CompanyCode = b~CompanyCode
        AND a~AccountingDocument = b~AccountingDocument
        AND a~FiscalYear = b~FiscalYear
    WHERE a~CompanyCode  IN @ir_compaycode
      AND a~AccountingDocument IN @ir_accountingdocument
      AND a~PostingDate  IN @ir_postingdate
      AND a~DocumentDate IN @ir_documentdate
      AND a~GLAccount    IN @ir_glaccount
      AND a~FiscalYear   IN @ir_fiscalyear
      AND (  a~Customer IN @ir_businesspartner OR a~Supplier IN @ir_businesspartner )
    INTO TABLE @DATA(lt_data).

***""" Message Lỗi
    IF sy-subrc NE 0.
      ls_return-type = 'E'.
      ls_return-message = 'Không có dữ liệu'.
*      APPEND ls_return TO e_return.
*      EXIT.
    ENDIF.

***""" Tính số dư đầu kỳ
    lo_common_app->get_glaccount_sodu(
        EXPORTING
        ir_companycode = ir_compaycode
        ir_glaccount = ir_glaccount
        ir_date = ir_postingdate
        IMPORTING
        it_sodu_dk = DATA(lt_sodu_dk)
        it_sodu_ck = DATA(lt_sodu_ck)
    ).

    SORT lt_sodu_dk BY companycode glaccount ASCENDING.
    SORT lt_sodu_ck BY companycode glaccount ASCENDING.

***""" Sort data.
    SORT lt_data BY CompanyCode AccountingDocument FiscalYear AccountingDocumentItem ASCENDING.

    DATA: ls_soquytienmat TYPE zjp_c_soquytienmat,
          lv_stt          TYPE int4 VALUE IS INITIAL.

    DATA: wa_document TYPE zst_document_info.

    LOOP AT lt_data INTO DATA(ls_data).
      lv_stt = lv_stt + 1.

      ls_soquytienmat-stt = lv_stt.
      ls_soquytienmat-CompanyCode = ls_data-CompanyCode.
      ls_soquytienmat-AccountingDocument = ls_data-AccountingDocument.
      ls_soquytienmat-FiscalYear = ls_data-FiscalYear.
      ls_soquytienmat-AccountingDocumentItem = ls_data-AccountingDocumentItem.
      ls_soquytienmat-PostingDate = ls_data-PostingDate.
      ls_soquytienmat-DocumentDate = ls_data-DocumentDate.

      ls_soquytienmat-CompanyCodeCurrency = ls_data-CompanyCodeCurrency.
      ls_soquytienmat-TransactionCurrency = ls_data-TransactionCurrency.

      wa_document-companycode = ls_data-CompanyCode.
      wa_document-accountingdocument = ls_data-AccountingDocument.
      wa_document-fiscalyear = ls_data-FiscalYear.

      IF ls_data-Customer IS NOT INITIAL.
        wa_document-customer = ls_data-Customer.

        lo_common_app->get_customer_details(
            EXPORTING
            wa_document = wa_document
            IMPORTING
            wa_customer_details = DATA(ls_customer_detail)
        ).
        ls_soquytienmat-Doituong = ls_customer_detail-customername.

      ELSE.
        wa_document-customer = ls_data-Supplier.

        lo_common_app->get_supplier_details(
            EXPORTING
            wa_document = wa_document
            IMPORTING
            wa_supplier_details = DATA(ls_supplier_detail)
        ).
        ls_soquytienmat-Doituong = ls_customer_detail-customername.
      ENDIF.

      ls_soquytienmat-businesspartner = wa_document-customer.
      ls_soquytienmat-Diengiai = ls_data-DocumentItemText.

      ls_soquytienmat-CreationUser = ls_data-AccountingDocCreatedByUser.
      ls_soquytienmat-CreationDate = ls_data-AccountingDocumentCreationDate.
      ls_soquytienmat-CreationTime = ls_data-CreationTime.

      "S = Nợ <-> Thu/ H = Có <-> Chi
      IF ls_data-IsNegativePosting IS NOT INITIAL.
        IF ls_data-DebitCreditCode = 'S'.
          ls_soquytienmat-sophatsinhchi = ls_data-AbsoluteAmountInCoCodeCrcy * -1.
          ls_soquytienmat-sophatsinhchi_nt = ls_data-AbsoluteAmountInTransacCrcy * -1.

          ls_soquytienmat-sohieuCTchi = ls_data-DocumentReferenceID.
        ELSE.
          ls_soquytienmat-sophatsinhthu = ls_data-AbsoluteAmountInCoCodeCrcy * -1.
          ls_soquytienmat-sophatsinhthu_nt = ls_data-AbsoluteAmountInTransacCrcy * -1.

          ls_soquytienmat-sohieuCTthu = ls_data-DocumentReferenceID.
        ENDIF.
      ELSE.
        IF ls_data-DebitCreditCode = 'S'.
          ls_soquytienmat-sophatsinhthu = ls_data-AbsoluteAmountInCoCodeCrcy.
          ls_soquytienmat-sophatsinhthu_nt = ls_data-AbsoluteAmountInTransacCrcy.

          ls_soquytienmat-sohieuCTthu = ls_data-DocumentReferenceID.
        ELSE.
          ls_soquytienmat-sophatsinhchi = ls_data-AbsoluteAmountInCoCodeCrcy.
          ls_soquytienmat-sophatsinhchi_nt = ls_data-AbsoluteAmountInTransacCrcy.

          ls_soquytienmat-sohieuCTchi = ls_data-DocumentReferenceID.
        ENDIF.
      ENDIF.

      "Số dư
      READ TABLE lt_sodu_dk ASSIGNING FIELD-SYMBOL(<fs_sodu_dk>)
      WITH KEY companycode = ls_soquytienmat-CompanyCode
               glaccount = ls_soquytienmat-GLAccount BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_soquytienmat-Sodu = ls_soquytienmat-sophatsinhthu - ls_soquytienmat-sophatsinhchi
                               + <fs_sodu_dk>-sodudk.

        <fs_sodu_dk>-sodudk =  ls_soquytienmat-Sodu.

        ls_soquytienmat-Sodu_nt = ls_soquytienmat-sophatsinhthu_nt - ls_soquytienmat-sophatsinhchi_nt
                               + <fs_sodu_dk>-sodudk_nt.

        <fs_sodu_dk>-sodudk_nt =  ls_soquytienmat-Sodu_nt.
      ENDIF.

      APPEND ls_soquytienmat TO e_soquytienmat.

      CLEAR: ls_soquytienmat.

    ENDLOOP.

*    APPEND VALUE #( STT = '001' AccountingDocument = '20000000' ) TO e_soquytienmat.
*    APPEND VALUE #( STT = '002' AccountingDocument = '20000001' ) TO e_soquytienmat.

  ENDMETHOD.


  METHOD if_rap_query_provider~select.
**--- Custom Entities ---**
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
          ir_testrun            TYPE zcl_jp_common_core=>tt_ranges,

          ir_businesspartner    TYPE zcl_jp_common_core=>tt_ranges
          .

    DATA: lt_returns TYPE tt_returns.
    DATA: lo_report_fi TYPE REF TO zcl_jp_get_data_report_fi.

    FREE: lt_returns.

    DATA(lv_entity_id) = io_request->get_entity_id( ).

    lo_report_fi = NEW #( ).

    DATA(lo_common_app) = zcl_jp_common_core=>get_instance( ).

    lo_common_app->get_fillter_app(
        EXPORTING
            io_request  = io_request
            io_response = io_response
        IMPORTING
            ir_companycode        = ir_companycode
            ir_accountingdocument = ir_accountingdocument
            ir_fiscalyear         = ir_fiscalyear
            ir_glaccount          = ir_glaccount
            ir_postingdate        = ir_postingdate
            ir_documentdate       = ir_documentdate

*            ir_statussap          = ir_statussap
*            ir_einvoicenumber     = ir_einvoicenumber
*            ir_einvoicetype       = ir_einvoicetype
*            ir_currencytype       = ir_currencytype
*            ir_usertype           = ir_usertype
*            ir_typeofdate         = ir_typeofdate
*            ir_createdbyuser      = ir_createdbyuser
*            ir_enduser            = ir_enduser
*            ir_testrun            = ir_testrun

            ir_businesspartner     = ir_businesspartner

            wa_page_info          = ls_page_info
    ).

    IF ls_page_info-page_size < 0.
      ls_page_info-page_size = 50.
    ENDIF.

    DATA(max_rows) = COND #( WHEN ls_page_info-page_size = if_rap_query_paging=>page_size_unlimited THEN 0
               ELSE ls_page_info-page_size ).

    max_rows = ls_page_info-page_size + ls_page_info-offset.

    CASE lv_entity_id.

      WHEN 'ZJP_C_SOQUYTIENMAT'.
        lo_report_fi->get_soqytienmat(
            EXPORTING
            ir_compaycode   = ir_companycode
            ir_glaccount    = ir_glaccount
            ir_accountingdocument = ir_accountingdocument
            ir_postingdate  = ir_postingdate
            ir_fiscalyear   = ir_fiscalyear
            ir_documentdate = ir_documentdate
            IMPORTING
            e_soquytienmat = gt_soquytienmat
            e_return       = lt_returns
        ).

        IF lt_returns IS NOT INITIAL.
          READ TABLE lt_returns INTO DATA(ls_returns) INDEX 1.

          RAISE EXCEPTION TYPE zcl_einvoice_data
              MESSAGE ID ''
              TYPE ls_returns-type
              NUMBER ''
              WITH |{ ls_returns-message }|.
          RETURN.

        ENDIF.

        DATA: lt_soquytienmat TYPE tt_soquytienmat.

        LOOP AT gt_soquytienmat INTO DATA(ls_soquytienmat).
          IF sy-tabix > ls_page_info-offset.
            IF sy-tabix > max_rows.
              EXIT.
            ELSE.
              APPEND ls_soquytienmat TO lt_soquytienmat.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lines( gt_soquytienmat ) ).
        ENDIF.

        IF io_request->is_data_requested( ).
          io_response->set_data( lt_soquytienmat ).
        ENDIF.

      WHEN ''.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.

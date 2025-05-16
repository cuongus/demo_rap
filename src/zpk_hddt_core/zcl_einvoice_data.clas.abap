CLASS zcl_einvoice_data DEFINITION
  PUBLIC
  INHERITING FROM cx_rap_query_provider
*  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Custom Entities
    INTERFACES if_rap_query_provider.

    "Read Entities

    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           tt_ranges          TYPE TABLE OF ty_range_option,

           tt_returns         TYPE TABLE OF bapiret2,

           wa_document        TYPE zjp_a_hddt_h,

           tt_einvoice_header TYPE TABLE OF zjp_a_hddt_h,
           tt_einvoice_item   TYPE TABLE OF zjp_a_hddt_i.

    CLASS-DATA: gr_companycode        TYPE tt_ranges,
                gr_accountingdocument TYPE tt_ranges,
                gr_fiscalyear         TYPE tt_ranges,
                gr_period             TYPE tt_ranges,
                gr_postingdate        TYPE tt_ranges,
                gr_documentdate       TYPE tt_ranges,
                gr_customer           TYPE tt_ranges,

                gr_datetype           TYPE tt_ranges,
                gr_einvoicetype       TYPE tt_ranges,
                gr_statussap          TYPE tt_ranges,
                gr_createdbyuser      TYPE tt_ranges,
                gr_enduser            TYPE tt_ranges,

                gr_usertype           TYPE tt_ranges,

                mo_instance           TYPE REF TO zcl_einvoice_data,

                go_jp_common_core     TYPE REF TO zcl_jp_common_core,

                gt_einvoice_headers   TYPE tt_einvoice_header,
                gt_einvoice_items     TYPE tt_einvoice_item,

                go_einvoice_data      TYPE REF TO zcl_einvoice_data.
    .

    CLASS-METHODS:
      "Contructor.
      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_einvoice_data,

      "get data einvoice.
      get_einvoice_data IMPORTING ir_companycode        TYPE tt_ranges
                                  ir_accountingdocument TYPE tt_ranges OPTIONAL
                                  ir_fiscalyear         TYPE tt_ranges
                                  ir_period             TYPE tt_ranges OPTIONAL
                                  ir_postingdate        TYPE tt_ranges OPTIONAL
                                  ir_documentdate       TYPE tt_ranges OPTIONAL

                                  ir_customer           TYPE tt_ranges OPTIONAL

                                  ir_einvoicetype       TYPE tt_ranges OPTIONAL
                                  ir_statussap          TYPE tt_ranges OPTIONAL
                                  ir_einvoicenumber     TYPE tt_ranges OPTIONAL

                                  ir_createdbyuser      TYPE tt_ranges OPTIONAL "Created einvoice from SAP
                                  ir_enduser            TYPE tt_ranges OPTIONAL "Created document in SAP
                                  ir_usertype           TYPE tt_ranges OPTIONAL
                                  ir_TypeOfDate         TYPE tt_ranges OPTIONAL
                                  ir_CurrencyType       TYPE tt_ranges OPTIONAL

                                  ir_TestRun            TYPE tt_ranges OPTIONAL

                        EXPORTING it_einvoice_header    TYPE tt_einvoice_header
                                  it_einvoice_item      TYPE tt_einvoice_item
                                  it_returns            TYPE tt_returns
                        RAISING
                                  cx_abap_context_info_error
                        ,

      getdate_EInvoice IMPORTING i_document TYPE wa_document
                       EXPORTING e_document TYPE wa_document
                                 o_date     TYPE zde_einv_date
                                 o_time     TYPE zde_einv_time
                       RAISING
                                 cx_abap_context_info_error,
      "Read ranges index 1.
      read_ranges IMPORTING it_ranges TYPE tt_ranges
                  EXPORTING o_value   TYPE string
                  .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_einvoice_data IMPLEMENTATION.

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
          ir_testrun            TYPE zcl_jp_common_core=>tt_ranges
          .

    DATA: lt_einvoice_header TYPE tt_einvoice_header,
          lt_einvoice_item   TYPE tt_einvoice_item.

    DATA: lt_returns TYPE tt_returns.

    FREE: lt_einvoice_header, lt_einvoice_item, lt_returns.

    DATA(lv_entity_id) = io_request->get_entity_id( ).

    TRY.
        go_einvoice_data  = zcl_einvoice_data=>get_instance( ).

        DATA(lo_common_app) = zcl_jp_common_core=>get_instance( ).

        lo_common_app->get_fillter_app(
            EXPORTING
                io_request  = io_request
                io_response = io_response
            IMPORTING
                ir_companycode        = ir_companycode
                ir_accountingdocument = ir_accountingdocument
                ir_fiscalyear         = ir_fiscalyear
*                ir_glaccount          = ir_glaccount
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

                wa_page_info          = ls_page_info
        ).

*        AUTHORITY-CHECK OBJECT 'ZOBJECT***'
*          ID 'ACTVT' FIELD '03'
*          ID 'LV_FIELD' FIELD lv_field.
*        IF sy-subrc NE 0.

*        ENDIF.

        go_einvoice_data->get_einvoice_data(
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
                it_einvoice_header    = gt_einvoice_headers
                it_einvoice_item      = gt_einvoice_items
                it_returns            = lt_returns
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

        IF ls_page_info-page_size < 0.
          ls_page_info-page_size = 50.
        ENDIF.

        DATA(max_rows) = COND #( WHEN ls_page_info-page_size = if_rap_query_paging=>page_size_unlimited THEN 0
                   ELSE ls_page_info-page_size ).

        max_rows = ls_page_info-page_size + ls_page_info-offset.

        CASE lv_entity_id.
          WHEN 'ZJP_C_HDDT_H' OR 'EINVOICE_HEADERS'. ""---EInvoice Headers
            LOOP AT gt_einvoice_headers INTO DATA(ls_einvoice_h).
              IF sy-tabix > ls_page_info-offset.
                IF sy-tabix > max_rows.
                  EXIT.
                ELSE.
                  APPEND ls_einvoice_h TO lt_einvoice_header.
                ENDIF.
              ENDIF.
            ENDLOOP.

            IF io_request->is_total_numb_of_rec_requested( ).
              io_response->set_total_number_of_records( lines( gt_einvoice_headers ) ).
            ENDIF.

            IF io_request->is_data_requested( ).
              io_response->set_data( lt_einvoice_header ).
            ENDIF.
          WHEN 'ZJP_C_HDDT_I' OR 'EINVOICE_ITEMS'. ""---EInvoice Items
            LOOP AT gt_einvoice_items INTO DATA(ls_einvoice_i).
              IF sy-tabix > ls_page_info-offset.
                IF sy-tabix > max_rows.
                  EXIT.
                ELSE.
                  APPEND ls_einvoice_i TO lt_einvoice_item.
                ENDIF.
              ENDIF.
            ENDLOOP.

            IF io_request->is_total_numb_of_rec_requested( ).
              io_response->set_total_number_of_records( lines( gt_einvoice_items ) ).
            ENDIF.

            IF io_request->is_data_requested( ).
              io_response->set_data( lt_einvoice_item ).
            ENDIF.
        ENDCASE.

      CATCH cx_root INTO DATA(exception).

        DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).

        DATA(exception_t100_key) = cl_message_helper=>get_latest_t100_exception( exception )->t100key.

        RAISE EXCEPTION TYPE zcl_jp_get_data_scttgnh
          EXPORTING
            textid   = VALUE scx_t100key(
            msgid = exception_t100_key-msgid
            msgno = exception_t100_key-msgno
            attr1 = exception_t100_key-attr1
            attr2 = exception_t100_key-attr2
            attr3 = exception_t100_key-attr3
            attr4 = exception_t100_key-attr4 )
            previous = exception.
    ENDTRY.
  ENDMETHOD.

  METHOD get_einvoice_data.
    DATA:
      wa_document         TYPE zst_document_info,
      wa_customer_details TYPE zst_customer_info.

    DATA: lr_glaccount TYPE tt_ranges.

    DATA: lt_einvoice_header TYPE tt_einvoice_header,
          ls_einvoice_header LIKE LINE OF lt_einvoice_header,
          lt_einvoice_item   TYPE tt_einvoice_item,
          ls_einvoice_item   LIKE LINE OF lt_einvoice_item.

    DATA: ls_returns TYPE bapiret2.

    DATA: lv_index TYPE int4 VALUE IS INITIAL,
          lv_count TYPE int4 VALUE IS INITIAL.

    DATA: lv_usertype     TYPE string,
          lv_currencytype TYPE string,
          lv_typeofdate   TYPE string,
          lv_einvoicetype TYPE string,
          lv_testrun      TYPE string.

    DATA: lv_FiscalYear TYPE gjahr VALUE IS INITIAL,
          lv_taxcode    TYPE zde_taxcode VALUE IS INITIAL.

    DATA: lo_einvoice_data TYPE REF TO zcl_einvoice_data.

    CREATE OBJECT lo_einvoice_data.

**--Create Common
    CREATE OBJECT go_jp_common_core.

**--Lấy cấu hình GLACCT
    SELECT * FROM zjp_hd_glacc
    WHERE companycode IN @ir_companycode
    INTO TABLE @DATA(lt_hd_glacc).
    IF sy-subrc NE 0.
      "Message Error!
      ls_returns-type = 'E'.
      ls_returns-message = TEXT-001.
      APPEND ls_returns TO it_returns.
      CLEAR: ls_returns.
    ENDIF.

    "1 - TK hạch toán "2 - TK thuế
    LOOP AT lt_hd_glacc INTO DATA(ls_hd_glacc) WHERE glacctype EQ '1'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_hd_glacc-glaccount ) TO lr_glaccount.
    ENDLOOP.

**--Lấy cấu hình
    lo_einvoice_data->read_ranges(
        EXPORTING it_ranges = ir_currencytype[] IMPORTING o_value = lv_currencytype ).

    lo_einvoice_data->read_ranges(
        EXPORTING it_ranges = ir_typeofdate[] IMPORTING o_value = lv_typeofdate ).

    lo_einvoice_data->read_ranges(
        EXPORTING it_ranges = ir_usertype[] IMPORTING o_value = lv_usertype ).

    lo_einvoice_data->read_ranges(
        EXPORTING it_ranges = ir_einvoicetype[] IMPORTING o_value = lv_einvoicetype ).

    lo_einvoice_data->read_ranges(
        EXPORTING it_ranges = ir_testrun[] IMPORTING o_value = lv_testrun ).

**--Lấy Data BKPF
    SELECT   a~companycode ,
             a~accountingdocument,
             a~fiscalyear,
             a~billingdocument,
             a~accountingdocumentitem,
             b~FiscalPeriod,
             a~postingdate,
             a~documentdate,
             b~AccountingDocumentCreationDate,
             a~financialaccounttype,
             a~accountingdocumenttype,
             a~postingkey,
             a~debitcreditcode,
             a~glaccount,
             a~customer,
             a~taxcode,
             a~product,
             a~companycodecurrency,
             a~transactioncurrency,
             c~einvoicenumber,
             c~einvoiceserial,
             c~einvoiceform,
             b~absoluteexchangerate,
             b~reversedocument,
             b~reversedocumentfiscalyear,
             b~isreversal,
             b~isreversed
      FROM i_operationalacctgdocitem AS a

      INNER JOIN i_journalentry AS b
          ON  a~companycode        = b~companycode
          AND a~accountingdocument = b~accountingdocument
          AND a~fiscalyear         = b~fiscalyear

      LEFT OUTER JOIN zjp_a_hddt_h AS c
          ON  a~companycode        = c~companycode
          AND a~accountingdocument = c~accountingdocument
          AND a~fiscalyear         = c~fiscalyear

      WHERE a~CompanyCode        IN @ir_companycode
        AND a~AccountingDocument IN @ir_accountingdocument
        AND a~FiscalYear         IN @ir_fiscalyear
        AND a~FiscalPeriod       IN @ir_period
        AND a~PostingDate        IN @ir_postingdate
        AND a~DocumentDate       IN @ir_documentdate
        AND a~Customer           IN @ir_customer
        AND a~GLAccount          IN @lr_glaccount

        AND b~AccountingDocCreatedByUser IN @ir_enduser

        AND a~FinancialAccountType = 'D'
  "Trường hợp chứng từ huỷ chưa phát hành hoá đơn ko lấy lên
        AND b~IsReversal     NE 'X' "--> "Loại chứng từ huỷ
        AND ( ( b~IsReversed NE 'X' ) "--> "Loại bỏ chứng từ gốc đã huỷ
  "Trường hợp huỷ chứng từ sau khi đã phát hành hoá đơn vẫn lấy lên
        OR ( b~IsReversed EQ 'X' AND c~einvoicenumber NE '' ) )

        AND ( a~taxcode LIKE 'O%'
         OR   a~taxcode = '**' )

      ORDER BY a~companycode, a~accountingdocument, a~fiscalyear
      INTO TABLE @DATA(lt_bkpf)
      .

    "Log data E-Invoice Header
    SELECT * FROM zjp_a_hddt_h
    WHERE companycode           IN @ir_companycode
      AND fiscalyear            IN @ir_fiscalyear
      AND accountingdocument    IN @ir_accountingdocument
      AND customer              IN @ir_customer
      AND einvoicenumber        IN @ir_einvoicenumber
      AND statussap             IN @ir_statussap
      AND postingdate           IN @ir_postingdate
      AND documentdate          IN @ir_documentdate
      AND createdbyuser         IN @ir_createdbyuser
      ORDER BY companycode, accountingdocument, fiscalyear
    INTO TABLE @DATA(lt_a_hddt_h).

    IF lt_a_hddt_h IS NOT INITIAL.
      "Log data E-Invoice Item
      SELECT * FROM zjp_a_hddt_i
      FOR ALL ENTRIES IN @lt_a_hddt_h
      WHERE companycode         = @lt_a_hddt_h-companycode
        AND accountingdocument  = @lt_a_hddt_h-accountingdocument
        AND fiscalyear          = @lt_a_hddt_h-fiscalyear
      INTO TABLE @DATA(lt_a_hddt_i).
    ENDIF.

    SORT lt_bkpf BY CompanyCode AccountingDocument FiscalYear ASCENDING.

    CHECK lt_bkpf IS NOT INITIAL.

**---Lấy Data BSEG
    SELECT a~companycode ,
           a~accountingdocument,
           a~fiscalyear,
           a~billingdocument,
           a~accountingdocumentitem,
           a~postingdate,
           a~documentdate,
           a~financialaccounttype,
           a~accountingdocumenttype,
           a~postingkey,
           a~debitcreditcode,
           a~glaccount,
           a~customer,
           a~taxcode,
           a~product,
           a~documentitemtext,
           a~baseunit,
           a~quantity,
           a~amountintransactioncurrency, "
           a~amountincompanycodecurrency, "Local
           a~companycodecurrency,
           a~transactioncurrency,
           a~PaymentMethod,
           a~ProfitCenter
*           a~yy1_longtext_cob as LongText
    FROM i_operationalacctgdocitem AS a
    FOR ALL ENTRIES IN @lt_bkpf
    WHERE a~companycode = @lt_bkpf-CompanyCode
      AND a~accountingdocument = @lt_bkpf-AccountingDocument
      AND a~fiscalyear = @lt_bkpf-FiscalYear
      AND a~GLAccount IN @lr_glaccount
      AND a~FinancialAccountType = 'S'
      AND a~TaxCode IS NOT INITIAL
    INTO TABLE @DATA(lt_bseg).
**-----------------------------------------------------------------------**

**--Lấy data VAT
    TYPES: BEGIN OF lty_SumVAT,
             CompanyCode               TYPE bukrs,
             AccountingDocument        TYPE belnr_d,
             FiscalYear                TYPE gjahr,
             TaxCode                   TYPE zde_taxcode,
             DebitCreditCode           TYPE shkzg,
             TransactionCurrency       TYPE waers,
             CompanyCodeCurrency       TYPE waers,
             SumVATAmountintransaction TYPE zde_dmbtr,
             SumVATAmountincompanyco   TYPE zde_dmbtr,
           END OF lty_SumVAT.

    DATA: lt_SumVAT   TYPE TABLE OF lty_SumVAT,
          lt_SumVAT_T TYPE TABLE OF lty_SumVAT.

    DATA: lv_PaymentMethod TYPE zde_PaymentMethod VALUE IS INITIAL.

    FREE: lr_glaccount.
    "1 - TK hạch toán "2 - TK thuế
    LOOP AT lt_hd_glacc INTO ls_hd_glacc WHERE glacctype = '2'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_hd_glacc-glaccount ) TO lr_glaccount.
    ENDLOOP.

    SELECT companycode,
           accountingdocument,
           fiscalyear,
           taxcode,
           DebitCreditCode,
           TransactionCurrency,
           companycodecurrency,
           amountintransactioncurrency  AS SumVATAmountintransaction, "
           amountincompanycodecurrency  AS SumVATAmountincompanyco "Local
      FROM i_operationalacctgdocitem
      WHERE companycode         IN @ir_companycode
        AND accountingdocument  IN @ir_accountingdocument
        AND fiscalyear          IN @ir_fiscalyear
        AND GLAccount           IN @lr_glaccount
        AND ( taxcode LIKE 'O%' OR taxcode = '**' )
        INTO CORRESPONDING FIELDS OF TABLE @lt_SumVAT
        .
**-----------------------------------------------------------------------**

**SORT Table
    SORT lt_a_hddt_h BY companycode accountingdocument fiscalyear ASCENDING.
    SORT lt_bseg BY CompanyCode AccountingDocument fiscalyear AccountingDocumentItem ASCENDING.


**--PRocess Data
    LOOP AT lt_bkpf INTO DATA(ls_bkpf).
      CLEAR: lv_count, lv_taxcode.

      READ TABLE lt_a_hddt_h INTO DATA(ls_a_hddt_h)
      WITH KEY companycode        = ls_bkpf-CompanyCode
               accountingdocument = ls_bkpf-AccountingDocument
               fiscalyear         = ls_bkpf-FiscalYear BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF ls_a_hddt_h-messagetype = 'S'.
**---Trường hợp Document Phát hành thành công -> Lấy data từ bảng log hddt
          APPEND ls_a_hddt_h TO lt_einvoice_header.
          CLEAR: ls_a_hddt_h.

          READ TABLE lt_a_hddt_i TRANSPORTING NO FIELDS
          WITH KEY companycode        = ls_bkpf-CompanyCode
                   accountingdocument = ls_bkpf-AccountingDocument
                   fiscalyear         = ls_bkpf-FiscalYear BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_index = sy-tabix.
            LOOP AT lt_a_hddt_i INTO DATA(ls_a_hddt_i) FROM lv_index.
              IF NOT ( ls_a_hddt_i-companycode        EQ ls_bkpf-CompanyCode AND
                       ls_a_hddt_i-accountingdocument EQ ls_bkpf-AccountingDocument AND
                       ls_a_hddt_i-fiscalyear         EQ ls_bkpf-FiscalYear ).
                EXIT.
              ENDIF.

              APPEND ls_a_hddt_i TO lt_einvoice_item.
              CLEAR: ls_a_hddt_i.
            ENDLOOP.
          ENDIF.

          CONTINUE.
**-----------------------------------------------------------------------**
        ELSE.
        ENDIF.
      ENDIF.

*  DATA: lv_fwste  TYPE zde_dmbtr.

**--Status SAP
      IF ls_a_hddt_h-messagetype = 'E'.
        ls_einvoice_header-statussap = '03'.
        ls_einvoice_header-messagetype = 'E'.
      ELSE.
        ls_einvoice_header-statussap = '01'.
      ENDIF.

      IF NOT ls_einvoice_header-statussap IN ir_statussap[].
        CONTINUE.
      ENDIF.

      IF NOT ls_einvoice_header-einvoicenumber IN ir_einvoicenumber[].
        CONTINUE.
      ENDIF.

**---Trường hợp Document Chưa Phát hành thành công -> Lấy data từ bkpf, bseg
      READ TABLE lt_bseg TRANSPORTING NO FIELDS
      WITH KEY companycode        = ls_bkpf-CompanyCode
               accountingdocument = ls_bkpf-AccountingDocument
               fiscalyear         = ls_bkpf-FiscalYear BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.

        LOOP AT lt_bseg INTO DATA(ls_bseg) FROM lv_index.
          IF NOT ( ls_bseg-companycode        EQ ls_bkpf-CompanyCode AND
                   ls_bseg-accountingdocument EQ ls_bkpf-AccountingDocument AND
                   ls_bseg-fiscalyear         EQ ls_bkpf-FiscalYear ).
            EXIT.
          ENDIF.

*          CALL FUNCTION 'RECP_FI_TAX_CALCULATE'
*            EXPORTING
*              ic_bukrs    = ls_bseg-CompanyCode
*              ic_mwskz    = ls_bseg-TaxCode
*              ic_waers    = ls_bseg-CompanyCodeCurrency
*            IMPORTING
*              ep_tax_rate = lv_fwste.
          lv_count = lv_count + 1.

          ls_einvoice_item-companycode        = ls_bkpf-CompanyCode.
          ls_einvoice_item-accountingdocument = ls_bkpf-AccountingDocument.
          ls_einvoice_item-fiscalyear         = ls_bkpf-FiscalYear.

          ls_einvoice_item-accountingdocumentitem = ls_bseg-AccountingDocumentItem.
          ls_einvoice_item-itemeinvoice           = lv_count.

          "Mã Hàng hóa
          ls_einvoice_item-product                = ls_bseg-Product.
          "Tên hàng hóa
*          ls_einvoice_item-longtext               = ls_bseg-longtext.
          ls_einvoice_item-documentitemtext       = ls_bseg-DocumentItemText.

          "Đơn vị
          ls_einvoice_item-baseunit = ls_bseg-BaseUnit.
          "Số lượng
          ls_einvoice_item-quantity = ls_bseg-Quantity.
          "Text Đơn vị
          SELECT SINGLE unitofmeasurelongname FROM i_unitofmeasuretext
          WHERE unitofmeasure = @ls_bseg-BaseUnit INTO @ls_einvoice_item-unitofmeasurelongname.

          "Tax Percentage
          SELECT SINGLE taxpercentage FROM zjp_hd_taxcode
          WHERE companycode  = @ls_bkpf-CompanyCode
            AND currency     = @ls_bkpf-CompanyCodeCurrency
            AND taxcode      = @ls_bseg-TaxCode
          INTO @ls_einvoice_item-taxpercentage.
          IF sy-subrc NE 0.
            "Message Error!
            ls_returns-type = 'E'.
            ls_returns-message = TEXT-003.
            APPEND ls_returns TO it_returns.
            CLEAR: ls_returns.
          ENDIF.

          "Số tiền
          ls_bseg-amountintransactioncurrency = ls_bseg-amountintransactioncurrency * ( -1 ).
          ls_bseg-amountincompanycodecurrency = ls_bseg-amountincompanycodecurrency * ( -1 ).

          "Amount - Transaction Currency
          "VAT - Transaction Currency
          IF ls_bseg-TransactionCurrency = 'VND'. "Nếu VND thì làm tròn

            ls_einvoice_item-amountintransaccrcy  = ls_bseg-amountintransactioncurrency * 100.
            ls_einvoice_item-vatamountintransaccrcy   = round( val = ls_bseg-amountintransactioncurrency * 100 * ls_einvoice_item-taxpercentage / 100 dec = 0 ).

          ELSE.
            IF ls_bseg-TransactionCurrency = 'USD' OR ls_bseg-TransactionCurrency = 'EUR' OR ls_bseg-TransactionCurrency = 'GBP'.

              ls_einvoice_item-amountintransaccrcy  = ls_bseg-amountintransactioncurrency.
              ls_einvoice_item-vatamountintransaccrcy   = ls_bseg-amountintransactioncurrency * ls_einvoice_item-taxpercentage / 100 .

            ELSE.
              ls_einvoice_item-amountintransaccrcy  = ls_bseg-amountintransactioncurrency * 100.
              ls_einvoice_item-vatamountintransaccrcy   = ls_bseg-amountintransactioncurrency * 100 * ls_einvoice_item-taxpercentage / 100 .
            ENDIF.
          ENDIF.
          "Total - Transaction Currency
          ls_einvoice_item-totalamountintransaccrcy   = ls_einvoice_item-amountintransaccrcy + ls_einvoice_item-vatamountintransaccrcy.

          "Amount Local - CompanyCode Currency
          ls_einvoice_item-amountincocodecrcy = ls_bseg-amountincompanycodecurrency * 100.
          "VAT Local - CompanyCode Currency
          IF ls_bseg-TransactionCurrency = 'VND'. "Nếu VND thì làm tròn
            ls_einvoice_item-vatamountincocodecrcy  = round( val = ls_bseg-amountincompanycodecurrency * 100 * ls_einvoice_item-taxpercentage / 100 dec = 0 ).
          ELSE.
            ls_einvoice_item-vatamountincocodecrcy  = ls_bseg-amountincompanycodecurrency * 100 * ls_einvoice_item-taxpercentage / 100 .
          ENDIF.

          "Total Local - CompanyCode Currency
          ls_einvoice_item-totalamountincocodecrcy   = ls_einvoice_item-amountincocodecrcy + ls_einvoice_item-vatamountincocodecrcy.

          IF ls_bseg-Quantity NE 0.
            "Price
            ls_einvoice_item-priceintransaccrcy = ls_einvoice_item-amountintransaccrcy / ls_bseg-Quantity.
            "Price Local
            ls_einvoice_item-priceincocodecrcy = ls_einvoice_item-amountincocodecrcy / ls_bseg-Quantity.
          ENDIF.

**--Header Amount
          "CompanyCode Amount
          ls_einvoice_header-amountincocodecrcy = ls_einvoice_header-amountincocodecrcy + ls_einvoice_item-amountincocodecrcy.
          ls_einvoice_header-vatamountincocodecrcy = ls_einvoice_header-vatamountincocodecrcy + ls_einvoice_item-vatamountincocodecrcy.
          ls_einvoice_header-totalamountincocodecrcy = ls_einvoice_header-totalamountincocodecrcy + ls_einvoice_item-totalamountincocodecrcy.

          "Transaction Amount
          ls_einvoice_header-amountintransaccrcy = ls_einvoice_header-amountintransaccrcy + ls_einvoice_item-amountintransaccrcy.
          ls_einvoice_header-vatamountintransaccrcy = ls_einvoice_header-vatamountintransaccrcy + ls_einvoice_item-vatamountintransaccrcy.
          ls_einvoice_header-totalamountintransaccrcy = ls_einvoice_header-totalamountintransaccrcy + ls_einvoice_item-totalamountintransaccrcy.

**--Payment Method
          IF lv_PaymentMethod IS INITIAL.
            SELECT SINGLE PaymentMethod FROM i_operationalacctgdocitem
            WHERE CompanyCode        = @ls_bkpf-CompanyCode
              AND AccountingDocument = @ls_bkpf-AccountingDocument
              AND FiscalYear         = @ls_bkpf-FiscalYear
              AND PaymentMethod NE ''
            INTO @lv_PaymentMethod
                .
          ENDIF.
**--Taxcode
          IF lv_taxcode IS INITIAL.
            lv_taxcode = ls_bseg-TaxCode.
          ELSE.
          ENDIF.

          IF lv_taxcode NE ls_bseg-TaxCode.
            ls_einvoice_header-taxcode = 'Nhiều loại'.
          ELSE.
            ls_einvoice_header-taxcode = lv_taxcode.
          ENDIF.

**--Profit Center
          IF ls_einvoice_header-profitcenter IS INITIAL.
            ls_einvoice_header-profitcenter = ls_bseg-ProfitCenter.
          ENDIF.
**--------------------------------------------------------------------------------------------**
          APPEND ls_einvoice_item TO lt_einvoice_item.
          CLEAR: ls_einvoice_item.

        ENDLOOP.
      ENDIF.

**--TestRun Flag
      IF lv_testrun IS NOT INITIAL.
        ls_einvoice_header-testrun = lv_testrun.
      ENDIF.

**--Process Data HDDT Header
      ls_einvoice_header-companycode        = ls_bkpf-CompanyCode.
      ls_einvoice_header-accountingdocument = ls_bkpf-AccountingDocument.
      ls_einvoice_header-fiscalyear         = ls_bkpf-FiscalYear.
      ls_einvoice_header-fiscalperiod       = ls_bkpf-FiscalPeriod.

      ls_einvoice_header-postingdate        = ls_bkpf-PostingDate.
      ls_einvoice_header-documentdate       = ls_bkpf-DocumentDate.

      ls_einvoice_header-accountingdocumenttype         = ls_bkpf-AccountingDocumentType.
      ls_einvoice_header-accountingdocumentcreationdate = ls_bkpf-AccountingDocumentCreationDate.
      ls_einvoice_header-accountingdocumentheadertext   = ls_bkpf-AccountingDocumentCreationDate.

      ls_einvoice_header-xreversed  = ls_bkpf-IsReversed.
      ls_einvoice_header-xreversing = ls_bkpf-IsReversal.

**--Payment Method Text
      SELECT SINGLE paymtext FROM zjp_hd_payment WHERE zlsch       = @lv_paymentmethod
                                                   AND companycode = @ls_einvoice_header-companycode
      INTO @ls_einvoice_header-paymentmethod.
      IF sy-subrc NE 0.
        "Message Error!
        ls_returns-type = 'E'.
        ls_returns-message = TEXT-004.
        APPEND ls_returns TO it_returns.
        CLEAR: ls_returns.
      ENDIF.

**--Exchange rate
      IF ls_bkpf-TransactionCurrency = 'VND'.
        ls_einvoice_header-absoluteexchangerate = 1.
      ELSE.
        IF ls_bkpf-TransactionCurrency = 'USD' OR ls_bkpf-TransactionCurrency = 'EUR' OR ls_bkpf-TransactionCurrency = 'GBP'.
          ls_einvoice_header-absoluteexchangerate = ls_bkpf-absoluteexchangerate * 1000.
        ELSE.
          ls_einvoice_header-absoluteexchangerate = ls_bkpf-absoluteexchangerate .
        ENDIF.
      ENDIF.
      "Currency
      ls_einvoice_header-companycodecurrency = ls_bkpf-CompanyCodeCurrency.
      ls_einvoice_header-transactioncurrency = ls_bkpf-TransactionCurrency.

**--SID Code
      ls_einvoice_header-sid = |{ sy-sysid }{ sy-mandt }{ ls_bkpf-CompanyCode }{ ls_bkpf-AccountingDocument }{ ls_bkpf-FiscalYear }|.

**--
      ls_einvoice_header-usertype       = lv_usertype.
      ls_einvoice_header-currencytype   = lv_currencytype.
      ls_einvoice_header-typeofdate     = lv_typeofdate.

**--Time CREATE
      go_einvoice_data->getdate_einvoice(
          EXPORTING
          i_document = ls_einvoice_header
          IMPORTING
          e_document = ls_einvoice_header
      ).

      lv_fiscalyear = ls_einvoice_header-einvoicedatecreate+0(4).

      IF lv_einvoicetype IS NOT INITIAL.
        SELECT SINGLE * FROM zjp_hd_serial
        WHERE companycode   IN @ir_companycode
          AND einvoicetype  EQ @lv_einvoicetype
          AND fiscalyear    EQ @lv_FiscalYear
          INTO @DATA(ls_hd_serial)
          .
        IF sy-subrc NE 0.
          "MESSAGE Error!
          ls_returns-type = 'E'.
          ls_returns-message = TEXT-002.
          APPEND ls_returns TO it_returns.
          CLEAR: ls_returns.
        ELSE.
          ls_einvoice_header-einvoiceform   = ls_hd_serial-einvoiceform.
          ls_einvoice_header-einvoiceserial = ls_hd_serial-einvoiceserial.
          ls_einvoice_header-einvoicetype   = ls_hd_serial-einvoicetype.
        ENDIF.
      ENDIF.

**--Get Customer Information
      ls_einvoice_header-customer = ls_bkpf-Customer.
      IF ls_bkpf-Customer IS NOT INITIAL.
        go_jp_common_core->get_customer_details(
            EXPORTING
            wa_document = wa_document
            IMPORTING
            wa_customer_details = wa_customer_details
        ).
        "--Customer Name
        ls_einvoice_header-customername          = wa_customer_details-customername.
        "--Customer Address
        ls_einvoice_header-customeraddress       = wa_customer_details-customeraddress.
        "--Customer Email
        ls_einvoice_header-emailaddress          = wa_customer_details-emailaddress.
        "--Customer ID
        ls_einvoice_header-identificationnumber  = wa_customer_details-identificationnumber.
        "--Customer Phone
        ls_einvoice_header-telephonenumber       = wa_customer_details-telephonenumber.
      ENDIF.

**-----------------------------------------------------------------------**
      CLEAR: ls_einvoice_header.
      CLEAR: ls_a_hddt_h.
      CLEAR: lv_PaymentMethod.
**-----------------------------------------------------------------------**
    ENDLOOP.

**--Xử lý số tiền thuế - làm tròn
    lt_sumvat_t = lt_sumvat.

    FREE: lt_sumvat.
    LOOP AT lt_sumvat_t INTO DATA(ls_SumVAT).
      IF ls_SumVAT-transactioncurrency = 'USD' OR ls_SumVAT-transactioncurrency = 'EUR'
      OR ls_SumVAT-transactioncurrency = 'GBP'.

        ls_SumVAT-sumvatamountintransaction = ls_SumVAT-sumvatamountintransaction * ( -1 ) .
      ELSE.
        ls_SumVAT-sumvatamountintransaction = ls_SumVAT-sumvatamountintransaction * ( -1 ) * 100.
      ENDIF.
      ls_SumVAT-SumVATAmountincompanyco = ls_SumVAT-SumVATAmountincompanyco * ( -1 ) * 100.
      COLLECT ls_SumVAT INTO lt_SumVAT.
      CLEAR: ls_SumVAT.
    ENDLOOP.

    SORT lt_SumVAT BY companycode accountingdocument fiscalyear taxcode ASCENDING.

    TYPES: BEGIN OF lty_item_hd,
             companycode              TYPE bukrs,
             accountingdocument       TYPE belnr_d,
             fiscalyear               TYPE gjahr,
             taxcode                  TYPE zde_taxcode,
             accountingdocumentitem   TYPE buzei,
             priceincocodecrcy        TYPE zde_dmbtr,
             companycodecurrency      TYPE waers,
             amountincocodecrcy       TYPE zde_dmbtr,
             vatamountincocodecrcy    TYPE zde_dmbtr,
             totalamountincocodecrcy  TYPE zde_dmbtr,
             transactioncurrency      TYPE waers,
             priceintransaccrcy       TYPE zde_dmbtr,
             amountintransaccrcy      TYPE zde_dmbtr,
             vatamountintransaccrcy   TYPE zde_dmbtr,
             totalamountintransaccrcy TYPE zde_dmbtr,
           END OF lty_item_hd.

    DATA: lt_item_hd TYPE TABLE OF lty_item_hd,
          ls_item_hd TYPE lty_item_hd.

    DATA: lv_cl_vatintransaction TYPE zde_dmbtr,
          lv_cl_vatincompanycode TYPE zde_dmbtr.

    SORT lt_item_hd BY companycode accountingdocument fiscalyear taxcode accountingdocumentitem ASCENDING.

    LOOP AT lt_item_hd "INTO DATA(ls_templine)
      INTO DATA(lt_group) GROUP BY ( companycode        = lt_group-companycode
                                     accountingdocument = lt_group-accountingdocument
                                     fiscalyear         = lt_group-fiscalyear
                                     taxcode            = lt_group-taxcode )
                                     .
      lv_index = sy-tabix.

*      MOVE-CORRESPONDING ls_templine TO ls_item_hd.

      LOOP AT GROUP lt_group INTO ls_item_hd.
        lv_cl_vatintransaction = lv_cl_vatintransaction + ls_item_hd-vatamountintransaccrcy.
        lv_cl_vatincompanycode = lv_cl_vatincompanycode + ls_item_hd-vatamountincocodecrcy.
      ENDLOOP.

*      AT END OF taxcode.
      READ TABLE lt_sumvat INTO ls_sumvat WITH KEY CompanyCode = ls_item_hd-CompanyCode
                                                   AccountingDocument = ls_item_hd-AccountingDocument
                                                   FiscalYear = ls_item_hd-FiscalYear
                                                   taxcode = ls_item_hd-taxcode BINARY SEARCH.
      IF sy-subrc EQ 0.

        READ TABLE lt_einvoice_header ASSIGNING FIELD-SYMBOL(<ls_einvoice_h>)
        WITH KEY CompanyCode = ls_item_hd-CompanyCode
        AccountingDocument   = ls_item_hd-AccountingDocument
        FiscalYear           = ls_item_hd-FiscalYear BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF ls_sumvat-sumvatamountintransaction NE 0.
            <ls_einvoice_h>-vatamountintransaccrcy = <ls_einvoice_h>-vatamountintransaccrcy
                                                   + ls_sumvat-sumvatamountintransaction - lv_cl_vatintransaction.

            <ls_einvoice_h>-totalamountintransaccrcy = <ls_einvoice_h>-amountintransaccrcy + <ls_einvoice_h>-vatamountintransaccrcy.
          ENDIF.
          IF ls_sumvat-sumvatamountincompanyco NE 0.

            <ls_einvoice_h>-vatamountincocodecrcy = <ls_einvoice_h>-vatamountincocodecrcy
                                                  + ls_sumvat-sumvatamountincompanyco - lv_cl_vatincompanycode.

            <ls_einvoice_h>-totalamountincocodecrcy = <ls_einvoice_h>-amountincocodecrcy + <ls_einvoice_h>-vatamountincocodecrcy.
          ENDIF.

        ENDIF.
      ENDIF.

      READ TABLE lt_einvoice_item ASSIGNING FIELD-SYMBOL(<ls_einvoice_i>) WITH KEY companycode = ls_item_hd-companycode
      accountingdocument      = ls_item_hd-accountingdocument
      fiscalyear              = ls_item_hd-fiscalyear
      accountingdocumentitem  = ls_item_hd-accountingdocumentitem BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF ls_sumvat-sumvatamountintransaction NE 0.
          <ls_einvoice_i>-vatamountintransaccrcy = <ls_einvoice_i>-vatamountintransaccrcy
                                                 + ls_sumvat-sumvatamountintransaction - lv_cl_vatintransaction.

          <ls_einvoice_i>-totalamountintransaccrcy = <ls_einvoice_i>-amountintransaccrcy + <ls_einvoice_i>-vatamountintransaccrcy.
        ENDIF.
        IF ls_sumvat-sumvatamountincompanyco NE 0.

          <ls_einvoice_i>-vatamountincocodecrcy = <ls_einvoice_i>-vatamountincocodecrcy
                                                + ls_sumvat-sumvatamountincompanyco - lv_cl_vatincompanycode.

          <ls_einvoice_i>-totalamountincocodecrcy = <ls_einvoice_i>-amountincocodecrcy + <ls_einvoice_i>-vatamountincocodecrcy.
        ENDIF.
      ENDIF.

      CLEAR: lv_cl_vatincompanycode, lv_cl_vatintransaction, ls_item_hd.
*      ENDAT.

    ENDLOOP.

*    IF ir_statussap[] IS NOT INITIAL.
*      DELETE lt_einvoice_header WHERE statussap NOT IN ir_statussap.
*    ENDIF.
*
*    IF ir_einvoicenumber[] IS NOT INITIAL.
*      DELETE lt_einvoice_header WHERE einvoicenumber NOT IN ir_einvoicenumber.
*    ENDIF.

  ENDMETHOD.

  METHOD get_instance.
    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                               THEN mo_instance
                                               ELSE NEW #( ) ).
  ENDMETHOD.

  METHOD read_ranges.
    READ TABLE it_ranges INTO DATA(wa_ranges) INDEX 1.
    IF sy-subrc EQ 0.
      o_value = wa_ranges-low.
    ELSE.
      CLEAR: o_value.
    ENDIF.
  ENDMETHOD.

  METHOD getdate_einvoice.
    MOVE-CORRESPONDING i_document TO e_document.

    CASE e_document-typeofdate.
      WHEN '01'. "Posting Date
        e_document-einvoicedatecreate = e_document-postingdate.
        e_document-einvoicetimecreate = '090000'.
      WHEN '02'. "Document Date
        e_document-einvoicedatecreate = e_document-documentdate.
        e_document-einvoicetimecreate = '090000'.
      WHEN '03'. "Entry Date
        e_document-einvoicedatecreate = e_document-accountingdocumentcreationdate.
        e_document-einvoicetimecreate = '090000'.
      WHEN '04'. "System Date
        DATA(time_zone) = cl_abap_context_info=>get_user_time_zone(  ).

        DATA(lv_datlo) = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value.
        DATA(lv_timlo) = xco_cp=>sy->time( )->as( xco_cp_time=>format->abap )->value.

        e_document-einvoicedatecreate = lv_datlo.
        e_document-einvoicetimecreate = lv_timlo.
    ENDCASE.

    o_date = e_document-einvoicedatecreate.
    o_time = e_document-einvoicetimecreate.

  ENDMETHOD.

ENDCLASS.

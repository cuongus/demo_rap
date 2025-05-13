CLASS zcl_jp_get_data_scttgnh DEFINITION
  PUBLIC
  INHERITING FROM cx_rap_query_provider
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Custom Entities
    INTERFACES if_rap_query_provider.

    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           tt_ranges    TYPE TABLE OF ty_range_option,

           tt_so_cttgnh TYPE TABLE OF zjp_c_scttgnh.

    "Variable
    CLASS-DATA: gt_data TYPE TABLE OF zjp_c_scttgnh.

    CLASS-DATA: gr_companycode        TYPE tt_ranges,
                gr_accountingdocument TYPE tt_ranges,
                gr_fiscalyear         TYPE tt_ranges,

                mo_instance           TYPE REF TO zcl_jp_get_data_scttgnh.

    CLASS-METHODS:
      "Contructor
      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_jp_get_data_scttgnh,

      "Method

      "Method Get Data From journal entry
      get_so_cttgnh IMPORTING ir_companycode        TYPE tt_ranges
                              ir_accountingdocument TYPE tt_ranges OPTIONAL
                              ir_fiscalyear         TYPE tt_ranges
                              ir_glaccount          TYPE tt_ranges
                              ir_postingdate        TYPE tt_ranges OPTIONAL
                              ir_documentdate       TYPE tt_ranges OPTIONAL
                    EXPORTING it_data               TYPE tt_so_cttgnh.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_jp_get_data_scttgnh IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

**--- Custom Entities ---**
    DATA: ls_page_info          TYPE zcl_jp_common_core=>st_page_info,

          ir_companycode        TYPE tt_ranges,
          ir_accountingdocument TYPE tt_ranges,
          ir_glaccount          TYPE tt_ranges,
          ir_fiscalyear         TYPE tt_ranges,
          ir_postingdate        TYPE tt_ranges,
          ir_documentdate       TYPE tt_ranges
          .

    DATA: lt_data TYPE tt_so_cttgnh.
    FREE: lt_data.

    TRY.
        DATA(lo_so_cttgnh)  = zcl_jp_get_data_scttgnh=>get_instance( ).

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
                wa_page_info          = ls_page_info
        ).

*        AUTHORITY-CHECK OBJECT 'ZOBJECT**'
*          ID 'ACTVT' FIELD '03'
*          ID 'LV_FIELD' FIELD lv_field.
*        IF sy-subrc NE 0.
*          RAISE EXCEPTION TYPE zcl_jp_get_data_scttgnh
*          MESSAGE ID ''
*          TYPE 'E'
*          NUMBER ''
*          WITH |Message Error!|.
*           RETURN.
*        ENDIF.

        lo_so_cttgnh->get_so_cttgnh(
            EXPORTING
                ir_companycode        = ir_companycode
                ir_accountingdocument = ir_accountingdocument
                ir_glaccount          = ir_glaccount
                ir_fiscalyear         = ir_fiscalyear
                ir_postingdate        = ir_postingdate
                ir_documentdate       = ir_documentdate
            IMPORTING
                it_data               = gt_data
        ).

        IF ls_page_info-page_size < 0.
          ls_page_info-page_size = 50.
        ENDIF.

        DATA(max_rows) = COND #( WHEN ls_page_info-page_size = if_rap_query_paging=>page_size_unlimited THEN 0
                   ELSE ls_page_info-page_size ).

        max_rows = ls_page_info-page_size + ls_page_info-offset.

        LOOP AT gt_data INTO DATA(ls_data).
          IF sy-tabix > ls_page_info-offset.
            IF sy-tabix > max_rows.
              EXIT.
            ELSE.
              APPEND ls_data TO lt_data.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lines( gt_data ) ).
        ENDIF.

        IF io_request->is_data_requested( ).
          io_response->set_data( lt_data ).
        ENDIF.

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

  METHOD get_instance.
    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                           THEN mo_instance
                                           ELSE NEW #( ) ).
  ENDMETHOD.

  METHOD get_so_cttgnh.
    DATA: lt_data TYPE tt_so_cttgnh.

    SELECT headers~CompanyCode,
           headers~AccountingDocument,
           headers~FiscalYear,
           items~AccountingDocumentItem,

           items~AccountingDocumentItemType,

           headers~AccountingDocumentType,
           headers~PostingDate,
           headers~DocumentDate,

           headers~DocumentReferenceID,
           headers~absoluteexchangerate AS ExchangeRate,
           headers~AccountingDocumentHeaderText,

           GLAccount AS CashAccounting,
           items~DebitCreditCode,

           headers~CompanyCodeCurrency,
           items~AbsoluteAmountInCoCodeCrcy,

           headers~TransactionCurrency,

           CASE
               WHEN headers~TransactionCurrency NE 'VND'
               THEN items~AbsoluteAmountInTransacCrcy
               ELSE 0
           END AS AbsoluteAmountInTransacCrcy,

           items~AssignmentReference,
           items~DocumentItemText,

           items~ProfitCenter,

           CASE
               WHEN items~customer NE ' '
               THEN items~customer
               ELSE items~supplier
           END AS BusinessPartner,
*

           items~IsNegativePosting AS NegativePosting,

           headers~AccountingDocCreatedByUser AS CreationUser,
           headers~CreationTime AS CreationTime,
           headers~AccountingDocumentCreationDate AS CreationDate,

               ControllingArea
         FROM i_operationalacctgdocitem AS items
         INNER JOIN i_journalentry AS headers ON items~CompanyCode        = headers~CompanyCode
                                             AND items~AccountingDocument = headers~AccountingDocument
                                             AND items~FiscalYear         = headers~FiscalYear

         WHERE headers~CompanyCode        IN @ir_companycode
           AND headers~AccountingDocument IN @ir_accountingdocument
           AND headers~FiscalYear         IN @ir_fiscalyear
           AND headers~PostingDate        IN @ir_postingdate
           AND headers~DocumentDate       IN @ir_documentdate

         INTO CORRESPONDING FIELDS OF TABLE @lt_data.

*        DATA: lt_text TYPE STANDARD TABLE OF tdline.
*        DATA: lv_objectid TYPE char10,
*              lv_tdname   TYPE char70,
*              lv_tdid     TYPE char4.

    "Customer info
    DATA: ls_document TYPE zst_document_info,
          ls_customer TYPE zst_customer_info.

    DATA(lo_common_core) = zcl_jp_common_core=>get_instance( ).

    DATA: lt_sodudk TYPE zcl_jp_common_core=>tt_sodu.

    lo_common_core->get_glaccount_sodu(
        EXPORTING
        ir_companycode = ir_companycode
        ir_glaccount = ir_glaccount
        ir_date = ir_postingdate
        IMPORTING
        it_sodu_dk = lt_sodudk
    ).

    SORT lt_sodudk BY companycode glaccount ASCENDING.

    SORT lt_data BY CompanyCode AccountingDocument AccountingDocumentItem PostingDate FiscalYear ASCENDING.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

*          cf_reca_text=>find( id_tdobject  = lv_objectid
*                              id_tdname    = lv_tdname
*                              id_tdid      = lv_tdid )->get_text_as_stream( IMPORTING et_text = lt_text ).

      ls_document-customer           = <fs_data>-BusinessPartnerName.
      ls_document-companycode        = <fs_data>-CompanyCode.
      ls_document-accountingdocument = <fs_data>-AccountingDocument.
      ls_document-fiscalyear         = <fs_data>-FiscalYear.

      lo_common_core->get_customer_details(
          EXPORTING
          wa_document = ls_document
          IMPORTING
          wa_customer_details = ls_customer
      ).

      <fs_data>-BusinessPartnerName    = ls_customer-customername.
      <fs_data>-BusinessPartnerAddress = ls_customer-customeraddress.

      IF <fs_data>-NegativePosting = ''.
        IF <fs_data>-DebitCreditCode = 'H'.
          <fs_data>-PhatSinhCo = <fs_data>-AbsoluteAmountInCoCodeCrcy.

          <fs_data>-PhatSinhCo_NT = <fs_data>-AbsoluteAmountInTransacCrcy.
        ELSE.
          <fs_data>-PhatSinhNo = <fs_data>-AbsoluteAmountInCoCodeCrcy.

          <fs_data>-PhatSinhNo_NT = <fs_data>-AbsoluteAmountInTransacCrcy.
        ENDIF.
      ELSE.
        IF <fs_data>-DebitCreditCode = 'S'.
          <fs_data>-PhatSinhCo = <fs_data>-AbsoluteAmountInCoCodeCrcy * ( -1 ).

          <fs_data>-PhatSinhCo_NT = <fs_data>-AbsoluteAmountInTransacCrcy * ( -1 ).
        ELSE.
          <fs_data>-PhatSinhNo = <fs_data>-AbsoluteAmountInCoCodeCrcy * ( -1 ).

          <fs_data>-PhatSinhNo_NT = <fs_data>-AbsoluteAmountInTransacCrcy * ( -1 ).
        ENDIF.
      ENDIF.

      READ TABLE lt_sodudk ASSIGNING FIELD-SYMBOL(<fs_sodudk>) WITH KEY glaccount = <fs_data>-CashAccounting BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_data>-Sodu = <fs_sodudk>-sodudk + <fs_data>-PhatSinhNo - <fs_data>-PhatSinhCo.

        <fs_data>-Sodu_NT = <fs_sodudk>-sodudk_nt + <fs_data>-PhatSinhNo_NT - <fs_data>-PhatSinhCo_NT.

        <fs_sodudk>-sodudk =  <fs_data>-Sodu.
        <fs_sodudk>-sodudk_nt =  <fs_data>-Sodu_nt.
      ENDIF.

      CLEAR: ls_customer, ls_document.
      UNASSIGN: <fs_data>, <fs_sodudk>.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

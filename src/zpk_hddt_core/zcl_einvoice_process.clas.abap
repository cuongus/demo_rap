CLASS zcl_einvoice_process DEFINITION
  PUBLIC
  INHERITING FROM zcl_einvoice_data
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:

      BEGIN OF ty_message,
        companycode        TYPE bukrs,
        accountingdocument TYPE belnr_d,
        fiscalyear         TYPE gjahr,
        msgtype            TYPE sy-msgty,
        msgtext            TYPE zde_txt255,
      END OF ty_message,

      BEGIN OF ty_param,
        AccountingDocumentSource TYPE belnr_d,
        FiscalYearSource         TYPE gjahr,
        AdjustType               TYPE zde_adjusttype,
      END OF ty_param,

      tt_message        TYPE TABLE OF ty_message,

**--Database
      wa_items          TYPE zjp_a_hddt_i,
      wa_hd_userpass    TYPE zjp_hd_userpass,
      wa_hd_serial      TYPE zjp_hd_serial,

      wa_param          TYPE ty_param,

      tt_headers        TYPE TABLE OF zjp_a_hddt_h,
      tt_items          TYPE TABLE OF zjp_a_hddt_i,

**--Behavior Variables

      "Action delete
      tt_header_delete  TYPE TABLE FOR DELETE zjp_c_hddt_h\\hddt_headers,

      "Action Read
      tt_header_read    TYPE TABLE FOR READ IMPORT zjp_c_hddt_h\\hddt_headers,
      tt_result_readh   TYPE TABLE FOR READ RESULT zjp_c_hddt_h\\hddt_headers,

      tt_items_read     TYPE TABLE FOR READ IMPORT zjp_c_hddt_h\\hddt_items,
      tt_result_readi   TYPE TABLE FOR READ RESULT zjp_c_hddt_h\\hddt_items,

      "Action Integration
      tt_header_integr  TYPE TABLE FOR ACTION IMPORT zjp_c_hddt_h\\hddt_headers~integr,
      tt_result_integr  TYPE TABLE FOR ACTION RESULT zjp_c_hddt_h\\hddt_headers~integr,

      "Action Cancel
      tt_cancel         TYPE TABLE FOR ACTION IMPORT zjp_c_hddt_h\\hddt_headers~cancel,
      tt_cancel_result  TYPE TABLE FOR ACTION RESULT zjp_c_hddt_h\\hddt_headers~cancel,

      "Action Search
      tt_search         TYPE TABLE FOR ACTION IMPORT zjp_c_hddt_h\\hddt_headers~Search,
      tt_search_result  TYPE TABLE FOR ACTION RESULT zjp_c_hddt_h\\hddt_headers~Search,

      "Action Adjust
      tt_adjust         TYPE TABLE FOR ACTION IMPORT zjp_c_hddt_h\\hddt_headers~Adjust,
      tt_result_adjust  TYPE TABLE FOR ACTION RESULT zjp_c_hddt_h\\hddt_headers~Adjust,

      "Action Replace
      tt_replace        TYPE TABLE FOR ACTION IMPORT zjp_c_hddt_h\\hddt_headers~Replace,
      tt_result_replace TYPE TABLE FOR ACTION RESULT zjp_c_hddt_h\\hddt_headers~Replace,

      "Update
      tt_update_entry   TYPE TABLE FOR UPDATE zjp_c_hddt_h\\hddt_headers,

      "Cba entities
      tt_entities_cba   TYPE TABLE FOR CREATE zjp_c_hddt_h\\hddt_headers\_einvoiceitems,

      "Common
      tt_mapped_early   TYPE RESPONSE FOR MAPPED EARLY zjp_c_hddt_h,
      tt_failed_early   TYPE RESPONSE FOR FAILED EARLY zjp_c_hddt_h,
      tt_reported_early TYPE RESPONSE FOR REPORTED EARLY zjp_c_hddt_h,
      tt_reported_late  TYPE RESPONSE FOR REPORTED LATE zjp_c_hddt_h
      .

    CLASS-DATA:
      go_einvoice_process   TYPE REF TO zcl_einvoice_process,

      gv_action             TYPE zde_action_invoice,

      gs_document           TYPE wa_document,
      gs_userpass           TYPE wa_hd_userpass,
      gs_formserial         TYPE wa_hd_serial,
      gs_status             TYPE wa_document,
      gs_docsrc             TYPE wa_document,

      gs_json               TYPE string,
      gs_return             TYPE bapiret2,
      gv_testrun            TYPE abap_boolean,

      ir_companycode        TYPE tt_ranges,
      ir_accountingdocument TYPE tt_ranges,
      ir_fiscalyear         TYPE tt_ranges,

      gt_headers_save       TYPE TABLE OF wa_document,
      gt_docsrc_save        TYPE TABLE OF wa_document.

    CLASS-DATA:
              go_fpt_einvoice TYPE REF TO zcl_manage_fpt_einvoices.

    METHODS constructor .

    CLASS-METHODS:

*      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_einvoice_process,

      clear_variables ,

      get_document      IMPORTING keys      TYPE ANY TABLE
                        EXPORTING e_headers TYPE tt_headers
                                  e_items   TYPE tt_items,

      get_password IMPORTING i_document   TYPE wa_document
                   EXPORTING e_document   TYPE wa_document
                             e_userpass   TYPE wa_hd_userpass
                             e_formserial TYPE wa_hd_serial
                             e_return     TYPE bapiret2
                   RAISING
                             cx_abap_context_info_error,

      move_log          IMPORTING i_input  TYPE wa_document
                        EXPORTING o_output TYPE wa_document,

      integration_einvoice IMPORTING i_document            TYPE wa_document OPTIONAL
                                     i_action              TYPE zde_action_invoice
                                     i_param               TYPE wa_param OPTIONAL
                                     ir_companycode        TYPE tt_ranges OPTIONAL
                                     ir_accountingdocument TYPE tt_ranges OPTIONAL
                                     ir_fiscalyear         TYPE tt_ranges OPTIONAL
                           EXPORTING e_headers             TYPE tt_headers
                                     e_items               TYPE tt_items
                                     e_docsrc              TYPE tt_headers
                                     e_return              TYPE tt_message
                           RAISING
                                     cx_abap_context_info_error,

      check_adjust_document IMPORTING i_document TYPE wa_document
                            EXPORTING e_return   TYPE bapiret2,

*      "Class for Behavior
      delete_header IMPORTING keys     TYPE tt_header_delete"table for delete zcs_rap_einv_entry\\hddt_headers
                    CHANGING  mapped   TYPE tt_mapped_early "response for mapped early zcs_rap_einv_entry
                              failed   TYPE tt_failed_early "response for failed early zcs_rap_einv_entry
                              reported TYPE tt_reported_early "response for reported early zcs_rap_einv_entry,
                              e_return TYPE tt_message, "Return message error
*
      read_header IMPORTING keys     TYPE tt_header_read "table for read import zcs_rap_einv_entry\\hddt_headers
                  CHANGING  result   TYPE tt_result_readh "table for read result zcs_rap_einv_entry\\hddt_headers
                            failed   TYPE tt_failed_early "response for failed early zi_rap_einv_header
                            reported TYPE tt_reported_early, "response for reported early zi_rap_einv_header

      read_items IMPORTING keys     TYPE tt_items_read "table for read import zcs_rap_einv_entry\\hddt_items
                 CHANGING  result   TYPE tt_result_readi "table for read result zcs_rap_einv_entry\\hddt_items
                           failed   TYPE tt_failed_early "response for failed early zcs_rap_einv_entry
                           reported TYPE tt_reported_early, "response for reported early zcs_rap_einv_entry

      update_entry IMPORTING entities TYPE tt_update_entry "table for update zcs_rap_einv_entry\\hddt_headers
                   CHANGING  mapped   TYPE tt_mapped_early "response for mapped early zcs_rap_einv_entry
                             failed   TYPE tt_failed_early "response for failed early zcs_rap_einv_entry
                             reported TYPE tt_reported_early, "response for reported early zcs_rap_einv_entry

      cba_einvoiceitems IMPORTING entities_cba TYPE tt_entities_cba "table for create zcs_rap_einv_entry\\hddt_headers\_einvoiceitems
                        CHANGING  mapped       TYPE tt_mapped_early "response for mapped early zcs_rap_einv_entry
                                  failed       TYPE tt_failed_early "response for failed early zcs_rap_einv_entry
                                  reported     TYPE tt_reported_early, "response for reported early zcs_rap_einv_entry

      "Action Integration EInvoices
      integr_einvoice
        IMPORTING keys     TYPE tt_header_integr "table for action import zcs_rap_einv_entry\\hddt_headers~inteeinv
        CHANGING  result   TYPE tt_result_integr "table for action result zcs_rap_einv_entry\\hddt_headers~inteeinv
                  mapped   TYPE tt_mapped_early "response for mapped early zcs_rap_einv_entry
                  failed   TYPE tt_failed_early "response for failed early zcs_rap_einv_entry
                  reported TYPE tt_reported_early "response for reported early zcs_rap_einv_entry
                  e_return TYPE tt_message
        RAISING
                  cx_abap_context_info_error, "Return message error

      "Action Cancel EInvoices
      cancel_einvoice IMPORTING keys     TYPE tt_cancel "table for action import zcs_rap_einv_entry\\hddt_headers~canceleinv
                      CHANGING  result   TYPE tt_cancel_result "table for action result zcs_rap_einv_entry\\hddt_headers~canceleinv
                                mapped   TYPE tt_mapped_early "response for mapped early zcs_rap_einv_entry
                                failed   TYPE tt_failed_early "response for failed early zcs_rap_einv_entry
                                reported TYPE tt_reported_early "response for reported early zcs_rap_einv_entry
                                e_return TYPE tt_message
                      RAISING
                                cx_abap_context_info_error, "Return message error

      "Action Search EInvoices
      Search_einvoice IMPORTING keys     TYPE tt_search "table for action import zcs_rap_einv_entry\\hddt_headers~updatesteinv
                      CHANGING  result   TYPE tt_search_result "table for action result zcs_rap_einv_entry\\hddt_headers~updatesteinv
                                mapped   TYPE tt_mapped_early "response for mapped early zcs_rap_einv_entry
                                failed   TYPE tt_failed_early "response for failed early zcs_rap_einv_entry
                                reported TYPE tt_reported_early "response for reported early zcs_rap_einv_entry
                                e_return TYPE tt_message, "Return message error

      "Action Adjust EInvoices
      adjust_einvoice IMPORTING keys     TYPE tt_adjust "table for action import zcs_rap_einv_entry\\einvoicesheader~adjusteinv
                      CHANGING  result   TYPE tt_result_adjust "table for action result zcs_rap_einv_entry\\einvoicesheader~adjusteinv
                                mapped   TYPE tt_mapped_early "response for mapped early zcs_rap_einv_entry
                                failed   TYPE tt_failed_early "response for failed early zcs_rap_einv_entry
                                reported TYPE tt_reported_early "response for reported early zcs_rap_einv_entry
                                e_return TYPE tt_message
                      RAISING
                                cx_abap_context_info_error, "Return message error

      "Action Replace EInvoices
      replace_einvoice  IMPORTING keys     TYPE tt_replace "table for action import zcs_rap_einv_entry\\hddt_headers~replaceeinv
                        CHANGING  result   TYPE tt_result_replace "table for action result zcs_rap_einv_entry\\hddt_headers~replaceeinv
                                  mapped   TYPE tt_mapped_early "response for mapped early zcs_rap_einv_entry
                                  failed   TYPE tt_failed_early "response for failed early zcs_rap_einv_entry
                                  reported TYPE tt_reported_early "response for reported early zcs_rap_einv_entry
                                  e_return TYPE tt_message
                        RAISING
                                  cx_abap_context_info_error, "Return message error

      "Common Methods
      save_einvoice
        CHANGING reported TYPE tt_reported_late "response for reported late zcs_rap_einv_entry
        ,

      cleanup,
      cleanup_finalize.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EINVOICE_PROCESS IMPLEMENTATION.


  METHOD adjust_einvoice.
    TYPES: BEGIN OF lty_adjust,
             companycode        TYPE bukrs,
             accountingdocument TYPE belnr_d,
             fiscalyear         TYPE gjahr,
           END OF lty_adjust.
    DATA: ls_adjust TYPE lty_adjust.

    DATA: ls_result         LIKE LINE OF result,
          ls_mapped_headers LIKE LINE OF mapped-hddt_headers.

    DATA: ls_param TYPE zpr_adjust_einvoice.

    DATA: it_headers TYPE TABLE OF wa_document,
          it_items   TYPE TABLE OF wa_items.

    LOOP AT keys INTO DATA(ls_keys).
      ls_adjust-accountingdocument = ls_param-AccountingDocumentSource = ls_keys-%param-AccountingDocumentSource.
      ls_adjust-fiscalyear = ls_param-FiscalYearSource = ls_keys-%param-FiscalYearSource.
      ls_param-adjusttype = ls_keys-%param-adjusttype.
    ENDLOOP.

    FREE: ir_companycode, ir_accountingdocument, ir_fiscalyear.

    go_einvoice_process->clear_variables( ).

    go_einvoice_process->integration_einvoice(
        EXPORTING
        i_action                = 'ADJUST_INVOICE'
        i_param                 = ls_param
        ir_companycode          = ir_companycode
        ir_accountingdocument   = ir_accountingdocument
        ir_fiscalyear           = ir_fiscalyear
        IMPORTING
        e_headers               = gt_headers_save
        e_items                 = it_items
        e_docsrc                = gt_docsrc_save
        e_return                = e_return
    ).

    LOOP AT gt_headers_save ASSIGNING FIELD-SYMBOL(<fs_headers>).

      ls_result-%tky-Companycode        = <fs_headers>-Companycode.
      ls_result-%tky-Accountingdocument = <fs_headers>-Accountingdocument.
      ls_result-%tky-Fiscalyear         = <fs_headers>-Fiscalyear.

      ls_result-%key-Companycode        = <fs_headers>-Companycode.
      ls_result-%key-Accountingdocument = <fs_headers>-Accountingdocument.
      ls_result-%key-Fiscalyear         = <fs_headers>-Fiscalyear.

      ls_result-Companycode             = <fs_headers>-Companycode.
      ls_result-Accountingdocument      = <fs_headers>-Accountingdocument.
      ls_result-Fiscalyear              = <fs_headers>-Fiscalyear.

      ls_mapped_headers-%tky                 = ls_result-%tky.
      ls_mapped_headers-Companycode          = <fs_headers>-Companycode.
      ls_mapped_headers-Accountingdocument   = <fs_headers>-Accountingdocument.
      ls_mapped_headers-Fiscalyear           = <fs_headers>-Fiscalyear.

      MOVE-CORRESPONDING <fs_headers> TO ls_result-%param.
      IF gs_docsrc IS NOT INITIAL.
        APPEND gs_docsrc TO gt_docsrc_save.
      ENDIF.

      INSERT CORRESPONDING #( ls_result ) INTO TABLE result.
      INSERT CORRESPONDING #( ls_mapped_headers ) INTO TABLE mapped-hddt_headers.

    ENDLOOP.

  ENDMETHOD.


  METHOD cancel_einvoice.
    DATA: ls_param  TYPE zpr_cancel_einvoice,
          ls_result LIKE LINE OF result.

    LOOP AT keys INTO DATA(ls_keys).
      ls_param-noti_taxtype = ls_keys-%param-noti_taxtype.
      ls_param-noti_taxnum  = ls_keys-%param-noti_taxnum.
      ls_param-place        = ls_keys-%param-place.
      ls_param-noti_type    = ls_keys-%param-noti_type.
    ENDLOOP.

    go_einvoice_process->get_einvoice_data(
        EXPORTING
        ir_companycode = ir_companycode
        ir_accountingdocument = ir_accountingdocument
        ir_fiscalyear = ir_fiscalyear
        IMPORTING
        it_einvoice_header = DATA(it_headers)
    ).

    gv_action = 'CANCEL_INVOICE'.

    LOOP AT it_headers ASSIGNING FIELD-SYMBOL(<fs_headers>).

      go_einvoice_process->clear_variables( ).

      IF <fs_headers>-StatusSap = '98' OR <fs_headers>-StatusSap = '99'.

        MOVE-CORRESPONDING <fs_headers> TO gs_document.

        SELECT SINGLE isreversed FROM i_journalentry WHERE companycode        = @<fs_headers>-companycode
                                                       AND accountingdocument = @<fs_headers>-accountingdocument
                                                       AND fiscalyear         = @<fs_headers>-fiscalyear
        INTO @DATA(lv_xreversed).
        IF sy-subrc NE 0.
          CLEAR: lv_xreversed.
        ENDIF.

        IF lv_Xreversed IS INITIAL.
          "Message Error
          APPEND VALUE #( companycode        = <fs_headers>-companycode
                          accountingdocument = <fs_headers>-accountingdocument
                          fiscalyear         = <fs_headers>-fiscalyear
                          msgtype            = 'E'
                          msgtext            = TEXT-011 ) TO e_return.
        ELSE.

          go_einvoice_process->get_password(
              EXPORTING
              i_document = gs_document
              IMPORTING
              e_userpass = gs_userpass
          ).

          go_fpt_einvoice->cancel_einvoices(
            EXPORTING
            i_action    = gv_action
            i_einvoice  = gs_document
            i_param     = ls_param
            i_userpass  = gs_userpass
            IMPORTING
            e_status    = gs_status
            e_return    = gs_return
            e_json      = gs_json
          ).

        ENDIF.
      ELSE. "End check status
        "Message Error
        APPEND VALUE #( companycode = <fs_headers>-companycode
                        accountingdocument = <fs_headers>-accountingdocument
                        fiscalyear = <fs_headers>-fiscalyear
                        msgtype = 'E'
                        msgtext = TEXT-010 ) TO e_return.
      ENDIF.

      IF gs_return-type EQ 'E'.
        <fs_headers>-Iconsap = gs_status-Iconsap.
        <fs_headers>-Statussap = '03'.
        <fs_headers>-messagetext = gs_status-messagetext.
      ELSE.
        go_einvoice_process->move_log(
           EXPORTING
           i_input = gs_status
           IMPORTING
           o_output = <fs_headers>
        ).
      ENDIF.

      ls_result-%tky-Companycode        = <fs_headers>-Companycode.
      ls_result-%tky-Accountingdocument = <fs_headers>-Accountingdocument.
      ls_result-%tky-Fiscalyear         = <fs_headers>-Fiscalyear.

      ls_result-%key-Companycode        = <fs_headers>-Companycode.
      ls_result-%key-Accountingdocument = <fs_headers>-Accountingdocument.
      ls_result-%key-Fiscalyear         = <fs_headers>-Fiscalyear.

      ls_result-Companycode             = <fs_headers>-Companycode.
      ls_result-Accountingdocument      = <fs_headers>-Accountingdocument.
      ls_result-Fiscalyear              = <fs_headers>-Fiscalyear.


      MOVE-CORRESPONDING <fs_headers> TO ls_result-%param.
      INSERT CORRESPONDING #( ls_result ) INTO TABLE result.

    ENDLOOP.

    MOVE-CORRESPONDING it_headers TO gt_headers_save.
  ENDMETHOD.


  METHOD cba_einvoiceitems.

  ENDMETHOD.


  METHOD check_adjust_document.
    DATA: lv_count TYPE int4.

    IF i_document-accountingdocumentsource IS NOT INITIAL AND i_document-fiscalyearsource IS NOT INITIAL AND i_document-adjusttype IS NOT INITIAL.

**--Check Không điều chỉnh/thay thế chứng từ khác Customer
      SELECT SINGLE * FROM zjp_a_hddt_h
      WHERE Companycode      = @i_document-Companycode
      AND Accountingdocument = @i_document-accountingdocumentsource
      AND Fiscalyear         = @i_document-fiscalyearsource
      INTO @DATA(ls_check).
      IF sy-subrc EQ 0.
        IF ls_check-customer NE i_document-customer.
          e_return-type     = 'E'.
          e_return-message  = TEXT-008.
        ENDIF.

      ENDIF.

**--Check Trạng thái chứng từ &1 - &2 không hợp lệ
      lv_count = 0.
      SELECT COUNT( 1 )
      FROM zjp_a_hddt_h
      WHERE Companycode      = @i_document-Companycode
      AND Accountingdocument = @i_document-AccountingDocumentSource
      AND Fiscalyear         = @i_document-FiscalYearSource
      AND statussap IN ('98','99','06')
      INTO @lv_count.
      IF lv_count = 0.
        e_return-type    = 'E'.
        e_return-message = TEXT-003.
        e_return-message = replace( val =  TEXT-003
                                    sub = '&1'
                                    with = i_document-AccountingDocumentSource
                                   ).
        e_return-message = replace( val =  TEXT-003
                                    sub = '&2'
                                    with = i_document-fiscalyearsource
                                   ).
      ELSE.

        lv_count = 0.
**--Trường hợp thay thế chứng từ --> Check Hủy chứng từ &1 - &2 trước khi thay thế
        IF i_document-adjusttype = '3'.
          SELECT COUNT( 1 )
          FROM I_JournalEntry
          WHERE companycode         = @i_document-Companycode
            AND AccountingDocument  = @i_document-AccountingDocumentSource
            AND Fiscalyear          = @i_document-FiscalYearSource
            AND reversedocument NE ''
            INTO @lv_count.
          IF lv_count = 0.
            e_return-type = 'E'.
            e_return-message = replace( val =  TEXT-007
                                    sub = '&1'
                                    with = i_document-AccountingDocumentSource
                                   ).
            e_return-message = replace( val =  TEXT-007
                                        sub = '&2'
                                        with = i_document-fiscalyearsource
                                       ).
          ENDIF.
        ENDIF.

**--Check Không điều chỉnh/thay thế chứng từ khác loại tiền tệ
        lv_count = 0.
        SELECT COUNT( 1 )
        FROM zjp_a_hddt_h
        WHERE Companycode       = @i_document-Companycode
        AND Accountingdocument  = @i_document-AccountingDocumentSource
        AND Fiscalyear          = @i_document-FiscalYearSource
        AND transactioncurrency = @i_document-transactioncurrency
        INTO @lv_count.
        IF lv_count = 0.
          e_return-type = 'E'.
          e_return-message = TEXT-004.
        ELSE.
        ENDIF.

**--Check Không điều chỉnh/thay thế chứng từ đã bị thay thế
        lv_count = 0.
        SELECT COUNT( 1 )
        FROM zjp_a_hddt_h
        WHERE Companycode       = @i_document-Companycode
        AND Accountingdocument  = @i_document-AccountingDocumentSource
        AND Fiscalyear          = @i_document-FiscalYearSource
        AND statussap IN ('07')
        AND FiscalYearSource NE ''
        INTO @lv_count.
        IF lv_count NE 0.
          e_return-type     = 'E'.
          e_return-message  = TEXT-005.
        ENDIF.

      ENDIF.

    ELSEIF i_document-accountingdocumentsource IS INITIAL AND i_document-FiscalYearSource IS INITIAL AND i_document-adjusttype IS INITIAL.

    ELSE.
      e_return-type = 'E'.
      e_return-message = TEXT-006.
    ENDIF.
  ENDMETHOD.


  METHOD cleanup.

  ENDMETHOD.


  METHOD cleanup_finalize.

  ENDMETHOD.


  METHOD clear_variables.
    CLEAR: gv_action, gs_document, gs_userpass, gs_formserial, gs_status, gs_json, gs_docsrc,
    gs_return, gv_testrun.

  ENDMETHOD.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
*      if_t100_message~t100key = textid.
    ENDIF.

    CREATE OBJECT go_einvoice_process.
  ENDMETHOD.


  METHOD delete_header.

  ENDMETHOD.


  METHOD get_document.

  ENDMETHOD.


  METHOD get_password.

    DATA: lv_fiscalyear TYPE gjahr.

    SELECT SINGLE * FROM zjp_hd_userpass WHERE Companycode = @i_document-companycode
                                           AND usertype    = @i_document-usertype
    INTO CORRESPONDING FIELDS OF @e_userpass.
    IF sy-subrc NE 0.
      e_return-type = 'E'.
      e_return-message = TEXT-001.
    ENDIF.

    go_einvoice_process->getdate_einvoice(
        EXPORTING
        i_document = i_document
        IMPORTING
        e_document = e_document
    ).

    lv_fiscalyear = e_document-einvoicedatecreate+0(4).

    SELECT SINGLE * FROM zjp_hd_serial WHERE Companycode  = @i_document-companycode
                                         AND einvoicetype = @i_document-einvoicetype
                                         AND Fiscalyear   = @lv_fiscalyear
    INTO CORRESPONDING FIELDS OF @e_formserial.
    IF sy-subrc NE 0.
      e_return-type = 'E'.
      e_return-message = TEXT-002 && ` ` && lv_fiscalyear.
    ENDIF.

  ENDMETHOD.


  METHOD integration_einvoice.

    DATA: it_headers TYPE tt_headers,
          it_items   TYPE tt_items.

    go_einvoice_process->get_einvoice_data(
    EXPORTING
        ir_companycode        = ir_companycode
        ir_accountingdocument = ir_accountingdocument
        ir_fiscalyear         = ir_fiscalyear
    IMPORTING
        it_einvoice_header    = it_headers
        it_einvoice_item      = it_items
    ).

    LOOP AT it_headers ASSIGNING FIELD-SYMBOL(<fs_headers>).

      go_einvoice_process->clear_variables( ).

      gv_action = i_action.

      IF <fs_headers>-StatusSap = '01' OR <fs_headers>-StatusSap = '03'.
        IF <fs_headers>-einvoicenumber IS INITIAL.

          IF <fs_headers>-Xreversed IS INITIAL AND <fs_headers>-Xreversing IS INITIAL.

            MOVE-CORRESPONDING <fs_headers> TO gs_document.

            go_einvoice_process->get_password(
                EXPORTING
                i_document = gs_document
                IMPORTING
                e_userpass = gs_userpass
            ).

            IF gv_action EQ 'ADJUST_INVOICE' OR gv_action EQ 'REPLACE_INVOICE'.

              <fs_headers>-AccountingDocumentSource = i_param-AccountingDocumentSource.
              <fs_headers>-FiscalYearSource         = i_param-FiscalYearSource.
              IF gv_action EQ 'ADJUST_INVOICE'.
                <fs_headers>-AdjustType               = i_param-AdjustType.
              ELSE.
                <fs_headers>-AdjustType               = '3'. "Replace HĐ
              ENDIF.


              go_einvoice_process->check_adjust_document(
                 EXPORTING
                 i_document   = <fs_headers>
                 IMPORTING
                 e_return     = gs_return
              ).

              IF gs_return-type EQ 'E'.

                <fs_headers>-AccountingDocumentSource = ''.
                <fs_headers>-FiscalYearSource         = ''.
                <fs_headers>-AdjustType               = ''.

                APPEND VALUE #( companycode        = <fs_headers>-companycode
                                accountingdocument = <fs_headers>-accountingdocument
                                fiscalyear         = <fs_headers>-fiscalyear
                                msgtype            = gs_return-type
                                msgtext            = gs_return-message ) TO e_return.
              ELSE.
                IF gs_return-type EQ 'W'.
                  APPEND VALUE #( companycode      = <fs_headers>-companycode
                                accountingdocument = <fs_headers>-accountingdocument
                                fiscalyear         = <fs_headers>-fiscalyear
                                msgtype            = gs_return-type
                                msgtext            = gs_return-message ) TO e_return.
                ENDIF.

                IF <fs_headers>-AccountingDocumentSource IS NOT INITIAL.

                  go_einvoice_process->get_password(
                      EXPORTING
                      i_document = <fs_headers>
                      IMPORTING
                      e_userpass = gs_userpass
                      e_return   = gs_return
                  ).

                  MOVE-CORRESPONDING <fs_headers> TO gs_document.

**--Adjust/Replace EInvoice
                  go_fpt_einvoice->adjust_einvoices(
                      EXPORTING
                      i_action    = gv_action
                      i_einvoice  = gs_document
                      i_items     = it_items
                      i_userpass  = gs_userpass
                      IMPORTING
                      e_status    = gs_status
                      e_docsrc    = gs_docsrc
                      e_json      = gs_json
                      e_return    = gs_return
                  ).

                  go_einvoice_process->move_log(
                    EXPORTING
                    i_input = gs_status
                    IMPORTING
                    o_output = <fs_headers>
                ).

                  IF gs_return-type           = 'E'.
                    <fs_headers>-Iconsap      = '@0A@'.
                    <fs_headers>-Statussap    = '03'.
                    <fs_headers>-messagetype  = gs_return-type.
                    <fs_headers>-messagetext  = gs_return-message.
                  ENDIF.
                ELSE. "End Check Accounting Document Source

                  <fs_headers>-AccountingDocumentSource = ''.
                  <fs_headers>-FiscalYearSource         = ''.
                  <fs_headers>-AdjustType               = ''.
                ENDIF.
              ENDIF.

            ELSE.
**--Create New EInvoice
              go_fpt_einvoice->create_einvoices(
                  EXPORTING
                  i_action    = gv_action
                  i_einvoice  = gs_document
                  i_items     = it_items
                  i_userpass  = gs_userpass
                  IMPORTING
                  e_status    = gs_status
                  e_docsrc    = gs_docsrc
                  e_json      = gs_json
                  e_return    = gs_return
              ).

            ENDIF.

            go_einvoice_process->move_log(
                EXPORTING
                i_input = gs_status
                IMPORTING
                o_output = <fs_headers>
            ).

            IF gs_return-type = 'E'.
              <fs_headers>-Iconsap     = '@0A@'.
              <fs_headers>-Statussap   = '03'.
              <fs_headers>-messagetype = gs_return-type.
              <fs_headers>-messagetext = gs_return-message.

            ENDIF.

          ELSE.  "End check reverse
            <fs_headers>-Statussap     = '03'.
            <fs_headers>-messagetype   = 'E'.
            <fs_headers>-messagetext   = 'Document Is Reversal/Reversed'.
          ENDIF.

        ELSE. "End check EInvoice Number
          <fs_headers>-AccountingDocumentSource = ''.
          <fs_headers>-FiscalYearSource         = ''.
          <fs_headers>-AdjustType               = ''.

          APPEND VALUE #( companycode           = <fs_headers>-companycode
                          accountingdocument    = <fs_headers>-accountingdocument
                          fiscalyear            = <fs_headers>-fiscalyear
                          msgtype = 'E'
                          msgtext = TEXT-012 ) TO e_return.
        ENDIF.

      ELSE. "End check status
        <fs_headers>-AccountingDocumentSource = ''.
        <fs_headers>-FiscalYearSource = ''.
        <fs_headers>-AdjustType = ''.

        APPEND VALUE #( companycode         = <fs_headers>-companycode
                        accountingdocument  = <fs_headers>-accountingdocument
                        fiscalyear          = <fs_headers>-fiscalyear
                        msgtype = 'E'
                        msgtext = TEXT-010 ) TO e_return.
      ENDIF.

      IF gs_docsrc IS NOT INITIAL.
        APPEND gs_docsrc TO e_docsrc.
      ENDIF.

    ENDLOOP.

    MOVE-CORRESPONDING it_headers   TO e_headers.
    MOVE-CORRESPONDING it_items     TO e_items.

  ENDMETHOD.


  METHOD integr_einvoice.

    DATA: ls_mapped_header   LIKE LINE OF mapped-hddt_headers,
          ls_mapped_item     LIKE LINE OF mapped-hddt_items,

          ls_reported_header LIKE LINE OF reported-hddt_headers,
          ls_reported_items  LIKE LINE OF reported-hddt_items.

    DATA: ls_result LIKE LINE OF result.

    DATA: it_headers TYPE TABLE OF wa_document,
          it_items   TYPE TABLE OF wa_items.

    FREE: ir_companycode, ir_accountingdocument, ir_fiscalyear.

    go_einvoice_process->clear_variables( ).

    go_einvoice_process->integration_einvoice(
        EXPORTING
        i_action                = 'CREATE_INVOICE'
        ir_companycode          = ir_companycode
        ir_accountingdocument   = ir_accountingdocument
        ir_fiscalyear           = ir_fiscalyear
        IMPORTING
        e_headers               = gt_headers_save
        e_items                 = it_items
        e_docsrc                = gt_docsrc_save
        e_return                = e_return
    ).

    LOOP AT gt_headers_save ASSIGNING FIELD-SYMBOL(<fs_headers>).
      ls_result-%tky-Companycode        = <fs_headers>-Companycode.
      ls_result-%tky-Accountingdocument = <fs_headers>-Accountingdocument.
      ls_result-%tky-Fiscalyear         = <fs_headers>-Fiscalyear.

      ls_result-%key-Companycode        = <fs_headers>-Companycode.
      ls_result-%key-Accountingdocument = <fs_headers>-Accountingdocument.
      ls_result-%key-Fiscalyear         = <fs_headers>-Fiscalyear.

      ls_result-Companycode             = <fs_headers>-Companycode.
      ls_result-Accountingdocument      = <fs_headers>-Accountingdocument.
      ls_result-Fiscalyear              = <fs_headers>-Fiscalyear.

      ls_mapped_header-%tky                 = ls_result-%tky.
      ls_mapped_header-Companycode          = <fs_headers>-Companycode.
      ls_mapped_header-Accountingdocument   = <fs_headers>-Accountingdocument.
      ls_mapped_header-Fiscalyear           = <fs_headers>-Fiscalyear.

      MOVE-CORRESPONDING <fs_headers> TO ls_result-%param.

      IF gs_docsrc IS NOT INITIAL.
        APPEND gs_docsrc TO gt_docsrc_save.
      ENDIF.

      INSERT CORRESPONDING #( ls_result ) INTO TABLE result.
      INSERT CORRESPONDING #( ls_mapped_header ) INTO TABLE mapped-hddt_headers.

    ENDLOOP.

  ENDMETHOD.


  METHOD move_log.
    o_output-einvoicenumber        = i_input-einvoicenumber .
    o_output-einvoiceserial        = i_input-einvoiceSerial .
    o_output-einvoiceform          = i_input-einvoiceForm .
    o_output-einvoiceType          = i_input-einvoiceType .
    o_output-mscqt                 = i_input-Mscqt .
    o_output-link                  = i_input-link .
    o_output-einvoicedatecancel    = i_input-einvoicedatecreate .
    o_output-einvoicetimecreate    = i_input-einvoicetimecreate .
    o_output-einvoicedatecancel    = i_input-einvoicedatecancel .
    o_output-statussap             = i_input-StatusSap .
    o_output-statusinvres          = i_input-statusinvres .
    o_output-statuscqtres          = i_input-statuscqtres .
    o_output-messagetype           = i_input-messagetype .
    o_output-messagetext           = i_input-messagetext .
    o_output-createdbyuser         = i_input-createdbyuser.
    o_output-createddate           = i_input-createddate.
    o_output-createdtime           = i_input-createdtime.
  ENDMETHOD.


  METHOD read_header.

  ENDMETHOD.


  METHOD read_items.

  ENDMETHOD.


  METHOD replace_einvoice.
    DATA: ls_param TYPE wa_param.
    DATA: it_headers TYPE tt_headers,
          it_items   TYPE tt_items.

    DATA: ls_result         LIKE LINE OF result,
          ls_mapped_headers LIKE LINE OF mapped-hddt_headers.

    FREE: ir_companycode, ir_accountingdocument, ir_fiscalyear.

    go_einvoice_process->clear_variables( ).

    go_einvoice_process->integration_einvoice(
        EXPORTING
        i_action                = 'REPLACE_INVOICE'
        i_param                 = ls_param
        ir_companycode          = ir_companycode
        ir_accountingdocument   = ir_accountingdocument
        ir_fiscalyear           = ir_fiscalyear
        IMPORTING
        e_headers               = gt_headers_save
        e_items                 = it_items
        e_docsrc                = gt_docsrc_save
        e_return                = e_return
    ).

    LOOP AT gt_headers_save ASSIGNING FIELD-SYMBOL(<fs_headers>).

      ls_result-%tky-Companycode            = <fs_headers>-Companycode.
      ls_result-%tky-Accountingdocument     = <fs_headers>-Accountingdocument.
      ls_result-%tky-Fiscalyear             = <fs_headers>-Fiscalyear.

      ls_result-%key-Companycode            = <fs_headers>-Companycode.
      ls_result-%key-Accountingdocument     = <fs_headers>-Accountingdocument.
      ls_result-%key-Fiscalyear             = <fs_headers>-Fiscalyear.

      ls_result-Companycode                 = <fs_headers>-Companycode.
      ls_result-Accountingdocument          = <fs_headers>-Accountingdocument.
      ls_result-Fiscalyear                  = <fs_headers>-Fiscalyear.

      ls_mapped_headers-%tky                = ls_result-%tky.
      ls_mapped_headers-Companycode         = <fs_headers>-Companycode.
      ls_mapped_headers-Accountingdocument  = <fs_headers>-Accountingdocument.
      ls_mapped_headers-Fiscalyear          = <fs_headers>-Fiscalyear.

      INSERT CORRESPONDING #( ls_result ) INTO TABLE result.
      INSERT CORRESPONDING #( ls_mapped_headers ) INTO TABLE mapped-hddt_headers.

    ENDLOOP.

  ENDMETHOD.


  METHOD save_einvoice.

    SORT gt_headers_save    BY companycode accountingdocument fiscalyear ASCENDING.
    SORT gt_docsrc_save     BY companycode accountingdocument fiscalyear ASCENDING.

    LOOP AT gt_headers_save INTO DATA(ls_header).

      READ TABLE gt_docsrc_save TRANSPORTING NO FIELDS WITH KEY
        companycode         = ls_header-companycode
        accountingdocument  = ls_header-accountingdocument
        fiscalyear          = ls_header-fiscalyear BINARY SEARCH.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.

      SELECT COUNT(*) FROM zjp_a_hddt_h
      WHERE companycode        = @ls_header-companycode
        AND accountingdocument = @ls_header-accountingdocument
        AND fiscalyear         = @ls_header-fiscalyear
        INTO @DATA(lv_count).
      IF sy-subrc NE 0.
        CLEAR: lv_count.
      ENDIF.

      IF lv_count = 0.
        MODIFY zjp_a_hddt_h FROM @ls_header.
      ELSE.
        UPDATE zjp_a_hddt_h SET CustomerName                = @ls_header-CustomerName ,
                                CustomerAddress             = @ls_header-CustomerAddress ,
                                EmailAddress                = @ls_header-EmailAddress ,
                                identificationnumber        = @ls_header-identificationnumber ,
                                telephonenumber             = @ls_header-telephonenumber ,
                                typeofdate                  = @ls_header-typeofdate ,
                                usertype                    = @ls_header-usertype ,
                                accountingdocumentsource    = @ls_header-accountingdocumentsource ,
                                fiscalyearsource            = @ls_header-fiscalyearsource ,
                                adjusttype                  = @ls_header-adjusttype ,
                                currencytype                = @ls_header-currencytype ,
                                paymentmethod               = @ls_header-paymentmethod ,
                                einvoicenumber              = @ls_header-einvoicenumber ,
                                einvoiceserial              = @ls_header-einvoiceSerial ,
                                einvoiceform                = @ls_header-einvoiceForm ,
                                einvoicetype                = @ls_header-einvoicetype ,
                                mscqt                       = @ls_header-Mscqt ,
                                link                        = @ls_header-link ,
                                einvoicedatecreate          = @ls_header-einvoicedatecreate ,
                                einvoicetimecreate          = @ls_header-einvoicetimecreate ,
                                einvoicedatecancel          = @ls_header-einvoicedatecancel ,
                                statussap                   = @ls_header-StatusSap ,
                                statusinvres                = @ls_header-statusinvres ,
                                statuscqtres                = @ls_header-statuscqtres ,
                                messagetype                 = @ls_header-messagetype ,
                                messagetext                 = @ls_header-messagetext,
                                invdat                      = @ls_header-invdat ,
                                xreversed                   = @ls_header-xreversed,
                                xreversing                  = @ls_header-xreversing
      WHERE companycode         = @ls_header-Companycode
        AND accountingdocument  = @ls_header-Accountingdocument
        AND fiscalyear          = @ls_header-Fiscalyear.
      ENDIF.

    ENDLOOP.

    LOOP AT gt_docsrc_save INTO DATA(ls_einv_docsrc).
      UPDATE zjp_a_hddt_h SET iconsap       = @ls_einv_docsrc-Iconsap ,
                              statussap     = @ls_einv_docsrc-StatusSap ,
                              messagetext   = @ls_einv_docsrc-messagetext

      WHERE companycode          = @ls_einv_docsrc-Companycode
        AND accountingdocument   = @ls_einv_docsrc-Accountingdocument
        AND fiscalyear           = @ls_einv_docsrc-Fiscalyear.
    ENDLOOP.

  ENDMETHOD.


  METHOD search_einvoice.

    DATA: ls_result LIKE LINE OF result.

    DATA: it_headers TYPE TABLE OF wa_document,
          it_items   TYPE TABLE OF wa_items.

    LOOP AT it_headers ASSIGNING FIELD-SYMBOL(<fs_headers>).
      IF <fs_headers>-usertype IS NOT INITIAL.
        MOVE-CORRESPONDING <fs_headers> TO gs_document.

        gv_action = 'SEARCH_INVOICE'.
        go_fpt_einvoice->search_einvoices(
            EXPORTING
            i_action      = gv_action
            i_einvoice    = gs_document
            i_userpass    = gs_userpass
            IMPORTING
            e_return      = gs_return
            e_status      = gs_status
            e_docsrc      = gs_docsrc
        ).

        SELECT SINGLE * FROM zjp_a_hddt_h
        WHERE Companycode              = @<fs_headers>-Companycode
          AND accountingdocumentsource = @<fs_headers>-Accountingdocument
          AND fiscalyearsource         = @<fs_headers>-Fiscalyear
          AND einvoicenumber    NE ''
        INTO @DATA(ls_adjust).
        IF sy-subrc EQ 0 AND ls_adjust-einvoicenumber IS NOT INITIAL
        AND ( ls_adjust-statussap = '99' OR ls_adjust-statussap = '98' ).
          CASE ls_adjust-adjusttype.
            WHEN '3'. "Thay thế
              <fs_headers>-Iconsap = '@20@'.
              <fs_headers>-StatusSap = '07'.
              <fs_headers>-messagetext = 'Hóa đơn đã bị thay thế'.
            WHEN '1' OR '2'. "Điều chỉnh tiền
              <fs_headers>-Iconsap = '@4K@'.
              <fs_headers>-StatusSap = '06'.
              <fs_headers>-messagetext = 'Hóa đơn đã bị điều chỉnh'.
            WHEN OTHERS.
          ENDCASE.
        ELSE.

          go_einvoice_process->move_log(
              EXPORTING
              i_input = gs_status
              IMPORTING
              o_output = <fs_headers>
          ).

        ENDIF.

        IF gs_docsrc IS NOT INITIAL.
          APPEND gs_docsrc TO gt_docsrc_save.
        ENDIF.

      ELSE.
        <fs_headers>-statussap = '01'.
        <fs_headers>-messagetype = ''.
        <fs_headers>-messagetext = ''.
      ENDIF.

      ls_result-%tky-companycode        = <fs_headers>-companycode.
      ls_result-%tky-accountingdocument = <fs_headers>-accountingdocument.
      ls_result-%tky-fiscalyear         = <fs_headers>-fiscalyear.

      MOVE-CORRESPONDING <fs_headers> TO ls_result-%param.
      INSERT CORRESPONDING #( ls_result ) INTO TABLE result.

    ENDLOOP.

    MOVE-CORRESPONDING it_headers TO gt_headers_save.

  ENDMETHOD.


  METHOD update_entry.

  ENDMETHOD.
ENDCLASS.

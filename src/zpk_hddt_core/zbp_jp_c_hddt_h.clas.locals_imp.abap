CLASS lhc_hddt_headers DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_message,
             companycode        TYPE bukrs,
             accountingdocument TYPE belnr_d,
             fiscalyear         TYPE gjahr,
             msgtype            TYPE sy-msgty,
             msgtext            TYPE zde_txt255,
           END OF ty_message,
           tt_message TYPE TABLE OF ty_message.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR hddt_headers RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR hddt_headers RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ hddt_headers RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK hddt_headers.

    METHODS rba_Einvoiceitems FOR READ
      IMPORTING keys_rba FOR READ hddt_headers\_Einvoiceitems FULL result_requested RESULT result LINK association_links.

    METHODS Adjust FOR MODIFY
      IMPORTING keys FOR ACTION hddt_headers~Adjust RESULT result.

    METHODS Integration FOR MODIFY
      IMPORTING keys FOR ACTION hddt_headers~Integration RESULT result.

    METHODS Search FOR MODIFY
      IMPORTING keys FOR ACTION hddt_headers~Search RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE hddt_headers.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE hddt_headers.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE hddt_headers.

    METHODS cba_Einvoiceitems FOR MODIFY
      IMPORTING entities_cba FOR CREATE hddt_headers\_Einvoiceitems.

ENDCLASS.

CLASS lhc_hddt_headers IMPLEMENTATION.

  METHOD get_instance_features.

    DATA: ls_result LIKE LINE OF result.

    LOOP AT keys INTO DATA(ls_key).
        MOVE-CORRESPONDING ls_key-%tky TO ls_result-%tky.
        APPEND ls_result TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.

    TRY.
        DATA(lock) = cl_abap_lock_object_factory=>get_instance( iv_name = 'EZLOCK_HDDT_H' ).
      CATCH cx_abap_lock_failure INTO DATA(exception).
        RAISE SHORTDUMP exception.
    ENDTRY.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<lfs_keys>).
      TRY.
          lock->enqueue(
*              it_table_mode =
            it_parameter = VALUE #( ( name = 'COMPANYCODE' value = REF #( <lfs_keys>-Companycode ) )
                                    ( name = 'DOCUMENT'    value = REF #( <lfs_keys>-accountingdocument ) )
                                    ( name = 'FISCALYEAR'  value = REF #( <lfs_keys>-fiscalyear ) )
                                  )
*           _scope       =
*           _wait        =
          ).
        CATCH cx_abap_foreign_lock INTO DATA(foreign_lock).
          APPEND VALUE #(
              companycode        = keys[ 1 ]-Companycode
              accountingdocument = keys[ 1 ]-accountingdocument
              fiscalyear         = keys[ 1 ]-fiscalyear
              %msg               = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = 'Record is locked by ' && foreign_lock->user_name
              )
          ) TO reported-hddt_headers.

        CATCH cx_abap_lock_failure INTO exception.
          RAISE SHORTDUMP exception.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD rba_Einvoiceitems.
  ENDMETHOD.

  METHOD Adjust.

    DATA: tt_return TYPE tt_message.
    FREE: tt_return.

    TRY.
        zcl_einvoice_process=>handle_adjust_einvoice_v2(
          EXPORTING
            keys     = keys
          CHANGING
            result   = result
            mapped   = mapped
            failed   = failed
            reported = reported
            e_return = tt_return
        ).
      CATCH cx_abap_context_info_error.
        "handle exception
    ENDTRY.

    IF tt_return IS NOT INITIAL.
      LOOP AT tt_return INTO DATA(ls_return) WHERE msgtype = 'E' OR msgtype = 'W'.
        IF ls_return-msgtype = 'E'.
          INSERT VALUE #(
                 companycode        = ls_return-companycode
                 accountingdocument = ls_return-accountingdocument
                 fiscalyear         = ls_return-fiscalyear
                 %msg               = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                             text     = ls_return-msgtext )
                        ) INTO TABLE reported-hddt_headers.
        ELSEIF ls_return-msgtype = 'W'.
          INSERT VALUE #(
                 companycode        = ls_return-companycode
                 accountingdocument = ls_return-accountingdocument
                 fiscalyear         = ls_return-fiscalyear
                 %msg               = new_message_with_text( severity = if_abap_behv_message=>severity-warning
                                                             text     = ls_return-msgtext )
                        ) INTO TABLE reported-hddt_headers.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD Integration.

    DATA: tt_return TYPE tt_message.
    FREE: tt_return.

    TRY.
        zcl_einvoice_process=>handle_integration_einvoice(
          EXPORTING
            keys     = keys
          CHANGING
            result   = result
            mapped   = mapped
            failed   = failed
            reported = reported
            e_return = tt_return
        ).
      CATCH cx_abap_context_info_error.
        "handle exception
    ENDTRY.

    IF tt_return IS NOT INITIAL.
      LOOP AT tt_return INTO DATA(ls_return) WHERE msgtype = 'E'.
        INSERT VALUE #(
               companycode        = ls_return-companycode
               accountingdocument = ls_return-accountingdocument
               fiscalyear         = ls_return-fiscalyear
               %msg               = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                           text     = ls_return-msgtext )
                      ) INTO TABLE reported-hddt_headers.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD Search.

    DATA: tt_return TYPE tt_message.
    FREE: tt_return.

    zcl_einvoice_process=>handle_search_einvoice(
      EXPORTING
        keys     = keys
      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
        e_return = tt_return
    ).

  ENDMETHOD.

  METHOD create.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD cba_Einvoiceitems.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_hddt_items DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ hddt_items RESULT result.

    METHODS rba_Einvoicesheaders FOR READ
      IMPORTING keys_rba FOR READ hddt_items\_Einvoicesheaders FULL result_requested RESULT result LINK association_links.
    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE hddt_items.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE hddt_items.

ENDCLASS.

CLASS lhc_hddt_items IMPLEMENTATION.

  METHOD read.
  ENDMETHOD.

  METHOD rba_Einvoicesheaders.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZJP_C_HDDT_H DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZJP_C_HDDT_H IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

    zcl_einvoice_process=>save_einvoice(
      CHANGING
        reported = reported
    ).

  ENDMETHOD.

  METHOD cleanup.

    zcl_einvoice_process=>cleanup( ).

  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

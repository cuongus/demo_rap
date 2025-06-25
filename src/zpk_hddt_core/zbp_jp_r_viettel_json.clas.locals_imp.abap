CLASS lhc_viettel_json DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    CLASS-DATA: gt_viettel_json TYPE TABLE OF zjp_viettel_json.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR viettel_json RESULT result.

    METHODS Generate FOR MODIFY
      IMPORTING keys FOR ACTION viettel_json~Generate RESULT result.

    METHODS auto_gen_data.

ENDCLASS.

CLASS lhc_viettel_json IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD Generate.
    DATA: ls_result LIKE LINE OF result.
    DATA: create TYPE TABLE FOR CREATE zjp_r_viettel_json.

    SELECT COUNT(*) FROM zjp_viettel_json INTO @DATA(lv_count) PRIVILEGED ACCESS.
    IF lv_count NE 0.

    ELSE.
      me->auto_gen_data( ).

      IF gt_viettel_json IS NOT INITIAL.

        SORT gt_viettel_json by tagmain tagname ASCENDING.
        DELETE ADJACENT DUPLICATES FROM gt_viettel_json COMPARING tagmain tagname.

        create = VALUE #(
            FOR i IN gt_viettel_json (
                %key-TagMain = i-tagmain
                %key-TagName = i-tagname
*                TagMain = i-tagmain
*                TagName = i-tagname
                Value = i-value
                Createdbyuser = i-createdbyuser
                Createddate = i-createddate
            )
        ).

        MODIFY ENTITIES OF zjp_r_viettel_json IN LOCAL MODE
        ENTITY viettel_json CREATE AUTO FILL CID FIELDS ( TagMain TagName Value Createdbyuser Createddate ) WITH create
            MAPPED mapped
            FAILED failed
            REPORTED reported.

      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD auto_gen_data.

    FREE: gt_viettel_json.

    DATA: lv_string TYPE string,
          lv_char1  TYPE zde_char255,
          lv_char2  TYPE zde_char255.

    TYPES: BEGIN OF lty_strings,
             string TYPE zde_char255,
           END OF lty_strings.

    DATA: lt_strings TYPE TABLE OF lty_strings.

    lv_string = |generalInvoiceInfo invoiceType,generalInvoiceInfo templateCode,generalInvoiceInfo invoiceSeries,generalInvoiceInfo invoiceIssuedDate,generalInvoiceInfo DetailedListNo,generalInvoiceInfo DetailedListDate,generalInvoiceInfo currencyCo| &&
|de,generalInvoiceInfo adjustmentType,generalInvoiceInfo adjustedNote,generalInvoiceInfo adjustmentInvoiceType,generalInvoiceInfo originalInvoiceId,generalInvoiceInfo originalInvoiceIssueDate,generalInvoiceInfo additionalReferenceDesc,generalInvoice| &&
|Info additionalReferenceDate,generalInvoiceInfo paymentStatus,generalInvoiceInfo cusGetInvoiceRight,generalInvoiceInfo exchangeRate,generalInvoiceInfo transactionUuid,generalInvoiceInfo certificateSerial,generalInvoiceInfo originalInvoiceType,gener| &&
|alInvoiceInfo originalTemplateCode,generalInvoiceInfo reservationCode,generalInvoiceInfo adjustAmount20,generalInvoiceInfo invoiceNote,generalInvoiceInfo validation,generalInvoiceInfo qrCode,generalInvoiceInfo otherTax,sellerInfo sellerLegalName,se| &&
|llerInfo sellerTaxCode,sellerInfo sellerAddressLine,sellerInfo sellerPhoneNumber,sellerInfo sellerFaxNumber,sellerInfo sellerEmail,sellerInfo sellerBankName,sellerInfo sellerBankAccount,sellerInfo sellerDistrictName,sellerInfo sellerCityName,seller| &&
|Info sellerCountryCode,sellerInfo sellerWebsite,sellerInfo storeCode,sellerInfo storeName,sellerInfo merchantCode,sellerInfo merchantName,sellerInfo merchantCity,buyerInfo buyerName,buyerInfo buyerCode,buyerInfo buyerLegalName,buyerInfo buyerTaxCod| &&
|e,buyerInfo buyerBudgetCode,buyerInfo buyerAddressLine,buyerInfo buyerPhoneNumber,buyerInfo buyerFaxNumber,buyerInfo buyerEmail,buyerInfo buyerBankName,buyerInfo buyerBankAccount,buyerInfo buyerDistrictName,buyerInfo buyerCityName,buyerInfo buyerCo| &&
|untryCode,buyerInfo buyerIdType,buyerInfo buyerIdNo,buyerInfo buyerBirthDay,buyerInfo buyerNotGetInvoice,payments paymentMethod,itemInfo lineNumber,itemInfo selection,itemInfo itemType,itemInfo itemCode,itemInfo itemName,itemInfo unitCode,itemInfo | &&
|unitName,itemInfo unitPrice,itemInfo quantity,itemInfo itemTotalAmountWithoutTax,itemInfo taxPercentage,itemInfo taxAmount,itemInfo isIncreaseItem,itemInfo itemNote,itemInfo batchNo,itemInfo expDate,itemInfo discount,itemInfo discount2,itemInfo ite| &&
|mDiscount,itemInfo itemTotalAmountAfterDiscount,itemInfo itemTotalAmountWithTax,itemInfo adjustRatio,itemInfo unitPriceWithTax,itemInfo specialInfo,taxBreakdowns taxPercentage,taxBreakdowns taxableAmount,taxBreakdowns taxA| &&
|mount,taxBreakdowns taxableAmountPos,taxBreakdowns taxAmountPos,taxBreakdowns taxExemptionReason,summarizeInfo totalAmountWithoutTax,summarizeInfo totalTaxAmount,summarizeInfo totalAmountWithTax,summarizeInfo totalAmountWithTaxFrn,summarizeInfo tot| &&
|alAmountWithTaxInWords,summarizeInfo isTotalAmountPos,summarizeInfo isTotalTaxAmountPos,summarizeInfo isTotalAmtWithoutTaxPos,summarizeInfo discountAmount,summarizeInfo settlementDiscountAmount,summarizeInfo isDiscountAmtPos,summarizeInfo extraName| &&
|,summarizeInfo extraValue,summarizeInfo totalAmountAfterDiscount,metadata keyTag,metadata valueType,metadata dateValue,metadata stringValue,metadata numberValue,metadata keyLabel,metadata isRequired,metadata isSeller,meterReading meterName,meterRea| &&
|ding previousIndex,meterReading currentIndex,meterReading factor,meterReading amount|.

    SPLIT lv_string AT ',' INTO TABLE lt_strings.

    DATA: ls_viettel_json LIKE LINE OF gt_viettel_json.

    LOOP AT lt_strings INTO DATA(ls_string).

      SPLIT ls_string-string AT ' ' INTO lv_char1 lv_char2.

      ls_viettel_json-tagmain = lv_char1.
      ls_viettel_json-tagname = lv_char2.
      ls_viettel_json-value = lv_char2.
      ls_viettel_json-createdbyuser = sy-uname.

      TRY.
          DATA(lv_tzone) = cl_abap_context_info=>get_user_time_zone( ).
        CATCH cx_abap_context_info_error.
          "handle exception
      ENDTRY.

      DATA(lv_datlo) = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value.
      DATA(lv_timlo) = xco_cp=>sy->time( )->as( xco_cp_time=>format->abap )->value.

      CONVERT DATE lv_datlo TIME lv_timlo DAYLIGHT SAVING TIME 'X'
              INTO TIME STAMP ls_viettel_json-createddate TIME ZONE lv_tzone.
      APPEND ls_viettel_json TO gt_viettel_json.
      CLEAR : ls_viettel_json.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

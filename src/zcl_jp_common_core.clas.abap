CLASS zcl_jp_common_core DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           BEGIN OF ty_page_info,
             paging           TYPE REF TO if_rap_query_paging,
             page_size        TYPE int8,
             offset           TYPE int8,
             requested_fields TYPE if_rap_query_request=>tt_requested_elements,
             sort_order       TYPE if_rap_query_request=>tt_sort_elements,
             ro_filter        TYPE REF TO if_rap_query_filter,
             entity_id        TYPE string,
           END OF ty_page_info,

           BEGIN OF ty_sodu,
             companycode TYPE bukrs,
             GLaccount   TYPE hkont,
             sodudk      TYPE zde_dmbtr,
             sodudk_nt   TYPE zde_dmbtr,
           END OF ty_sodu,

           tt_ranges    TYPE TABLE OF ty_range_option,
           st_page_info TYPE ty_page_info,

           tt_sodu      TYPE TABLE OF ty_sodu
           .

    CLASS-DATA:
      "Instance Singleton
      mo_instance      TYPE REF TO zcl_jp_common_core,

      "Table customer info
      gt_customer_info TYPE SORTED TABLE OF zst_customer_info WITH UNIQUE KEY customer.

    CLASS-METHODS:
      "Contructor
      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_jp_common_core,

      "Get fillter app
      get_fillter_app IMPORTING io_request            TYPE REF TO if_rap_query_request
                                io_response           TYPE REF TO if_rap_query_response
                      EXPORTING ir_companycode        TYPE tt_ranges
                                ir_accountingdocument TYPE tt_ranges
                                ir_glaccount          TYPE tt_ranges
                                ir_fiscalyear         TYPE tt_ranges
                                ir_postingdate        TYPE tt_ranges
                                ir_documentdate       TYPE tt_ranges

                                wa_page_info          TYPE st_page_info
                      ,

      "Method get Customer info
      get_customer_details IMPORTING wa_document         TYPE zst_document_info OPTIONAL
                           EXPORTING wa_customer_details TYPE zst_customer_info,

      "Method get Company Code info
      get_companycode_details IMPORTING iv_companycode         TYPE bukrs
                              EXPORTING wa_companycode_details TYPE zst_companycode_info,

      "Method get Address ID
      get_address_id_details IMPORTING AddressID            TYPE ad_addrnum
                             EXPORTING wa_addressid_details TYPE zst_addresId_info,

      "Method get Số dư đầu kỳ/Cuối kỳ GLaccount
      get_GLaccount_sodu IMPORTING ir_companycode TYPE tt_ranges
                                   ir_GLaccount   TYPE tt_ranges
                                   ir_date        TYPE tt_ranges

                         EXPORTING it_sodu_dk     TYPE tt_sodu
                                   it_sodu_ck     TYPE tt_sodu
                         .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_jp_common_core IMPLEMENTATION.

  METHOD get_customer_details.

    DATA: lv_url TYPE string VALUE IS INITIAL. "API read BP Details
    DATA: lv_country TYPE land1 VALUE IS INITIAL.

    SELECT SINGLE isonetimeaccount FROM i_customer WHERE Customer = @wa_document-customer
    INTO @DATA(lv_xcpdk).

    IF lv_xcpdk IS NOT INITIAL.
      SELECT SINGLE
            businesspartnername1 AS name1,
            businesspartnername2 AS name2,
            businesspartnername3 AS name3,
            businesspartnername4 AS name4,
            streetaddressname AS stras,
            cityname AS ort01,
            taxid1 AS stcd1,
            accountingclerkinternetaddress AS intad,
            Country AS land1
        FROM i_onetimeaccountcustomer
        WHERE accountingdocument = @wa_document-accountingdocument AND
              companycode        = @wa_document-companycode AND
              fiscalyear         = @wa_document-fiscalyear
        INTO @DATA(ls_bsec).

      IF sy-subrc EQ 0. "Nếu Mã khách lẻ

        wa_customer_details-customername = |{ ls_bsec-name2 } { ls_bsec-name3 } { ls_bsec-name4 } | .
        IF ls_bsec-name2 IS INITIAL AND ls_bsec-name3 IS INITIAL AND ls_bsec-name4 IS INITIAL.
          wa_customer_details-customername = ls_bsec-name1 .
        ENDIF.
        wa_customer_details-customeraddress = |{ ls_bsec-stras }{ ls_bsec-ort01 }| .
        wa_customer_details-identificationnumber  = ls_bsec-stcd1.
        wa_customer_details-emailaddress = ls_bsec-intad.
        "Country
        lv_country = ls_bsec-land1.
      ENDIF.

    ELSE.

      READ TABLE gt_customer_info INTO wa_customer_details WITH KEY customer = wa_document-customer BINARY SEARCH.
      IF sy-subrc NE 0.
        SELECT SINGLE cus~customer,
                      cus~addressid,
                      cus~VATRegistration,
                      cus~isonetimeaccount,
                      cus~createdbyuser,
                      cus~creationdate,
                      bp~creationtime
        FROM i_customer AS cus
        INNER JOIN i_businesspartner AS bp ON cus~Customer = bp~BusinessPartner
        WHERE cus~Customer = @wa_document-customer
        INTO CORRESPONDING FIELDS OF @wa_customer_details
        .

        "Customer Identification Number
        SELECT SINGLE BPIdentificationNumber FROM I_BuPaIdentification
        INTO @wa_customer_details-identificationnumber
*       WHERE BPIdentificationType = ?
        .

        "Customer Address
        zcl_jp_common_core=>get_address_id_details(
            EXPORTING
            addressid = wa_customer_details-addressid
            IMPORTING
            wa_addressid_details = DATA(ls_addressid)
        ).
**-----------------------------------------------------------------------**
        wa_customer_details-customername    = ls_addressid-addressname.
        wa_customer_details-customeraddress = ls_addressid-address.
        wa_customer_details-emailaddress    = ls_addressid-emailaddress.
        wa_customer_details-telephonenumber = ls_addressid-telephonenumber.

        APPEND wa_customer_details TO gt_customer_info.
        CLEAR: wa_customer_details.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_instance.
    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                           THEN mo_instance
                                           ELSE NEW #( ) ).
  ENDMETHOD.

  METHOD get_fillter_app.

    wa_page_info-paging            = io_request->get_paging( ).

    wa_page_info-page_size         = io_request->get_paging( )->get_page_size( ).

    wa_page_info-offset            = io_request->get_paging( )->get_offset( ).

    wa_page_info-requested_fields  = io_request->get_requested_elements( ).

    wa_page_info-sort_order        = io_request->get_sort_elements( ).

    wa_page_info-ro_filter         = io_request->get_filter( ).

    wa_page_info-entity_id         = io_request->get_entity_id( ).

    TRY.
        DATA(lr_ranges) = wa_page_info-ro_filter->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range.
        "handle exception
    ENDTRY.

    LOOP AT lr_ranges INTO DATA(ls_ranges).
      CASE ls_ranges-name.
        WHEN 'COMPANYCODE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_companycode.
        WHEN 'ACCOUNTINGDOCUMENT'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_accountingdocument.
        WHEN 'FISCALYEAR'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_fiscalyear.
        WHEN 'CASHACCOUNTING'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_glaccount.
        WHEN 'POSTINGDATE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_postingdate.
        WHEN 'DOCUMENTDATE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_documentdate.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_companycode_details.

    SELECT SINGLE
              companycode,
              addressid,
              vatregistration,
              Currency
*              createdbyuser,
*              creationdate,
*              creationtime
    FROM I_CompanyCode
    WHERE CompanyCode = @iv_companycode
    INTO CORRESPONDING FIELDS OF @wa_companycode_details
    .

  ENDMETHOD.

  METHOD get_address_id_details.

    "Customer Address
    SELECT SINGLE AddresseeFullName,
                  HouseNumber,
                  StreetName,             "Street
                  StreetPrefixName1,      "str_suppl1 - Street 2
                  StreetPrefixName2,      "str_suppl2 - Street 3
                  StreetSuffixName1,      "str_suppl3 - Street 4
                  StreetSuffixName2,      "location - Street 5
                  DistrictName,
                  CityName,
                  Country
    FROM i_address_2
    WITH PRIVILEGED ACCESS
    WHERE AddressID = @wa_addressid_details-addressid
      AND AddressRepresentationCode = 'R'
    INTO @DATA(ls_address_2)
    .

    wa_addressid_details-addressname = ls_address_2-AddresseeFullName.
    wa_addressid_details-address =
    |{ ls_address_2-StreetName }, { ls_address_2-StreetPrefixName1 }, { ls_address_2-StreetPrefixName2 }, { ls_address_2-StreetSuffixName1 }, { ls_address_2-DistrictName }, { ls_address_2-CityName }|.

    REPLACE ALL OCCURRENCES OF |, , , , , ,| IN wa_addressid_details-address WITH |,|.
    REPLACE ALL OCCURRENCES OF |, , , , ,| IN wa_addressid_details-address WITH |,|.
    REPLACE ALL OCCURRENCES OF |, , , ,| IN wa_addressid_details-address WITH |,|.
    REPLACE ALL OCCURRENCES OF |, , ,| IN wa_addressid_details-address WITH |,|.
    REPLACE ALL OCCURRENCES OF |, ,| IN wa_addressid_details-address WITH |,|.

    "Customer Email
    SELECT SINGLE EmailAddress FROM I_AddressEmailAddress_2
    WITH PRIVILEGED ACCESS
    WHERE AddressID = @wa_addressid_details-addressid
    INTO @wa_addressid_details-emailaddress
    .

    "Customer Telephone
    SELECT SINGLE PhoneExtensionNumber FROM I_AddressPhoneNumber_2
    WITH PRIVILEGED ACCESS
    WHERE AddressID = @wa_addressid_details-addressid
    INTO @wa_addressid_details-telephonenumber
    .

  ENDMETHOD.

  METHOD get_glaccount_sodu.
    DATA: lv_date_dk TYPE budat VALUE IS INITIAL,
          lv_date_ck TYPE budat VALUE IS INITIAL.

    DATA: ls_sodu TYPE ty_sodu.

    READ TABLE ir_date INTO DATA(ls_date) INDEX 1.

    lv_date_dk = ls_date-low.
    lv_date_ck = ls_date-high.

    IF lv_date_dk IS NOT INITIAL.
      SELECT
          CompanyCode,
          GLAccount,
          DebitCreditCode,
      SUM( AbsoluteAmountInCoCodeCrcy ) AS sodudk ,
      SUM( AbsoluteAmountInTransacCrcy ) AS sodudk_nt
      FROM i_operationalacctgdocitem
      WHERE CompanyCode        IN @ir_companycode
        AND AccountingDocument IN @ir_glaccount
        AND PostingDate        LT @lv_date_dk
      GROUP BY CompanyCode, GLAccount, DebitCreditCode
      INTO TABLE @DATA(lt_sodudk)
     .

      LOOP AT lt_sodudk INTO DATA(ls_sodudk).
        MOVE-CORRESPONDING ls_sodudk TO ls_sodu.
        IF ls_sodudk-DebitCreditCode = 'H'.
          ls_sodu-sodudk = ls_sodudk-sodudk * ( -1 ).
          ls_sodu-sodudk_nt = ls_sodudk-sodudk_nt * ( -1 ).
        ENDIF.

        COLLECT ls_sodu INTO it_sodu_dk.
        CLEAR: ls_sodu.
      ENDLOOP.
    ENDIF.

    IF lv_date_ck IS NOT INITIAL.
      SELECT
           CompanyCode,
           GLAccount,
           DebitCreditCode,
       SUM( AbsoluteAmountInCoCodeCrcy ) AS sodudk ,
       SUM( AbsoluteAmountInTransacCrcy ) AS sodudk_nt
       FROM i_operationalacctgdocitem
       WHERE CompanyCode        IN @ir_companycode
         AND AccountingDocument IN @ir_glaccount
         AND PostingDate        LE @lv_date_ck
       GROUP BY CompanyCode, GLAccount, DebitCreditCode
       INTO TABLE @DATA(lt_soduck)
      .

      LOOP AT lt_soduck INTO DATA(ls_soduck).
        MOVE-CORRESPONDING ls_sodudk TO ls_sodu.
        IF ls_soduck-DebitCreditCode = 'H'.
          ls_sodu-sodudk = ls_soduck-sodudk * ( -1 ).
          ls_sodu-sodudk_nt = ls_soduck-sodudk_nt * ( -1 ).
        ENDIF.

        COLLECT ls_sodu INTO it_sodu_ck.
        CLEAR: ls_sodu.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

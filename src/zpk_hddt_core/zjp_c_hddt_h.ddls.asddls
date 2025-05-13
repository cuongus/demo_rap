@EndUserText.label: 'Projection CDS for HDDT Header'
@ObjectModel: {
    query: {
            implementedBy: 'ABAP:ZCL_EINVOICE_DATA' }
    }
@Metadata.allowExtensions: true
@Search.searchable: true
define root custom entity ZJP_C_HDDT_H
  // with parameters parameter_name : parameter_type
{
      @Search.defaultSearchElement   : true
      @Consumption.filter            : { mandatory:  true }
  key CompanyCode                    : bukrs;
  key AccountingDocument             : belnr_d;
      @Search.defaultSearchElement   : true
      @Consumption.filter            : { mandatory:  true }
  key FiscalYear                     : gjahr;
      testrun                        : abap_boolean;
      IconSAP                        : abap.char(5);
      AccountingDocumentType         : blart;
      FiscalPeriod                   : monat;
      PostingDate                    : budat;
      DocumentDate                   : bldat;
      AccountingDocumentCreationDate : abp_creation_date;
      AbsoluteExChangeRate           : zde_kursf;
      SupplierTax                    : zde_id_number;
      Customer                       : zde_kunnr;
      CustomerName                   : zde_bp_name;
      CustomerAddress                : zde_bp_address;
      IdentificationNumber           : zde_id_number;
      EmailAddress                   : zde_email;
      TelephoneNumber                : zde_telephone;
      PaymentMethod                  : zde_payment;
      ProfitCenter                   : prctr;
      AdjustType                     : zde_adjusttype;
      AccountingDocumentSource       : belnr_d;
      FiscalYearSource               : gjahr;
      AccountingDocumentHeaderText   : bktxt;
      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'ZDE_CURRTYPE', usage: #FILTER }]
                , distinctValues     : true
      }]
      @Consumption.filter            : { defaultValue: '', selectionType: #SINGLE}
      CurrencyType                   : zde_currtype;
      CompanycodeCurrency            : waers;
      AmountInCoCodeCrcy             : zde_dmbtr;
      VatAmountInCoCodeCrcy          : zde_dmbtr;
      TotalAmountInCoCodeCrcy        : zde_dmbtr;
      TransactionCurrency            : waers;
      AmountInTransacCrcy            : zde_dmbtr;
      VatAmountInTransacCrcy         : zde_dmbtr;
      TotalAmountInTransacCrcy       : zde_dmbtr;
      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'ZDE_USERTYPE', usage: #FILTER }]
                , distinctValues     : true
      }]
      @Consumption.filter            : { defaultValue: '', selectionType: #SINGLE}
      //      @ObjectModel.text.element: ['UserTypeText']
      Usertype                       : zde_usertype;
      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'ZDE_TYPEOFDATE', usage: #FILTER }]
                , distinctValues     : true
      }]
      @Consumption.filter            : { mandatory:  true, defaultValue: '', selectionType: #SINGLE}
      TypeOfDate                     : zde_typeofdate;
      EinvoiceForm                   : zde_einvoiceform;
      EinvoiceSerial                 : zde_einvoiceserial;
      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'ZDE_EINVOICETYPE', usage: #FILTER }]
                , distinctValues     : true
      }]
      @Consumption.filter            : { mandatory:  true, defaultValue: '', selectionType: #SINGLE}
      EinvoiceType                   : zde_einvoicetype;
      EinvoiceNumber                 : zde_einvoicenumber;
      SID                            : zde_sid;
      EinvoiceTimeCreate             : zde_einv_time;
      EinvoiceDateCreate             : zde_einv_date;
      EinvoiceDateCancel             : zde_einv_datecancel;
      Link                           : zde_link;
      MSCQT                          : zde_mscqt;
      StatusSAP                      : zde_statussap;
      StatusInvRes                   : zde_statusinvres;
      StatusCQTRes                   : zde_statuscqtres;
      MessageType                    : zde_messagetype;
      MessageText                    : zde_messagetext;
      CreatedByUser                  : abp_creation_user;
      CreatedDate                    : abp_creation_date;
      CreatedTime                    : abp_creation_time;
      ChangedByUser                  : abp_lastchange_user;
      ChangedDate                    : abp_lastchange_date;
      ChangedTime                    : abp_lastchange_time;

}

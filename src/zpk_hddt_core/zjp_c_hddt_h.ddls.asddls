@EndUserText.label: 'Projection CDS for HDDT Header'
@ObjectModel: {
    query: {
            implementedBy: 'ABAP:ZCL_EINVOICE_DATA'
//            combinedCountAndDataRetrievalEnabled: true
            }
    }
@Metadata.allowExtensions: true
@Search.searchable: true
define root custom entity ZJP_C_HDDT_H
  //   with parameters IdSys_pr : abap.char(10)
{
      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name: 'I_CompanyCodeStdVH', element: 'CompanyCode' }
      }]
      @Consumption.filter            : { mandatory:  true}
      @ObjectModel.text.element      : [ '_CompanyCode.CompanyCodeName' ]
  key CompanyCode                    : bukrs;
  key AccountingDocument             : belnr_d;
      @Search.defaultSearchElement   : true
      @Consumption.filter            : { mandatory:  true }
  key FiscalYear                     : gjahr;
      @Search.defaultSearchElement   : true
      //ID System Integration Invoice
      IDSys                          : abap.char(10);
      @Search.defaultSearchElement   : true
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

      @ObjectModel.text.element      : [ '_ConfigAdjType.Description' ]
      AdjustType                     : zde_adjusttype;

      AccountingDocumentSource       : belnr_d;
      FiscalYearSource               : gjahr;
      AccountingDocumentHeaderText   : bktxt;

      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'CURRTYPE', usage: #FILTER }]
                , distinctValues     : true
      }]
      @Consumption.filter            : { defaultValue: '', selectionType: #SINGLE}
      CurrencyType                   : zde_currtype;

      taxcode                        : zde_taxcode;
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
                localConstant        : 'USERTYPE', usage: #FILTER }]
                , distinctValues     : true
      }]
      @Consumption.filter            : { selectionType: #SINGLE}
      @ObjectModel.text.element      : ['_ConfigUsertype.Description']
      Usertype                       : zde_usertype;

      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'TYPEOFDATE', usage: #FILTER }]
                , distinctValues     : true
      }]
      @Consumption.filter            : { mandatory:  true, selectionType: #SINGLE}
      @ObjectModel.text.element      : [ '_ConfigTypeOfDate.Description' ]
      TypeOfDate                     : zde_typeofdate;

      EinvoiceForm                   : zde_einvoiceform;
      EinvoiceSerial                 : zde_einvoiceserial;

      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_R_HD_SERIAL' , element: 'EinvoiceType' }
      }]
      @Consumption.filter            : { mandatory:  true, selectionType: #SINGLE}
      EinvoiceType                   : zde_einvoicetype;

      EinvoiceNumber                 : zde_einvoicenumber;
      SID                            : zde_sid;
      EinvoiceTimeCreate             : zde_einv_time;
      EinvoiceDateCreate             : zde_einv_date;
      EinvoiceDateCancel             : zde_einv_datecancel;
      Link                           : zde_link;
      MSCQT                          : zde_mscqt;

      @Search.defaultSearchElement   : true
      @Consumption.valueHelpDefinition:[
      { entity                       : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
      additionalBinding              : [{ element: 'domain_name',
                localConstant        : 'ZDE_STATUSSAP', usage: #FILTER }]
                , distinctValues     : true
      }]
      @Consumption.filter            : { selectionType: #SINGLE}
      @ObjectModel.text.element      : [ '_ConfigStatusSAP.Description' ]
      StatusSAP                      : zde_statussap;

      @ObjectModel.text.element      : [ '_ConfigStatusINVRES.Description' ]
      StatusInvRes                   : zde_statusinvres;

      @ObjectModel.text.element      : [ '_ConfigStatusCQT.Description' ]
      StatusCQTRes                   : zde_statuscqtres;

      MessageType                    : zde_messagetype;
      MessageText                    : zde_messagetext;
      CreatedByUser                  : abp_creation_user;
      CreatedDate                    : abp_creation_date;
      CreatedTime                    : abp_creation_time;
      ChangedByUser                  : abp_lastchange_user;
      ChangedDate                    : abp_lastchange_date;
      ChangedTime                    : abp_lastchange_time;

      _EInvoiceItems                 : composition [0..*] of ZJP_C_HDDT_I;

      _CompanyCode                   : association [0..1] to I_CompanyCodeStdVH on _CompanyCode.CompanyCode = $projection.CompanyCode;

      _Customer                      : association [0..1] to I_Customer on _Customer.Customer = $projection.Customer;

      _ConfigUsertype                : association [0..1] to ZJP_CFG_USERTYPE on _ConfigUsertype.Value = $projection.Usertype;

      _ConfigTypeOfDate              : association [0..1] to ZJP_CFG_TYPEOFDATE on _ConfigTypeOfDate.Value = $projection.TypeOfDate;

      _ConfigStatusSAP               : association [0..1] to zjp_cfg_statussap on _ConfigStatusSAP.Value = $projection.StatusSAP;

      _ConfigStatusINVRES            : association [0..1] to ZJP_CFG_STATUSINV on _ConfigStatusINVRES.Value = $projection.StatusInvRes;

      _ConfigStatusCQT               : association [0..1] to ZJP_CFG_STATUSCQT on _ConfigStatusCQT.Value = $projection.StatusCQTRes;

      _ConfigAdjType                 : association [0..1] to ZJP_CFG_ADJTYPE on _ConfigAdjType.Value = $projection.AdjustType;
}

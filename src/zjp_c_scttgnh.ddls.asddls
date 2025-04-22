@EndUserText.label: 'CDS View for Sổ CT tiền gửi ngân hàng'
@ObjectModel: {
    query: {
            implementedBy: 'ABAP:ZCL_JP_GET_DATA_SCTTGNH' }
    }
@Metadata.allowExtensions: true
@Search.searchable: true
define custom entity ZJP_C_SCTTGNH
  // with parameters parameter_name : parameter_type
{
      @Search.defaultSearchElement : true
  key CompanyCode                  : bukrs;
  key AccountingDocument           : belnr_d;
  key FiscalYear                   : gjahr;
  key AccountingDocumentItem       : buzei;
      AccountingDocumentItemType   : zde_buzid;

      AccountingDocumentType       : blart;
      PostingDate                  : budat;
      DocumentDate                 : bldat;

      DocumentReferenceID          : xblnr1;
      ExchangeRate                 : zde_kursf;
      AccountingDocumentHeaderText : bktxt;
      CashAccounting               : hkont;
      DebitCreditCode              : shkzg;

      CompanyCodeCurrency          : waers;
      @Aggregation.default         : #SUM
      @Semantics                   : { amount : {currencyCode: 'CompanyCodeCurrency'} }
      AbsoluteAmountInCoCodeCrcy   : dmbtr;

      TransactionCurrency          : waers;
      @Aggregation.default         : #SUM
      @Semantics                   : { amount : {currencyCode: 'TransactionCurrency'} }
      AbsoluteAmountInTransacCrcy  : wrbtr;

      AssignmentReference          : dzuonr;
      DocumentItemText             : sgtxt;
      @ObjectModel.foreignKey.association: '_ProfitCenter'
      ProfitCenter                 : prctr;

      BusinessPartner              : zde_partner;
      BusinessPartnerName          : zde_bp_name;
      BusinessPartnerAddress       : zde_bp_address;

      NegativePosting              : zde_xnegp;

      @Aggregation.default         : #SUM
      @Semantics                   : { amount : {currencyCode: 'CompanyCodeCurrency'} }
      PhatSinhNo                   : dmbtr;

      @Aggregation.default         : #SUM
      @Semantics                   : { amount : {currencyCode: 'CompanyCodeCurrency'} }
      PhatSinhCo                   : dmbtr;

      @Aggregation.default         : #SUM
      @Semantics                   : { amount : {currencyCode: 'CompanyCodeCurrency'} }
      Sodu                         : dmbtr;

      @Aggregation.default         : #SUM
      @Semantics                   : { amount : {currencyCode: 'TransactionCurrency'} }
      PhatSinhNo_NT                : wrbtr;

      @Aggregation.default         : #SUM
      @Semantics                   : { amount : {currencyCode: 'TransactionCurrency'} }
      PhatSinhCo_NT                : wrbtr;

      @Aggregation.default         : #SUM
      @Semantics                   : { amount : {currencyCode: 'TransactionCurrency'} }
      Sodu_NT                      : wrbtr;

      CompanyCodeName              : zde_companycode_name;
      CompanyCodeAddr              : zde_companycode_addr;

      GLacctText                   : zde_glacctext;
      ChiNhanhNganHang             : zde_chinhanh_nh;

      CreationUser                 : abp_creation_user;
      CreationDate                 : abp_creation_date;
      CreationTime                 : abp_creation_time;



      @ObjectModel.foreignKey.association: '_ControllingArea'
      ControllingArea              : kokrs;


      _CompanyCode                 : association [1..1] to I_CompanyCode on $projection.CompanyCode = _CompanyCode.CompanyCode;

      _ControllingArea             : association [0..1] to I_ControllingArea on $projection.ControllingArea = _ControllingArea.ControllingArea;
      _ControllingAreaText         : association [0..1] to I_ControllingArea on $projection.ControllingArea = _ControllingAreaText.ControllingArea;

      _ProfitCenter                : association [0..*] to I_ProfitCenter on  $projection.ControllingArea = _ProfitCenter.ControllingArea
                                                                          and $projection.ProfitCenter    = _ProfitCenter.ProfitCenter;

      _ProfitCenterText            : association [0..*] to I_ProfitCenterText on  $projection.ControllingArea = _ProfitCenterText.ControllingArea
                                                                              and $projection.ProfitCenter    = _ProfitCenterText.ProfitCenter;

      //      _CurrentProfitCenter         : association [0..1] to I_ProfitCenter on  $projection.ControllingArea            = _CurrentProfitCenter.ControllingArea
      //                                                                          and $projection.ProfitCenter               = _CurrentProfitCenter.ProfitCenter
      //                                                                          and _CurrentProfitCenter.ValidityStartDate <= $session.system_date
      //                                                                          and _CurrentProfitCenter.ValidityEndDate   >= $session.system_date;

      // do not use: #DEPRECATED  ; use _JournalEntryItemOneTimeData

      //      _OneTimeAccountBP            : association [0..1] to I_OneTimeAccountBP on  $projection.CompanyCode            = _OneTimeAccountBP.CompanyCode
      //                                                                              and $projection.FiscalYear             = _OneTimeAccountBP.FiscalYear
      //                                                                              and $projection.AccountingDocument     = _OneTimeAccountBP.AccountingDocument
      //                                                                              and $projection.AccountingDocumentItem = _OneTimeAccountBP.AccountingDocumentItem;

      _JournalEntryItemOneTimeData : association [0..1] to I_JournalEntryItemOneTimeData on  $projection.CompanyCode            = _JournalEntryItemOneTimeData.CompanyCode
                                                                                         and $projection.FiscalYear             = _JournalEntryItemOneTimeData.FiscalYear
                                                                                         and $projection.AccountingDocument     = _JournalEntryItemOneTimeData.AccountingDocument
                                                                                         and $projection.AccountingDocumentItem = _JournalEntryItemOneTimeData.AccountingDocumentItem;

}

@EndUserText.label: 'Projection CDS for HDDT Items'
@ObjectModel: {
    query: {
            implementedBy: 'ABAP:ZCL_EINVOICE_DATA' }
    }
@Metadata.allowExtensions: true
@Search.searchable: true
define custom entity ZJP_C_HDDT_I
  // with parameters parameter_name : parameter_type
{
      @Search.defaultSearchElement   : true
  key CompanyCode              : bukrs;
  key AccountingDocument       : belnr_d;
  key FiscalYear               : gjahr;

  key AdjustType               : zde_adjusttype;
  key AccountingDocumentSource : belnr_d;
  key FiscalYearSource         : gjahr;

  key CurrencyType             : zde_currtype;
  key Usertype                 : zde_usertype;
  key TypeOfDate               : zde_typeofdate;

  key EinvoiceForm             : zde_einvoiceform;
  key EinvoiceSerial           : zde_einvoiceserial;
  key EinvoiceType             : zde_einvoicetype;
  key EinvoiceNumber           : zde_einvoicenumber;

  key testrun                  : abap_boolean;

  key AccountingDocumentItem   : buzei;

      TaxCode                  : zde_taxcode;
      TaxPercentage            : zde_dmbtr;
      ItemEinvoice             : buzei;
      Product                  : matnr;
      Longtext                 : zde_txt255;
      DocumentItemText         : zde_txt255;
      Quantity                 : zde_menge;
      BaseUnit                 : meins;
      UnitofMeasureLongname    : zde_txt25;
      CompanyCodeCurrency      : waers;
      priceInCoCodeCrcy        : zde_dmbtr;
      AmountInCoCodeCrcy       : zde_dmbtr;
      VatAmountInCoCodeCrcy    : zde_dmbtr;
      TotalAmountInCoCodeCrcy  : zde_dmbtr;
      TransactionCurrency      : waers;
      priceintransaccrcy       : zde_dmbtr;
      AmountInTransacCrcy      : zde_dmbtr;
      VatAmountInTransacCrcy   : zde_dmbtr;
      TotalAmountInTransacCrcy : zde_dmbtr;

      _EInvoicesHeaders        : association to parent ZJP_C_HDDT_H on  $projection.CompanyCode              = _EInvoicesHeaders.CompanyCode
                                                                    and $projection.AccountingDocument       = _EInvoicesHeaders.AccountingDocument
                                                                    and $projection.FiscalYear               = _EInvoicesHeaders.FiscalYear

                                                                    and $projection.AdjustType               = _EInvoicesHeaders.AdjustType
                                                                    and $projection.AccountingDocumentSource = _EInvoicesHeaders.AccountingDocumentSource
                                                                    and $projection.FiscalYearSource         = _EInvoicesHeaders.FiscalYearSource

                                                                    and $projection.CurrencyType             = _EInvoicesHeaders.CurrencyType
                                                                    and $projection.Usertype                 = _EInvoicesHeaders.Usertype
                                                                    and $projection.TypeOfDate               = _EInvoicesHeaders.TypeOfDate

                                                                    and $projection.EinvoiceForm             = _EInvoicesHeaders.EinvoiceForm
                                                                    and $projection.EinvoiceSerial           = _EInvoicesHeaders.EinvoiceSerial
                                                                    and $projection.EinvoiceType             = _EInvoicesHeaders.EinvoiceType
                                                                    and $projection.EinvoiceNumber           = _EInvoicesHeaders.EinvoiceNumber
                                                                    
                                                                    and $projection.testrun                  = _EInvoicesHeaders.testrun;

}

@EndUserText.label: 'Projection CDS for HDDT Item'
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

      _EInvoicesHeaders        : association to parent ZJP_C_HDDT_H on  $projection.CompanyCode        = _EInvoicesHeaders.CompanyCode
                                                                    and $projection.AccountingDocument = _EInvoicesHeaders.AccountingDocument
                                                                    and $projection.FiscalYear         = _EInvoicesHeaders.FiscalYear;


}

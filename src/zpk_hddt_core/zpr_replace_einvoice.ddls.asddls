@EndUserText.label: 'Parameter For Action Replace EInvoice'
define abstract entity zpr_replace_einvoice
  //  with parameters parameter_name : parameter_type
{
  @Consumption.valueHelpDefinition: [{ entity: {name: 'ZJP_R_HDDT_H' , element: 'Accountingdocument' },
  additionalBinding        : [{ localElement: 'AccountingDocumentSource', element: 'FiscalYearSource' }] }
  ]
  @EndUserText.label       : 'Replace Document'
  @UI.defaultValue         : #( 'ELEMENT_OF_REFERENCED_ENTITY: AccountingDocumentSource' )
  AccountingDocumentSource : belnr_d;
  @EndUserText.label       : 'Replace Fiscal Year'
  @UI.defaultValue         : #( 'ELEMENT_OF_REFERENCED_ENTITY: FiscalYearSource' )
  FiscalYearSource         : gjahr;

}

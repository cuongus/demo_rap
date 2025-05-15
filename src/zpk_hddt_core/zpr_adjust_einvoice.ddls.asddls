@EndUserText.label: 'Parameter Action Adjust EInvoices'
define abstract entity zpr_adjust_einvoice
  //  with parameters parameter_name : parameter_type
{
  @Consumption.valueHelpDefinition: [{ entity: {name: 'ZJP_R_HDDT_H' , element: 'Accountingdocument' },
     additionalBinding     : [{ localElement: 'AccountingDocumentSource', element: 'FiscalYearSource' }] }
     ]
  @UI.defaultValue         : #( 'ELEMENT_OF_REFERENCED_ENTITY: AccountingDocumentSource')
  @EndUserText.label       : 'Adjust Document'
  AccountingDocumentSource : belnr_d;

  @UI.defaultValue         : #( 'ELEMENT_OF_REFERENCED_ENTITY: FiscalYearSource')
  @EndUserText.label       : 'Adjust Fiscal Year'
  FiscalYearSource         : gjahr;

  @Consumption.valueHelpDefinition: [
  { entity                 : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
  additionalBinding        : [{ element: 'domain_name',
                  localConstant: 'ZDE_ADJUSTTYPE', usage: #FILTER }]
                  , distinctValues: true
  }]
  @Consumption.filter      : { mandatory: true, selectionType: #SINGLE}
  @UI.defaultValue         : #( 'ELEMENT_OF_REFERENCED_ENTITY: ADJUSTTYPE')
  @EndUserText.label       : 'Adjust Type'
  adjusttype               : zde_adjusttype;

}

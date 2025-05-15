@EndUserText.label: 'Parameter Action Cancel EInvoices'
define abstract entity zpr_cancel_einvoice
  //  with parameters parameter_name : parameter_type
{
  @Consumption.valueHelpDefinition: [
  { entity     : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
  additionalBinding  : [{ element: 'domain_name',
                 localConstant: 'ZDE_NOTI_TAXTYPE', usage: #FILTER }]
                 , distinctValues: true
  }]
  @Consumption.filter: { mandatory: true, selectionType: #SINGLE}
  @EndUserText.label: 'Loại thông báo'
  noti_taxtype : abap.char(30);

  @EndUserText.label: 'Số thông báo'
  noti_taxnum  : abap.char(30);

  @EndUserText.label: 'Ngày CQT thông báo'
  noti_taxdt   : abap.char(50);

  @EndUserText.label: 'Địa danh'
  @Consumption.filter: { mandatory: true }
  place        : abap.char(100);

  @Consumption.valueHelpDefinition: [
  { entity     : { name : 'ZJP_C_DOMAIN_FIX_VAL' , element: 'low' } ,
  additionalBinding  : [{ element: 'domain_name',
                  localConstant: 'ZDE_NOTI_TYPE', usage: #FILTER }]
                  , distinctValues: true
  }]
  @Consumption.filter: { mandatory: true, selectionType: #SINGLE}
  @EndUserText.label: 'Tính chất thông báo'
  noti_type    : abap.char(30);

}

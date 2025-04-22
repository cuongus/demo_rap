@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS for HDDT Serial'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HD_SERIAL
  provider contract transactional_query
  as projection on ZJP_R_HD_SERIAL
{
  key Companycode,
  key Fiscalyear,
  key EinvoiceType,
      EinvoiceSerial,
      EinvoiceForm,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate
}

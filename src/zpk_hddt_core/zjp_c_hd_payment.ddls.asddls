@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS for HDDT Payment method'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HD_PAYMENT
  provider contract transactional_query
  as projection on ZJP_R_HD_PAYMENT
{
  key Companycode,
  key Zlsch,
      Paymtext,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate
}

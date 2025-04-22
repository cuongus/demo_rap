@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS for HDDT Document type'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HD_DOCTYPE
  provider contract transactional_query
  as projection on ZJP_R_HD_DOCTYPE
{
  key Companycode,
  key Accountingdocumenttype,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate
}

@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS for HDDT GLaccount'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HD_GLACC
  provider contract transactional_query
  as projection on ZJP_R_HD_GLACC
{
  key Companycode,
  key Glaccount,
      Glacctype,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate
}

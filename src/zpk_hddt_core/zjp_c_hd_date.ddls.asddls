@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS for HDDT Date'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HD_DATE
  provider contract transactional_query
  as projection on ZJP_R_HD_DATE
{
  key Datetype,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate
}

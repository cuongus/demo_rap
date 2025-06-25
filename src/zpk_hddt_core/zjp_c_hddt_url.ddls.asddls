@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS Config HDDT Url'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HDDT_URL
  provider contract transactional_query
  as projection on ZJP_R_HDDT_URL
{
  key IdSys,
  key Action,
      UrlValue,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate
}

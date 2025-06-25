@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Convert Json Viettel'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_VIETTEL_JSON
  provider contract transactional_query
  as projection on ZJP_R_VIETTEL_JSON
{
  key TagMain,
  key TagName,
      Value,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate
}

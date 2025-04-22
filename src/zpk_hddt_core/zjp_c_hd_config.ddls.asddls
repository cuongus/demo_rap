@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS for config domains HDDT'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HD_CONFIG
  provider contract transactional_query
  as projection on ZJP_R_hd_CONFIG
{
  key IdSys,
  key IdDomain,
  key Value,
      Description,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate
}

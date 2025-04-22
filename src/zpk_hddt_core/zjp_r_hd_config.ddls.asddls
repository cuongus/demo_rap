@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base CDS for config domains HDDT'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZJP_R_HD_CONFIG
  as select from zjp_hd_config
  //composition of target_data_source_name as _association_name
{
  key id_sys        as IdSys,
  key id_domain     as IdDomain,
  key value         as Value,
      description   as Description,
      @Semantics.user.createdBy: true
      createdbyuser as Createdbyuser,
      @Semantics.systemDateTime.createdAt: true
      createddate   as Createddate,
      @Semantics.user.localInstanceLastChangedBy: true
      changedbyuser as Changedbyuser,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      changeddate   as Changeddate
}

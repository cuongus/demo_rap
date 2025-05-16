@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Status Invoice Response Text'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZJP_CFG_STATUSINV
  as select from zjp_hd_config
{
  key id_sys        as IdSys,
  key id_domain     as IdDomain,
  key value         as Value,
      description   as Description,
      createdbyuser as Createdbyuser,
      createddate   as Createddate,
      changedbyuser as Changedbyuser,
      changeddate   as Changeddate
}
where
      id_domain = 'STATUSINV'
  and id_sys    = 'FPT'

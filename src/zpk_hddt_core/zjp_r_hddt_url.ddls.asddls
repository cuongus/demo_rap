@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Views Config HDDT Url'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZJP_R_HDDT_URL
  as select from zjp_hddt_url
  //  composition of target_data_source_name as _association_name
{
  key id_sys        as IdSys,
  key action        as Action,
      url_value     as UrlValue,
      @Semantics.user.createdBy: true
      createdbyuser as Createdbyuser,
      @Semantics.systemDateTime.createdAt: true
      createddate   as Createddate,
      @Semantics.user.lastChangedBy: true
      changedbyuser as Changedbyuser,
      @Semantics.systemDateTime.lastChangedAt: true
      changeddate   as Changeddate
      //    _association_name // Make association public
}

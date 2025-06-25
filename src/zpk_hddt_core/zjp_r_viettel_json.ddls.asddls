@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Convert Json Viettel'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZJP_R_VIETTEL_JSON
  as select from zjp_viettel_json
  //  composition of target_data_source_name as _association_name
{
  key tagmain       as TagMain,
  key tagname       as TagName,
      value         as Value,
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

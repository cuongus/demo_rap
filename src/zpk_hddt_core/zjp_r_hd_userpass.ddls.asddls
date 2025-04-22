@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base CDS for HDDT userpass'
@Metadata.ignorePropagatedAnnotations: true
define root view entity zjp_r_HD_USERPASS
  as select from zjp_hd_userpass
  //  composition of target_data_source_name as _association_name
{
  key companycode   as Companycode,
  key usertype      as Usertype,
      username      as Username,
      password      as Password,
      suppliertax   as Suppliertax,
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

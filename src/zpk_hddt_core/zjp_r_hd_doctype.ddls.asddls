@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base CDS for HDDT Document type'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZJP_R_HD_DOCTYPE
  as select from zjp_hd_doctype
  //composition of target_data_source_name as _association_name
{
  key companycode            as Companycode,
  key accountingdocumenttype as Accountingdocumenttype,
      @Semantics.user.createdBy: true
      createdbyuser          as Createdbyuser,
      @Semantics.systemDateTime.createdAt: true
      createddate            as Createddate,
      @Semantics.user.lastChangedBy: true
      changedbyuser          as Changedbyuser,
      @Semantics.systemDateTime.lastChangedAt: true
      changeddate            as Changeddate
      //    _association_name // Make association public
}

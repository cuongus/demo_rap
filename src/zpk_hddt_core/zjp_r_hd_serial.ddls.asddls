@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base CDS for HDDT Serial'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZJP_R_HD_SERIAL
  as select from zjp_hd_serial
  //composition of target_data_source_name as _association_name
{
  key companycode    as Companycode,
  key fiscalyear     as Fiscalyear,
  key einvoicetype   as EinvoiceType,
      einvoiceserial as EinvoiceSerial,
      einvoiceform   as EinvoiceForm,
      @Semantics.user.createdBy: true
      createdbyuser  as Createdbyuser,
      @Semantics.systemDateTime.createdAt: true
      createddate    as Createddate,
      @Semantics.user.lastChangedBy: true
      changedbyuser  as Changedbyuser,
      @Semantics.systemDateTime.lastChangedAt: true
      changeddate    as Changeddate
      //    _association_name // Make association public
}

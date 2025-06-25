@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS for TaxCode'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HD_TAXCODE
  provider contract transactional_query
  as projection on ZJP_R_HD_TAXCODE
{
  key Companycode,
  key Currency,
  key Taxcode,
      Taxpercentage,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate
}

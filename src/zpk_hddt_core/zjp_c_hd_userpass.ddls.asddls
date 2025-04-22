@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS for HDDT Userpass'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HD_USERPASS
  provider contract transactional_query as projection on zjp_r_HD_USERPASS
{
    key Companycode,
    key Usertype,
    Username,
    Password,
    Suppliertax,
    Createdbyuser,
    Createddate,
    Changedbyuser,
    Changeddate
}

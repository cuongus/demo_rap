@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report'
define root view entity ZI_REPORT as select from ztb_report
composition [0..*] of ZI_rp_item as _rp_item
{
    key rp_id as RpId,
    rp_name as RpName,
    rp_code as RpCode,
    @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedat,
    _rp_item // Make association public
    
}

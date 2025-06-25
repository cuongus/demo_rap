@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report'
@Metadata.allowExtensions: true
define root view entity ZC_REPORT
  provider contract transactional_query
  as projection on ZI_REPORT
{   

  key RpId,
      RpCode,
      RpName,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedat,
      /* Associations */
      _rp_item : redirected to composition child ZC_RP_ITEM
}

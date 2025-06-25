@EndUserText.label: 'Report Item'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_RP_ITEM as projection on ZI_rp_item
{
    key RpId,
    key ItemId,
    ItemCode,
    ItemCode1,
    DisplayCode,
    ItemDesc,
    ItemCond,
    ItemCond2,
    ItemCond3,
    ItemCond4,
    Formula,
    Display,
    /* Associations */
    _Report : redirected to parent ZC_REPORT
}

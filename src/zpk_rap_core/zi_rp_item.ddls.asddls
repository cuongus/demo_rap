@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report Item'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_rp_item as select from ztb_rp_item
association        to parent ZI_REPORT  as _Report         on  $projection.RpId = _Report.RpId
{ 
    key rp_id as RpId,
    key item_id as ItemId,
    item_code as ItemCode,
    item_code1 as ItemCode1,
    display_code as DisplayCode,
    item_desc as ItemDesc,
    item_cond as ItemCond,
    item_cond2 as ItemCond2,
    item_cond3 as ItemCond3,
    item_cond4 as ItemCond4,
    formula as Formula,
    display as Display,
    _Report
}

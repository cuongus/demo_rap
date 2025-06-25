@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection CDS for HDDT GLaccount'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZJP_C_HD_GLACC
  provider contract transactional_query
  as projection on ZJP_R_HD_GLACC
  association [0..1] to ZJP_CFG_GLACC as _GlacctypeText on _GlacctypeText.Value = $projection.Glacctype
{
  key Companycode,
  key Glaccount,
      @Consumption.valueHelpDefinition:[
      { entity                       : { name: 'ZJP_CFG_GLACC', element: 'Value' }
      }]
      @ObjectModel.text.element: [ '_GlacctypeText.Description' ]
      Glacctype,
      Createdbyuser,
      Createddate,
      Changedbyuser,
      Changeddate,
      _GlacctypeText
}

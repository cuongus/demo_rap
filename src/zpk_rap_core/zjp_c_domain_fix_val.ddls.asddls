@EndUserText.label: 'Custom entity for Search Help'
@ObjectModel.resultSet.sizeCategory: #XS
@ObjectModel.query.implementedBy: 'ABAP:ZCL_GET_DOMAIN_FIX_VALUES'
define custom entity ZJP_C_DOMAIN_FIX_VAL
  // with parameters parameter_name : parameter_type
{
      @EndUserText.label     : 'domain name'
      @UI.hidden  : true
  key domain_name : sxco_ad_object_name;
      @UI.hidden  : true
  key pos         : abap.numc( 4 );
      @EndUserText.label     : 'lower_limit'
      @UI.lineItem: [{ position: 10 }]
      @UI.identification: [{position: 10 }]
      @ObjectModel.text.element: ['description']
      low         : abap.char( 10 );
      //      @EndUserText.label     : 'upper_limit'
      @UI.hidden  : true
      high        : abap.char(10);
      @EndUserText.label     : 'Description'
      @UI.lineItem: [{ position: 20 }]
      @UI.identification: [{ position: 20 }]
      description : abap.char(60);

}

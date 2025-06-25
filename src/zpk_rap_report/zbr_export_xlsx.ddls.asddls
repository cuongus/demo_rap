@EndUserText.label: 'Export Excel'
@Metadata.allowExtensions: true
define abstract entity zbr_export_xlsx
  //  with parameters parameter_name : parameter_type
{
      //  FileName : abap.char(255);
      //  @Semantics.mimeType: true
      //  MimeType : abap.char(255);
      //  @Consumption.semanticObject: 'FileDownload'
      //  Value    : zde_attachment;

  key skey          : abap.char(32);
      @EndUserText.label: 'Attachments'
      @Semantics.largeObject:{
          mimeType  : 'Mimetype',
          fileName  : 'Filename',
          contentDispositionPreference: #INLINE
      }
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_FILETEMP_VIRTUAL'
      Attachment    : zde_attachment;
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_FILETEMP_VIRTUAL'
      @EndUserText.label: 'File Type'
      @Semantics.mimeType: true
      Mimetype      : abap.char(255);
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_FILETEMP_VIRTUAL'
      @EndUserText.label: 'File Name'
      Filename      : abap.char(100);
      ContentBase64 : abap.string; // BASE64 encoded string
}

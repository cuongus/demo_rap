@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Manage File FI'
//@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@Search.searchable: true
define root view entity ZJP_C_FILE_FI
  provider contract transactional_query
  as projection on ZJP_R_FILE_FI as FileUpload
{
  key Uuid,
  key EndUser,
      Zcnt,
      Status,
      Attachment,
      Mimetype,
      @Search.defaultSearchElement: true
      @Semantics.text: true
      Filename,
      @Search.defaultSearchElement: true
      Createdbyuser,
      @Search.defaultSearchElement: true
      Createddate,
      @Search.defaultSearchElement: true
      Changedbyuser,
      @Search.defaultSearchElement: true
      Changeddate,
      _DataFile : redirected to composition child ZJP_C_DATAFILE_FI
}

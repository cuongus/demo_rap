@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Manage File FI'
//@Metadata.ignorePropagatedAnnotations: true
define root view entity ZJP_R_FILE_FI
  as select from zuser_upload_fi as FileUpload
  composition [0..*] of ZJP_R_DATAFILE_FI as _DataFile
{
  key uuid          as Uuid,
  key end_user      as EndUser,
      zcnt          as Zcnt,
      status        as Status,
      @Semantics.largeObject: { mimeType: 'Mimetype',
                                fileName: 'Filename',
                                contentDispositionPreference: #INLINE }
      attachment    as Attachment,
      @Semantics.mimeType: true
      mimetype      as Mimetype,
      filename      as Filename,
      @Semantics.user.createdBy: true
      createdbyuser as Createdbyuser,
      @Semantics.systemDateTime.createdAt: true
      createddate   as Createddate,
      @Semantics.user.lastChangedBy: true
      changedbyuser as Changedbyuser,
      @Semantics.systemDateTime.lastChangedAt: true
      changeddate   as Changeddate,
      //      _association_name // Make association public
      _DataFile
}

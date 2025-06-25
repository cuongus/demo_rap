CLASS zcl_file_download DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FILE_DOWNLOAD IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA: file_id     TYPE string,
          lv_filename TYPE string,
          lv_mimetype TYPE string,
          lv_data     TYPE xstring.

    file_id = request->get_form_field( 'file_id' ).

    SELECT SINGLE * FROM zfile_download WHERE zguid = @file_id INTO @data(ls_file).
    if sy-subrc EQ 0.
        lv_data = ls_file-file_content.
    ENDIF.
    " Set headers
    response->set_header_field( i_name = 'content-disposition'
                                i_value = |attachment; filename="{ lv_filename }"| ).
    response->set_content_type( lv_mimetype ).
    response->set_binary( lv_data ).
  ENDMETHOD.
ENDCLASS.

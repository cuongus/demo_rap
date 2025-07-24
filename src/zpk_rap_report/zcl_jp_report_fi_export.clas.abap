CLASS zcl_jp_report_fi_export DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*    TYPES:
*      key_sqtm      TYPE TABLE FOR ACTION IMPORT zjp_c_soquytienmat~btnexportexcel,
*      result_sqtm   TYPE TABLE FOR ACTION RESULT zjp_c_soquytienmat~btnexportexcel,
*      mapped_sqtm   TYPE RESPONSE FOR MAPPED EARLY zjp_c_soquytienmat,
*      failed_sqtm   TYPE RESPONSE FOR FAILED EARLY zjp_c_soquytienmat,
*      reported_sqtm TYPE RESPONSE FOR REPORTED EARLY zjp_c_soquytienmat.

    CLASS-METHODS:
      btnexportexcel
*        IMPORTING keys     TYPE key_sqtm
*        CHANGING  result   TYPE result_sqtm
*                  mapped   TYPE mapped_sqtm
*                  failed   TYPE failed_sqtm
*                  reported TYPE reported_sqtm
                  .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JP_REPORT_FI_EXPORT IMPLEMENTATION.


  METHOD btnexportexcel.
    DATA: lt_data      TYPE TABLE OF zjp_c_soquytienmat.
    " The write access to the worksheet.
    DATA lo_worksheet TYPE REF TO if_xco_xlsx_wa_worksheet.
    DATA: lo_cursor    TYPE REF TO if_xco_xlsx_wa_cursor.
    DATA: lv_row       TYPE i VALUE 0.

    DATA(lo_write_access) = xco_cp_xlsx=>document->empty( )->write_access( ).
    lo_worksheet = lo_write_access->get_workbook( )->worksheet->at_position( 1 ).

    " Prepare demo data
    lt_data = VALUE #( ( stt = 1 PostingDate = '20250101' DocumentDate = '20250101'
                         AccountingDocument = '10000001' SoHieuCTThu = 'THU01'
                         Diengiai = 'Thu tiền mặt' sophatsinhthu = '1000000'
                         sophatsinhchi = '0' Sodu = '1000000' GhiChu = 'Giao dịch 1' ) ).

    lt_data = VALUE #( ( stt = 2 PostingDate = '20250101' DocumentDate = '20250101'
    AccountingDocument = '20000001' SoHieuCTThu = 'THU02'
    Diengiai = 'Thu tiền mặt' sophatsinhthu = '2000000'
    sophatsinhchi = '0' Sodu = '2000000' GhiChu = 'Giao dịch 2' ) ).

    "Tên Cty
    lv_row = 1. " bắt đầu từ dòng 1
    lo_cursor = lo_worksheet->cursor(
        io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'A' )
        io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_row )
      ).
    lo_cursor->get_cell( )->value->write_from( 'TEST Công ty Cổ phần CASLA' ).
    "Địa chỉ
    lo_cursor->move_down( )->get_cell( )->value->write_from( 'TEST Công ty Cổ phần CASLA' ).

    "Mẫu Số
    lv_row = 1. " bắt đầu từ dòng 1 Column K
    lo_cursor = lo_worksheet->cursor(
        io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'K' )
        io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_row )
      ).
    lo_cursor->get_cell( )->value->write_from( 'Mẫu số S07-DN' ).
    lo_cursor->move_down( )->get_cell( )->value->write_from( '(Ban hành theo thông tư số 200/2014/TT-BTC' ).
    lo_cursor->move_down( )->get_cell( )->value->write_from( 'Ngày 22/12/2014 của Bộ Tài Chính)' ).

    "Title Báo cáo
    lv_row = 4. " bắt đầu từ dòng 4 Column G
    lo_cursor = lo_worksheet->cursor(
        io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'G' )
        io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_row )
      ).
    lo_cursor->get_cell( )->value->write_from( 'SỔ QUỸ TIỀN MẶT' ).
    lo_cursor->move_down( )->get_cell( )->value->write_from( 'Từ ngày: 20/10/2020 đến ngày 20/11/2020' ).
    lo_cursor->move_down( )->get_cell( )->value->write_from( 'Tài khoản: 1111100000 - Tiền Việt Nam' ).

    lv_row = 13. " bắt đầu từ dòng 13
    LOOP AT lt_data INTO DATA(ls_data).
      lo_cursor = lo_worksheet->cursor(
          io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'A' )
          io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_row )
        ).
      "STT
      lo_cursor->get_cell( )->value->write_from( ls_data-stt ).
      "Ngày, tháng ghi sổ
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-DocumentDate ).
      "Ngày, tháng chứng từ
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-PostingDate ).
      "Số chứng từ
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-AccountingDocument ).
      "Số hiệu chứng từ Thu
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-SoHieuCTThu ).
      "Số hiệu chứng từ Chi
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-SoHieuCTChi ).
      "Diễn giải
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-Diengiai ).
      "Số phát sinh ( VND )
      "Thu
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-sophatsinhthu ).
      "Chi
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-sophatsinhchi ).
      "Tồn
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-Sodu ).
      "Số phát sinh ( trên giao dịch )
      "Thu
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-sophatsinhthu_nt ).
      "Chi
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-sophatsinhchi_nt ).
      "Tồn
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-Sodu_NT ).
      "Ghi Chú
      lo_cursor->move_right( )->get_cell( )->value->write_from( ls_data-GhiChu ).

      "Next Row
      lv_row += 1.

    ENDLOOP.

    lv_row = 17.
    lo_cursor = lo_worksheet->cursor(
          io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'A' )
          io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_row )
        ).
    "- Sổ này có … trang, đánh số từ trang 01 đến trang …
    lo_cursor->get_cell( )->value->write_from( '- Sổ này có … trang, đánh số từ trang 01 đến trang …' ).
    "- Ngày mở sổ:
    lo_cursor->move_down( )->get_cell( )->value->write_from( '- Ngày mở sổ:' ).

    "- Ngày … tháng … năm
    lv_row = 19.
    lo_cursor = lo_worksheet->cursor(
          io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'M' )
          io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_row )
        ).
    lo_cursor->get_cell( )->value->write_from( 'Ngày … tháng … năm' ).

    "- Thủ quỹ:
    lv_row = 20.
    lo_cursor = lo_worksheet->cursor(
          io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'C' )
          io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_row )
        ).
    lo_cursor->get_cell( )->value->write_from( 'Thủ quỹ' ).
    lo_cursor->move_down( )->get_cell( )->value->write_from( '(Ký, họ tên)' ).

    "-Kế toán trưởng
    lv_row = 20.
    lo_cursor = lo_worksheet->cursor(
          io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'G' )
          io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_row )
        ).
    lo_cursor->get_cell( )->value->write_from( 'Kế toán trưởng' ).
    lo_cursor->move_down( )->get_cell( )->value->write_from( '(Ký, họ tên)' ).

    "-Giám đốc
    lv_row = 20.
    lo_cursor = lo_worksheet->cursor(
          io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'M' )
          io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_row )
        ).
    lo_cursor->get_cell( )->value->write_from( 'Giám đốc' ).
    lo_cursor->move_down( )->get_cell( )->value->write_from( '(Ký, họ tên, đóng dấu)' ).

    DATA(ld_excel) = lo_write_access->get_file_content( ).

*    APPEND VALUE #(
*                    %param-FileName = 'SoQuyTienMat.xlsx'
*                    %param-MimeType = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
*                    %param-Value    = ld_excel ) TO result.

    DATA: lv_encoded TYPE string.

    cl_web_http_utility=>encode_x_base64(
      EXPORTING
        unencoded = ld_excel
      RECEIVING
        encoded = lv_encoded
    ).

    TRY.
        DATA(lv_guid) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        "handle exception
    ENDTRY.

*    APPEND VALUE #( %param-skey          = lv_guid
*                    %param-attachment    = ld_excel
*                    %param-filename      = 'SoQuyTienMat.xlsx'
*                    %param-mimetype      = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
*                    %param-contentbase64 = lv_encoded ) TO result.

*            cl_demo_output=>write_data( lo_write_access ).
*
**             HTML-Code vom Demo-Output holen
*            DATA(lv_html) = cl_demo_output=>get( ).
*
**             Daten im Inline-Browser im SAP-Fenster anzeigen
*            cl_abap_browser=>show_html( EXPORTING title       = 'Excel'
*                                                  html_string = lv_html
*                                                  container   = cl_gui_container=>default_screen ).
*
*    TRY.
*        DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).
*
*        CONSTANTS c_sender   TYPE cl_bcs_mail_message=>ty_address VALUE ''.
*        CONSTANTS c_receiver TYPE cl_bcs_mail_message=>ty_address VALUE ''.
*
*        lo_mail->set_sender( c_sender ).
*        lo_mail->add_recipient( c_receiver ).
*
*        lo_mail->set_subject( 'New Excel File' ).
*
*        lo_mail->set_main( cl_bcs_mail_textpart=>create_instance(
*                               iv_content      = '<h1>Hello,</h1><p>here you fresh printed Excel File</p>'
*                               iv_content_type = 'text/html' ) ).
*
*        lo_mail->add_attachment(
*            cl_bcs_mail_binarypart=>create_instance(
*                iv_content      = ld_excel
*                iv_content_type = `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`
*                iv_filename     = `My-generated-Excel.xlsx` ) ).
*
*        lo_mail->send( ).
*      CATCH cx_bcs_mail.
*    ENDTRY.


  ENDMETHOD.
ENDCLASS.

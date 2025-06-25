CLASS zcl_gen_data_cf01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GEN_DATA_CF01 IMPLEMENTATION.


METHOD if_oo_adt_classrun~main.
    DATA: ls_report type ztb_report,
          ls_rp_item type ztb_rp_item,
          lt_rp_item type TABLE OF ztb_rp_item.
    DELETE FROM ztb_report WHERE rp_id = '001' .
    DELETE FROM ztb_rp_item WHERE rp_id = '001' .

    ls_report-rp_id = '001'.
    ls_report-rp_code = 'ZCF1'.
    ls_report-rp_name = 'Báo cáo lưu chuyển tiền tệ theo phương pháp trực tiếp'.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0010'
  item_code1 = ' 0010'
  display_code = ' '
  item_desc = 'I. Lưu chuyển tiền từ hoạt động kinh doanh'
  item_cond = ''
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0020'
  item_code1 = ' 0020'
  display_code = ' 1'
  item_desc = '1. Tiền thu từ bán hàng, cung cấp dịch vụ và doanh thu khác'
  item_cond = '01'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0030'
  item_code1 = ' 0030'
  display_code = ' 2'
  item_desc = '2. Tiền chi trả cho người cung cấp hàng hóa và dịch vụ'
  item_cond = '02'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0040'
  item_code1 = ' 0040'
  display_code = ' 3'
  item_desc = '3. Tiền chi trả cho người lao động'
  item_cond = '03'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0050'
  item_code1 = ' 0050'
  display_code = ' 4'
  item_desc = '4. Tiền chi trả lãi vay'
  item_cond = '04'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0060'
  item_code1 = ' 0060'
  display_code = ' 5'
  item_desc = '5. Tiền chi nộp thuế thu nhập doanh nghiệp'
  item_cond = '05'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0070'
  item_code1 = ' 0070'
  display_code = ' 6'
  item_desc = '6. Tiền thu khác từ hoạt động kinh doanh'
  item_cond = '06'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0080'
  item_code1 = ' 0080'
  display_code = ' 7'
  item_desc = '7. Tiền chi khác cho hoạt động kinh doanh'
  item_cond = '07'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0090'
  item_code1 = ' 0090'
  display_code = ' 20'
  item_desc = 'Lưu chuyển tiền thuần từ hoạt động kinh doanh'
  item_cond = ''
  formula   = '20,30,40,50,60,70,80' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0100'
  item_code1 = ' 0100'
  display_code = ' '
  item_desc = 'II. Lưu chuyển tiền từ hoạt động đầu tư'
  item_cond = ''
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0110'
  item_code1 = ' 0110'
  display_code = ' 21'
  item_desc = '1. Tiền chi để mua sắm, xây dựng tscđ và các tài sản dài hạn khác'
  item_cond = '21'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0120'
  item_code1 = ' 0120'
  display_code = ' 22'
  item_desc = '2. Tiền thu từ thanh lý, nhượng bán tscđ và các tài sản dài hạn khác'
  item_cond = '22'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0130'
  item_code1 = ' 0130'
  display_code = ' 23'
  item_desc = '3. Tiền chi cho vay, mua các công cụ nợ của đơn vị khác'
  item_cond = '23'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0140'
  item_code1 = ' 0140'
  display_code = ' 24'
  item_desc = '4. Tiền thu hồi cho vay, bán lại các công cụ nợ của đơn vị khác'
  item_cond = '24'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0150'
  item_code1 = ' 0150'
  display_code = ' 25'
  item_desc = '5.Tiền chi đầu tư góp vốn vào đơn vị khác'
  item_cond = '25'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0160'
  item_code1 = ' 0160'
  display_code = ' 26'
  item_desc = '6. Tiền thu hồi đầu tư góp vốn vào đơn vị khác'
  item_cond = '26'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0170'
  item_code1 = ' 0170'
  display_code = ' 27'
  item_desc = '7. Tiền thu lãi cho vay, cổ tức và lợi nhuận được chia'
  item_cond = '27'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0180'
  item_code1 = ' 0180'
  display_code = ' 30'
  item_desc = 'Lưu chuyển tiền thuần từ hoạt động đầu tư'
  item_cond = ''
  formula   = '110,120,130,140,150,160,170' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0190'
  item_code1 = ' 0190'
  display_code = ' '
  item_desc = 'III. Lưu chuyển tiền từ hoạt động tài chính'
  item_cond = ''
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0200'
  item_code1 = ' 0200'
  display_code = ' 31'
  item_desc = '1. Tiền thu từ phát hành cổ phiếu, nhận vốn góp của chủ sở hữu'
  item_cond = '31'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0210'
  item_code1 = ' 0210'
  display_code = ' 32'
  item_desc = '2. Tiền chi trả vốn góp cho các chủ sở hữu, mua lại cổ phiếu của doanh'
  item_cond = '32'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0220'
  item_code1 = ' 0220'
  display_code = ' 33'
  item_desc = '3. Tiền vay ngắn hạn, dài hạn nhận được'
  item_cond = '33'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0230'
  item_code1 = ' 0230'
  display_code = ' 34'
  item_desc = '4. Tiền chi trả nợ gốc vay'
  item_cond = '34'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0240'
  item_code1 = ' 0240'
  display_code = ' 35'
  item_desc = '5. Tiền chi trả nợ thuê tài chính'
  item_cond = '35'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0250'
  item_code1 = ' 0250'
  display_code = ' 36'
  item_desc = '6. Cổ tức, lợi nhuận đã trả cho chủ sở hữu'
  item_cond = '36'
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0260'
  item_code1 = ' 0260'
  display_code = ' 40'
  item_desc = 'Lưu chuyển tiền thuần từ hoạt động tài chính'
  item_cond = ''
  formula   = '200,210,220,230,240,250,' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0270'
  item_code1 = ' 0270'
  display_code = ' 50'
  item_desc = 'Lưu chuyển tiền thuần trong kỳ (50 = 20+30+40)'
  item_cond = ''
  formula   = '300,-290,-280' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0280'
  item_code1 = ' 0280'
  display_code = ' 60'
  item_desc = 'Tiền và tương đương tiền đầu kỳ'
  item_cond = ''
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0290'
  item_code1 = ' 0290'
  display_code = ' 61'
  item_desc = 'Ảnh hưởng của thay đổi tỷ giá hối đoái quy đổi ngoại tệ'
  item_cond = ''
  formula   = '' ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '001'
  item_id   = '0300'
  item_code1 = ' 0300'
  display_code = ' 70'
  item_desc = 'Tiền và tương đương tiền cuối kỳ (70 = 50+60+61)'
  item_cond = ''
  formula   = '' ). append ls_rp_item to lt_rp_item.

    "insert data
    INSERT ztb_report FROM @ls_report.
    INSERT ztb_rp_item FROM TABLE @lt_rp_item.
    COMMIT WORK.
    out->write( |Complete| ).
  ENDMETHOD.
ENDCLASS.

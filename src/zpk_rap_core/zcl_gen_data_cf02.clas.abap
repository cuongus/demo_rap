CLASS zcl_gen_data_cf02 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GEN_DATA_CF02 IMPLEMENTATION.


METHOD if_oo_adt_classrun~main.
    DATA: ls_report type ztb_report,
          ls_rp_item type ztb_rp_item,
          lt_rp_item type TABLE OF ztb_rp_item.
    DELETE FROM ztb_report WHERE rp_id = '002' .
    DELETE FROM ztb_rp_item WHERE rp_id = '002' .

    ls_report-rp_id = '002'.
    ls_report-rp_name = 'Báo cáo lưu chuyển tiền tệ gián tiếp'.
    ls_report-rp_code = 'ZCF2'.

    ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0010'
  item_code1 = ' 0010'
  display_code = ' '
  item_desc = 'I. Lưu chuyển tiền từ hoạt động kinh doanh'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0020'
  item_code1 = ' 0020'
  display_code = ' 01'
  item_desc = '1. Lợi nhuận trước thuế'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(110,170)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0030'
  item_code1 = ' 0030'
  display_code = ' '
  item_desc = 'LN Thuần từ HĐKD'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(120,130,-140,-150,-160)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0040'
  item_code1 = ' 0040'
  display_code = ' '
  item_desc = 'Lợi nhuận gộp về BH CCDV'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(121,-124)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0050'
  item_code1 = ' 0050'
  display_code = ' '
  item_desc = 'Doanh thu thuần về BH và CCDV'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(122,-123)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0060'
  item_code1 = ' 0060'
  display_code = ' '
  item_desc = 'Doanh thu bán hàng'
  item_cond = '511' item_cond2 = '911,521' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0070'
  item_code1 = ' 0070'
  display_code = ' '
  item_desc = 'Giá vốn hàng bán'
  item_cond = '632' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0080'
  item_code1 = ' 0080'
  display_code = ' '
  item_desc = 'Doanh thu hoạt động tài chính'
  item_cond = '515' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0090'
  item_code1 = ' 0090'
  display_code = ' '
  item_desc = 'Chi phí tài chính'
  item_cond = '635' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0100'
  item_code1 = ' 0100'
  display_code = ' '
  item_desc = 'Chi phí bán hàng'
  item_cond = '641' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0110'
  item_code1 = ' 0110'
  display_code = ' '
  item_desc = 'Chi phí quản lý doanh nghiệp'
  item_cond = '642' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0120'
  item_code1 = ' 0120'
  display_code = ' '
  item_desc = 'Lợi nhuận khác'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(171,-172)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0130'
  item_code1 = ' 0130'
  display_code = ' '
  item_desc = 'Thu nhập khác'
  item_cond = '711' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0140'
  item_code1 = ' 0140'
  display_code = ' '
  item_desc = 'Chi phí khác'
  item_cond = '811' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0150'
  item_code1 = ' 0150'
  display_code = ' '
  item_desc = '2. Điều chỉnh cho các khoản'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0160'
  item_code1 = ' 0160'
  display_code = ' 02'
  item_desc = '- Khấu hao TSCĐ và BĐSĐT'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(211,-212)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0170'
  item_code1 = ' 0170'
  display_code = ' '
  item_desc = 'Khấu hao tài sản cố định'
  item_cond = '6274,6414,6424' item_cond2 = '214' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0180'
  item_code1 = ' 0180'
  display_code = ' '
  item_desc = 'Loại trừ các TK CP khấu hao TS sự nghiệp'
  item_cond = '6274003,6414003,6424003' item_cond2 = '214' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0190'
  item_code1 = ' 0190'
  display_code = ' 03'
  item_desc = '- Các khoản dự phòng'
  item_cond = '229,352' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0200'
  item_code1 = ' 0200'
  display_code = ' 04'
  item_desc = '- Lãi, lỗ chênh lệch tỷ giá hối đoái do đánh giá lại các khoản mục tiền tệ có gốc ngoại tệ'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(231,-232)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0210'
  item_code1 = ' 0210'
  display_code = ' '
  item_desc = 'PS Có 515 đối ứng 413'
  item_cond = '5151001010' item_cond2 = '4131' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0220'
  item_code1 = ' 0220'
  display_code = ' '
  item_desc = 'PS Nợ 635 đối ứng 413'
  item_cond = '6351001000' item_cond2 = '4131' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0230'
  item_code1 = ' 0230'
  display_code = ' 05'
  item_desc = '- Lãi, lỗ từ hoạt động đầu tư'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(241,-242,-243,244)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0240'
  item_code1 = ' 0240'
  display_code = ' '
  item_desc = 'Lãi từ hoạt động đầu tư'
  item_cond = '5151001050,5151001060,5151001070,5151001080,5151001020,5151001040' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0250'
  item_code1 = ' 0250'
  display_code = ' '
  item_desc = 'Lỗ từ hoạt động đầu tư'
  item_cond = '6351002000,6351004000,6351005000,6351006000,6351007000,6351008000' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0260'
  item_code1 = ' 0260'
  display_code = ' '
  item_desc = 'Thanh lý, nhượng bán tài sản cố định'
  item_cond = '8111001000' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0270'
  item_code1 = ' 0270'
  display_code = ' '
  item_desc = 'Thu về nhượng bán, thanh lý TSCĐ'
  item_cond = '7111001000' item_cond2 = '911' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0280'
  item_code1 = ' 0280'
  display_code = ' 06'
  item_desc = '- Chi phí lãi vay'
  item_cond = '6351005000' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0290'
  item_code1 = ' 0290'
  display_code = ' 07'
  item_desc = '- Các khoản điều chỉnh khác'
  item_cond = '356,357' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0300'
  item_code1 = ' 0300'
  display_code = ' 08'
  item_desc = '3. Lợi nhuận từ hoạt động kinh doanh trước thay đổi vốn lưu động'
  item_cond = '' item_cond2 = '' item_cond3 = 'DR'
  formula   = 'SUM(100,210,220,230,240,250,260)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0310'
  item_code1 = ' 0310'
  display_code = ' 09'
  item_desc = '- Tăng, giảm Các khoản phải thu'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(312,-311)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0320'
  item_code1 = ' 0320'
  display_code = ' '
  item_desc = 'Dư có cuối kì (1311,133,136,138,141,244,3312)'
  item_cond = '1311*,133*,136*,1381001000,1388005000,1388008000,1388008999,1389003000,141*,2441001000,2441008000,2449003000,3312001000' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0330'
  item_code1 = ' 0330'
  display_code = ' '
  item_desc = 'Dư có đầu kì (1311,133,136,138,141,244,3312)'
  item_cond = '1311*,133*,136*,1381001000,1388005000,1388008000,1388008999,1389003000,141*,2441001000,2441008000,2449003000,3312001000' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0340'
  item_code1 = ' 0340'
  display_code = ' 10'
  item_desc = '- Tăng, giảm hàng tồn kho'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(322,-321)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0350'
  item_code1 = ' 0350'
  display_code = ' '
  item_desc = 'Dư Nợ cuối kỳ'
  item_cond = '15' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0360'
  item_code1 = ' 0360'
  display_code = ' '
  item_desc = 'Dư Nợ đầu kỳ'
  item_cond = '15' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0370'
  item_code1 = ' 0370'
  display_code = ' 11'
  item_desc = '- Tăng, giảm Các khoản phải trả (Không kể Lãi vay phải trả, thuế thu nhập doanh nghiệp phải nộp)'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(331,-332,333,339,345,348)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0380'
  item_code1 = ' 0380'
  display_code = ' '
  item_desc = 'Dư Có cuối kỳ (3311,1312)'
  item_cond = '3311,1312' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0390'
  item_code1 = ' 0390'
  display_code = ' '
  item_desc = 'Dư Có đầu kỳ (3311,1312)'
  item_cond = '3311,1312' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0400'
  item_code1 = ' 0400'
  display_code = ' '
  item_desc = 'Chênh lệch TK 333'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(334,-335,-336)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0410'
  item_code1 = ' 0410'
  display_code = ' '
  item_desc = 'Dư Có cuối kỳ TK 333'
  item_cond = '333' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0420'
  item_code1 = ' 0420'
  display_code = ' '
  item_desc = 'Dư Có đầu kỳ TK 333'
  item_cond = '333' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0430'
  item_code1 = ' 0430'
  display_code = ' '
  item_desc = 'Loại trừ TK 3334'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(337,-338)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0440'
  item_code1 = ' 0440'
  display_code = ' '
  item_desc = 'Dư Có cuối kỳ TK 3334 Thuế TNDN'
  item_cond = '3334' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0450'
  item_code1 = ' 0450'
  display_code = ' '
  item_desc = 'Dư Có đầu kỳ TK 3334 Thuế TNDN'
  item_cond = '3334' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0460'
  item_code1 = ' 0460'
  display_code = ' '
  item_desc = 'Chênh lệch TK 335'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(340,-341,-342)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0470'
  item_code1 = ' 0470'
  display_code = ' '
  item_desc = 'Dư Có cuối kỳ TK 335'
  item_cond = '335' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0480'
  item_code1 = ' 0480'
  display_code = ' '
  item_desc = 'Dư Có đầu kỳ TK 335'
  item_cond = '335' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0490'
  item_code1 = ' 0490'
  display_code = ' '
  item_desc = 'Loại trừ TK 3351001 - Lãi vay'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(343,-344)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0500'
  item_code1 = ' 0500'
  display_code = ' '
  item_desc = 'Dư Có cuối kỳ TK 3351001 - Lãi vay'
  item_cond = '3351001' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0510'
  item_code1 = ' 0510'
  display_code = ' '
  item_desc = 'Dư Có đầu kỳ TK 3351001 - Lãi vay'
  item_cond = '3351001' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0520'
  item_code1 = ' 0520'
  display_code = ' '
  item_desc = 'Chênh lệch TK 338'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(346,-347)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0530'
  item_code1 = ' 0530'
  display_code = ' '
  item_desc = 'Dư Có cuối kỳ TK 338'
  item_cond = '3381001,3382001,3383001,3384001,3385001,3386001,3387,3388008000,3388008020,3388008999,3389003' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0540'
  item_code1 = ' 0540'
  display_code = ' '
  item_desc = 'Dư Có đầu kỳ TK 338'
  item_cond = '3381001,3382001,3383001,3384001,3385001,3386001,3387,3388008000,3388008020,3388008999,3389003' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0550'
  item_code1 = ' 0550'
  display_code = ' 12'
  item_desc = '- Tăng, giảm Chi phí trả trước'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(342,-341)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0560'
  item_code1 = ' 0560'
  display_code = ' '
  item_desc = 'Dư nợ cuối kỳ'
  item_cond = '242' item_cond2 = '' item_cond3 = 'DZ,FV,KA,KZ,SA'
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0570'
  item_code1 = ' 0570'
  display_code = ' '
  item_desc = 'Dư nợ đầu kỳ'
  item_cond = '242' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0580'
  item_code1 = ' 0580'
  display_code = ' 13'
  item_desc = '- Tăng, giảm chứng khoán kinh doanh'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(372,-371)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0590'
  item_code1 = ' 0590'
  display_code = ' '
  item_desc = 'Dư nợ cuối kỳ'
  item_cond = '121' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0600'
  item_code1 = ' 0600'
  display_code = ' '
  item_desc = 'Dư nợ đầu kỳ'
  item_cond = '121' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0610'
  item_code1 = ' 0610'
  display_code = ' 14'
  item_desc = '- Tiền lãi vay đã trả'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(381,-335)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0620'
  item_code1 = ' 0620'
  display_code = ' '
  item_desc = 'Chi phí lãi vay'
  item_cond = '6351005000' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0630'
  item_code1 = ' 0630'
  display_code = ' '
  item_desc = 'PS TK 335'
  item_cond = '335' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0640'
  item_code1 = ' 0640'
  display_code = ' 15'
  item_desc = '- Thuế thu nhập doanh nghiệp đã nộp'
  item_cond = '3334' item_cond2 = '11' item_cond3 = 'AB,KR,KZ'
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0650'
  item_code1 = ' 0650'
  display_code = ' 16'
  item_desc = '- Tiền thu khác từ hoạt động kinh doanh'
  item_cond = '11' item_cond2 = '461' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0660'
  item_code1 = ' 0660'
  display_code = ' 17'
  item_desc = '- Tiền chi khác cho hoạt động kinh doanh'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(411,412,413)'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0670'
  item_code1 = ' 0670'
  display_code = ' '
  item_desc = 'Chi phúc lợi'
  item_cond = '11' item_cond2 = '353' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0680'
  item_code1 = ' 0680'
  display_code = ' '
  item_desc = 'Chi sự nghiệp'
  item_cond = '11' item_cond2 = '461' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0690'
  item_code1 = ' 0690'
  display_code = ' '
  item_desc = 'I. Lưu chuyển tiền từ hoạt động kinh doanh'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0700'
  item_code1 = ' 0700'
  display_code = ' 1'
  item_desc = '1. Tiền thu từ bán hàng, cung cấp dịch vụ và doanh thu khác'
  item_cond = '01' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0710'
  item_code1 = ' 0710'
  display_code = ' 2'
  item_desc = '2. Tiền chi trả cho người cung cấp hàng hóa và dịch vụ'
  item_cond = '02' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0720'
  item_code1 = ' 0720'
  display_code = ' 3'
  item_desc = '3. Tiền chi trả cho người lao động'
  item_cond = '03' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0730'
  item_code1 = ' 0730'
  display_code = ' 4'
  item_desc = '4. Tiền chi trả lãi vay'
  item_cond = '04' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0740'
  item_code1 = ' 0740'
  display_code = ' 5'
  item_desc = '5. Tiền chi nộp thuế thu nhập doanh nghiệp'
  item_cond = '05' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0750'
  item_code1 = ' 0750'
  display_code = ' 6'
  item_desc = '6. Tiền thu khác từ hoạt động kinh doanh'
  item_cond = '06' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0760'
  item_code1 = ' 0760'
  display_code = ' 7'
  item_desc = '7. Tiền chi khác cho hoạt động kinh doanh'
  item_cond = '07' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0770'
  item_code1 = ' 0770'
  display_code = ' 20'
  item_desc = 'Lưu chuyển tiền thuần từ hoạt động kinh doanh'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = '9020;9030;9040;9050;9060;9070;9080'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0780'
  item_code1 = ' 0780'
  display_code = ' '
  item_desc = 'II. Lưu chuyển tiền từ hoạt động đầu tư'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0790'
  item_code1 = ' 0790'
  display_code = ' 21'
  item_desc = '1. Tiền chi để mua sắm, xây dựng tscđ và các tài sản dài hạn khác'
  item_cond = '21' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0800'
  item_code1 = ' 0800'
  display_code = ' 22'
  item_desc = '2. Tiền thu từ thanh lý, nhượng bán tscđ và các tài sản dài hạn khác'
  item_cond = '22' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0810'
  item_code1 = ' 0810'
  display_code = ' 23'
  item_desc = '3. Tiền chi cho vay, mua các công cụ nợ của đơn vị khác'
  item_cond = '23' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0820'
  item_code1 = ' 0820'
  display_code = ' 24'
  item_desc = '4. Tiền thu hồi cho vay, bán lại các công cụ nợ của đơn vị khác'
  item_cond = '24' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0830'
  item_code1 = ' 0830'
  display_code = ' 25'
  item_desc = '5.Tiền chi đầu tư góp vốn vào đơn vị khác'
  item_cond = '25' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0840'
  item_code1 = ' 0840'
  display_code = ' 26'
  item_desc = '6. Tiền thu hồi đầu tư góp vốn vào đơn vị khác'
  item_cond = '26' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0850'
  item_code1 = ' 0850'
  display_code = ' 27'
  item_desc = '7. Tiền thu lãi cho vay, cổ tức và lợi nhuận được chia'
  item_cond = '27' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0860'
  item_code1 = ' 0860'
  display_code = ' 30'
  item_desc = 'Lưu chuyển tiền thuần từ hoạt động đầu tư'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = '9110;9120;9130;9140;9150;9160;9170'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0870'
  item_code1 = ' 0870'
  display_code = ' '
  item_desc = 'III. Lưu chuyển tiền từ hoạt động tài chính'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0880'
  item_code1 = ' 0880'
  display_code = ' 31'
  item_desc = '1. Tiền thu từ phát hành cổ phiếu, nhận vốn góp của chủ sở hữu'
  item_cond = '31' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0890'
  item_code1 = ' 0890'
  display_code = ' 32'
  item_desc = '2. Tiền chi trả vốn góp cho các chủ sở hữu, mua lại cổ phiếu của doanh'
  item_cond = '32' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0900'
  item_code1 = ' 0900'
  display_code = ' 33'
  item_desc = '3. Tiền vay ngắn hạn, dài hạn nhận được'
  item_cond = '33' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0910'
  item_code1 = ' 0910'
  display_code = ' 34'
  item_desc = '4. Tiền chi trả nợ gốc vay'
  item_cond = '34' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0920'
  item_code1 = ' 0920'
  display_code = ' 35'
  item_desc = '5. Tiền chi trả nợ thuê tài chính'
  item_cond = '35' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0930'
  item_code1 = ' 0930'
  display_code = ' 36'
  item_desc = '6. Cổ tức, lợi nhuận đã trả cho chủ sở hữu'
  item_cond = '36' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0940'
  item_code1 = ' 0940'
  display_code = ' 40'
  item_desc = 'Lưu chuyển tiền thuần từ hoạt động tài chính'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = '9200;9210;9220;9230;9240;9250;'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0950'
  item_code1 = ' 0950'
  display_code = ' 50'
  item_desc = 'Lưu chuyển tiền thuần trong kỳ (50 = 20+30+40)'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = '9300;-9290;-9280'  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0960'
  item_code1 = ' 0960'
  display_code = ' 60'
  item_desc = 'Tiền và tương đương tiền đầu kỳ'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0970'
  item_code1 = ' 0970'
  display_code = ' 61'
  item_desc = 'Ảnh hưởng của thay đổi tỷ giá hối đoái quy đổi ngoại tệ'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0980'
  item_code1 = ' 0980'
  display_code = ' 70'
  item_desc = 'Tiền và tương đương tiền cuối kỳ (70 = 50+60+61)'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = 'X'  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '0990'
  item_code1 = ' 0990'
  display_code = ' '
  item_desc = 'Chênh lệch TK 334'
  item_cond = '' item_cond2 = '' item_cond3 = ''
  formula   = 'SUM(349,-350)'  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '1000'
  item_code1 = ' 1000'
  display_code = ' '
  item_desc = 'Dư Có cuối kỳ TK 334'
  item_cond = '3441001000' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '1010'
  item_code1 = ' 1010'
  display_code = ' '
  item_desc = 'Dư Có đầu kỳ TK 334'
  item_cond = '3441001000' item_cond2 = '' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.
ls_rp_item = VALUE #(
  rp_id     = '002'
  item_id   = '1020'
  item_code1 = ' 1020'
  display_code = ' '
  item_desc = 'các TK CP khấu hao TS sự nghiệp đối ứng với 466'
  item_cond = '6274003,6414003,6424003' item_cond2 = '466' item_cond3 = ''
  formula   = ''  display   = ''  ). append ls_rp_item to lt_rp_item.


    "insert data
    INSERT ztb_report FROM @ls_report.
    INSERT ztb_rp_item FROM TABLE @lt_rp_item.
    COMMIT WORK.
    out->write( |Complete| ).
  ENDMETHOD.
ENDCLASS.

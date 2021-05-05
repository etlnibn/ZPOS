*&---------------------------------------------------------------------*
*& Include MZPOS_SEARCH_RETURN_O02 - Invoice Related Modules
*&---------------------------------------------------------------------*

MODULE tc_return_bask_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_return_basket LINES tc_return_bask-lines.
ENDMODULE.

MODULE tc_return_bask_get_lines OUTPUT.
  g_tc_return_bask_lines = sy-loopc.
ENDMODULE.

MODULE status_9103 OUTPUT.
* View Return Basket
  SET PF-STATUS 'ZPOS_SCANLINE_STATUS'.
  SET TITLEBAR 'ZPOS_RETURN_BASKET'.
ENDMODULE.

MODULE status_9104 OUTPUT.
* Scan GTIN
  SET PF-STATUS 'ZPOS_SCANLINE_STATUS'.
  SET TITLEBAR 'ZPOS_BARCODE_SCAN'.
  tc_pos_scan_line-lines = lines( gt_pos_scan_line ).
ENDMODULE.

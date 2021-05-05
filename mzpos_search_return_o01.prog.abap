*&---------------------------------------------------------------------*
*& Include          MZPOS_RECEIPT_SEARCH_O01 - POS Related Modules
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZPOS_MAIN_STATUS'.
  SET TITLEBAR 'ZPOS_MAIN_TITLE'.

  IF gv_destination IS INITIAL.

    PERFORM set_car_destination.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZPOS_RECEIPT_STATUS'.
  SET TITLEBAR 'ZPOS_RECEIPT_TITLE'.

  tc_pos_receipt-lines = lines( gt_pos_receipt ).

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  tc_pos_line-lines   = lines( gt_pos_line ).
  tc_pos_tender-lines = lines( gt_pos_tender ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INPUT_STATUS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pos_input_status OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'WA_POS_LINE-RETURNQUANTITY'.
      IF   wa_pos_line-retailquantity  LE 0 OR wa_pos_line-lineitem_void = abap_true OR wa_pos_line-openquantity LE 0.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.

MODULE invoice_input_status OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'WA_INVOICE_DETAILS-RETURNQUANTITY'.
      IF wa_invoice_details-openquantity LE 0.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_SCAN_LINE'. DO NOT CHANGE THIS L
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_scan_line_change_tc_attr OUTPUT.
* Append Blank Lines to the Scanner Input Table

  DESCRIBE TABLE gt_pos_scan_line LINES tc_pos_scan_line-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_SCAN_LINE'. DO NOT CHANGE THIS L
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_scan_line_get_lines OUTPUT.
  g_tc_scan_line_lines = sy-loopc.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TABSTRIP_CNTRL'. DO NOT CHANGE THIS LI
*&SPWIZARD: SETS ACTIVE TAB
MODULE tabstrip_cntrl_active_tab_set OUTPUT.
  tabstrip_cntrl-activetab = g_tabstrip_cntrl-pressed_tab.
  CASE g_tabstrip_cntrl-pressed_tab.
    WHEN c_tabstrip_cntrl-tab1.
      g_tabstrip_cntrl-subscreen = '9010'.
    WHEN c_tabstrip_cntrl-tab2.
      g_tabstrip_cntrl-subscreen = '9011'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_9003 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9003 OUTPUT.
  SET PF-STATUS 'ZPOS_SCANLINE_STATUS'.
  SET TITLEBAR 'ZPOS_BARCODE_SCAN'.
  tc_pos_scan_line-lines = lines( gt_pos_scan_line ).

ENDMODULE.

MODULE status_9101 OUTPUT.

  SET PF-STATUS 'ZPOS_INVOICE_STATUS'.
  SET TITLEBAR 'ZPOS_INVOICE_TITLE'.

  tc_invoice_head-lines = lines( gt_invoice_header ).

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_INVOICE_HEAD'. DO NOT CHANGE THIS L
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_invoice_head_change_tc_attr OUTPUT.

  SET PF-STATUS 'ZPOS_INVOICE_STATUS'.
  SET TITLEBAR 'ZPOS_INVOICE_TITLE'.

  tc_invoice_head-lines = lines( gt_invoice_header ).

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_INVOICE_HEAD'. DO NOT CHANGE THIS L
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_invoice_head_get_lines OUTPUT.
  g_tc_invoice_head_lines = sy-loopc.
ENDMODULE.


MODULE status_9102 OUTPUT.

  tc_invoice_line-lines = lines( gt_invoice_details ).

ENDMODULE.

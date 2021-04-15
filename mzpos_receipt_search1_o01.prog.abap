*&---------------------------------------------------------------------*
*& Include          MZPOS_RECEIPT_SEARCH_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZPOS_MAIN_STATUS'.
  SET TITLEBAR 'ZPOS_MAIN_TITLE'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZPOS_RESULT_STATUS'.
  SET TITLEBAR 'ZPOS_MAIN_TITLE'.

  tc_receipt-lines = lines( gt_receipt ).

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  tc_line-lines = lines( gt_line ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INPUT_STATUS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE input_status OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'WA_LINE-RETAILQUANTITY'.
      IF   wa_line-retailquantity  LE 0.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9003 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9003 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  tc_tender-lines = lines( gt_tender ).
ENDMODULE.

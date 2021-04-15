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
  tc_tender-lines = lines( gt_tender ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INPUT_STATUS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE input_status OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'WA_LINE-RETURNQUANTITY'.
      IF   wa_line-retailquantity  LE 0 OR wa_line-lineitem_void = abap_true.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module SET_HIGHLIGHT_STATUS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_highlight_status OUTPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module RECEIPT_LIST_STATUS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE receipt_list_status OUTPUT.

  IF tc_receipt-lines < 2.

    LOOP AT SCREEN.

      IF screen-name = 'TC_RECEIPT'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDMODULE.

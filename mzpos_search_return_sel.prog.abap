*&---------------------------------------------------------------------*
*& Include MZPOS_RECEIPT_SEARCH_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF SCREEN 9010 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-b00.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c00 FOR FIELD p_bcode MODIF ID gp3.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_bcode TYPE tp_receipt_barcode MODIF ID gp3.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN END OF BLOCK b0.


  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.

    SELECT-OPTIONS: s_store       FOR  gv_store.
    SELECT-OPTIONS: s_date        FOR  gv_bdd.
    SELECT-OPTIONS: s_rcpt        FOR  gv_receipt.
    SELECT-OPTIONS: s_wrkstn      FOR  gv_workstationid.
    SELECT-OPTIONS: s_dept        FOR  gv_department NO-DISPLAY.
    SELECT-OPTIONS: s_opera       FOR  gv_operatorid NO-DISPLAY.
    SELECT-OPTIONS: s_total       FOR  gv_turnover.

  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c01 FOR FIELD p_cend.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_cend TYPE char4.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c02 FOR FIELD p_cstart.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_cstart TYPE char6.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c03 FOR FIELD p_cnumbr.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_cnumbr TYPE zpos_referenceid.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN END OF BLOCK b3.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.

    SELECT-OPTIONS: s_item    FOR  gv_itemid.
    SELECT-OPTIONS: s_itamt   FOR  gv_itemamount.
    SELECT-OPTIONS: s_count   FOR  gv_itemcount.

  SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN END OF SCREEN 9010.

SELECTION-SCREEN BEGIN OF SCREEN 9011 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-b10.

    PARAMETERS: p_vkorg TYPE vkorg NO-DISPLAY.
    PARAMETERS: p_curr  TYPE waers NO-DISPLAY.
    PARAMETERS: p_werks TYPE werks_d NO-DISPLAY.

    SELECT-OPTIONS: s_vbeln    FOR  gv_vbeln.
    SELECT-OPTIONS: s_date1    FOR  gv_date.
    SELECT-OPTIONS: s_total1   FOR  gv_turnover.

  SELECTION-SCREEN END OF BLOCK b10.

  SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE TEXT-b11.

    SELECT-OPTIONS: s_name     FOR  gv_name.
    SELECT-OPTIONS: s_orgnbr   FOR  gv_orgnbr.
    SELECT-OPTIONS: s_mobile   FOR  gv_phone.
    SELECT-OPTIONS: s_email    FOR  gv_email.

    SELECT-OPTIONS: s_kunnr    FOR  gv_kunnr.


  SELECTION-SCREEN END OF BLOCK b11.

  SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME TITLE TEXT-b12.

    SELECT-OPTIONS: s_gtin     FOR  gv_gtin.
    SELECT-OPTIONS: s_itamt1   FOR  gv_itemamount.


  SELECTION-SCREEN END OF BLOCK b12.

  SELECTION-SCREEN BEGIN OF BLOCK b13 WITH FRAME TITLE TEXT-b13.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c13 FOR FIELD p_rows.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_rows TYPE int4 DEFAULT 400.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c14 FOR FIELD p_zero.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_zero AS CHECKBOX DEFAULT ' '.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN END OF BLOCK b13.

SELECTION-SCREEN END OF SCREEN 9011.





*----------------------------------------------------------------------*
* SELECTION-SCREEN.                                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  ASSERT 1 = 1.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT.                                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  CLEAR:
*         p_bcode, s_store[], s_date[], s_wrkstn[],
*         s_rcpt[], s_dept[], s_opera[], s_total,
*         p_cend, p_cstart, p_cnumbr, s_item[], s_itamt[],
*         s_count[], gt_pos_receipt[], gt_pos_line[],
*         s_vbeln[],
         gt_pos_tender[], gt_pos_scan_line[],
         gt_return_basket[], gt_invoice_header[], gt_invoice_details[].

  GET PARAMETER ID 'VKO' FIELD p_vkorg.
  GET PARAMETER ID 'CUR' FIELD p_curr.
  GET PARAMETER ID 'WRK' FIELD p_werks.

  IF NOT p_werks IS INITIAL.
    APPEND INITIAL LINE TO s_store ASSIGNING FIELD-SYMBOL(<ls_store>).
    <ls_store>-sign = zcl_pos_util=>co_selopt_sign_i.
    <ls_store>-option = zcl_pos_util=>co_selopt_opt_eq.
    <ls_store>-low = p_werks.
  ENDIF.

  SET CURSOR FIELD 'P_BCODE' OFFSET 0.

*&---------------------------------------------------------------------*
*& Include          MZPOS_RECEIPT_SEARCH_I02 - Invoice Related
*&---------------------------------------------------------------------*


MODULE tc_invoice_head_user_command INPUT.
  ok_code = sy-ucomm.

  READ TABLE gt_invoice_header INTO wa_invoice_header WITH KEY mark = abap_true.
  wa_selected_invoice = CORRESPONDING #( wa_invoice_header ).

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.

      PERFORM confirm_exit.
*      SET SCREEN 0.
*      LEAVE TO SCREEN 0.

    WHEN 'DETL'.      "Details

      GET CURSOR LINE gv_pick_line AREA gv_area.

      IF gv_area = 'TC_INVOICE_HEAD'.
        GET CURSOR FIELD gv_field LINE gv_pick_line.
        gv_pick_line = tc_invoice_head-top_line + gv_pick_line - 1.            " Selecting the row even after scrolling

        PERFORM header_detail_refresh USING gv_pick_line.

      ENDIF.

    WHEN 'SCAN'.

      PERFORM single_invoice_scan.
      PERFORM view_invoice_return_basket.

    WHEN 'BASK'.       "Add to Basket

      PERFORM add_invoice_to_basket USING wa_selected_invoice.

    WHEN 'VIEW'.        "View the Basket

      PERFORM view_invoice_return_basket.

    WHEN 'FLOW'.

      gr_search->view_document_flow( wa_invoice_header-documentnumber ).

    WHEN 'INVO'.

      gr_search->view_invoice( wa_invoice_header-documentnumber ).

    WHEN 'CUST_FILTER'.      "Keeps Only One Customer

      DATA(lv_soldtoparty)  = wa_invoice_header-soldtoparty.
      PERFORM customer_filter USING lv_soldtoparty.

    WHEN 'MULTI'.

      PERFORM multi_invoice_scan.
      PERFORM view_invoice_return_basket.

    WHEN OTHERS.
      PERFORM user_ok_tc USING    'TC_INVOICE_HEAD'
                                  'GT_INVOICE_HEADER'
                                  'MARK'
                         CHANGING ok_code.


  ENDCASE.

  CLEAR ok_code.
  sy-ucomm = ok_code.

ENDMODULE.

MODULE user_command_9103 INPUT.

  CASE ok_code.
    WHEN 'ENTER' .
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'CREATE_ARM'.
      PERFORM create_invoice_adv_return.

  ENDCASE.
  CLEAR gt_refresh_invoice_details[].
  APPEND wa_selected_invoice TO gt_refresh_invoice_details.
  PERFORM refresh_invoice_details USING gt_refresh_invoice_details.

ENDMODULE.

MODULE user_command_9104 INPUT.

  CASE ok_code.
    WHEN 'ENTER' .
      PERFORM modify_invoice_from_barcode.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

MODULE modify_invoice_line INPUT.

*--------------------------------------------------------------------*
* Workaround for a very strange bug. The values in wa_pos_line are all positive
* for the first entry in the table - even they are not changed anywhere in the code
* the problem only seems to apply to the first line.

* The only value a user can update is the return quantity so we only use that field

* Here we use the correct values from gt_pos_line
  READ TABLE gt_invoice_details INDEX tc_invoice_line-current_line INTO DATA(ls_invoice_details).

  IF wa_invoice_details-returnquantity NE ls_invoice_details-returnquantity.        " Presumably the user has made a change
* We move over the return qty in case a user has entered a value without scanning and remove it from the openqty

    ls_invoice_details-openquantity = ls_invoice_details-openquantity + ls_invoice_details-returnquantity - wa_invoice_details-returnquantity.
    ls_invoice_details-returnquantity = wa_invoice_details-returnquantity.

  ENDIF.

  MODIFY gt_invoice_details INDEX tc_invoice_line-current_line FROM ls_invoice_details TRANSPORTING returnquantity openquantity.

ENDMODULE.


MODULE check_invoice_line INPUT.

  IF wa_invoice_details-returnquantity GE 0 AND wa_invoice_details-returnquantity > wa_invoice_details-quantity .
    MESSAGE TEXT-011 TYPE 'E'.
  ENDIF.

ENDMODULE.



MODULE tc_invoice_head_mark INPUT.

  IF tc_invoice_head-line_sel_mode = 1
  AND wa_invoice_header-mark = 'X'.
    LOOP AT gt_invoice_header INTO DATA(ls_invoice_header) WHERE mark = 'X'.
      ls_invoice_header-mark = ''.
      MODIFY gt_invoice_header
        FROM ls_invoice_header
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY gt_invoice_header
    FROM wa_invoice_header
    INDEX tc_invoice_head-current_line
    TRANSPORTING mark.
ENDMODULE.

MODULE tc_invoice_line_mark INPUT.
  DATA: g_tc_invoice_line_wa2 LIKE LINE OF gt_invoice_details.
  IF tc_invoice_line-line_sel_mode = 1
  AND wa_invoice_details-mark = 'X'.
    LOOP AT gt_invoice_details INTO g_tc_invoice_line_wa2
      WHERE mark = 'X'.
      g_tc_invoice_line_wa2-mark = ''.
      MODIFY gt_invoice_details
        FROM g_tc_invoice_line_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY gt_invoice_details
    FROM wa_invoice_details
    INDEX tc_invoice_line-current_line
    TRANSPORTING mark.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_RETURN_BASK'. DO NOT CHANGE THIS LIN
*&SPWIZARD: MODIFY TABLE
MODULE tc_return_bask_modify INPUT.
  MODIFY gt_return_basket
    FROM wa_return_basket
    INDEX tc_return_bask-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TC_RETURN_BASK'. DO NOT CHANGE THIS LINE
*&SPWIZARD: MARK TABLE
MODULE tc_return_bask_mark INPUT.
  DATA: g_tc_return_bask_wa2 LIKE LINE OF gt_return_basket.
  IF tc_return_bask-line_sel_mode = 1
  AND wa_return_basket-mark = 'X'.
    LOOP AT gt_return_basket INTO g_tc_return_bask_wa2
      WHERE mark = 'X'.
      g_tc_return_bask_wa2-mark = ''.
      MODIFY gt_return_basket
        FROM g_tc_return_bask_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY gt_return_basket
    FROM wa_return_basket
    INDEX tc_return_bask-current_line
    TRANSPORTING mark.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_RETURN_BASK'. DO NOT CHANGE THIS LIN
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_return_bask_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_RETURN_BASK'
                              'GT_RETURN_BASKET'
                              'MARK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Include          MZPOS_RECEIPT_SEARCH_I01 - POS Related
*&---------------------------------------------------------------------*

MODULE user_command_9000 INPUT.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE PROGRAM.
    WHEN 'SRCH' OR 'ENTR'.
      CLEAR gv_check_code.
      PERFORM check_mandatory_data CHANGING gv_check_code.

      IF gv_check_code NE 'E'.

        IF gv_pos = abap_true.
          PERFORM pos_transaction_search. " USING gv_pick_line.

          IF gr_search->mt_pos_result_list[] IS NOT INITIAL.
            PERFORM prepare_pos_search_result.
            PERFORM get_article_description.    " All descriptions for all receipt line items at once

*    gv_pick_line = 1.
            READ TABLE gt_pos_receipt INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_pos_receipt>).
            <ls_pos_receipt>-mark = abap_true.
            PERFORM refresh_pos_receipt_details USING <ls_pos_receipt>.
            CALL SCREEN 9001.
          ELSE.
            MESSAGE TEXT-s03 TYPE 'I'.
          ENDIF.

        ELSE.
          PERFORM invoice_search.

          gv_pick_line = 1.         " There is a reason to fill this
          PERFORM header_detail_refresh USING gv_pick_line.

          CALL SCREEN 9101.

        ENDIF.

      ENDIF.
  ENDCASE.

ENDMODULE.

MODULE user_command_9001 INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.

      SET SCREEN 0.
      LEAVE TO SCREEN 0.

    WHEN 'DETL'.      "Details

      GET CURSOR LINE gv_pick_line AREA gv_area.

      IF gv_area = 'TC_POS_RECEIPT'.
        GET CURSOR FIELD gv_field LINE gv_pick_line.
        gv_pick_line = tc_pos_receipt-top_line + gv_pick_line - 1.            " Selecting the row even after scrolling

        READ TABLE gt_pos_receipt INDEX gv_pick_line INTO wa_pos_receipt.

        PERFORM refresh_pos_receipt_details USING wa_pos_receipt.
        PERFORM set_receipt_highlight USING wa_pos_receipt.
      ELSE.

      ENDIF.

    WHEN 'SELC'.
      PERFORM return_partial_receipt.

    WHEN 'REFR'.
      PERFORM refresh_pos_receipt_details USING wa_pos_selected_receipt.

    WHEN 'SCAN'.

      LOOP AT gt_pos_line INTO DATA(ls_pos_line).
        IF ls_pos_line-openquantity > 0.

          DO floor( ls_pos_line-openquantity ) TIMES.
            APPEND INITIAL LINE TO gt_pos_scan_line.
          ENDDO.
        ENDIF.
      ENDLOOP.

      IF NOT gt_pos_scan_line IS INITIAL.
        CALL SCREEN 9003 STARTING AT 20 08.
      ELSE.
        MESSAGE TEXT-004 TYPE 'I'.
      ENDIF.

    WHEN 'CAR'.

      gr_search->view_receipt_in_car( EXPORTING
                                       iv_destination     = gv_destination
                                       iv_retailstoreid   = wa_pos_selected_receipt-retailstoreid
                                       iv_businessdaydate = wa_pos_selected_receipt-businessdaydate
                                       iv_transindex      = wa_pos_selected_receipt-transindex ).

  ENDCASE.
ENDMODULE.

MODULE modify_pos_receipt INPUT.

ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TABSTRIP_CNTRL'. DO NOT CHANGE THIS LIN
*&SPWIZARD: GETS ACTIVE TAB
MODULE tabstrip_cntrl_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tabstrip_cntrl-tab1.
      g_tabstrip_cntrl-pressed_tab = c_tabstrip_cntrl-tab1.
      gv_pos = abap_true.
    WHEN c_tabstrip_cntrl-tab2.
      g_tabstrip_cntrl-pressed_tab = c_tabstrip_cntrl-tab2.
      gv_pos = abap_false.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

MODULE user_command_9002 INPUT.

  CASE ok_code.

    WHEN 'DETL'.      "Details

      GET CURSOR LINE gv_pick_line AREA gv_area.

      IF gv_area = 'TC_POS_LINE'.
        GET CURSOR FIELD gv_field LINE gv_pick_line.

        READ TABLE gt_pos_line INDEX gv_pick_line INTO wa_pos_line.

        IF NOT wa_pos_line-returndocnum IS INITIAL.
          PERFORM view_adv_return USING wa_pos_line-returndocnum.
        ENDIF.

      ENDIF.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_gt_pos_line  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_pos_line INPUT.

*--------------------------------------------------------------------*
* Workaround for a very strange bug. The values in wa_pos_line are all positive
* for the first entry in the table - even they are not changed anywhere in the code
* the problem only seems to apply to the first line.

* The only value a user can update is the return quantity so we only use that field

* Here we use the correct values from gt_pos_line
  READ TABLE gt_pos_line INDEX tc_pos_line-current_line INTO gs_pos_line.

  IF wa_pos_line-returnquantity NE gs_pos_line-returnquantity.        " Presumably the user has made a change
* We move over the return qty in case a user has entered a value without scanning and remove it from the openqty

    gs_pos_line-openquantity = gs_pos_line-openquantity + gs_pos_line-returnquantity - wa_pos_line-returnquantity.
    gs_pos_line-returnquantity = wa_pos_line-returnquantity.

  ENDIF.

  MODIFY gt_pos_line INDEX tc_pos_line-current_line FROM gs_pos_line TRANSPORTING returnquantity openquantity openamount.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  MODIFY_GT_TENDER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_pos_tender INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_RETURN_LINE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pos_line INPUT.

  IF wa_pos_line-retailquantity GE 0 AND wa_pos_line-returnquantity > wa_pos_line-retailquantity .
    MESSAGE TEXT-011 TYPE 'E'.
  ENDIF.

ENDMODULE.


*&SPWIZARD: INPUT MODULE FOR TC 'tc_pos_scan_line'. DO NOT CHANGE THIS LI
*&SPWIZARD: MODIFY TABLE
MODULE tc_pos_scan_line_modify INPUT.

  IF NOT wa_pos_scan_line-ean11 IS INITIAL.
    wa_pos_scan_line-status = 'N'.           "N for New
  ENDIF.
  MODIFY gt_pos_scan_line
    FROM wa_pos_scan_line
    INDEX tc_pos_scan_line-current_line.

ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'tc_pos_scan_line'. DO NOT CHANGE THIS LIN
*&SPWIZARD: MARK TABLE
MODULE tc_pos_scan_line_mark INPUT.

  IF tc_pos_scan_line-line_sel_mode = 1 AND wa_pos_scan_line-mark = 'X'.
    LOOP AT gt_pos_scan_line INTO DATA(ls_pos_scan_line) WHERE mark = 'X'.
      ls_pos_scan_line-mark = ''.
      MODIFY gt_pos_scan_line
        FROM ls_pos_scan_line
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY gt_pos_scan_line
    FROM wa_pos_scan_line
    INDEX tc_pos_scan_line-current_line
    TRANSPORTING mark.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'tc_pos_scan_line'. DO NOT CHANGE THIS LI
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_pos_scan_line_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_POS_SCAN_LINE'
                              'GT_POS_SCAN_LINE'
                              'MARK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9003 INPUT.

  CASE ok_code.
    WHEN 'ENTER' .
      PERFORM modify_receipt_from_barcode.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

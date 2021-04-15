*&---------------------------------------------------------------------*
*& Include          MZPOS_RECEIPT_SEARCH_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form check_mandatory_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_mandatory_data USING gv_check_code.

  IF p_bcode IS INITIAL.

    IF s_store[] IS INITIAL.
      MESSAGE i000(zpos) WITH 'Please enter one or more stores'.
      gv_check_code = 'E'.
    ELSEIF s_date[] IS INITIAL.
      MESSAGE i000(zpos) WITH 'Please enter one or more dates'.
      gv_check_code = 'E'.
    ELSEIF p_cnumbr IS NOT INITIAL AND ( p_cstart IS NOT INITIAL OR p_cend IS NOT INITIAL ).
      MESSAGE i005(zpos).
      gv_check_code = 'E'.
    ENDIF.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form receipt_search
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM receipt_search .

  IF gr_search IS INITIAL.
    gr_search = NEW #( ).
  ENDIF.

  IF gr_search IS NOT BOUND.
    MESSAGE s000(zpos) WITH 'Search object class not initialized'(003).
    LEAVE PROGRAM.
  ENDIF.

  gt_message = gr_search->execute_search(
                 EXPORTING
                   it_r_retailstoreid = s_store[]
                   it_r_businessdaydate = s_date[]
                   it_r_workstationid  = s_wrkstn[]
                   it_r_receipt  = s_rcpt[]
                   it_r_operatorid  = s_opera[]
                   it_r_department  = s_dept[]
                   it_r_itemid  = s_item[]
                   it_r_itemcount = s_count[]
                   it_r_transamount = s_total[]
                   iv_ccard_start = p_cstart
                   iv_ccard_end = p_cend
                   iv_other_card = p_cnumbr
                   iv_records    = 200
                   iv_currency = 'EUR' ).

  IF gr_search->mt_result_list[] IS NOT INITIAL.
    PERFORM prepare_search_result.
    READ TABLE gt_receipt INDEX 1 INTO wa_receipt.
    PERFORM refresh_receipt_details USING wa_receipt.
    CALL SCREEN 9001.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form prepare_search_result
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepare_search_result.

  CLEAR gt_receipt.
  LOOP AT gr_search->mt_result_list INTO DATA(ls_result_line).

    APPEND INITIAL LINE TO gt_receipt ASSIGNING FIELD-SYMBOL(<ls_receipt>).
    MOVE-CORRESPONDING ls_result_line TO <ls_receipt>.
    <ls_receipt>-time = ls_result_line-begintimestamp+8(6).
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form prepare_search_result
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_receipt_details USING wa_receipt TYPE tp_receipt.

  CLEAR: gt_line[], gt_tender[].

  READ TABLE gr_search->mt_result_list
               INTO DATA(ls_receipt)
               WITH KEY retailstoreid = wa_receipt-retailstoreid
                        businessdaydate = wa_receipt-businessdaydate
                        transindex  = wa_receipt-transindex
                        transnumber = wa_receipt-transnumber
                        workstationid = wa_receipt-workstationid
                        operatorid  = wa_receipt-operatorid
                        department  = wa_receipt-department.

  IF sy-subrc = 0.

    LOOP AT ls_receipt-receipt_lineitem INTO DATA(ls_lineitem).

      APPEND INITIAL LINE TO gt_line ASSIGNING FIELD-SYMBOL(<ls_line>).
      MOVE-CORRESPONDING ls_lineitem TO <ls_line>.

    ENDLOOP.

    LOOP AT ls_receipt-receipt_tender INTO DATA(ls_tender).

      APPEND INITIAL LINE TO gt_tender ASSIGNING FIELD-SYMBOL(<ls_tender>).
      MOVE-CORRESPONDING ls_tender TO <ls_tender>.

*Z400	BAUHAUS Cash
*Z401	BAUHAUS Credit Card
*Z402	BAUHAUS Gift Cards
*Z403	BAUHAUS I.O.U. (Tillgodo)
*Z404	BAUHAUS Bank Transfer
*Z405	BAUHAUS Invoice

      CASE <ls_tender>-tendertypegroup.
        WHEN 'Z400'.
          <ls_tender>-category = 'Cash'.
        WHEN 'Z401'.
          <ls_tender>-category = 'Credit Card'.
        WHEN 'Z402'.
          <ls_tender>-category = 'Gift Card'.
        WHEN 'Z403'.
          <ls_tender>-category = 'Store Credit (Tillgodo)'.
        WHEN 'Z404'.
          <ls_tender>-category = 'Bank Transfer'.
        WHEN 'Z405'.
          <ls_tender>-category = 'Invoice'.
      ENDCASE.

    ENDLOOP.

  ENDIF.

ENDFORM.

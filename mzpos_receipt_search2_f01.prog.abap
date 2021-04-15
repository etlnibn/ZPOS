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

FORM prepare_search_criteria.

  IF NOT p_bcode IS INITIAL.

    CLEAR: s_store[], s_date[], s_wrkstn[], s_rcpt[].

*-----------------------------------------------------------------------------------------
* RETAILSTOREID
*-----------------------------------------------------------------------------------------
    APPEND INITIAL LINE TO s_store ASSIGNING FIELD-SYMBOL(<ls_store>).
    <ls_store>-sign = co_selection_options-include.
    <ls_store>-option = co_selection_options-equal.
    <ls_store>-low = p_bcode-store.
    SHIFT <ls_store>-low LEFT DELETING LEADING '0'.

*-----------------------------------------------------------------------------------------
* BUSINESSDAYDATE
*-----------------------------------------------------------------------------------------
    APPEND INITIAL LINE TO s_date ASSIGNING FIELD-SYMBOL(<ls_date>).
    <ls_date>-sign = co_selection_options-include.
    <ls_date>-option = co_selection_options-equal.
    <ls_date>-low = '20' && p_bcode-date.

*-----------------------------------------------------------------------------------------
* WORKSTATION ID
*-----------------------------------------------------------------------------------------
    APPEND INITIAL LINE TO s_wrkstn ASSIGNING FIELD-SYMBOL(<ls_workstationid>).
    <ls_workstationid>-sign = co_selection_options-include.
    <ls_workstationid>-option = co_selection_options-equal.
    <ls_workstationid>-low = p_bcode-till.
    SHIFT <ls_workstationid>-low LEFT DELETING LEADING '0'.

*-----------------------------------------------------------------------------------------
* TRANSACTION NUMBER
*-----------------------------------------------------------------------------------------
    APPEND INITIAL LINE TO s_rcpt ASSIGNING FIELD-SYMBOL(<ls_transnumber>).
    <ls_transnumber>-sign = co_selection_options-include.
    <ls_transnumber>-option = co_selection_options-equal.
    <ls_transnumber>-low = p_bcode-receipt.
    SHIFT <ls_transnumber>-low LEFT DELETING LEADING '0'.


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

  PERFORM prepare_search_criteria.

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
    READ TABLE gt_receipt INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_receipt>).
    <ls_receipt>-mark = abap_true.

    PERFORM refresh_receipt_details USING <ls_receipt>.
    CALL SCREEN 9001.
  ELSE.
    MESSAGE i000(zpos) WITH 'Receipt Not Found - Check Search Criteria'.
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
*    <ls_receipt>-time = ls_result_line-begintimestamp+8(6).
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

  PERFORM get_article_description.

  IF sy-subrc = 0.

    LOOP AT ls_receipt-receipt_lineitem INTO DATA(ls_lineitem).

      APPEND INITIAL LINE TO gt_line ASSIGNING FIELD-SYMBOL(<ls_line>).
      MOVE-CORRESPONDING ls_lineitem TO <ls_line>.
      READ TABLE gt_makt WITH KEY matnr = <ls_line>-itemid INTO DATA(ls_makt) BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_line>-description = ls_makt-maktx.
      ENDIF.

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
          <ls_tender>-category = 'Tillgodo'.
        WHEN 'Z404'.
          <ls_tender>-category = 'Bank Transfer'.
        WHEN 'Z405'.
          <ls_tender>-category = 'Invoice'.
      ENDCASE.

    ENDLOOP.

    wa_selected_receipt = wa_receipt.

  ENDIF.

ENDFORM.

FORM set_receipt_highlight USING wa_receipt TYPE tp_receipt.

  DATA: lv_mark TYPE ce_mark VALUE abap_false.

  LOOP AT gt_receipt ASSIGNING FIELD-SYMBOL(<ls_receipt>) WHERE mark = abap_true.
    <ls_receipt>-mark = abap_false.
  ENDLOOP.

  wa_receipt-mark = abap_true.
  MODIFY gt_receipt FROM wa_receipt TRANSPORTING mark
                    WHERE retailstoreid = wa_receipt-retailstoreid
                    AND businessdaydate = wa_receipt-businessdaydate
                    AND transindex = wa_receipt-transindex
                    AND workstationid = wa_receipt-workstationid
                    AND operatorid = wa_receipt-operatorid.

ENDFORM.

FORM return_complete_receipt.

  DATA: ls_transaction TYPE zpos_search_transaction_sty.

  MOVE-CORRESPONDING wa_selected_receipt TO ls_transaction.

  LOOP AT gt_line INTO DATA(ls_line) WHERE retailquantity > 0 AND lineitem_void = abap_false.

    APPEND INITIAL LINE TO ls_transaction-receipt_lineitem ASSIGNING FIELD-SYMBOL(<ls_lineitem>).
    MOVE-CORRESPONDING ls_line TO <ls_lineitem>.

  ENDLOOP.

  IF NOT ls_transaction-receipt_lineitem IS INITIAL.

    gr_search->create_return_order( EXPORTING is_pos_transaction = ls_transaction IMPORTING ev_docnum = DATA(lv_docnum) et_return = DATA(lt_return) ).

    IF lv_docnum IS NOT INITIAL.
      CALL TRANSACTION 'VA02' AND SKIP FIRST SCREEN.
    ENDIF.
  ELSE.

    MESSAGE 'No lines could be returned on this receipt ' TYPE 'I'.

  ENDIF.


ENDFORM.

FORM return_partial_receipt.

  DATA: ls_transaction TYPE zpos_search_transaction_sty.

  MOVE-CORRESPONDING wa_selected_receipt TO ls_transaction.

  LOOP AT gt_line INTO DATA(ls_line) WHERE returnquantity > 0 AND lineitem_void = abap_false.

    APPEND INITIAL LINE TO ls_transaction-receipt_lineitem ASSIGNING FIELD-SYMBOL(<ls_lineitem>).
    MOVE-CORRESPONDING ls_line TO <ls_lineitem>.
    <ls_lineitem>-retailquantity = ls_line-returnquantity.

  ENDLOOP.

  IF NOT ls_transaction-receipt_lineitem IS INITIAL.

    gr_search->create_return_order( EXPORTING is_pos_transaction = ls_transaction IMPORTING ev_docnum = DATA(lv_docnum) et_return = DATA(lt_return) ).

    IF lv_docnum IS NOT INITIAL.
      CALL TRANSACTION 'VA02' AND SKIP FIRST SCREEN.
    ENDIF.
  ELSE.

    MESSAGE 'You must enter the return quantities for a partial return' TYPE 'I'.

  ENDIF.


ENDFORM.

FORM get_article_description.

* This will need modification as the itemID will normally be the barcode (GTIN) and not the matnr.

  DATA: lt_pre09 TYPE STANDARD TABLE OF pre09.

  LOOP AT gr_search->mt_result_list INTO DATA(ls_transaction).
    LOOP AT ls_transaction-receipt_lineitem INTO DATA(ls_lineitem).

      APPEND INITIAL LINE TO lt_pre09 ASSIGNING FIELD-SYMBOL(<ls_pre09>).
      <ls_pre09>-matnr = ls_lineitem-itemid.
      <ls_pre09>-spras = sy-langu.

    ENDLOOP.
  ENDLOOP.
  SORT lt_pre09 BY matnr ASCENDING. DELETE ADJACENT DUPLICATES FROM lt_pre09 COMPARING ALL FIELDS.

  CALL FUNCTION 'MAKT_ARRAY_READ'
* EXPORTING
*   KZRFB                      = ' '
*   NEUFLAG                    = ' '
    TABLES
      ipre09               = lt_pre09
      makt_tab             = gt_makt
    EXCEPTIONS
      enqueue_mode_changed = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Unable to retrieve Article Descriptions' TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Include          MZPOS_RECEIPT_SEARCH_F01
*&---------------------------------------------------------------------*

FORM check_mandatory_data CHANGING gv_check_code.

  IF gv_pos = abap_true.
* POS Search
    IF p_bcode IS INITIAL.

      IF s_store[] IS INITIAL.
        MESSAGE TEXT-s00 TYPE 'I'.
        gv_check_code = 'E'.
      ELSEIF s_date[] IS INITIAL.
        MESSAGE TEXT-s01 TYPE 'I'.
        gv_check_code = 'E'.
      ELSEIF p_cnumbr IS NOT INITIAL AND ( p_cstart IS NOT INITIAL OR p_cend IS NOT INITIAL ).
        MESSAGE i005(zpos).
        gv_check_code = 'E'.
      ENDIF.

    ENDIF.

  ELSE.
* Invoice Search
    IF s_vbeln IS INITIAL.

      IF s_kunnr[]  IS INITIAL AND s_name[] IS INITIAL AND
         s_orgnbr[] IS INITIAL AND s_mobile[] IS INITIAL AND
         s_email[]  IS INITIAL.

        MESSAGE TEXT-s10 TYPE 'I'.
        gv_check_code = 'E'.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

FORM prepare_pos_bcode_criteria.

  IF gv_pos = abap_true.

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

  ENDIF.

ENDFORM.


FORM pos_transaction_search. " USING gv_pick_line.

  IF gr_search IS INITIAL.
    gr_search = NEW #( ).
  ENDIF.

  IF gr_search IS NOT BOUND.
    MESSAGE TEXT-s02 TYPE 'E'.
    LEAVE PROGRAM.
  ENDIF.

  PERFORM prepare_pos_bcode_criteria.

  gt_message = gr_search->execute_pos_search(
                 EXPORTING
                   iv_destination     = gv_destination
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
                   iv_currency = p_curr ).

*  IF gr_search->mt_pos_result_list[] IS NOT INITIAL.
*    PERFORM prepare_pos_search_result.
*    PERFORM get_article_description.    " All descriptions for all receipt line items at once
*
**    gv_pick_line = 1.
*    READ TABLE gt_pos_receipt INDEX gv_pick_line ASSIGNING FIELD-SYMBOL(<ls_pos_receipt>).
*    <ls_pos_receipt>-mark = abap_true.
*    PERFORM refresh_pos_receipt_details USING <ls_pos_receipt>.
*    CALL SCREEN 9001.
*  ELSE.
*    MESSAGE TEXT-s03 TYPE 'I'.
*  ENDIF.

ENDFORM.

FORM prepare_pos_search_result.

  gt_pos_receipt = CORRESPONDING #( gr_search->mt_pos_result_list ).

ENDFORM.


FORM refresh_pos_receipt_details USING wa_pos_receipt TYPE tp_pos_receipt.

  CLEAR: gt_pos_line[], gt_pos_tender[], gt_pos_scan_line[].

  READ TABLE gr_search->mt_pos_result_list
               INTO DATA(ls_receipt)
               WITH KEY retailstoreid = wa_pos_receipt-retailstoreid
                        businessdaydate = wa_pos_receipt-businessdaydate
                        transindex  = wa_pos_receipt-transindex
                        transnumber = wa_pos_receipt-transnumber
                        workstationid = wa_pos_receipt-workstationid
                        operatorid  = wa_pos_receipt-operatorid
                        department  = wa_pos_receipt-department.

  SORT ls_receipt-receipt_lineitem BY retailnumber.

  IF sy-subrc = 0.

    LOOP AT ls_receipt-receipt_lineitem INTO DATA(ls_lineitem).

      APPEND INITIAL LINE TO gt_pos_line ASSIGNING FIELD-SYMBOL(<ls_pos_line>).
      MOVE-CORRESPONDING ls_lineitem TO <ls_pos_line>.
      READ TABLE gt_makt WITH KEY matnr = <ls_pos_line>-itemid INTO DATA(ls_makt) BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_pos_line>-description = ls_makt-maktx.
      ENDIF.
* If we have already calculated and we have cleared the fields from CAR then we must not recalculate it
      IF <ls_pos_line>-openquantity IS INITIAL.
        <ls_pos_line>-openquantity = ls_lineitem-retailquantity - ls_lineitem-returnquantity.
      ENDIF.
      IF <ls_pos_line>-openamount IS INITIAL.
        <ls_pos_line>-openamount   = ls_lineitem-salesamount - ls_lineitem-returnamount.
      ENDIF.
      IF <ls_pos_line>-lineitem_void = abap_true.
        CLEAR: <ls_pos_line>-openamount, <ls_pos_line>-openquantity.
      ENDIF.
      CLEAR: <ls_pos_line>-returnquantity, <ls_pos_line>-returnamount.

    ENDLOOP.

    LOOP AT ls_receipt-receipt_tender INTO DATA(ls_tender).

      APPEND INITIAL LINE TO gt_pos_tender ASSIGNING FIELD-SYMBOL(<ls_tender>).
      MOVE-CORRESPONDING ls_tender TO <ls_tender>.

*Z400	BAUHAUS Cash
*Z401	BAUHAUS Credit Card
*Z402	BAUHAUS Gift Cards
*Z403	BAUHAUS I.O.U. (Tillgodo)
*Z404	BAUHAUS Bank Transfer
*Z405	BAUHAUS Invoice

      CASE <ls_tender>-tendertypegroup.
        WHEN 'Z400'.
          <ls_tender>-category = TEXT-t01.
        WHEN 'Z401'.
          <ls_tender>-category = TEXT-t02.
        WHEN 'Z402'.
          <ls_tender>-category = TEXT-t03.
        WHEN 'Z403'.
          <ls_tender>-category = TEXT-t04.
        WHEN 'Z404'.
          <ls_tender>-category = TEXT-t05.
        WHEN 'Z405'.
          <ls_tender>-category = TEXT-t06.
      ENDCASE.

    ENDLOOP.

    wa_pos_selected_receipt = wa_pos_receipt.

  ENDIF.

ENDFORM.

FORM set_receipt_highlight USING wa_pos_receipt TYPE tp_pos_receipt.

  DATA: lv_mark TYPE ce_mark VALUE abap_false.

  LOOP AT gt_pos_receipt ASSIGNING FIELD-SYMBOL(<ls_pos_receipt>) WHERE mark = abap_true.
    <ls_pos_receipt>-mark = abap_false.
  ENDLOOP.

  wa_pos_receipt-mark = abap_true.
  MODIFY gt_pos_receipt FROM wa_pos_receipt TRANSPORTING mark
                    WHERE retailstoreid = wa_pos_receipt-retailstoreid
                    AND businessdaydate = wa_pos_receipt-businessdaydate
                    AND transindex = wa_pos_receipt-transindex
                    AND workstationid = wa_pos_receipt-workstationid
                    AND operatorid = wa_pos_receipt-operatorid.

ENDFORM.


FORM return_partial_receipt.

  DATA: ls_transaction TYPE zpos_search_transaction_sty.

  MOVE-CORRESPONDING wa_pos_selected_receipt TO ls_transaction.

  LOOP AT gt_pos_line INTO gs_pos_line WHERE returnquantity > 0 AND lineitem_void = abap_false.

    APPEND INITIAL LINE TO ls_transaction-receipt_lineitem ASSIGNING FIELD-SYMBOL(<ls_lineitem>).
    MOVE-CORRESPONDING gs_pos_line TO <ls_lineitem>.
    <ls_lineitem>-returnamount = <ls_lineitem>-salesamount * ( <ls_lineitem>-returnquantity / <ls_lineitem>-retailquantity ) .
  ENDLOOP.

  IF NOT ls_transaction-receipt_lineitem IS INITIAL.

    gr_search->create_pos_adv_return( EXPORTING is_pos_transaction = ls_transaction IMPORTING ev_docnum = DATA(lv_docnum) et_message = gt_message ).

    IF lv_docnum IS NOT INITIAL.
      TRY.
          SET PARAMETER ID: lv_docnum FIELD 'AUN'  .
          CALL TRANSACTION 'VA02' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
        CATCH cx_sy_authorization_error.
          MESSAGE TEXT-005 TYPE 'I'.
          RETURN.
      ENDTRY.

      LOOP AT ls_transaction-receipt_lineitem ASSIGNING <ls_lineitem>.
        <ls_lineitem>-returndocnum = lv_docnum.
      ENDLOOP.

      gr_search->update_receipt_in_car( EXPORTING iv_destination     = gv_destination is_return_transaction = ls_transaction IMPORTING et_message = gt_message ).

      IF gt_message[] IS INITIAL.
* When we come back we should ideally refresh the current receipt from CAR - but position ourselves on the currently selected receipt.
        PERFORM pos_transaction_search. " USING gv_pick_line.

        IF gr_search->mt_pos_result_list[] IS NOT INITIAL.
          PERFORM prepare_pos_search_result.
*          PERFORM get_article_description.    " All descriptions for all receipt line items at once

          READ TABLE gt_pos_receipt INDEX gv_pick_line ASSIGNING FIELD-SYMBOL(<ls_pos_receipt>).
          <ls_pos_receipt>-mark = abap_true.
          PERFORM refresh_pos_receipt_details USING <ls_pos_receipt>.
*          CALL SCREEN 9001.

        ELSE.
          MESSAGE TEXT-s03 TYPE 'I'.
        ENDIF.


      ELSE.
* Add proper handling here later - show the returned messages
        MESSAGE TEXT-006 TYPE 'E'.
      ENDIF.

* Should we reset the display and set values back to zero?
      PERFORM pos_transaction_search. " USING gv_pick_line.

      IF gr_search->mt_pos_result_list[] IS NOT INITIAL.
        PERFORM prepare_pos_search_result.
*          PERFORM get_article_description.    " All descriptions for all receipt line items at once

        READ TABLE gt_pos_receipt INDEX gv_pick_line ASSIGNING <ls_pos_receipt>.
        <ls_pos_receipt>-mark = abap_true.
        PERFORM refresh_pos_receipt_details USING <ls_pos_receipt>.
*          CALL SCREEN 9001.

      ENDIF.

    ELSE.

      MESSAGE TEXT-007 TYPE 'I'.

    ENDIF.

  ENDIF.

ENDFORM.

FORM get_article_description.

* This will need modification as the itemID will normally be the barcode (GTIN) and not the matnr.

  DATA: lt_pre09 TYPE STANDARD TABLE OF pre09.

  LOOP AT gr_search->mt_pos_result_list INTO DATA(ls_transaction).
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



FORM modify_receipt_from_barcode.

  LOOP AT gt_pos_line ASSIGNING FIELD-SYMBOL(<ls_pos_line>).

* Is there any point in procesing this line
    IF NOT <ls_pos_line>-lineitem_void = abap_true AND <ls_pos_line>-openquantity GE 0.


      WHILE <ls_pos_line>-openquantity GT 0.

* Did we scan a matching item?
        READ TABLE gt_pos_scan_line ASSIGNING FIELD-SYMBOL(<ls_pos_scan_line>) WITH KEY ean11 = <ls_pos_line>-itemid status = co_scanner_status-new.

        IF sy-subrc = 0.
* For every matching Barcode we move Qty 1 from openQTY field to returnqty field
          <ls_pos_line>-openquantity -= 1.
          <ls_pos_line>-returnquantity += 1.
          <ls_pos_scan_line>-status = co_scanner_status-used.
        ELSE.
* As soon as we can no longer find any matching barcodes we exit
          EXIT.
        ENDIF.

*TODO!!  What to do about the amount field - come back to this one later!!!
      ENDWHILE.

    ENDIF.

  ENDLOOP.

  DELETE gt_pos_scan_line WHERE status = co_scanner_status-used.
  READ TABLE gt_pos_scan_line TRANSPORTING NO FIELDS WITH KEY status = co_scanner_status-new .

  IF sy-subrc = 0.          " Did we find New Entries
    MESSAGE TEXT-003 TYPE 'I'.
*    CALL SCREEN 9003 STARTING AT 20 10.
  ENDIF.
  CLEAR gt_pos_scan_line[].


ENDFORM.

FORM view_adv_return USING iv_docnum TYPE vbeln.

  CHECK iv_docnum NE co_multiple.     " If we don't know a realy docnum then we cannot use forward navigation

  TRY.
      SET PARAMETER ID 'AUN' FIELD iv_docnum.
      CALL TRANSACTION 'VA02' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
    CATCH cx_sy_authorization_error.
      MESSAGE TEXT-008 TYPE 'I'.
      RETURN.
  ENDTRY.

ENDFORM.

FORM set_car_destination.

  SELECT SINGLE low
  INTO  gv_destination
  FROM  tvarvc
  WHERE name = co_car_pos_search
  AND   type = 'P'
  AND   numb = '0000'.

  IF gv_destination IS INITIAL.

    MESSAGE TEXT-014 TYPE 'E'.

  ENDIF.

ENDFORM.

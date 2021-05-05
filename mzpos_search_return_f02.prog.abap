*&---------------------------------------------------------------------*
*& Include          MZPOS_RECEIPT_SEARCH_F02
*&---------------------------------------------------------------------*

FORM invoice_search.

  IF gr_search IS INITIAL.
    gr_search = NEW #( ).
  ENDIF.

  IF gr_search IS NOT BOUND.
    MESSAGE TEXT-s02 TYPE 'E'.
    LEAVE PROGRAM.
  ENDIF.

  gt_message = gr_search->execute_invoice_search(
                 EXPORTING
                   iv_vkorg     = p_vkorg
                   it_r_vbeln   = s_vbeln[]
                   it_r_date    = s_date1[]
                   it_r_invoicegross = s_total1[]
                   it_r_name    = s_name[]
                   it_r_taxreg  = s_orgnbr[]
                   it_r_ean11   = s_gtin[]
                   it_r_kunnr   = s_kunnr[]
                   it_r_itemgross = s_itamt1[]
                   it_r_email   = s_email[]
                   it_r_mobile  =  s_mobile[]
                   iv_currency  = p_curr
                   iv_records   = p_rows
                   iv_zero_qty  = p_zero ).

  gt_invoice_header = CORRESPONDING #( gr_search->mt_invoice_header ).

ENDFORM.

FORM set_invoice_highlight USING wa_invoice_header TYPE tp_s_invoice_header.

  DATA: lv_mark TYPE ce_mark VALUE abap_false.

  LOOP AT gt_invoice_header ASSIGNING FIELD-SYMBOL(<ls_invoice_header>) WHERE mark = abap_true.
    <ls_invoice_header>-mark = abap_false.
  ENDLOOP.

  wa_invoice_header-mark = abap_true.
  MODIFY gt_invoice_header FROM wa_invoice_header TRANSPORTING mark
                    WHERE documentnumber = wa_invoice_header-documentnumber.

ENDFORM.

FORM customer_filter USING lv_soldtoparty TYPE kunnr.

* Check that we only have 1 single sold-to
  gt_soldtoparty = CORRESPONDING #( gt_invoice_header  ).
  SORT gt_soldtoparty BY soldtoparty.
  DELETE ADJACENT DUPLICATES FROM gt_soldtoparty COMPARING soldtoparty.

  IF lines( gt_soldtoparty ) GT 1.

    DELETE gt_invoice_header WHERE soldtoparty NE lv_soldtoparty.

    gv_pick_line = 1.         " There is a reason to fill this
    PERFORM header_detail_refresh USING gv_pick_line.

  ENDIF.

ENDFORM.



FORM refresh_invoice_details USING gt_refresh_invoice_details TYPE tp_t_invoice_details.

  DATA: lt_invoice_details TYPE tp_t_invoice_details.
  CLEAR gt_invoice_details.

  LOOP AT gt_refresh_invoice_details INTO DATA(ls_refresh_invoice_details).

    lt_invoice_details = CORRESPONDING #( gr_search->mt_invoice_details ).
    DELETE lt_invoice_details WHERE documentnumber NE ls_refresh_invoice_details-documentnumber.
    APPEND LINES OF lt_invoice_details TO gt_invoice_details.

  ENDLOOP.

  SORT gt_invoice_details BY documentdate DESCENDING documentnumber DESCENDING documentitem ASCENDING.

  LOOP AT gt_invoice_details ASSIGNING FIELD-SYMBOL(<ls_invoice_details>).
    READ TABLE gt_return_basket INTO wa_return_basket
                          WITH KEY documentnumber = <ls_invoice_details>-documentnumber
                                   documentitem   = <ls_invoice_details>-documentitem.

    IF sy-subrc = 0.
      <ls_invoice_details>-openquantity   = <ls_invoice_details>-openquantity - wa_return_basket-returnquantity.
    ENDIF.
  ENDLOOP.


ENDFORM.

FORM add_invoice_to_basket USING wa_selected_invoice TYPE tp_s_invoice_details.

  DATA: lv_check TYPE boole_d VALUE abap_true.
  DATA(lt_return_basket) = CORRESPONDING tp_t_invoice_details( gt_invoice_details ).
  DELETE lt_return_basket WHERE returnquantity LE 0.

  IF lt_return_basket IS NOT INITIAL.

* Need to check that all invoices are for the same Sold-To-Party

    LOOP AT gt_return_basket
               INTO wa_return_basket
               WHERE soldtoparty NE lt_return_basket[ 1 ]-soldtoparty.
      lv_check = abap_false.
      EXIT.
    ENDLOOP.

    IF lv_check = abap_true.
      LOOP AT lt_return_basket INTO wa_return_basket.
        READ TABLE gt_return_basket
                             ASSIGNING FIELD-SYMBOL(<ls_return_basket>)
                             WITH KEY documentnumber = wa_return_basket-documentnumber
                                      documentitem   = wa_return_basket-documentitem.

        IF sy-subrc = 0.
* Existing Entry and therefore values should be aggregated
          <ls_return_basket>-returnquantity += wa_return_basket-returnquantity.
          <ls_return_basket>-openquantity    = <ls_return_basket>-quantity - <ls_return_basket>-returnquantity.
        ELSE.
* New Entry so Append
          APPEND wa_return_basket  TO gt_return_basket.
        ENDIF.

      ENDLOOP.

      CLEAR gt_refresh_invoice_details[].
      APPEND wa_selected_invoice TO gt_refresh_invoice_details.
      PERFORM refresh_invoice_details USING gt_refresh_invoice_details.
      DATA(lv_lines) = lines( lt_return_basket ).
      MESSAGE s007(zpos) WITH lv_lines.

    ELSE.
      MESSAGE i009(zpos) WITH wa_return_basket-soldtoparty lt_return_basket[ 1 ]-soldtoparty.
    ENDIF.

  ELSE.
    MESSAGE i008(zpos).
  ENDIF.


ENDFORM.

FORM view_invoice_return_basket.

  IF NOT gt_return_basket IS INITIAL.
    CALL SCREEN 9103 STARTING AT 20 08.
  ELSE.
    MESSAGE TEXT-012 TYPE 'I'.
  ENDIF.


ENDFORM.

FORM single_invoice_scan.

  LOOP AT gt_invoice_details INTO wa_invoice_details.
    IF wa_invoice_details-openquantity > 0.

      DO floor( wa_invoice_details-openquantity ) TIMES.
        APPEND INITIAL LINE TO gt_pos_scan_line.
      ENDDO.
    ENDIF.
  ENDLOOP.

  IF NOT gt_pos_scan_line IS INITIAL.
    CALL SCREEN 9104 STARTING AT 20 08.
  ELSE.
    MESSAGE TEXT-004 TYPE 'I'.
  ENDIF.

ENDFORM.

FORM multi_invoice_scan.

* Check that we only have 1 single sold-to
  gt_soldtoparty = CORRESPONDING #( gt_invoice_header  ).
  DELETE ADJACENT DUPLICATES FROM gt_soldtoparty COMPARING ALL FIELDS.

  IF lines( gt_soldtoparty ) EQ 1.

    CLEAR gt_pos_scan_line[].
    DO 20 TIMES.
      APPEND INITIAL LINE TO gt_pos_scan_line.
    ENDDO.

    gv_multi = abap_true.
    CALL SCREEN 9104 STARTING AT 20 08.
    gv_multi = abap_false.

  ELSE.

    MESSAGE i010(zpos).

  ENDIF.


ENDFORM.



FORM modify_invoice_from_barcode.

  DELETE gt_pos_scan_line WHERE ean11 IS INITIAL.
  CLEAR gt_refresh_invoice_details[].

  IF gv_multi = abap_true.

* Adding the lines of several invoices to the details area for multi-scanning
    LOOP AT gt_invoice_header INTO wa_invoice_header.
      wa_selected_invoice = CORRESPONDING #( wa_invoice_header ).
      APPEND wa_selected_invoice TO gt_refresh_invoice_details.
    ENDLOOP.
    PERFORM refresh_invoice_details USING gt_refresh_invoice_details.

* Getting Data Ready for another refresh later
    CLEAR gt_refresh_invoice_details[].
    gv_pick_line = 1.
    READ TABLE gt_invoice_header INDEX gv_pick_line INTO wa_invoice_header.
    wa_selected_invoice = CORRESPONDING #(  wa_invoice_header ) .
    APPEND wa_selected_invoice TO gt_refresh_invoice_details.

  ELSE.

* Getting Data Ready for another refresh later
    READ TABLE gt_invoice_header WITH KEY mark = abap_true INTO wa_invoice_header.
    wa_selected_invoice = CORRESPONDING #(  wa_invoice_header ) .
    APPEND wa_selected_invoice TO gt_refresh_invoice_details.

  ENDIF.


  IF NOT gt_pos_scan_line IS INITIAL.

    LOOP AT gt_invoice_details ASSIGNING FIELD-SYMBOL(<ls_invoice_details>).

      WHILE <ls_invoice_details>-openquantity GT 0.

* Did we scan a matching item?
        READ TABLE gt_pos_scan_line ASSIGNING FIELD-SYMBOL(<ls_pos_scan_line>) WITH KEY ean11 = <ls_invoice_details>-gtin status = co_scanner_status-new.

        IF sy-subrc = 0.
* For every matching Barcode we move Qty 1 from openQTY field to returnqty field
          <ls_invoice_details>-openquantity -= 1.
*          <ls_invoice_details>-returnquantity += 1.
          <ls_pos_scan_line>-status = co_scanner_status-used.

          READ TABLE gt_return_basket
                               ASSIGNING FIELD-SYMBOL(<ls_return_basket>)
                               WITH KEY documentnumber = <ls_invoice_details>-documentnumber
                                        documentitem   = <ls_invoice_details>-documentitem.

          IF sy-subrc = 0.
* Existing Entry and therefore values should be aggregated
          ELSE.
* New Entry so Append
            APPEND INITIAL LINE TO gt_return_basket ASSIGNING <ls_return_basket>.
            MOVE-CORRESPONDING <ls_invoice_details> TO <ls_return_basket>.
          ENDIF.
          <ls_return_basket>-returnquantity += 1.
          <ls_return_basket>-openquantity   -= 1.

        ELSE.
* As soon as we can no longer find any matching barcodes we exit
          EXIT.
        ENDIF.

*TODO!!  What to do about the amount field - come back to this one later!!!
      ENDWHILE.

    ENDLOOP.

    DELETE gt_pos_scan_line WHERE status = co_scanner_status-used.

    IF line_exists( gt_pos_scan_line[ status = co_scanner_status-new ] ) .
      MESSAGE TEXT-003 TYPE 'I'.
    ENDIF.
    CLEAR gt_pos_scan_line[].

  ENDIF.

  IF gv_multi = abap_true.
* For multi we are moving the highlight to the top of the list
    PERFORM set_invoice_highlight   USING wa_invoice_header.
  ENDIF.

  PERFORM refresh_invoice_details USING gt_refresh_invoice_details.

ENDFORM.

FORM create_invoice_adv_return.

  CLEAR: gs_head_comv, gt_item_comv.
  DATA: lv_posnr TYPE posnr VALUE '000010'.

  LOOP AT gt_return_basket INTO wa_return_basket.

    IF gs_head_comv IS INITIAL.
      gs_head_comv-doc_type   = 'RE2'.
      gs_head_comv-salesorg   = wa_return_basket-salesorg.
      gs_head_comv-distr_chan = wa_return_basket-distchannel.
      gs_head_comv-division   = wa_return_basket-division.
      gs_head_comv-sold_to    = wa_return_basket-soldtoparty.
      gs_head_comv-ship_to    = wa_return_basket-soldtoparty.
    ENDIF.

    APPEND INITIAL LINE TO gt_item_comv ASSIGNING FIELD-SYMBOL(<ls_item_comv>).
    <ls_item_comv>-itm_number = lv_posnr.
    <ls_item_comv>-req_qty    = wa_return_basket-returnquantity.
    <ls_item_comv>-ref_doc    = wa_return_basket-documentnumber.
    <ls_item_comv>-ref_doc_it = wa_return_basket-documentitem.

    lv_posnr += 10.

  ENDLOOP.

  IF NOT gt_item_comv IS  INITIAL.
    gr_search->create_inv_adv_retur(
                    EXPORTING is_head_comv = gs_head_comv it_item_comv = gt_item_comv
                    IMPORTING ev_docnum = DATA(lv_docnum) et_message = DATA(lt_message) ).


    IF lv_docnum IS NOT INITIAL.
      TRY.
          SET PARAMETER ID: lv_docnum FIELD 'AUN'  .
          CALL TRANSACTION 'VA02' WITHOUT AUTHORITY-CHECK AND SKIP FIRST SCREEN.
        CATCH cx_sy_authorization_error.
          MESSAGE TEXT-005 TYPE 'I'.
      ENDTRY.

      PERFORM filter_invoice_header USING gt_return_basket.
      gv_pick_line = 1.         " There is a reason to fill this
      PERFORM header_detail_refresh USING gv_pick_line.

      CLEAR gt_return_basket[].

    ELSE.
* Not Possible to Create Document
      PERFORM display_message USING lt_message.
    ENDIF.
  ENDIF.

ENDFORM.

FORM confirm_exit.

  DATA: lv_answer.

  IF lines( gt_return_basket ) >  0.
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        defaultoption  = 'N'
        diagnosetext1  = 'Return Basket contains unprocessed items'
        diagnosetext3  = 'The basket will be emptied if you proceed '
        diagnosetext2  = ' '
        textline1      = 'Do you want to Exit'
        textline2      = ' '
        titel          = 'Return Basket Not Empty'
        start_column   = 25
        start_row      = 6
*----for the display of cancel button  do like this.
        cancel_display = ' '
      IMPORTING
        answer         = lv_answer.

    IF lv_answer = 'J' .
      SET SCREEN 0.
      LEAVE TO SCREEN 0.
    ENDIF.

  ELSE.
    SET SCREEN 0.
    LEAVE TO SCREEN 0.
  ENDIF.


ENDFORM.

FORM filter_invoice_header USING lt_invoice_details TYPE tp_t_invoice_details.

  CLEAR s_vbeln[].

  LOOP AT lt_invoice_details INTO DATA(ls_invoice_details).

    APPEND INITIAL LINE TO s_vbeln ASSIGNING FIELD-SYMBOL(<ls_vbeln>).
    <ls_vbeln>-sign = 'I'.
    <ls_vbeln>-option = 'EQ'.
    <ls_vbeln>-low = ls_invoice_details-documentnumber.

  ENDLOOP.
  p_zero = abap_true.

  PERFORM invoice_search.

ENDFORM.

FORM header_detail_refresh USING lv_index TYPE sy-tabix.

  CLEAR gt_refresh_invoice_details[].

  READ TABLE gt_invoice_header INDEX lv_index INTO wa_invoice_header.
  PERFORM set_invoice_highlight   USING wa_invoice_header.

  wa_selected_invoice = CORRESPONDING #( wa_invoice_header ).
  APPEND wa_selected_invoice TO gt_refresh_invoice_details.
  PERFORM refresh_invoice_details USING gt_refresh_invoice_details.

ENDFORM.

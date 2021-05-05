*&---------------------------------------------------------------------*
*& Report  ZPOS_MAGENTO_NOFIFY
*&
*&---------------------------------------------------------------------*
*&  Original Program used for inspiration
*&
*&   /SPE/DLV_IBGI_PROCESSING
*&---------------------------------------------------------------------*

REPORT  zpos_magento_nofify.

TABLES: nast.
DATA:
  lt_bapiret2 TYPE bapiret2_t,
  ls_bapiret2 TYPE bapiret2.

*
*&---------------------------------------------------------------------*
*&      Form  ENTRY
*&---------------------------------------------------------------------*
*       create new bill
*----------------------------------------------------------------------*
FORM entry_delivery USING returncode  TYPE sy-subrc
                 use_screen  TYPE c.

  DATA: lv_delivery  TYPE vbeln,
        lv_leave     TYPE c,                                "n_1238590
        lv_error     TYPE c,
        lt_msg_lines TYPE TABLE OF symsgv,
        lv_fieldname TYPE fieldname.

  FIELD-SYMBOLS: <ls_return> TYPE bapiret2,
                 <field>.
  DATA: lf_update_task   TYPE sy-subrc.

* Redirect this NAST processing to separate LUW - in case it is
* called with dispatching time 4 (immediate processing)
* during update task                                        "v_n_1238590
  CALL FUNCTION '/SPE/CALL_PROC_IN_NEW_LUW'
    EXPORTING
      is_nast  = nast
    IMPORTING
      ef_leave = lv_leave.
  IF NOT lv_leave IS INITIAL.
*   Per default we set the status on error, so that it can be reprocessed
*   in error processing (RSNAST0F) if there is any syntax error / update
*   termination / ... in the decoupled NAST processing.
    returncode = 4.
    RETURN.
  ENDIF.

  returncode = 0.
  lv_delivery = nast-objky.

  TRY.
      zcl_pos_ship_api=>shipping_notification_handler( EXPORTING iv_delivery = lv_delivery
                                                       IMPORTING ev_http_status  = DATA(lv_http_status)
                                                                 ev_http_message = DATA(lv_http_message)
                                                                 ev_body         = DATA(lv_body) ).

      IF cl_rest_status_code=>is_success( CONV #( lv_http_status ) ) = abap_true.

        returncode = 0.
        ls_bapiret2-type       = zcl_pos_util=>co_msgty-success.
        ls_bapiret2-id         = zcl_pos_util=>co_pos_msgid.
        ls_bapiret2-number     = '000'.
        ls_bapiret2-message_v1 = 'Shipment Created in Magento. ID : '.
        ls_bapiret2-message_v2 = lv_body.
        APPEND ls_bapiret2 TO lt_bapiret2. CLEAR ls_bapiret2.

      ELSE.

        lv_error = 'X'.
        ls_bapiret2-type       = zcl_pos_util=>co_msgty-error.
        ls_bapiret2-id         = zcl_pos_util=>co_pos_msgid.
        ls_bapiret2-number     = '000'.

        SPLIT lv_http_message AT cl_abap_char_utilities=>newline INTO TABLE lt_msg_lines.
        DATA(lv_lines) = lines( lt_msg_lines ).

        DATA(lv_counter) = 1.
        DO lv_lines TIMES.
          lv_fieldname = 'LS_BAPIRET2-MESSAGE_V' && lv_counter.
          ASSIGN (lv_fieldname) TO <field>.
          READ TABLE lt_msg_lines INDEX lv_counter INTO <field>.
          lv_counter += 1.
          IF lv_counter > 4.
            EXIT.
          ENDIF.
        ENDDO.

        APPEND ls_bapiret2 TO lt_bapiret2. CLEAR ls_bapiret2.

      ENDIF.

    CATCH zcx_pos_exception INTO DATA(lx_error). " POS Exception Class
      lv_error = 'X'.
      lt_bapiret2 = lx_error->get_bapiret2_table( ).
  ENDTRY.


  CALL FUNCTION 'NAST_PROTOCOL_INITIALIZE'.
  LOOP AT lt_bapiret2 ASSIGNING <ls_return>.
    IF <ls_return>-type = 'E'.
      lv_error = 'X'.
    ENDIF.
    PERFORM protocol_update_return USING <ls_return>.
  ENDLOOP.
  IF lv_error = 'X'.
    returncode = 1.
    use_screen = ' '.
  ELSE.
    returncode = 0.
  ENDIF.

ENDFORM.                    " ENTRY


FORM entry_billing USING returncode  TYPE sy-subrc
                 use_screen  TYPE c.

  DATA: lv_billing  TYPE vbeln,
        lv_leave     TYPE c,                                "n_1238590
        lv_error     TYPE c,
        lt_msg_lines TYPE TABLE OF symsgv,
        lv_fieldname TYPE fieldname.

  FIELD-SYMBOLS: <ls_return> TYPE bapiret2,
                 <field>.
  DATA: lf_update_task   TYPE sy-subrc.

* Redirect this NAST processing to separate LUW - in case it is
* called with dispatching time 4 (immediate processing)
* during update task                                        "v_n_1238590
  CALL FUNCTION '/SPE/CALL_PROC_IN_NEW_LUW'
    EXPORTING
      is_nast  = nast
    IMPORTING
      ef_leave = lv_leave.
  IF NOT lv_leave IS INITIAL.
*   Per default we set the status on error, so that it can be reprocessed
*   in error processing (RSNAST0F) if there is any syntax error / update
*   termination / ... in the decoupled NAST processing.
    returncode = 4.
    RETURN.
  ENDIF.

  returncode = 0.
  lv_billing = nast-objky.

  TRY.
      zcl_pos_billing_api=>billing_notification_handler( EXPORTING iv_billing = lv_billing
                                                         IMPORTING ev_http_status  = DATA(lv_http_status)
                                                                 ev_http_message = DATA(lv_http_message)
                                                                 ev_body         = DATA(lv_body) ).

      IF cl_rest_status_code=>is_success( CONV #( lv_http_status ) ) = abap_true.

        returncode = 0.
        ls_bapiret2-type       = zcl_pos_util=>co_msgty-success.
        ls_bapiret2-id         = zcl_pos_util=>co_pos_msgid.
        ls_bapiret2-number     = '000'.
        ls_bapiret2-message_v1 = 'Invoice & Capture Triggerd in Magento  '.
        ls_bapiret2-message_v2 = lv_body.
        APPEND ls_bapiret2 TO lt_bapiret2. CLEAR ls_bapiret2.

      ELSE.

        lv_error = 'X'.
        ls_bapiret2-type       = zcl_pos_util=>co_msgty-error.
        ls_bapiret2-id         = zcl_pos_util=>co_pos_msgid.
        ls_bapiret2-number     = '000'.

        SPLIT lv_http_message AT cl_abap_char_utilities=>newline INTO TABLE lt_msg_lines.
        DATA(lv_lines) = lines( lt_msg_lines ).

        DATA(lv_counter) = 1.
        DO lv_lines TIMES.
          lv_fieldname = 'LS_BAPIRET2-MESSAGE_V' && lv_counter.
          ASSIGN (lv_fieldname) TO <field>.
          READ TABLE lt_msg_lines INDEX lv_counter INTO <field>.
          lv_counter += 1.
          IF lv_counter > 4.
            EXIT.
          ENDIF.
        ENDDO.

        APPEND ls_bapiret2 TO lt_bapiret2. CLEAR ls_bapiret2.

      ENDIF.

    CATCH zcx_pos_exception INTO DATA(lx_error). " POS Exception Class
      lv_error = 'X'.
      lt_bapiret2 = lx_error->get_bapiret2_table( ).
  ENDTRY.


  CALL FUNCTION 'NAST_PROTOCOL_INITIALIZE'.
  LOOP AT lt_bapiret2 ASSIGNING <ls_return>.
    IF <ls_return>-type = 'E'.
      lv_error = 'X'.
    ENDIF.
    PERFORM protocol_update_return USING <ls_return>.
  ENDLOOP.
  IF lv_error = 'X'.
    returncode = 1.
    use_screen = ' '.
  ELSE.
    returncode = 0.
  ENDIF.

ENDFORM.                    " ENTRY

*----------------------------------------------------------------------*
FORM protocol_update_return USING p_return TYPE bapiret2.

  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = p_return-id
      msg_nr    = p_return-number
      msg_ty    = p_return-type
      msg_v1    = p_return-message_v1
      msg_v2    = p_return-message_v2
      msg_v3    = p_return-message_v3
      msg_v4    = p_return-message_v4
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.                    " protocol_update_return

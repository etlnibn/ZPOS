FUNCTION z_pos_esl_status_idoc_input.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(INPUT_METHOD) TYPE  INPUTMETHD
*"     REFERENCE(MASS_PROCESSING) TYPE  MASS_PROC
*"  EXPORTING
*"     REFERENCE(WORKFLOW_RESULT) TYPE  WF_RESULT
*"     REFERENCE(APPLICATION_VARIABLE) TYPE  APPL_VAR
*"     REFERENCE(IN_UPDATE_TASK) TYPE  UPDATETASK
*"     REFERENCE(CALL_TRANSACTION_DONE) TYPE  CALLTRANS2
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"  EXCEPTIONS
*"      WRONG_FUNCTION_CALLED
*"----------------------------------------------------------------------

  DATA: lv_error_flag       TYPE boole_d,
        lt_new_shelf_label  TYPE STANDARD TABLE OF zpos_shelf_lbl,
        lt_prev_shelf_label TYPE STANDARD TABLE OF zpos_shelf_lbl,
        ls_idoc_data        TYPE zpos_esl_status01,
        lv_idoc_status      TYPE edi_status,
        lv_tabix            TYPE sytabix,
        ls_edidd            TYPE edidd,
        lt_edidd            TYPE edidd_tt,
        lt_message          TYPE bapiret2_t.


  READ TABLE idoc_contrl INDEX 1.
  IF sy-subrc <> 0.
    EXIT.
  ELSEIF idoc_contrl-mestyp <> 'ZPOS_ESL_STATUS'.
    RAISE wrong_function_called.
  ENDIF.

  SORT idoc_data STABLE BY docnum segnum.
* go through all IDocs                                                 *
  LOOP AT idoc_contrl.

* Consider segments belonging to one IDoc                              *
    REFRESH lt_edidd.
    LOOP AT idoc_data WHERE docnum = idoc_contrl-docnum.
      APPEND idoc_data TO lt_edidd.
    ENDLOOP.

    CLEAR lv_error_flag.
    CLEAR ls_edidd.

    LOOP AT lt_edidd INTO ls_edidd.
      ls_idoc_data = ls_edidd-sdata.

      APPEND INITIAL LINE TO lt_new_shelf_label ASSIGNING FIELD-SYMBOL(<ls_shelf_label>).
      MOVE-CORRESPONDING ls_idoc_data TO <ls_shelf_label>.

    ENDLOOP.


  ENDLOOP.
  SORT lt_new_shelf_label BY werks ean11 ASCENDING.


  IF lt_new_shelf_label IS NOT INITIAL.

    SELECT * FROM zpos_shelf_lbl
           INTO TABLE lt_prev_shelf_label
           FOR ALL ENTRIES IN lt_new_shelf_label
           WHERE werks = lt_new_shelf_label-werks
           AND   ean11 = lt_new_shelf_label-ean11.

* Now loop through the existing entries and see which one can be modified
    LOOP AT lt_prev_shelf_label ASSIGNING <ls_shelf_label>.

      READ TABLE lt_new_shelf_label INTO DATA(ls_shelf_label) WITH KEY werks = <ls_shelf_label>-werks ean11 = <ls_shelf_label>-ean11 BINARY SEARCH.

      IF sy-subrc = 0.
* This could be true or false (an update can be either)
        <ls_shelf_label>-esl_active = ls_shelf_label-esl_active.
      ELSE.
* This is a strange situation - which should never happen
        lv_error_flag = abap_true.
        APPEND INITIAL LINE TO lt_message ASSIGNING FIELD-SYMBOL(<ls_message>).
        <ls_message>-type = 'W'.
        <ls_message>-id = 'ZPOS'.
        <ls_message>-number = '011'.
        <ls_message>-message_v1 = CONV #( <ls_shelf_label>-werks ).
        <ls_message>-message_v2 = CONV #( <ls_shelf_label>-ean11 ).

      ENDIF.

    ENDLOOP.

* Commit the changes
    MODIFY zpos_shelf_lbl FROM TABLE lt_prev_shelf_label.

  ENDIF.

  IF lv_error_flag = abap_false.

    CLEAR idoc_status.
    idoc_status-status   = '53'.
    idoc_status-msgty    = 'S'.
    idoc_status-docnum   = idoc_contrl-docnum.
    idoc_status-repid    = sy-repid.
    INSERT idoc_status INDEX 1.

  ELSE.

    DATA(lt_lognumber) = zcl_pos_util=>create_appl_log( EXPORTING
    iv_object = 'ZPOS'
    iv_subobject = 'ESL'
    it_message = lt_message ).

    LOOP AT lt_lognumber INTO DATA(ls_lognumber).

      idoc_status-status   = '52'.
      idoc_status-msgty    = 'W'.
      idoc_status-msgid    = 'ZPOS'.
      idoc_status-msgno    = '010'.
      idoc_status-appl_log = ls_lognumber-lognumber.
*    idoc_status-msgv1    = retn_info-message_v1.
*    idoc_status-msgv2    = retn_info-message_v2.
*    idoc_status-msgv3    = retn_info-message_v3.
*    idoc_status-msgv4    = retn_info-message_v4.
      idoc_status-docnum   = idoc_contrl-docnum.
      idoc_status-repid    = sy-repid.
      INSERT idoc_status INDEX 1.

    ENDLOOP.

  ENDIF.


ENDFUNCTION.

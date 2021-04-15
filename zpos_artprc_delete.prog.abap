*&---------------------------------------------------------------------*
*& Report ZPOS_ARTPRC_DELETE
*&---------------------------------------------------------------------*
*& Creates Outbound Article IDoc
*&---------------------------------------------------------------------*
REPORT zpos_artprc_delete.

INCLUDE zpos_artprc_delete_top.
INCLUDE zpos_artprc_delete_sel.
INCLUDE zpos_artprc_delete_impl.


*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization_handler( ).


*----------------------------------------------------------------------*
* SELECTION-SCREEN.                                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  lcl_report=>selection_screen_handler( ).

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT.                                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  lcl_report=>screen_output_handler( ).


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*  IF gr_report IS INITIAL.
*    gr_report = NEW #( ).
*  ENDIF.
*
*  IF gr_report IS NOT BOUND.
*    MESSAGE s000(zpos) WITH 'Report object not initialized'(003).
*    LEAVE LIST-PROCESSING.
*  ENDIF.

  lcl_report=>check_mandatory_data( ).

  IF sy-batch = abap_false.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        default_button        = 2
        titlebar              = 'Deletion Confirmation'
        text_question         = 'Select OK to continue and delete data records?'
        text_button_1         = 'OK'
*       icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Cancel'
*       icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ' '
        popup_type            = 'ICON_MESSAGE_ERROR'
      IMPORTING
        answer                = gv_answer.

    IF gv_answer = 2.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ELSE.
    gv_answer = 1.
  ENDIF.


  IF gv_answer = 1.

    zcl_pos_wso_article_price=>delete_article_price_data(
                       EXPORTING
                          iv_status             = p_sts
                          iv_older_than         = p_dat
                          it_r_vkorg            = s_vkorg[]
                          it_r_vtweg            = s_vtweg[]
                          it_r_werks            = s_werks[]
                          it_r_recipient        = s_recp[]
                          )  .

  ENDIF.


*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

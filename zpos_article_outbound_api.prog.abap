*&---------------------------------------------------------------------*
*& Report ZPOS_ARTICLE_OUTBOUND
*&---------------------------------------------------------------------*
*& Creates Outbound Article IDoc
*&---------------------------------------------------------------------*
REPORT zpos_article_outbound_api.

INCLUDE zpos_article_outbound_api_top.
INCLUDE zpos_article_outbound_api_sel.
INCLUDE zpos_article_outbound_api_impl.


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

  lcl_report=>check_mandatory_data( ).

  TRY.

      IF sy-uname = 'BRENNANN' or sy-uname = 'MENDESJ'.
        zcl_pos_article_2_api=>article_collective_handler(
                       EXPORTING
                          iv_status   = p_sts
                          iv_valid_on = p_dat
                          iv_vkorg    = p_vkorg
                          it_r_werks  = s_werks[]
                          it_r_matnr  = s_matnr[]
                          )  .
      ELSE.

        zcl_pos_article_api=>article_collective_handler(
                       EXPORTING
                          iv_status   = p_sts
                          iv_valid_on = p_dat
                          iv_vkorg    = p_vkorg
                          it_r_werks  = s_werks[]
                          it_r_matnr  = s_matnr[]
                          )  .

      ENDIF.

    CATCH zcx_pos_exception INTO DATA(lo_error).
      MESSAGE lo_error->get_text( ) TYPE 'I'.
  ENDTRY.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

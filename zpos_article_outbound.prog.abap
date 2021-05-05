*&---------------------------------------------------------------------*
*& Report ZPOS_ARTICLE_OUTBOUND
*&---------------------------------------------------------------------*
*& Creates Outbound Article IDoc
*&---------------------------------------------------------------------*
REPORT zpos_article_outbound.

INCLUDE zpos_article_outbound_top.
INCLUDE zpos_article_outbound_sel.
INCLUDE zpos_article_outbound_impl.


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
      zcl_pos_wso_article_price=>create_outbound_article_idoc(
                     EXPORTING
                        iv_records            = p_recs
                        iv_create_bom         = p_bom
                        iv_status             = p_sts
                        iv_valid_on           = p_dat
                        iv_leadtime           = p_lead
                        iv_vkorg              = p_vkorg
                        it_r_vtweg            = s_vtweg[]
                        it_r_werks            = s_werks[]
                        it_r_matnr            = s_matnr[]
                        it_r_recipient        = s_recp[]
                        )  .

    CATCH zcx_pos_exception INTO DATA(lo_error).
      MESSAGE lo_error->get_text( ) TYPE 'I'.
  ENDTRY.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

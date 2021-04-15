*&---------------------------------------------------------------------*
*& Report ZPOS_PRICELIST
*&---------------------------------------------------------------------*
*& Persists Price List Data
*&---------------------------------------------------------------------*
REPORT zpos_persist_pricelist.

INCLUDE zpos_persist_pricelist_top.
INCLUDE zpos_persist_pricelist_sel.
INCLUDE zpos_persist_pricelist_impl.


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

  zcl_pos_wso_article_price=>create_persist_price_data(
                   EXPORTING
                      iv_initial_load = p_full        " You can always pass the single parameter - As it is either true of false
                      iv_datab              = p_datab
                      iv_datbi              = p_datbi
                      it_r_appl             = s_appl[]
                      it_r_recipient        = s_recp[]
                      iv_update_wind_status = p_wind
                      )  .

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

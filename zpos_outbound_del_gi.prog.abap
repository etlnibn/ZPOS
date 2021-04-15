*&---------------------------------------------------------------------*
*& Report ZPOS_OUTBOUND_DEL_GI
*&---------------------------------------------------------------------*
*& Testiung Utility for Outbound Delivery to Trigger the GI process
*&---------------------------------------------------------------------*
REPORT zpos_outbound_del_gi.

INCLUDE zpos_outbound_del_gi_top.
INCLUDE zpos_outbound_del_gi_sel.
INCLUDE zpos_outbound_del_gi_impl.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization_handler( ).
  FREE gr_report.


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

  DATA(lv_docnum) = zcl_pos_ship_api=>delivery_pgi(
                   EXPORTING
                      iv_delivery            = p_delv
*                      iv_partner             = p_part
                      iv_increase_inventory  = p_invt )  .

*  IF NOT lv_docnum IS INITIAL.
*    CLEAR gt_r_docnum.
*    APPEND INITIAL LINE TO gt_r_docnum ASSIGNING FIELD-SYMBOL(<ls_docnum>).
*    <ls_docnum>-sign   = zcl_pos_util=>co_selopt_sign_i.
*    <ls_docnum>-option = zcl_pos_util=>co_selopt_opt_eq.
*    <ls_docnum>-low    = lv_docnum.
*
**RBDAPP01 DCN
*    SET PARAMETER ID: lv_docnum FIELD 'DCN'  .    " Not Used anymore
*    SUBMIT rbdapp01 WITH docnum IN gt_r_docnum AND RETURN.
*  ENDIF.


*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

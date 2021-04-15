*&---------------------------------------------------------------------*
*& Report ZPOS_MAG_ORDER_TEST
*&---------------------------------------------------------------------*
*& Testiung Utility for Magento Orders
*&---------------------------------------------------------------------*
REPORT zpos_mag_order_test.

INCLUDE zpos_mag_order_test_top.
INCLUDE zpos_mag_order_test_sel.
INCLUDE zpos_mag_order_test_impl.


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

  SELECT SINGLE vbeln FROM vbkd INTO @DATA(lv_order)
     WHERE bstkd = @p_ord.

  IF lv_order IS NOT INITIAL.

    IF p_delv = abap_true.
      DATA(lv_delivery) = zcl_pos_mag_order_test=>create_delivery( iv_order = lv_order ).
    ENDIF.

    IF p_pgi = abap_true OR p_bill = abap_true AND lv_delivery IS INITIAL.
      lv_delivery = zcl_pos_mag_order_test=>get_delivery_via_docflow( iv_order = lv_order ).
      IF lv_delivery IS INITIAL.
        MESSAGE i000(zpos) WITH 'Delivery not found for this order'.
      ENDIF.
    ENDIF.

    IF p_pgi = abap_true AND lv_delivery IS NOT INITIAL.
      zcl_pos_mag_order_test=>perform_delivery_pgi( iv_delivery = lv_delivery
                                                    iv_increase_inventory = p_invt ).
    ENDIF.

    IF p_bill = abap_true AND lv_delivery IS NOT INITIAL.
      zcl_pos_mag_order_test=>create_billing_document( iv_delivery = lv_delivery ).
    ENDIF.

    IF p_flow = abap_true.

      APPEND INITIAL LINE TO gt_r_docnum ASSIGNING FIELD-SYMBOL(<ls_docnum>).
      <ls_docnum>-sign   = zcl_pos_util=>co_selopt_sign_i.
      <ls_docnum>-option = zcl_pos_util=>co_selopt_opt_eq.
      <ls_docnum>-low    = lv_order.
      WAIT UP TO 2 SECONDS.
      SUBMIT ribelf20 WITH s_vbeln5 IN gt_r_docnum AND RETURN.
    ENDIF.

  ELSE.
    MESSAGE i000(zpos) WITH 'Magento Order not Found'.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

class ZCL_IM_SD_MAG_SHIP_NOTIFY definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_V56K .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SD_MAG_SHIP_NOTIFY IMPLEMENTATION.


  METHOD if_ex_badi_v56k~raw_data_manipulation.

    DATA: lt_item  TYPE zpos_magneto_ship_item_tty,
          lt_track TYPE zpos_magneto_ship_track_tty.



    LOOP AT tab_vbak INTO DATA(ls_vbak).
      CLEAR: lt_item, lt_track.

      IF NOT ls_vbak-ihrez IS INITIAL.

        LOOP AT tab_vbkd INTO DATA(ls_vbkd) WHERE bstkd IS NOT INITIAL.
          READ TABLE tab_lips INTO DATA(ls_lips) WITH KEY vgbel = ls_vbkd-vbeln vgpos = ls_vbkd-posnr.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
            <ls_item>-order_item_id = ls_vbkd-bstkd.
            <ls_item>-qty = ls_lips-lfimg.
          ENDIF.

        ENDLOOP.

        LOOP AT tab_likp INTO DATA(ls_likp).
          APPEND INITIAL LINE TO lt_track ASSIGNING FIELD-SYMBOL(<ls_track>).
          <ls_track>-carrier_code = ls_likp-route+2(4).
          <ls_track>-track_number = ls_likp-bolnr.
*** USING HARDCODED VALUES FOR NOW
          <ls_track>-title        = 'BAUHAUS GBG' .
        ENDLOOP.



        IF NOT lt_item IS INITIAL. " AND lt_track IS NOT INITIAL.
          TRY.
              DATA(rlv_body) = zcl_pos_ship_api=>shipping_notification_handler(
                  it_item  = lt_item
                  it_track = lt_track
                  iv_order_id = CONV #( ls_vbak-ihrez ) ).

            CATCH zcx_pos_exception INTO DATA(lx_error).
*   Can't really do much about this
          ENDTRY.

        ENDIF.

      ENDIF.


    ENDLOOP.


  ENDMETHOD.
ENDCLASS.

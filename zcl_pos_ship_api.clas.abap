class ZCL_POS_SHIP_API definition
  public
  final
  create public .

public section.

  class-methods SHIPPING_NOTIFICATION_HANDLER
    importing
      !IV_DELIVERY type VBELN
    exporting
      !EV_HTTP_STATUS type ZPOS_HTTP_STATUS
      !EV_HTTP_MESSAGE type ZPOS_HTTP_MESSAGE
      !EV_BODY type STRING
    raising
      ZCX_POS_EXCEPTION .
  PROTECTED SECTION.
private section.

  class-data:
    mt_r_docnum TYPE RANGE OF edi_docnum .

  class-methods MAP_SHIP_NOTIFY_DATA
    importing
      !IT_ITEM type ZPOS_MAGNETO_SHIP_ITEM_TTY
      !IT_TRACK type ZPOS_MAGNETO_SHIP_TRACK_TTY
    returning
      value(RV_BODY) type STRING
    raising
      ZCX_POS_EXCEPTION .
  class-methods GET_DELIVERY_DATA
    importing
      !IV_DELIVERY type VBELN
    exporting
      !ES_LIKP type LIKP
      !ET_LIPS type TAB_LIPS
      !ET_VBAK type TAB_VBAK
      !ET_VBKD type TAB_VBKD
      !EV_TRACKING type STRING
    raising
      ZCX_POS_EXCEPTION .
ENDCLASS.



CLASS ZCL_POS_SHIP_API IMPLEMENTATION.


  METHOD get_delivery_data.

    DATA: lt_text TYPE STANDARD TABLE OF tline,
          lv_name TYPE tdobname.

    TYPES: BEGIN OF tp_s_vbak,
             vbeln TYPE vbeln,
           END OF tp_s_vbak.

    TYPES: BEGIN OF tp_s_vbap,
             vbeln TYPE vbeln,
             posnr TYPE posnr,
           END OF tp_s_vbap.

    DATA: lt_sel_vbak  TYPE STANDARD TABLE OF tp_s_vbak,
          lt_sel_vbap  TYPE STANDARD TABLE OF tp_s_vbap,
          ls_textid    TYPE scx_t100key,
          lx_exception TYPE REF TO zcx_pos_exception.


    SELECT SINGLE vbeln, route, bolnr INTO CORRESPONDING FIELDS OF @es_likp FROM likp
             WHERE vbeln = @iv_delivery.


    IF es_likp IS INITIAL.
      CLEAR ls_textid.
      ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
      ls_textid-msgno = '060'.
      ls_textid-attr1 = 'LIKP'.
      ls_textid-attr2 = iv_delivery.
      lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
      RAISE EXCEPTION lx_exception.
    ENDIF.


    SELECT vbeln, posnr, lfimg, vgbel, vgpos, vgtyp INTO CORRESPONDING FIELDS OF TABLE @et_lips FROM lips
             WHERE vbeln = @iv_delivery.
    SORT et_lips BY vbeln posnr.


    IF et_lips IS INITIAL.
      CLEAR ls_textid.
      ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
      ls_textid-msgno = '060'.
      ls_textid-attr1 = 'LIPS'.
      ls_textid-attr2 = iv_delivery.
      lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
      RAISE EXCEPTION lx_exception.
    ENDIF.


    LOOP AT et_lips INTO DATA(ls_lips) WHERE NOT vgbel IS INITIAL
                     AND  vgtyp EQ if_sd_doc_category=>inquiry OR  "CA vbtyp_verk.
                          vgtyp EQ if_sd_doc_category=>quotation OR
                          vgtyp EQ if_sd_doc_category=>order OR
                          vgtyp EQ if_sd_doc_category=>sched_agree OR
                          vgtyp EQ if_sd_doc_category=>sched_agree_ext_serv_agent OR
                          vgtyp EQ if_sd_doc_category=>returns OR
                          vgtyp EQ if_sd_doc_category=>order_wo_charge OR
                          vgtyp EQ if_sd_doc_category=>independent_reqts_plan OR
                          vgtyp EQ if_sd_doc_category=>item_proposal OR
                          vgtyp EQ if_sd_doc_category=>contract OR
                          vgtyp EQ if_sd_doc_category=>credit_memo_req OR
                          vgtyp EQ if_sd_doc_category=>debit_memo_req OR
                          vgtyp EQ if_sd_doc_category=>master_contract
                      AND vgsys IS INITIAL.                 "n_2063032.

      APPEND ls_lips-vgbel TO lt_sel_vbak.
      IF NOT ls_lips-vgpos IS INITIAL.
        APPEND INITIAL LINE TO lt_sel_vbap ASSIGNING FIELD-SYMBOL(<ls_sel_vbap>).
        <ls_sel_vbap>-vbeln = ls_lips-vgbel.
        <ls_sel_vbap>-posnr = ls_lips-vgpos.
      ENDIF.

      lv_name = iv_delivery.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'Z001'
          language                = 'E'
          name                    = lv_name
          object                  = 'VBBK'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
* IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = lt_text
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        READ TABLE lt_text INDEX 1 INTO ev_tracking.

      ELSE.
* Implement suitable error handling here
      ENDIF.


    ENDLOOP.
    SORT lt_sel_vbak.  DELETE ADJACENT DUPLICATES FROM lt_sel_vbak.
    SORT lt_sel_vbap.  DELETE ADJACENT DUPLICATES FROM lt_sel_vbap.

    IF NOT lt_sel_vbak[] IS INITIAL.
      SELECT vbeln, ihrez FROM vbak INTO CORRESPONDING FIELDS OF TABLE @et_vbak
        FOR ALL ENTRIES IN @lt_sel_vbak
          WHERE vbeln = @lt_sel_vbak-vbeln.
      SORT et_vbak BY vbeln.

      IF et_vbak IS INITIAL.
        CLEAR ls_textid.
        ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
        ls_textid-msgno = '060'.
        ls_textid-attr1 = 'VBAK'.
        ls_textid-attr2 = iv_delivery.
        lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
        RAISE EXCEPTION lx_exception.
      ENDIF.


      SELECT vbeln, posnr, bstkd FROM vbkd INTO CORRESPONDING FIELDS OF TABLE @et_vbkd
        FOR ALL ENTRIES IN @lt_sel_vbak
          WHERE vbeln = @lt_sel_vbak-vbeln.
      SORT et_vbkd BY vbeln.

      IF et_vbkd IS INITIAL.
        CLEAR ls_textid.
        ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
        ls_textid-msgno = '060'.
        ls_textid-attr1 = 'VBKD'.
        ls_textid-attr2 = iv_delivery.
        lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
        RAISE EXCEPTION lx_exception.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD map_ship_notify_data.

    DATA: ls_shipment TYPE zpos_magneto_ship_sty.

    ls_shipment-items  = it_item.
    ls_shipment-tracks = it_track.

    ls_shipment-notify = abap_true.

* Convert to JSON
    rv_body = /ui2/cl_json=>serialize( data = ls_shipment compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.


  METHOD shipping_notification_handler.


    DATA: ls_textid    TYPE scx_t100key,
          lx_exception TYPE REF TO zcx_pos_exception,
          lv_string    TYPE string.

    DATA: lt_item  TYPE zpos_magneto_ship_item_tty,
          lt_track TYPE zpos_magneto_ship_track_tty.

    get_delivery_data(
      EXPORTING
        iv_delivery = iv_delivery
      IMPORTING
        es_likp     = DATA(ls_likp)
        et_lips     = DATA(lt_lips)
        et_vbak     = DATA(lt_vbak)
        et_vbkd     = DATA(lt_vbkd)
        ev_tracking = DATA(lv_tracking)
    ).


    IF ls_likp-route IS  INITIAL.
      CLEAR ls_textid.
      ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
      ls_textid-msgno = '061'.
      ls_textid-attr1 = iv_delivery. SHIFT ls_textid-attr1 LEFT DELETING LEADING '0'.
      lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
      RAISE EXCEPTION lx_exception.
    ENDIF.

* Retrieve the carrier information
    SELECT SINGLE * FROM zmag_carriers INTO @DATA(ls_carriers) WHERE partnerid = @ls_likp-route.

    IF ls_carriers IS INITIAL.
      CLEAR ls_textid.
      ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
      ls_textid-msgno = '060'.
      ls_textid-attr1 = 'ZMAG_CARRIERS'.
      ls_textid-attr2 = iv_delivery. SHIFT ls_textid-attr2 LEFT DELETING LEADING '0'.
      lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
      RAISE EXCEPTION lx_exception.
    ENDIF.


    LOOP AT lt_vbak INTO DATA(ls_vbak).
      CLEAR: lt_item, lt_track.

      IF NOT ls_vbak-ihrez IS INITIAL.

        LOOP AT lt_lips INTO DATA(ls_lips).

          READ TABLE lt_vbkd INTO DATA(ls_vbkd) WITH KEY vbeln = ls_lips-vgbel posnr = ls_lips-vgpos.
          IF sy-subrc = 0.
            IF  ls_vbkd-bstkd IS NOT INITIAL.
              APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
              <ls_item>-order_item_id = ls_vbkd-bstkd.
              <ls_item>-qty = ls_lips-lfimg.
            ELSE.
              CLEAR ls_textid.
              ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
              ls_textid-msgno = '064'.
              ls_textid-attr1 = ls_vbkd-vbeln. SHIFT ls_textid-attr1 LEFT DELETING LEADING '0'.
              ls_textid-attr2 = ls_vbkd-posnr. SHIFT ls_textid-attr2 LEFT DELETING LEADING '0'.
              lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
              RAISE EXCEPTION lx_exception.
            ENDIF.
          ENDIF.

        ENDLOOP.

        APPEND INITIAL LINE TO lt_track ASSIGNING FIELD-SYMBOL(<ls_track>).
        <ls_track>-carrier_code = ls_carriers-carrier_code.

* Temporary Fix for S4D for testing purposes
        IF sy-sysid = 'S4D' AND lv_tracking IS INITIAL.

          DATA(lr_random) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                      min  = 10000000
                                      max  = 99999999 ).
          <ls_track>-track_number = |{ ls_carriers-url } { lr_random->get_next( ) } |.
* Temporary Fix for S4D for testing purposes


        ELSEIF sy-sysid NE 'S4D' AND lv_tracking IS INITIAL.
          CLEAR ls_textid.
          ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ls_textid-msgno = '062'.
          ls_textid-attr1 = iv_delivery. SHIFT ls_textid-attr1 LEFT DELETING LEADING '0'.
          lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION lx_exception.
        ELSE.
          <ls_track>-track_number = ls_carriers-url && lv_tracking.
        ENDIF.
        CONDENSE <ls_track>-track_number NO-GAPS.

        <ls_track>-title        = ls_carriers-partner_name .

        IF NOT lt_item IS INITIAL. " AND lt_track IS NOT INITIAL.

          DATA(lv_body) = map_ship_notify_data( it_item  = lt_item it_track = lt_track ).

          DATA(lv_endpoint) = zcl_pos_magento_rest_api=>build_endpoint_string( iv_type = zcl_pos_magento_rest_api=>co_endpoint-shipment iv_data = CONV #( ls_vbak-ihrez ) ).

          zcl_pos_magento_rest_api=>rest_operation( EXPORTING iv_body = lv_body iv_endpoint = lv_endpoint iv_operation = zcl_pos_magento_rest_api=>co_http_operation-post
                     IMPORTING ev_http_message = ev_http_message ev_http_status = ev_http_status ev_body = ev_body ).

*          IF sy-uname = 'BRENNANN'.
*            DATA(out) = cl_demo_output=>new( )->begin_section( 'Shipping Notification - Response Code : ' && ev_http_status ).
*            out->write_json( ev_body ).
*            out->display( ).
*          ENDIF.

        ENDIF.    " IF NOT lt_item IS INITIAL. " AND lt_track IS NOT INITIAL.

      ELSE.

        CLEAR ls_textid.
        ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
        ls_textid-msgno = '063'.
        ls_textid-attr1 = ls_vbak-vbeln. SHIFT ls_textid-attr1 LEFT DELETING LEADING '0'.
        lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
        RAISE EXCEPTION lx_exception.

      ENDIF.      " IF NOT ls_vbak-ihrez IS INITIAL.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

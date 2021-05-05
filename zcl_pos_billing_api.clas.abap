class ZCL_POS_BILLING_API definition
  public
  final
  create public .

public section.

  class-methods BILLING_NOTIFICATION_HANDLER
    importing
      !IV_BILLING type VBELN
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

  class-methods MAP_BILLING_NOTIFY_DATA
    returning
      value(RV_BODY) type STRING
    raising
      ZCX_POS_EXCEPTION .
  class-methods GET_MAGENTO_ORDERID
    importing
      !IV_BILLING type VBELN
    returning
      value(RV_ORDERID) type IHREZ
    raising
      ZCX_POS_EXCEPTION .
ENDCLASS.



CLASS ZCL_POS_BILLING_API IMPLEMENTATION.


  METHOD billing_notification_handler.


    DATA: ls_textid    TYPE scx_t100key,
          lx_exception TYPE REF TO zcx_pos_exception,
          lv_string    TYPE string.

    DATA(lv_magento_orderid) = get_magento_orderid( iv_billing ).

    DATA(lv_body) = map_billing_notify_data(  ).

    DATA(lv_endpoint) = zcl_pos_magento_rest_api=>build_endpoint_string( iv_type = zcl_pos_magento_rest_api=>co_endpoint-invoice iv_data = CONV #( lv_magento_orderid ) ).

    zcl_pos_magento_rest_api=>rest_operation( EXPORTING iv_body = lv_body iv_endpoint = lv_endpoint iv_operation = zcl_pos_magento_rest_api=>co_http_operation-post
                    IMPORTING ev_http_message = ev_http_message ev_http_status = ev_http_status ev_body = ev_body ).

*          IF sy-uname = 'BRENNANN'.
*            DATA(out) = cl_demo_output=>new( )->begin_section( 'Shipping Notification - Response Code : ' && ev_http_status ).
*            out->write_json( ev_body ).
*            out->display( ).
*          ENDIF.


  ENDMETHOD.


  METHOD get_magento_orderid.

    DATA: ls_textid    TYPE scx_t100key,
          lx_exception TYPE REF TO zcx_pos_exception,
          lt_vbkd      TYPE STANDARD TABLE OF vbkd.

    SELECT vbeln, posnr, aubel, aupos FROM vbrp INTO TABLE @DATA(lt_vbrp)
           WHERE vbeln = @iv_billing.

    IF sy-subrc = 0.

      SELECT vbeln, ihrez FROM vbkd INTO CORRESPONDING FIELDS OF TABLE @lt_vbkd
        FOR ALL ENTRIES IN @lt_vbrp
        WHERE vbeln = @lt_vbrp-aubel
        AND   posnr = '000000'.

      READ TABLE lt_vbkd INDEX 1 INTO DATA(ls_vbkd).
      rv_orderid = ls_vbkd-ihrez.

      IF ls_vbkd-ihrez IS INITIAL.
        CLEAR ls_textid.
        ls_textid-msgid = zcl_pos_util=>co_pos_msgid.
        ls_textid-msgno = '060'.
        ls_textid-attr1 = 'VBKD'.
        ls_textid-attr2 = iv_billing.
        lx_exception = NEW zcx_pos_exception( textid = ls_textid msgty = zcl_pos_util=>co_msgty-error ).
        RAISE EXCEPTION lx_exception.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD map_billing_notify_data.

*
*{
*    "capture": true,
*    "notify": false
*}

    DATA: BEGIN OF ls_capture,
            capture TYPE boole_d,
            notify  TYPE boole_d,
          END OF ls_capture.

    ls_capture-capture = abap_true.
    ls_capture-notify  = abap_false.

* Convert to JSON
    rv_body = /ui2/cl_json=>serialize( data = ls_capture compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.
ENDCLASS.

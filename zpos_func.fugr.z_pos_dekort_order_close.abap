FUNCTION Z_POS_DEKORT_ORDER_CLOSE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DOCNUM) TYPE  VBELN_VA
*"     VALUE(IV_REVERSAL) TYPE  BOOLE_D DEFAULT ABAP_FALSE
*"  TABLES
*"      ET_MESSAGE TYPE  BAPIRET2_TT
*"----------------------------------------------------------------------

*"----------------------------------------------------------------------
* Created by Niall Brennan 2021-January
*
* Purpose:  To close Dekort Orders (or reverse the closing)
*           Most likely scenario is from a POS transaction
*           The FM is RFC enabled and can be called from CAR
*           when processing the incoming data, either via a task
*           or directly in the inbound processing
*"----------------------------------------------------------------------


  DATA: ls_header_in      TYPE bapisdh1,
        ls_header_inx     TYPE bapisdh1x,
        lt_order_item_in  TYPE bapisditm_tt,
        lt_order_item_inx TYPE bapisditmx_tt,
        lt_message        TYPE bapiret2_tt.

  CONSTANTS: co_reason TYPE char2 VALUE 'ZD'.

*----------------------------------------------------------------------------
* ORDER HEADER
*----------------------------------------------------------------------------
  ls_header_in-dlv_block   = COND #( WHEN iv_reversal = abap_false THEN co_reason ELSE '' ).
  ls_header_in-bill_block  = COND #( WHEN iv_reversal = abap_false THEN co_reason ELSE '' ).

  ls_header_inx-updateflag  = 'U'.
  ls_header_inx-dlv_block   = abap_true.
  ls_header_inx-bill_block  = abap_true.


*----------------------------------------------------------------------------
* ORDER ITEM
*----------------------------------------------------------------------------
* Get the Order Lines for the update.
  SELECT posnr FROM vbap INTO TABLE @DATA(lt_data) WHERE vbeln = @iv_docnum.

  IF NOT lt_data IS INITIAL.

    LOOP AT lt_data INTO DATA(ls_data).
      APPEND INITIAL LINE TO lt_order_item_in ASSIGNING FIELD-SYMBOL(<ls_order_item_in>).
      <ls_order_item_in>-itm_number = ls_data-posnr.
      <ls_order_item_in>-reason_rej = COND #( WHEN iv_reversal = abap_false THEN co_reason ELSE '' ).

      APPEND INITIAL LINE TO lt_order_item_inx ASSIGNING FIELD-SYMBOL(<ls_order_item_inx>).
      <ls_order_item_inx>-updateflag = 'U'.
      <ls_order_item_inx>-itm_number = ls_data-posnr.
      <ls_order_item_inx>-reason_rej = abap_true.

    ENDLOOP.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = iv_docnum
        order_header_in  = ls_header_in
        order_header_inx = ls_header_inx
      TABLES
        return           = et_message
        order_item_in    = lt_order_item_in
        order_item_inx   = lt_order_item_inx.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDIF.

ENDFUNCTION.

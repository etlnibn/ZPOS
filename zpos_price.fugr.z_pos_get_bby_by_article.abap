FUNCTION z_pos_get_bby_by_article .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_VTWEG) TYPE  VTWEG DEFAULT '90'
*"     REFERENCE(IV_VKORG) TYPE  VKORG DEFAULT '1090'
*"     REFERENCE(IV_WERKS) TYPE  WERKS_D DEFAULT 'C003'
*"     REFERENCE(IV_PRICE_DATE) TYPE  DATS OPTIONAL
*"     REFERENCE(IV_CURRENCY) TYPE  WAERS DEFAULT 'EUR'
*"     REFERENCE(IT_SALES_ORDER) TYPE  ZPOS_ORDER_LINE_TTY
*"  EXPORTING
*"     REFERENCE(ET_PRICED_ORDER) TYPE  ZPOS_BBY_ORDER_TOTAL_TTY
*"     REFERENCE(ET_OPEN_ORDER) TYPE  ZPOS_ORDER_LINE_TTY
*"----------------------------------------------------------------------

  zcl_pos_bby_price=>initial_bby_selection(
           EXPORTING
                     it_sales_order  = it_sales_order
                     iv_vkorg        = iv_vkorg
                     iv_vtweg        = iv_vtweg
                     iv_werks      = iv_werks
                     iv_price_date = iv_price_date
                     iv_currency   = iv_currency
           IMPORTING
                     et_priced_order = et_priced_order
                     et_open_order   = et_open_order ).

ENDFUNCTION.

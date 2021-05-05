FUNCTION z_pos_get_bby_by_article .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_VTWEG) TYPE  VTWEG OPTIONAL
*"     REFERENCE(IV_VKORG) TYPE  VKORG OPTIONAL
*"     REFERENCE(IV_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(IV_PRICE_DATE) TYPE  DATS OPTIONAL
*"     REFERENCE(IV_CURRENCY) TYPE  WAERS OPTIONAL
*"     REFERENCE(IT_SALES_ORDER) TYPE  ZPOS_ORDER_LINE_TTY
*"     REFERENCE(IT_R_AKTNR) TYPE  FIP_T_AKTRN_RANGE
*"  EXPORTING
*"     REFERENCE(ET_PRICED_ORDER) TYPE  ZPOS_BBY_TOTAL_TTY
*"     REFERENCE(ET_OPEN_ORDER) TYPE  ZPOS_ORDER_LINE_TTY
*"----------------------------------------------------------------------

  zcl_pos_bby_price=>calc_sales_order_price(
    EXPORTING
      iv_vtweg        = iv_vtweg
      iv_vkorg        = iv_vkorg
      iv_werks        = iv_werks
      iv_price_date   = iv_price_date
      iv_currency     = iv_currency
      it_sales_order  = it_sales_order
      it_r_aktnr      = it_r_aktnr
    IMPORTING
      et_priced_order =  et_priced_order
      et_open_order   =  et_open_order
  ).


ENDFUNCTION.

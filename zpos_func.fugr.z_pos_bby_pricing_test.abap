FUNCTION z_pos_bby_pricing_test.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_VTWEG) TYPE  VTWEG DEFAULT '90'
*"     REFERENCE(IV_VKORG) TYPE  VKORG DEFAULT '1090'
*"     REFERENCE(IV_WERKS) TYPE  WERKS_D DEFAULT 'C003'
*"     REFERENCE(IV_PRICE_DATE) TYPE  DATS DEFAULT SY-DATUM
*"     REFERENCE(IV_CURRENCY) TYPE  WAERS DEFAULT 'EUR'
*"     REFERENCE(IT_SALES_ORDER) TYPE  ZPOS_ORDER_LINE_TTY
*"     REFERENCE(IT_R_AKTNR) TYPE  FIP_T_AKTRN_RANGE
*"  EXPORTING
*"     REFERENCE(ET_PRICED_ORDER) TYPE  ZPOS_BBY_TOTAL_TTY
*"     REFERENCE(ET_OPEN_ORDER) TYPE  ZPOS_ORDER_LINE_TTY
*"----------------------------------------------------------------------


  CALL METHOD zcl_pos_bby_price=>calc_sales_order_price
    EXPORTING
      iv_vtweg        = iv_vtweg
      iv_vkorg        = iv_vkorg
      iv_werks        = iv_werks
      iv_price_date   = iv_price_date
      iv_currency     = iv_currency
      it_sales_order  = it_sales_order
      it_r_aktnr      = it_r_aktnr
    IMPORTING
      et_priced_order = et_priced_order
      et_open_order   = et_open_order.



*  FIELD-SYMBOLS:
*      <ls_sales_order> TYPE zpos_order_line_sty.

*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_9901'.
*    <ls_sales_order>-quantity  = 2.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 31.
*    <ls_sales_order>-currency  = 'EUR'.
*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_9903'.
*    <ls_sales_order>-quantity  = 2.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 32.
*    <ls_sales_order>-currency  = 'EUR'.

*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_9904'.
*    <ls_sales_order>-quantity  = 2.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 50.
*    <ls_sales_order>-currency  = 'EUR'.
*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_9905'.
*    <ls_sales_order>-quantity  = 4.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 9.
*    <ls_sales_order>-currency  = 'EUR'.

*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_9906'.
*    <ls_sales_order>-quantity  = 4.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 13.
*    <ls_sales_order>-currency  = 'EUR'.
*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_9908'.
*    <ls_sales_order>-quantity  = 1.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 12.
*    <ls_sales_order>-currency  = 'EUR'.
*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_9909'.
*    <ls_sales_order>-quantity  = 2.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 10.
*    <ls_sales_order>-currency  = 'EUR'.
**
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_3009'.
*    <ls_sales_order>-quantity  = 2.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 39.
*    <ls_sales_order>-currency  = 'EUR'.
*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_3010'.
*    <ls_sales_order>-quantity  = 3.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 40.
*    <ls_sales_order>-currency  = 'EUR'.
*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_3011'.
*    <ls_sales_order>-quantity  = 1.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 41.
*    <ls_sales_order>-currency  = 'EUR'.

*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_3004'.
*    <ls_sales_order>-quantity  = 1.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 10.
*    <ls_sales_order>-currency  = 'EUR'.
*
*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_3006'.
*    <ls_sales_order>-quantity  = 1.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 12.
*    <ls_sales_order>-currency  = 'EUR'.
*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_3009'.
*    <ls_sales_order>-quantity  = 1.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 41.
*    <ls_sales_order>-currency  = 'EUR'.

*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_3010'.
*    <ls_sales_order>-quantity  = 2.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 41.
*    <ls_sales_order>-currency  = 'EUR'.

*
*
*    APPEND INITIAL LINE TO rt_sales_order ASSIGNING <ls_sales_order>.
*    <ls_sales_order>-matnr     = 'M_CR_3019'.
*    <ls_sales_order>-quantity  = 3.
*    <ls_sales_order>-suom      = 'ST'.
*    <ls_sales_order>-zwso_unit_price = 41.
*    <ls_sales_order>-currency  = 'EUR'.






ENDFUNCTION.

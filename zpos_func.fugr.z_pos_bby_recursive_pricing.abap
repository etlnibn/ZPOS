FUNCTION Z_POS_BBY_RECURSIVE_PRICING .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_OPEN_ORDER) TYPE  ZPOS_ORDER_LINE_TTY
*"     VALUE(IT_PRICED_ORDER) TYPE  ZPOS_BBY_TOTAL_TTY
*"     VALUE(IT_BONUS_BUY) TYPE  ZPOS_BONUS_BUY_ALL_TTY
*"     REFERENCE(IV_MAX_RECURSION) TYPE  I DEFAULT 10
*"  EXPORTING
*"     VALUE(ET_OPEN_ORDER) TYPE  ZPOS_ORDER_LINE_TTY
*"     REFERENCE(ET_PRICED_ORDER) TYPE  ZPOS_BBY_TOTAL_TTY
*"     VALUE(ET_BONUS_BUY) TYPE  ZPOS_BONUS_BUY_ALL_TTY
*"----------------------------------------------------------------------
* Pre-Processing Checks - as this is recursive we need to check each time
*
* If either of them is initial we have reached the bottom of the recursion tree
*"----------------------------------------------------------------------

  DATA: lv_prev_total TYPE kbetr,
        lv_new_total  TYPE kbetr.

  DATA(lt_new_open_order)   = it_open_order.
  DATA(lt_new_bonus_buy)    = it_bonus_buy.

  DATA: lt_new_priced_order  TYPE zpos_bby_total_tty.
  DATA: lt_prev_open_order   TYPE zpos_order_line_tty.
  DATA: lt_prev_priced_order TYPE zpos_bby_total_tty.

  CHECK  it_bonus_buy  IS NOT INITIAL.
  CHECK  it_open_order IS NOT INITIAL.

* Saftey Net
  DATA(lv_max_recursion) = iv_max_recursion - 1.
  CHECK lv_max_recursion GT 0.
*"----------------------------------------------------------------------


  LOOP AT lt_new_bonus_buy INTO DATA(ls_bonus_buy).

* This should price the first BBY - and Remove things from the Open_Order table and transfer them to the Priced_Order Table

    lt_prev_priced_order = lt_new_priced_order.
    lt_prev_open_order   = lt_new_open_order.

    CLEAR: lt_new_priced_order.
    lt_new_open_order = it_open_order.

    zcl_pos_bby_price=>calc_price_one_bby( EXPORTING is_bonus_buy = ls_bonus_buy CHANGING ct_open_order = lt_new_open_order ct_priced_order = lt_new_priced_order ).

* If we are not getting any more elements priced (added to lt_new_priced_order) then the process must have completed
    IF NOT lt_new_priced_order IS INITIAL.

      CALL FUNCTION 'Z_POS_BBY_RECURSIVE_PRICING'
        EXPORTING
          iv_max_recursion = lv_max_recursion
          it_open_order    = lt_new_open_order
          it_priced_order  = lt_new_priced_order
          it_bonus_buy     = lt_new_bonus_buy
        IMPORTING
          et_open_order    = lt_new_open_order
          et_priced_order  = lt_new_priced_order
*         et_bonus_buy     = lt_new_bonus_buy
        .

*"----------------------------------------------------------------------
*   When we get here we have reached the end of the recursion tree
*   and have bubbled back up to the top of the stack
*"----------------------------------------------------------------------


      IF lt_prev_priced_order IS INITIAL.
** This could miss the possibility of the standard prices being lower than the offer prices
        lt_prev_priced_order = lt_new_priced_order.
        lt_prev_open_order   = lt_new_open_order.

      ELSE.

*      CLEAR: lv_new_total, lv_prev_total.
*
* New Totals
        LOOP AT lt_new_priced_order INTO DATA(ls_priced_order).
          lv_new_total = lv_new_total + ls_priced_order-bby_total_amount.
        ENDLOOP.

        LOOP AT lt_new_open_order INTO DATA(ls_open_order).
          lv_new_total = lv_new_total + ls_open_order-zwso_total_price.
        ENDLOOP.

* Previous Total
        LOOP AT lt_prev_priced_order INTO ls_priced_order.
          lv_prev_total = lv_prev_total + ls_priced_order-bby_total_amount.
        ENDLOOP.

        LOOP AT lt_prev_open_order INTO ls_open_order.
          lv_prev_total = lv_prev_total + ls_open_order-zwso_total_price.
        ENDLOOP.

        IF lv_new_total LE lv_prev_total.

          lt_prev_priced_order = lt_new_priced_order.
          lt_prev_open_order   = lt_new_open_order.

        ENDIF.
      ENDIF.

    ENDIF.


    lv_max_recursion     = iv_max_recursion.

  ENDLOOP.

* Once all of the loops through the BBYs are complete
* we only want there to be one lowest-priced option which wins
  INSERT LINES OF lt_prev_priced_order INTO et_priced_order INDEX 1.
  INSERT LINES OF lt_prev_open_order INTO et_open_order INDEX 1.


ENDFUNCTION.

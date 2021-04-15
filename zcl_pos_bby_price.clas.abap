class ZCL_POS_BBY_PRICE definition
  public
  final
  create public .

public section.

  class-methods CALC_SALES_ORDER_PRICE
    importing
      !IV_VTWEG type VTWEG
      !IV_VKORG type VKORG
      !IV_WERKS type WERKS_D
      !IV_PRICE_DATE type DATS
      !IV_CURRENCY type WAERS
      !IT_SALES_ORDER type ZPOS_ORDER_LINE_TTY
      !IT_R_AKTNR type FIP_T_AKTRN_RANGE
    exporting
      value(ET_PRICED_ORDER) type ZPOS_BBY_TOTAL_TTY
      value(ET_OPEN_ORDER) type ZPOS_ORDER_LINE_TTY .
  class-methods CALC_PRICE_ONE_BBY
    importing
      !IS_BONUS_BUY type ZPOS_BONUS_BUY_DETAILS_STY
    changing
      !CT_OPEN_ORDER type ZPOS_ORDER_LINE_TTY
      value(CT_PRICED_ORDER) type ZPOS_BBY_TOTAL_TTY .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_s_matnr ,
      matnr TYPE matnr,
    END OF ty_s_matnr .
  types:
    ty_t_matnr TYPE STANDARD TABLE OF ty_s_matnr .
  types:
    BEGIN OF ty_s_reward_calc ,
      disctype        TYPE rdm_bby_disctype,
      btype           TYPE rdm_bby_disctype,
      zwso_unit_price TYPE kbetr,
      mquan           TYPE rdm_bby_buy_matquan,
      konwa           TYPE rdm_bby_konwa,
      kbetr           TYPE rdm_bby_kbetr,
      kpein           TYPE rdm_bby_kpein,
      kmein           TYPE rdm_bby_kmein,
      rewref          TYPE rdm_bby_rewref,
    END OF ty_s_reward_calc .

  class-methods CALC_PRICE_BUY_OR_GET
    importing
      !IT_REWARD type RDM_T_BBY_WA_REWARD
      !IT_MATGRP type RDM_T_BBY_WA_MATGRP
      !IT_BUY_GET type RDM_T_BBY_WA_BUY
    changing
      !CT_PRICED_ORDER_RULE type ZPOS_BBY_RULE_TTY
      !CT_OPEN_ORDER type ZPOS_ORDER_LINE_TTY .
  class-methods BBY_RULE_GROUP_FILL
    importing
      !IS_MAT_GRP_RULE type ZPOS_BBY_RULE_STY
    changing
      !CS_PRICED_ORDER_RULE type ZPOS_BBY_RULE_STY
      !CT_OPEN_ORDER type ZPOS_ORDER_LINE_TTY .
  class-methods BBY_PREREQUISITE_CHECK
    importing
      !IT_MATGRP type RDM_T_BBY_WA_MATGRP
      !IV_LINK_CONDITION type RDM_BBY_BUYCON
      !IT_BBY_BUY type RDM_T_BBY_WA_BUY
      !IT_OPEN_ORDER type ZPOS_ORDER_LINE_TTY
    returning
      value(RV_CHECKS_PASSED) type BOOLE_D .
  class-methods CALC_REWARD
    importing
      !IS_ARTGRP_MEMBER type ZPOS_BBY_ARTGRP_MEMBER_STY
    returning
      value(RV_AMOUNT) type KBETR .
ENDCLASS.



CLASS ZCL_POS_BBY_PRICE IMPLEMENTATION.


  METHOD bby_prerequisite_check.

    FIELD-SYMBOLS: <ls_order>      TYPE zpos_order_line_sty.

    IF it_bby_buy IS INITIAL.      " If there are no prerequsities then this must pass the prereq check
      rv_checks_passed = abap_true.
      RETURN.
    ENDIF.

    DATA(lt_open_order) = it_open_order.
    DATA(lt_bby_buy)    = it_bby_buy.


    LOOP AT lt_bby_buy ASSIGNING FIELD-SYMBOL(<ls_buy>).
      <ls_buy>-plnd_sales_qty = <ls_buy>-mquan.

      IF <ls_buy>-postype = 'MGP'.                                                   " Is it a Material Group

        LOOP AT it_matgrp INTO DATA(ls_matgrp) WHERE grpnr = <ls_buy>-grpnr.         " If so then loop on the articles in the group
          IF <ls_buy>-plnd_sales_qty EQ 0.                                                       " If we have already fulfilled the qty requirements then exit early
            EXIT.
          ENDIF.

          UNASSIGN <ls_order>.
*         Only using a Loop (instead of a Read) due to the need to have a where clause
          LOOP AT lt_open_order ASSIGNING <ls_order> WHERE matnr = ls_matgrp-matnr AND quantity GT 0.              " Now check the order for a matching article with an open qty
            IF <ls_buy>-plnd_sales_qty EQ 0.         " Inside the Inner Loop - If we have already fulfilled the qty requirements then exit early
              EXIT.
            ENDIF.

            IF <ls_order> IS ASSIGNED.
              IF <ls_order>-quantity GT <ls_buy>-plnd_sales_qty.                                  " If the basket qty can completely satisfy the required quantity then use it
                <ls_order>-quantity = <ls_order>-quantity - <ls_buy>-plnd_sales_qty.
                <ls_buy>-plnd_sales_qty = 0.
                EXIT.
              ELSE .                                                                          " If not, then use the quantity in the basket to reduce the required qty
                <ls_buy>-plnd_sales_qty = <ls_buy>-plnd_sales_qty - <ls_order>-quantity.
                <ls_order>-quantity = 0.

              ENDIF.
            ENDIF.

          ENDLOOP.
          DELETE lt_open_order WHERE quantity LE 0.

        ENDLOOP.

      ELSEIF <ls_buy>-postype = 'MAT'.

        IF <ls_buy>-plnd_sales_qty EQ 0.
          EXIT.
        ENDIF.

        UNASSIGN <ls_order>.
        LOOP AT lt_open_order ASSIGNING <ls_order> WHERE matnr = <ls_buy>-matnr AND quantity GT 0.
          IF <ls_buy>-plnd_sales_qty EQ 0.
            EXIT.
          ENDIF.

          IF <ls_order> IS ASSIGNED.
            IF <ls_order>-quantity GT <ls_buy>-plnd_sales_qty.
              <ls_order>-quantity = <ls_order>-quantity - <ls_buy>-plnd_sales_qty.
              <ls_buy>-plnd_sales_qty = 0.
              EXIT.
            ELSE .
              <ls_buy>-plnd_sales_qty = <ls_buy>-plnd_sales_qty - <ls_order>-quantity.
              <ls_order>-quantity = 0.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.


      IF iv_link_condition = 'A' .
        IF <ls_buy>-plnd_sales_qty GT 0.      " AND - Can do early exit - does this specifc line have a quantity which could not be fullfilled - for AND this would be a failure
          rv_checks_passed = abap_false.
          EXIT.
        ELSE.
*              buycon = AND  --> the quantity is equal to zero - can't make any decisions yet
        ENDIF.
      ELSEIF iv_link_condition = 'O'.
        IF <ls_buy>-plnd_sales_qty EQ 0.
          rv_checks_passed = abap_true.                                       " Already flagged as true - added again for readability
          EXIT.
        ELSE.
*              buycon = OR   --> the quantity is greater than zero - but other buy rows could change that - can't make any decisions yet
        ENDIF.
      ENDIF.

    ENDLOOP.

* After the loop we can check the overall picture

    IF iv_link_condition = 'A' .   " AND
      LOOP AT lt_bby_buy TRANSPORTING NO FIELDS WHERE plnd_sales_qty GT 0.
      ENDLOOP.
      IF sy-subrc = 0.
* In an AND condition no row should exist with a value greater than zero
        rv_checks_passed = abap_false.
      ELSE.
        rv_checks_passed = abap_true.
      ENDIF.

    ELSEIF iv_link_condition = 'O'.  " OR
      IF line_exists( lt_bby_buy[ plnd_sales_qty = 0 ] ).
* In an OR condition if one row exists with a value equal to zero then it is relevant
        rv_checks_passed = abap_true.
      ELSE.
        rv_checks_passed = abap_false.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD bby_rule_group_fill.

    FIELD-SYMBOLS: <ls_rule_artgrp_member> TYPE zpos_bby_artgrp_member_sty.

    DATA(lt_open_order) = ct_open_order.
    DATA(lv_quantity)   = is_mat_grp_rule-mquan.

    MOVE-CORRESPONDING is_mat_grp_rule TO cs_priced_order_rule.
    CLEAR cs_priced_order_rule-artgrp[].


    LOOP AT lt_open_order ASSIGNING FIELD-SYMBOL(<ls_open_order>) WHERE quantity GT 0.

      IF lv_quantity GT 0 .

        READ TABLE is_mat_grp_rule-artgrp INTO DATA(ls_artgrp_member) WITH KEY matnr = <ls_open_order>-matnr .

* The order article was found in the article group for the BBY rule
        IF sy-subrc = 0.


* Can one single order line fulfill the whole BBY rule quantity or will it need to be made up from several order lines?
          IF <ls_open_order>-quantity GE lv_quantity.


* In this case one order line satisfied the remaining open qty (it could have previously be reduced by another order line)

            APPEND INITIAL LINE TO cs_priced_order_rule-artgrp ASSIGNING <ls_rule_artgrp_member>.
*            Mainly used to fill the matnr number and reward details
            MOVE-CORRESPONDING ls_artgrp_member TO <ls_rule_artgrp_member>.

            <ls_rule_artgrp_member>-vrkme             = <ls_open_order>-suom.
            <ls_rule_artgrp_member>-mquan             = lv_quantity.

            <ls_rule_artgrp_member>-zwso_unit_price   = <ls_open_order>-zwso_unit_price.
            <ls_rule_artgrp_member>-zwso_total_amount = <ls_rule_artgrp_member>-zwso_unit_price * <ls_rule_artgrp_member>-mquan.
            <ls_rule_artgrp_member>-currency          = <ls_open_order>-currency.

*           Reduce the order Qty by the amount used to fullfil the BBY rule
            <ls_open_order>-quantity = <ls_open_order>-quantity - lv_quantity.
            lv_quantity = 0.      " Full Qty is fullfilled by this article

            IF NOT <ls_rule_artgrp_member>-reward IS INITIAL.
*           Does the individual Article Group Item have a specific reward rule which can be applied to calculate the price?
              <ls_rule_artgrp_member>-reward_unit_price  = calc_reward( EXPORTING is_artgrp_member  = <ls_rule_artgrp_member> ).
              <ls_rule_artgrp_member>-reward_total_amount = <ls_rule_artgrp_member>-reward_unit_price * <ls_rule_artgrp_member>-mquan.
              cs_priced_order_rule-rule_amount += <ls_rule_artgrp_member>-reward_total_amount.
            ENDIF.
*           The full rule quantity has been fulfilled
            EXIT.

          ELSE.

*           In this case one order line is not able to fulfill the necessary quantity
            APPEND INITIAL LINE TO cs_priced_order_rule-artgrp ASSIGNING <ls_rule_artgrp_member>.
*            Mainly used to fill the matnr number and reward details
            MOVE-CORRESPONDING ls_artgrp_member TO <ls_rule_artgrp_member>.

            <ls_rule_artgrp_member>-vrkme             = <ls_open_order>-suom.
            <ls_rule_artgrp_member>-mquan             = <ls_open_order>-quantity.

            <ls_rule_artgrp_member>-zwso_unit_price   = <ls_open_order>-zwso_unit_price.
            <ls_rule_artgrp_member>-zwso_total_amount = <ls_open_order>-zwso_unit_price * <ls_open_order>-quantity.
            <ls_rule_artgrp_member>-currency          = <ls_open_order>-currency.

            lv_quantity = lv_quantity - <ls_open_order>-quantity.
            <ls_open_order>-quantity = 0.     " Order Qty not enough to fullfill the target quantity

          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

    IF cs_priced_order_rule-disctype = 'S'.       " Standard Pricing

      LOOP AT cs_priced_order_rule-artgrp ASSIGNING <ls_rule_artgrp_member>.
        <ls_rule_artgrp_member>-reward_unit_price  = <ls_rule_artgrp_member>-zwso_unit_price.
        <ls_rule_artgrp_member>-reward_total_amount = <ls_rule_artgrp_member>-zwso_unit_price * <ls_rule_artgrp_member>-mquan.
        cs_priced_order_rule-rule_amount += <ls_rule_artgrp_member>-reward_total_amount .
      ENDLOOP.

* This is a reward type of Discount Price - For example Buy 3 items from a group for 13.49
    ELSEIF cs_priced_order_rule-disctype NE 'S' AND cs_priced_order_rule-reward-btype = 'P'.
      cs_priced_order_rule-rule_amount = cs_priced_order_rule-reward-kbetr * ( cs_priced_order_rule-mquan / cs_priced_order_rule-reward-kpein ).

    ENDIF.

    DELETE lt_open_order WHERE quantity LE 0.

    IF lv_quantity NE 0.
      CLEAR cs_priced_order_rule.        " If we don't fulfill the requirements leave this empty!
    ELSE.
* Only want to send changes back for open_order if we have been successful at filling the BBY
      ct_open_order = lt_open_order.
    ENDIF.


  ENDMETHOD.


  METHOD calc_price_buy_or_get.

    DATA:
      ls_buy_get           TYPE rdm_s_bby_wa_buy,
      ls_priced_order_rule TYPE zpos_bby_rule_sty,
      lt_mat_grp_rule      TYPE zpos_bby_rule_tty.

    FIELD-SYMBOLS: <ls_order_line>   TYPE zpos_order_line_sty,
                   <ls_mat_grp>      TYPE zpos_bby_total_sty,
                   <ls_mat_grp_rule> TYPE zpos_bby_rule_sty,
                   <ls_member>       TYPE zpos_bby_artgrp_member_sty.


*"----------------------------------------------------------------------
*  Loop through all of the buys / gets --> Preparing the data for processing
*"----------------------------------------------------------------------
    LOOP AT it_buy_get INTO ls_buy_get.

      CLEAR ls_priced_order_rule.
      DATA(lv_quantity) =  ls_buy_get-mquan.

      APPEND INITIAL LINE TO lt_mat_grp_rule ASSIGNING <ls_mat_grp_rule>.
      MOVE-CORRESPONDING ls_buy_get TO <ls_mat_grp_rule>.

      TRY.
          <ls_mat_grp_rule>-reward = it_reward[ rewnr = ls_buy_get-rewnr ].
        CATCH cx_sy_itab_line_not_found.
*        Do nothing
      ENDTRY.


      UNASSIGN <ls_order_line>.
      IF ls_buy_get-postype = 'MAT' AND ct_open_order[] IS NOT INITIAL.

        DELETE ct_open_order WHERE quantity LE 0.
*         Because of the sorting on price this should ensure that we are consuming the most expensive item first which should lead to the lowest price for the customer (I really hope so!)

        APPEND INITIAL LINE TO <ls_mat_grp_rule>-artgrp ASSIGNING <ls_member>.
        <ls_member>-matnr     = ls_buy_get-matnr.


      ELSEIF ls_buy_get-postype = 'MGP' AND ct_open_order[] IS NOT INITIAL.

        LOOP AT it_matgrp INTO DATA(ls_matgrp) WHERE grpnr = ls_buy_get-grpnr.                " If so then loop on the articles in the group
          APPEND INITIAL LINE TO <ls_mat_grp_rule>-artgrp ASSIGNING <ls_member>.
          <ls_member>-matnr = ls_matgrp-matnr.


*              1  Get Total
*              2  Get Line Item
*              3  Get Override
*              5  Buy Line Item

          IF NOT ls_matgrp-get_rewnr IS INITIAL.           " Use the default Header Value
            TRY.
                <ls_member>-reward = it_reward[ rewnr = ls_matgrp-get_rewnr ].       " We only need the reward at the member level if it is of type "Override"
              CATCH cx_sy_itab_line_not_found.
*                     Do nothing
            ENDTRY.
          ELSEIF <ls_mat_grp_rule>-reward-rewref = 2.
* Use Parent Level reward
            <ls_member>-reward = <ls_mat_grp_rule>-reward.
          ENDIF.

        ENDLOOP.

      ENDIF.

      CALL METHOD bby_rule_group_fill( EXPORTING is_mat_grp_rule = <ls_mat_grp_rule> CHANGING ct_open_order = ct_open_order cs_priced_order_rule = ls_priced_order_rule ).
      IF NOT ls_priced_order_rule IS INITIAL.
        APPEND ls_priced_order_rule TO ct_priced_order_rule.
      ENDIF.
      CLEAR ls_priced_order_rule.

    ENDLOOP.

  ENDMETHOD.


  METHOD calc_price_one_bby.

    DATA: lt_priced_order_rule TYPE zpos_bby_rule_tty.

    FIELD-SYMBOLS: <ls_bby_rule> TYPE zpos_bby_rule_sty.

* Check that this is being called from the recursive function
*      CALL FUNCTION 'SYSTEM_CALLSTACK'
*       EXPORTING
*         MAX_LEVEL          = 1
*       IMPORTING
*         CALLSTACK          =
*         ET_CALLSTACK       =
*                .
*




*"---------------------------------------------------------------------- START OF THE BUY SIDE

*"----------------------------------------------------------------------
* Check if the Buy Prerequisites can be fulfilled
*"----------------------------------------------------------------------
    DATA(lv_relevant_buy) = bby_prerequisite_check( EXPORTING it_bby_buy = is_bonus_buy-buy
                                                          it_matgrp = is_bonus_buy-matgrp
                                                          it_open_order = ct_open_order
                                                          iv_link_condition = is_bonus_buy-buycon ).
*"----------------------------------------------------------------------



    IF lv_relevant_buy = abap_true.

*"----------------------------------------------------------------------
* This append assumes at this stage that we are going to be able to move something over to the priced_order structure
* If it is not possible the entry is deleted later
*"----------------------------------------------------------------------
      APPEND INITIAL LINE TO ct_priced_order ASSIGNING FIELD-SYMBOL(<ls_priced_order>).
      <ls_priced_order>-bbynr    = is_bonus_buy-bbynr.
      <ls_priced_order>-offer_id = is_bonus_buy-offer_id.
      <ls_priced_order>-currency = is_bonus_buy-bbycurh.

      CLEAR lt_priced_order_rule.

*"----------------------------------------------------------------------
* Here we pass all of the buy rules - and determine the best way to fulfill
* the conditions / rules specified on the buy side
*
* Sales order lines are moved to the priced_order structure
*"----------------------------------------------------------------------
      calc_price_buy_or_get(
        EXPORTING
          it_reward       = is_bonus_buy-reward
          it_matgrp       = is_bonus_buy-matgrp
          it_buy_get      = is_bonus_buy-buy
        CHANGING
          ct_priced_order_rule = lt_priced_order_rule
          ct_open_order        = ct_open_order
          ).


      IF lt_priced_order_rule IS NOT INITIAL.
        APPEND LINES OF lt_priced_order_rule TO <ls_priced_order>-rule.
        CLEAR lt_priced_order_rule.
      ENDIF.

*"---------------------------------------------------------------------- END OF THE BUY SIDE




*"---------------------------------------------------------------------- START OF THE GET SIDE


* Check for Total BBY Reward
      TRY.
          DATA(ls_total_reward) = is_bonus_buy-reward[ rewref = 1 ].      " Total Level Reward

        CATCH cx_sy_itab_line_not_found.
          CLEAR ls_total_reward.
      ENDTRY.



*"----------------------------------------------------------------------
* Check if the Get Rules can be fulfilled (mainly applies to the use of the AND condition
*"----------------------------------------------------------------------
      DATA(lv_relevant_get) = bby_prerequisite_check( EXPORTING it_bby_buy = is_bonus_buy-get
                                                            it_matgrp = is_bonus_buy-matgrp
                                                            it_open_order = ct_open_order
                                                            iv_link_condition = is_bonus_buy-getcon ).


      IF lv_relevant_buy = abap_true AND lv_relevant_get = abap_true.

        CLEAR lt_priced_order_rule.

*"----------------------------------------------------------------------
* Here we pass all of the GET rules/conditions - and determine the best way to fulfill
* the get side to achieve the lowest price for the customer
*
* Sales order lines are moved to the priced_order structure
*"----------------------------------------------------------------------
        calc_price_buy_or_get(
          EXPORTING
            it_reward       = is_bonus_buy-reward
            it_matgrp       = is_bonus_buy-matgrp
            it_buy_get      = is_bonus_buy-get
          CHANGING
          ct_priced_order_rule = lt_priced_order_rule
          ct_open_order        = ct_open_order
          ).


        IF lt_priced_order_rule IS NOT INITIAL.
          IF ls_total_reward IS NOT INITIAL.
            <ls_priced_order>-bby_total_amount = ls_total_reward-kbetr.
          ENDIF.
          APPEND LINES OF lt_priced_order_rule TO <ls_priced_order>-rule.
          CLEAR lt_priced_order_rule.
        ENDIF.

      ENDIF.

    ENDIF.

    IF <ls_priced_order> IS ASSIGNED.
      LOOP AT <ls_priced_order>-rule INTO DATA(ls_bby_rule).
        <ls_priced_order>-bby_total_amount += ls_bby_rule-rule_amount.
      ENDLOOP.
    ENDIF.

*"---------------------------------------------------------------------- END OF THE GET SIDE

    DELETE ct_priced_order WHERE rule IS INITIAL.


  ENDMETHOD.


  METHOD calc_reward.

    CLEAR rv_amount.

*   P	Discount Price
*   R	Discount Amount
*   %	Discount Percent
*   S	No Discount

    IF is_artgrp_member-reward-btype = '%'.
        rv_amount     = ( is_artgrp_member-zwso_unit_price * ( 1 - is_artgrp_member-reward-kbetr / 100 ) ) * is_artgrp_member-mquan .
    ELSEIF is_artgrp_member-reward-btype = 'R'.
        rv_amount  = ( ( is_artgrp_member-zwso_unit_price - ( is_artgrp_member-reward-kbetr / is_artgrp_member-reward-kpein ) ) * is_artgrp_member-mquan ) .
    ENDIF.

  ENDMETHOD.


  METHOD calc_sales_order_price.

    DATA ls_bby_wa_header    TYPE  rdm_s_bby_wa_header.
    DATA lt_bby_wa_buy       TYPE  rdm_t_bby_wa_buy.
    DATA lt_bby_wa_get       TYPE  rdm_t_bby_wa_get.
    DATA lt_bby_wa_matgrp    TYPE  rdm_t_bby_wa_matgrp.
    DATA lt_bby_wa_reward    TYPE  rdm_t_bby_wa_reward.
    DATA ls_reward           TYPE  rdm_s_bby_wa_reward.
    DATA lt_bby_wa_scales    TYPE  rdm_t_bby_wa_scales.
    DATA ls_matgrp           TYPE  rdm_s_bby_wa_matgrp.
    DATA lt_bby_wa_orgitems  TYPE  rdm_t_bby_wa_orgitems.
    DATA lt_bby_wa_orgsites  TYPE  rdm_t_bby_wa_orgsites.
    DATA lt_bby_wa_texts     TYPE  rdm_t_bby_wa_texts.
    DATA lt_bby_wa_mqty_dis  TYPE  rdm_t_bby_wa_mqty_dis.
    DATA lt_conditions       TYPE  rdm_t_bby_conditions.
    DATA lv_quantity         TYPE rdm_bby_buy_matquan.
    DATA: lv_original_amount TYPE kbetr,
          lv_priced_amount   TYPE kbetr.


    DATA lt_sales_order      TYPE zpos_order_line_tty.

    DATA lt_bonus_buy        TYPE zpos_bonus_buy_all_tty.

    lt_sales_order[] = it_sales_order[].

* This is important later as we need to consume the most expensive items first
    SORT lt_sales_order BY zwso_unit_price DESCENDING.

* Initialize export parameters
    CLEAR: et_priced_order, et_open_order.

    CHECK iv_price_date IS NOT INITIAL.


* Very Basic Selection Criteria
    SELECT bbynr
       FROM konbbyh
       INTO TABLE @DATA(lt_bbynr)
       WHERE aktnr IN @it_r_aktnr
       AND status = @abap_false
       AND bbycurh = @iv_currency
       AND datab LE @iv_price_date
       AND datbi GE @iv_price_date.

    SORT lt_bbynr BY bbynr. DELETE ADJACENT DUPLICATES FROM lt_bbynr COMPARING ALL FIELDS.


    LOOP AT lt_bbynr INTO DATA(ls_bbynr).

      CALL FUNCTION 'RDM_BBY_DB_LOAD'
        EXPORTING
          i_bbynr             = ls_bbynr-bbynr
          i_buffer_refresh    = abap_false  "THV: Do not refresh the buffer (maybe already filled)
          i_buffer_read       = abap_true   "THV: Try to read data from buffer
          i_buffer_store      = abap_true   "THV: Store data read in buffer
        IMPORTING
          es_wa_header        = ls_bby_wa_header
          et_wa_buy           = lt_bby_wa_buy
          et_wa_get           = lt_bby_wa_get
          et_wa_reward        = lt_bby_wa_reward
          et_wa_scales        = lt_bby_wa_scales
          et_wa_matgrp        = lt_bby_wa_matgrp
          et_wa_orgitems      = lt_bby_wa_orgitems
          et_wa_orgsites      = lt_bby_wa_orgsites
          et_wa_texts         = lt_bby_wa_texts
          et_wa_mqty_dis      = lt_bby_wa_mqty_dis
        EXCEPTIONS
          db_header_not_found = 1
          other_error         = 2
          OTHERS              = 3.

**     read corresponding conditions
*      CALL FUNCTION 'RDM_BBY_CONDITIONS_GET'
*        EXPORTING
*          i_bbynr             = ls_bbynr-bbynr
*          i_buffer_read       = abap_true      "THV: Try to read data from buffer
*        IMPORTING
*          et_conditions       = lt_conditions
*        EXCEPTIONS
*          db_header_not_found = 1
*          other_error         = 2
*          OTHERS              = 3.


      TRY .
          IF line_exists( lt_bby_wa_orgsites[ vkorg = iv_vkorg vtweg = iv_vtweg werks = iv_werks vwaer = iv_currency ] ) OR
             line_exists( lt_bby_wa_orgitems[ vkorg = iv_vkorg vtweg = iv_vtweg ] ).


*--------------------------------------------------------------------*
* Now we have all of the matnrs for the BBY - we can check for overlap
*--------------------------------------------------------------------*
            LOOP AT lt_sales_order INTO DATA(ls_sales_order).
              IF line_exists( lt_bby_wa_mqty_dis[ matnr = ls_sales_order-matnr ] ).
*--------------------------------------------------------------------*
* As soon as at least one article in the sales order is referenced in the BBY
* we will flag it as potentially relevant
* Pricing will determine whether the conditions are really fulfilled later on.
*--------------------------------------------------------------------*
                DATA(lv_relevant) = abap_true.
                EXIT.
              ENDIF.
            ENDLOOP.

            IF lv_relevant = abap_true.

              APPEND INITIAL LINE TO lt_bonus_buy ASSIGNING FIELD-SYMBOL(<ls_bonus_buy>).
              MOVE-CORRESPONDING ls_bby_wa_header TO <ls_bonus_buy>.
              <ls_bonus_buy>-buy          = lt_bby_wa_buy.
              <ls_bonus_buy>-get          = CORRESPONDING #( lt_bby_wa_get ).
              <ls_bonus_buy>-reward       = lt_bby_wa_reward.
              <ls_bonus_buy>-scales       = lt_bby_wa_scales.
              <ls_bonus_buy>-matgrp       = lt_bby_wa_matgrp.
              <ls_bonus_buy>-orgitems     = lt_bby_wa_orgitems.
              <ls_bonus_buy>-orgsites     = lt_bby_wa_orgsites.
              <ls_bonus_buy>-texts        = lt_bby_wa_texts.
              <ls_bonus_buy>-conditions   = lt_conditions.
              <ls_bonus_buy>-relevant     = lv_relevant.

            ENDIF.

          ENDIF.
        CATCH cx_sy_itab_line_not_found.
*  Do Nothing - Data is not relevant
      ENDTRY.

      CLEAR ls_bby_wa_header.
      CLEAR lt_bby_wa_buy.
      CLEAR lt_bby_wa_get.
      CLEAR lt_bby_wa_reward.
      CLEAR lt_bby_wa_scales.
      CLEAR lt_bby_wa_matgrp.
      CLEAR lt_bby_wa_orgitems.
      CLEAR lt_bby_wa_orgsites.
      CLEAR lt_bby_wa_texts.
      CLEAR lt_conditions.

    ENDLOOP.

    CALL FUNCTION 'Z_POS_BBY_RECURSIVE_PRICING'
      EXPORTING
        it_open_order   = lt_sales_order
        it_priced_order = et_priced_order
        it_bonus_buy    = lt_bonus_buy
      IMPORTING
        et_open_order   = et_open_order
        et_priced_order = et_priced_order
        et_bonus_buy    = lt_bonus_buy.


* Check the Basket Pricing is not more expensive that the original sales order (rare Edge case)

    CLEAR: lv_original_amount, lv_priced_amount.
*
* New Totals
    LOOP AT it_sales_order INTO ls_sales_order.
      lv_original_amount += ls_sales_order-zwso_unit_price * ls_sales_order-quantity.
    ENDLOOP.

* Calculated
    LOOP AT et_open_order INTO DATA(ls_open_order).
      lv_priced_amount += ls_open_order-zwso_unit_price * ls_open_order-quantity.
    ENDLOOP.

    LOOP AT et_priced_order INTO DATA(ls_priced_order).
      lv_priced_amount += ls_priced_order-bby_total_amount.
    ENDLOOP.


    IF lv_original_amount LT lv_priced_amount.
* Then we should just return the original sales order
      CLEAR et_priced_order.
      et_open_order   = it_sales_order.

    ELSE.
* Distribution the prices to the line items based on monetary share.
      LOOP AT et_priced_order ASSIGNING FIELD-SYMBOL(<ls_priced_order>).

        LOOP AT <ls_priced_order>-rule ASSIGNING FIELD-SYMBOL(<ls_rule>).

          LOOP AT <ls_rule>-artgrp ASSIGNING FIELD-SYMBOL(<ls_member>).
* The reward amount of each member should be based on the percentage contribution to the <ls_rule>-rule_amount.

            IF NOT <ls_rule>-rule_amount IS INITIAL.

              <ls_member>-reward_total_amount = <ls_member>-zwso_total_amount / <ls_rule>-rule_amount.
              <ls_member>-reward_unit_price   = <ls_member>-reward_total_amount / <ls_member>-mquan.

            ENDIF.

          ENDLOOP.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

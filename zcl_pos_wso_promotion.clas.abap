class ZCL_POS_WSO_PROMOTION definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF tp_s_method_value,
        method TYPE zpos_vk_discount_method,
        value  TYPE kbetr,
      END OF  tp_s_method_value .
  types:
    tp_t_itemlist_id TYPE STANDARD TABLE OF zpos_vk_list .

  methods APPEND_BBY_DATA
    importing
      !IS_HEADER type RDM_S_BBY_WA_HEADER
      !IT_BUY type RDM_T_BBY_WA_BUY
      !IT_GET type RDM_T_BBY_WA_GET
      !IT_MATGRP type RDM_T_BBY_WA_MATGRP
      !IT_ORGITEMS type RDM_T_BBY_WA_ORGITEMS
      !IT_ORGSITES type RDM_T_BBY_WA_ORGSITES
      !IT_REWARD type RDM_T_BBY_WA_REWARD
      !IT_TEXTS type RDM_T_BBY_WA_TEXTS
      !IS_SENT_EXT type WES_BBY_SENT_EXT optional
      !IT_CONDITIONS type RDM_T_BBY_CONDITIONS
      !IT_PROMO_SENT_EXT type WES_TO_PROMO_SENT_EXT
      !IT_BBY_SENT type WES_T_BBY_SENT
    raising
      CX_WES_PROCESS_MESSAGES .
  methods CONSTRUCTOR .
  methods CREATE_RETAIL_EVENTS
    raising
      ZCX_POS_EXCEPTION .
  PROTECTED SECTION.
private section.

  constants CO_BOOL_01_TRUE type BOOLEAN_01 value 1 ##NO_TEXT.
  constants CO_BOOL_01_FALSE type BOOLEAN_01 value 0 ##NO_TEXT.
  constants CO_ENGLISH_LANGUAGE type SPRAS value 'E' ##NO_TEXT.
  class-data MS_MESSAGE type SCX_T100KEY .
  constants CO_ZPOS_MSGID type SYMSGID value ZCL_POS_UTIL=>CO_POS_MSGID ##NO_TEXT.
  class-data MT_LIST_ID type TP_T_ITEMLIST_ID .
  class-data MO_CONFIG_MAP type ref to ZCL_POS_CONFIG_TAB_HANLDER .
  data MT_HEADER type RDM_T_BBY_WA_HEADER .
  data MT_BUY type RDM_T_BBY_WA_BUY .
  data MT_GET type RDM_T_BBY_WA_GET .
  data MT_REWARD type RDM_T_BBY_WA_REWARD .
  data MT_MATGRP type RDM_T_BBY_WA_MATGRP .
  data MT_ORGITEMS type RDM_T_BBY_WA_ORGITEMS .
  data MT_ORGSITES type RDM_T_BBY_WA_ORGSITES .
  data MT_TEXTS type RDM_T_BBY_WA_TEXTS .
  data MT_CONDITIONS type RDM_T_BBY_CONDITIONS .
  data MT_BBY_SENT type WES_T_BBY_SENT .
  data MT_SENT_EXT type WES_T_BBY_SENT_EXT .
  data MT_PROMO_SENT_EXT type WES_TO_PROMO_SENT_EXT .
  data MV_IS_BASKET type BOOLE_D .
  data MX_EXCEPTION type ref to ZCX_POS_EXCEPTION .
  data MS_TEXTID type SCX_T100KEY .

  methods BUY_GET_HANDLER
    importing
      !IT_BUY type RDM_T_BBY_WA_BUY optional
      !IT_GET type RDM_T_BBY_WA_GET optional
      !IT_MATGRP type RDM_T_BBY_WA_MATGRP optional
    changing
      !CS_OFFER type ZPOS_OFFER_HEADER_STY
    raising
      ZCX_POS_EXCEPTION .
  methods CREATE_RETAIL_EVENT_IDOC
    importing
      !IT_HEADER type RDM_T_BBY_WA_HEADER
      !IT_BUY type RDM_T_BBY_WA_BUY
      !IT_GET type RDM_T_BBY_WA_GET
      !IT_REWARD type RDM_T_BBY_WA_REWARD
      !IT_MATGRP type RDM_T_BBY_WA_MATGRP
      !IT_ORGITEMS type RDM_T_BBY_WA_ORGITEMS
      !IT_ORGSITES type RDM_T_BBY_WA_ORGSITES
      !IT_TEXTS type RDM_T_BBY_WA_TEXTS
      !IT_PROMO_SENT_EXT type WES_TO_PROMO_SENT_EXT
      !IV_COUNTRY type LAND1
      !IV_VKORG type VKORG
    raising
      ZCX_POS_EXCEPTION .
  methods DISCGRP_STEP_HANDLER
    importing
      !IT_GET type RDM_T_BBY_WA_GET optional
      !IT_REWARD type RDM_T_BBY_WA_REWARD
    changing
      !CS_OFFER type ZPOS_OFFER_HEADER_STY
    raising
      ZCX_POS_EXCEPTION .
  methods ELEMENT_SORT
    changing
      !CS_OFFER type ZPOS_OFFER_HEADER_STY .
  methods GET_ARTICLELIST_ID
    importing
      !IV_MATGRP_NR type GRPGNR
      !IS_OFFER type ZPOS_OFFER_HEADER_STY
    returning
      value(RV_ARTICLELIST_ID) type ZPOS_VK_OFFER_ID
    raising
      ZCX_POS_EXCEPTION .
  methods GET_BASKET_INDICATOR
    importing
      !IT_BUY type RDM_T_BBY_WA_BUY optional
      !IT_GET type RDM_T_BBY_WA_GET optional
      !IS_HEADER type RDM_S_BBY_WA_HEADER optional
    returning
      value(RV_IS_BASKET) type BOOLE_D
    raising
      ZCX_POS_EXCEPTION .
  methods GET_BBY_ID
    importing
      !IS_OFFER type ZPOS_OFFER_HEADER_STY
    returning
      value(RV_OFFER_ID) type ZPOS_VK_OFFER_ID
    raising
      ZCX_POS_EXCEPTION .
  methods GET_DISCOUNT_METHOD_VALUE
    importing
      !IT_BUY type RDM_T_BBY_WA_BUY optional
      !IT_GET type RDM_T_BBY_WA_GET optional
      !IT_REWARD type RDM_T_BBY_WA_REWARD optional
      !IS_HEADER type RDM_S_BBY_WA_HEADER optional
    returning
      value(RS_METHOD_VALUE) type TP_S_METHOD_VALUE
    raising
      ZCX_POS_EXCEPTION .
  methods GET_MARM_EAN
    importing
      !IV_MATNR type MATNR
      !IV_MEINH type MEINH
    returning
      value(RV_EAN11) type EAN11
    raising
      ZCX_POS_EXCEPTION .
  methods GET_SALESORG_LANGAUGE
    importing
      !IV_VKORG type VKORG
    returning
      value(RV_LANGUAGE) type SPRAS
    raising
      ZCX_POS_EXCEPTION .
  methods ITEMLIST_HANDLER
    importing
      !IT_MATGRP type RDM_T_BBY_WA_MATGRP optional
    changing
      !CS_OFFER type ZPOS_OFFER_HEADER_STY
    raising
      ZCX_POS_EXCEPTION .
  methods MAP_STRUCT_TO_IDOC
    importing
      !IV_STORE type WERKS_D
    changing
      !CS_OFFER type ZPOS_OFFER_HEADER_STY
    returning
      value(RT_EDIDD) type EDIDD_TT .
  methods OFFER_HEADER_HANDLER
    importing
      !IV_CHGID type WES_CHGID
      !IS_HEADER type RDM_S_BBY_WA_HEADER
      !IV_VKORG type VKORG
      !IV_WERKS type WERKS_D
      !IT_TEXTS type RDM_T_BBY_WA_TEXTS
    changing
      value(CS_OFFER) type ZPOS_OFFER_HEADER_STY
    raising
      ZCX_POS_EXCEPTION .
  methods STEP_PRICING_HANDLER
    importing
      !IT_STEPS type ZPOS_OFFER_STEP_TTY
    returning
      value(RT_STEPS) type ZPOS_OFFER_STEP_TTY .
ENDCLASS.



CLASS ZCL_POS_WSO_PROMOTION IMPLEMENTATION.


  METHOD APPEND_BBY_DATA.

    APPEND is_header TO mt_header.
    APPEND LINES OF it_buy TO mt_buy.
    APPEND LINES OF it_get TO mt_get.
    APPEND LINES OF it_matgrp TO mt_matgrp.
    mt_orgitems = it_orgitems.                      "Same Data for all BBYs in a Promo
    mt_orgsites = it_orgsites.                      "Same Data for all BBYs in a Promo
    mt_bby_sent = it_bby_sent.                      "Same Data for all BBYs in a Promo
    APPEND LINES OF it_reward TO mt_reward.
    APPEND LINES OF it_texts TO mt_texts.
    APPEND is_sent_ext TO mt_sent_ext.
    APPEND LINES OF it_conditions TO mt_conditions.
    mt_promo_sent_ext = it_promo_sent_ext.            "Same Data for all BBYs in a Promo

  ENDMETHOD.


  METHOD BUY_GET_HANDLER.


*    DATA: ls_idoc_data  TYPE edidd,
*          ls_edidc      TYPE edidc,
*          <ls_item> TYPE zpos_offer_item_01.

    DATA: ls_buy_get TYPE rdm_s_bby_wa_buy.

*------------------------------- ZPOS_OFFER_ITEM_01 -----------------------------------

    LOOP AT it_buy INTO DATA(ls_buy).

*------------------------------- Single Article -----------------------------------
      IF ls_buy-postype = 'MAT'.

        APPEND INITIAL LINE TO cs_offer-items ASSIGNING FIELD-SYMBOL(<ls_item>).
        <ls_item>-itemtype  = '0'.
        <ls_item>-itemid    = get_marm_ean( EXPORTING iv_matnr = ls_buy-matnr iv_meinh = ls_buy-vrkme ).
        <ls_item>-articleid = ls_buy-matnr.

        IF mv_is_basket = abap_true.        " Not relevant for Discount Groups
          <ls_item>-quantity  = ls_buy-mquan.
          <ls_item>-discount_distribution = co_bool_01_true.
          <ls_item>-discount_calculation = co_bool_01_false.
        ENDIF.

      ELSE.
*------------------------------- Article Groups -----------------------------------
        DATA(lt_matgrp) = FILTER #( it_matgrp WHERE bbynr = cs_offer-bbynr AND grpnr EQ ls_buy-grpnr )  .

        IF NOT lt_matgrp IS INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_buy-grpnr
            IMPORTING
              output = ls_buy-grpnr.

          APPEND INITIAL LINE TO cs_offer-items ASSIGNING <ls_item>.
          <ls_item>-itemid =  get_articlelist_id( EXPORTING iv_matgrp_nr = ls_buy-grpnr is_offer = cs_offer  ).

          <ls_item>-itemtype = '5'.

          IF mv_is_basket = abap_true.        " Not relevant for Discount Groups
            <ls_item>-quantity = ls_buy-mquan.
            <ls_item>-discount_distribution = co_bool_01_true.
            <ls_item>-discount_calculation  = co_bool_01_false.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.



*------------------------------- ZPOS_OFFER_ITEM_01 -----------------------------------

    LOOP AT it_get INTO DATA(ls_get).

*------------------------------- Single Article -----------------------------------
      IF ls_get-postype = 'MAT'.

        APPEND INITIAL LINE TO cs_offer-items ASSIGNING <ls_item>.
        <ls_item>-itemtype  = '0'.
        <ls_item>-itemid    = get_marm_ean( EXPORTING iv_matnr = ls_get-matnr iv_meinh = ls_get-vrkme ).
        <ls_item>-articleid = ls_get-matnr.

        IF mv_is_basket = abap_true.        " Not relevant for Discount Groups
          <ls_item>-quantity  = ls_get-mquan.

          IF it_buy IS INITIAL.
            <ls_item>-discount_distribution = co_bool_01_true.
          ELSE.
            <ls_item>-discount_distribution = co_bool_01_false.
          ENDIF.

          <ls_item>-discount_calculation = co_bool_01_true.
        ENDIF.

      ELSE.
*------------------------------- Article Groups -----------------------------------
        lt_matgrp = FILTER #( it_matgrp WHERE bbynr = cs_offer-bbynr AND grpnr EQ ls_get-grpnr )  .

        IF NOT lt_matgrp IS INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_get-grpnr
            IMPORTING
              output = ls_get-grpnr.

          APPEND INITIAL LINE TO cs_offer-items ASSIGNING <ls_item>.
          <ls_item>-itemid =  get_articlelist_id( EXPORTING iv_matgrp_nr = ls_get-grpnr is_offer = cs_offer  ).

          <ls_item>-itemtype = '5'.

          IF mv_is_basket = abap_true.        " Not relevant for Discount Groups
            <ls_item>-quantity = ls_get-mquan.
            IF it_buy IS INITIAL.
              <ls_item>-discount_distribution = co_bool_01_true.
            ELSE.
              <ls_item>-discount_distribution = co_bool_01_false.
            ENDIF.

            <ls_item>-discount_calculation = co_bool_01_true.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD CONSTRUCTOR.
  ENDMETHOD.


  METHOD create_retail_events.

* A Promo can contain multiple BBYs
* When this happens it is because of scales (or this is the only scenario we are covering)
* This means that the BBYs are identical except for a couple of values on the GET side
* The values which will be different are the quantity and price
* So most of the ViKING offer data will be taken from the first BBY
* And any additional BBYs will be used to populate the Steps in the Discount Group
* It can also be the case that Sweden migth want these as separate baskets

    DATA:lv_vkorg TYPE vkorg.

    CHECK mt_promo_sent_ext IS NOT INITIAL.

* Should only be one Promo at a time, but can be for multiple stores.
    READ TABLE mt_promo_sent_ext INDEX 1 INTO DATA(ls_promo).

    READ TABLE mt_header INDEX 1 INTO DATA(ls_header).

* Even if there are multiple BBYs per Promo the Sites would be the same for all
    READ TABLE mt_orgsites INDEX 1 INTO DATA(ls_site).
    READ TABLE mt_orgitems INDEX 1 INTO DATA(ls_orgitem).

    lv_vkorg = COND #( WHEN ls_site IS NOT INITIAL THEN ls_site-vkorg ELSE ls_orgitem-vkorg ).

    IF lv_vkorg IS INITIAL.
      CLEAR ms_textid.
      ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
      ms_textid-msgno = '019'.
      ms_textid-attr1 = ls_promo-aktnr.
      ms_textid-attr2 = ls_header-bbynr.
      mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
      RAISE EXCEPTION mx_exception.
    ENDIF.

    mo_config_map = NEW #( iv_vkorg = lv_vkorg ).

    SELECT vkorg, land1 FROM t001w
              INTO TABLE @DATA(lt_country)
              WHERE vkorg = @lv_vkorg.

    TRY.
        DATA(lv_country) = lt_country[ vkorg = lv_vkorg ]-land1.
      CATCH cx_sy_itab_line_not_found.
        lv_country = 'XX'.
    ENDTRY.

    DATA(lt_buy)    = FILTER #( mt_buy    WHERE bbynr = ls_header-bbynr ).
    DATA(lt_get)    = FILTER #( mt_get    WHERE bbynr = ls_header-bbynr ).


*"----------------------------------------------------------------------
*  Is it a Basket
*"----------------------------------------------------------------------
* Where we have multiple BBYs in a Promotion they should be identical except from the quantity and price
    mv_is_basket = get_basket_indicator( EXPORTING is_header = ls_header
                                                   it_buy    = lt_buy
                                                   it_get    = lt_get ).

    IF mv_is_basket = abap_true.
* In this case then we want one Basket creating of each BBY
      LOOP AT mt_header INTO ls_header.

        DATA(lt_header) = VALUE rdm_t_bby_wa_header( ( ls_header ) ).
        lt_buy          = FILTER #( mt_buy    WHERE bbynr = ls_header-bbynr ).
        lt_get          = FILTER #( mt_get    WHERE bbynr = ls_header-bbynr ).
        DATA(lt_reward) = FILTER #( mt_reward WHERE bbynr = ls_header-bbynr ).
        DATA(lt_matgrp) = FILTER #( mt_matgrp WHERE bbynr = ls_header-bbynr ).

        create_retail_event_idoc(
          EXPORTING
            it_header         =  lt_header               " Bonus Buy: Header Table
            it_buy            =  lt_buy                  " Bonus Buy: "Buy" Side (Table Type)
            it_get            =  lt_get                  " Bonus Buy: "Get" Side (Table Type)
            it_reward         =  lt_reward               " Bonus Buy: Reward (Table Type)
            it_matgrp         =  lt_matgrp               " Bonus Buy: Table with Material Groupings
            it_orgitems       =  mt_orgitems             " Bonus Buy: Table with Organization Items
            it_orgsites       =  mt_orgsites             " Bonus Buy: Sites of Site Groups (Table Type)
            it_texts          =  mt_texts                " Bonus Buy: Table with Texts
            it_promo_sent_ext =  mt_promo_sent_ext       " Sorted Promotion Table: Per Service-Supplied Stores
            iv_country        = lv_country
            iv_vkorg          = lv_vkorg
        ).

      ENDLOOP.

    ELSE.
      create_retail_event_idoc(
        EXPORTING
          it_header         =  mt_header               " Bonus Buy: Header Table
          it_buy            =  mt_buy                  " Bonus Buy: "Buy" Side (Table Type)
          it_get            =  mt_get                  " Bonus Buy: "Get" Side (Table Type)
          it_reward         =  mt_reward               " Bonus Buy: Reward (Table Type)
          it_matgrp         =  mt_matgrp               " Bonus Buy: Table with Material Groupings
          it_orgitems       =  mt_orgitems             " Bonus Buy: Table with Organization Items
          it_orgsites       =  mt_orgsites             " Bonus Buy: Sites of Site Groups (Table Type)
          it_texts          =  mt_texts                " Bonus Buy: Table with Texts
          it_promo_sent_ext =  mt_promo_sent_ext       " Sorted Promotion Table: Per Service-Supplied Stores
          iv_country        = lv_country
          iv_vkorg          = lv_vkorg
      ).

    ENDIF.

  ENDMETHOD.


  METHOD CREATE_RETAIL_EVENT_IDOC.

* A Promo can contain multiple BBYs
* When this happens it is because of scales (or this is the only scenario we are covering)
* This means that the BBYs are identical except for a couple of values on the GET side
* The values which will be different are the quantity and price
* So most of the ViKING offer data will be taken from the first BBY
* And any additional BBYs will be used to populate the Steps in the Discount Group


    DATA: ls_offer TYPE zpos_offer_header_sty,
          lt_offer TYPE zpos_offer_header_tty,
          lt_edidd TYPE STANDARD TABLE OF edidd,
          ls_edidc TYPE edidc,
          lv_subrc TYPE sysubrc.

* Should only be one Promo at a time, but can be for multiple stores.
    READ TABLE it_promo_sent_ext INDEX 1 INTO DATA(ls_promo).

* Set the Header data based on the first BBY in the Promo - assumes they must all be same!
    READ TABLE it_header INDEX 1 INTO DATA(ls_header).

    ls_offer-country = iv_country.

    DATA(lt_buy)    = FILTER #( it_buy    WHERE bbynr = ls_header-bbynr ).
    DATA(lt_get)    = FILTER #( it_get    WHERE bbynr = ls_header-bbynr ).
    DATA(lt_reward) = FILTER #( it_reward WHERE bbynr = ls_header-bbynr ).
    DATA(lt_matgrp) = FILTER #( it_matgrp WHERE bbynr = ls_header-bbynr ).



*"-------------------------------------------------------------------------------------
*------------------------------- ZPOS_OFFER_HEADER ------------------------------------
*"-------------------------------------------------------------------------------------

* Once for all BBYs in the Promo
    offer_header_handler(
               EXPORTING iv_vkorg  = iv_vkorg
                         iv_werks  = ls_promo-werks
                         iv_chgid  = ls_promo-chgid
                         is_header = ls_header
                         it_texts  = it_texts
              CHANGING   cs_offer = ls_offer ).

*"----------------------------------------------------------------------
*  Is Basket
*"----------------------------------------------------------------------
*    mv_is_basket = get_basket_indicator(
*                               EXPORTING is_header = ls_header
*                                         it_buy    = lt_buy
*                                         it_get    = lt_get ).

**"----------------------------------------------------------------------
**  Discount Method
**"----------------------------------------------------------------------
    DATA(ls_method_value) = get_discount_method_value( EXPORTING is_header = ls_header it_buy = lt_buy  it_get = lt_get it_reward = lt_reward ).
    ls_offer-discount_method  = ls_method_value-method.
*
**"----------------------------------------------------------------------
**  Discount Value
**"----------------------------------------------------------------------
    IF mv_is_basket = abap_true.        " Not relevant for Discount Groups
      ls_offer-discount_value  = ls_method_value-value.
    ENDIF.
**"----------------------------------------------------------------------
**  Fixed Discount Indicator
**"----------------------------------------------------------------------
    ls_offer-set_discount = COND #( WHEN mo_config_map->is_set_discount_active( ) = abap_true THEN '1' ELSE '0' ).


*"-------------------------------------------------------------------------------------
*------------------------------- ZPOS_OFFER_HEADER ------------------------------------
*"-------------------------------------------------------------------------------------


*------------------------------- ZPOS_OFFER_STEP_01 -----------------------------------
    IF mv_is_basket = abap_false.
      discgrp_step_handler( EXPORTING it_get = lt_get
                                      it_reward = lt_reward
                            CHANGING  cs_offer = ls_offer ).
    ENDIF.
*------------------------------- ZPOS_OFFER_ITEM_01 -----------------------------------



*------------------------------- ZPOS_OFFER_ITEM_01 -----------------------------------

    buy_get_handler( EXPORTING it_buy = lt_buy it_get = lt_get
                               it_matgrp = lt_matgrp
                     CHANGING  cs_offer = ls_offer ).
*------------------------------- ZPOS_OFFER_ITEM_01 -----------------------------------


*------------------------------- ITEM List -----------------------------------
    itemlist_handler(
                       EXPORTING it_matgrp = lt_matgrp
                       CHANGING cs_offer = ls_offer ).

*------------------------------- ITEM List -----------------------------------


* Can be multiple BBYs in the same Promo
    IF lines( it_header ) GT 1 AND mv_is_basket = abap_false.

      LOOP AT it_header FROM 2 INTO ls_header.

        lt_get    = FILTER #( it_get    WHERE bbynr = ls_header-bbynr ).
        lt_reward = FILTER #( it_reward WHERE bbynr = ls_header-bbynr ).

* This should be a discount group - and we only need to create more steps

*------------------------------- ZPOS_OFFER_STEP_01 -----------------------------------

        discgrp_step_handler( EXPORTING it_get = lt_get
                                        it_reward = lt_reward
                              CHANGING  cs_offer = ls_offer ).
*------------------------------- ZPOS_OFFER_ITEM_01 -----------------------------------

      ENDLOOP.

* Only necessary for Countries with SetDiscount e.g. Sweden
      IF ls_offer-set_discount = co_bool_01_true AND lines( ls_offer-steps ) EQ 2.

        ls_offer-steps = step_pricing_handler( ls_offer-steps ).

      ENDIF.

    ENDIF.




*------------------------------- CONTROL record ------------------------------------
    ls_edidc-rcvpor = ls_edidc-sndpor = 'SAP' && sy-sysid.

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = ls_edidc-sndprn
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.
    IF sy-subrc <> 0.
      ms_message-msgid = zcl_pos_util=>co_pos_msgid.
      RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e000(zpos) WITH 'Tech Error determining own logical system'.
    ENDIF.

    ls_edidc-rcvpor = 'SAP_PO'.
    ls_edidc-rcvprn = 'SAP_PO'.
    ls_edidc-rcvprt = ls_edidc-sndprt = 'LS'.
    ls_edidc-outmod = '2'.  " Pass Immediately - For Testing

    ls_edidc-status = '30'.
    ls_edidc-direct = '1'.
    ls_edidc-mestyp = 'ZPOS_OFFER'.
    ls_edidc-idoctp = 'ZPOS_OFFER_01'.
    ls_edidc-mesfct = COND #( WHEN mv_is_basket = abap_true THEN 'BSK' ELSE 'DGP' ).
    ls_edidc-mescod = ls_offer-country.
*------------------------------- CONTROL RECORD ------------------------------------



    LOOP AT it_promo_sent_ext INTO ls_promo.

      lt_edidd = map_struct_to_idoc( EXPORTING iv_store = ls_promo-werks CHANGING cs_offer = ls_offer ).

      CALL FUNCTION 'IDOC_OUTBOUND_WRITE_TO_DB'
*           EXPORTING
*             ERROR_HANDLING_START       = 'X'
        IMPORTING
          state_of_idoc  = lv_subrc
        TABLES
          int_edidd      = lt_edidd
        CHANGING
          int_edidc      = ls_edidc
        EXCEPTIONS
          idoc_not_saved = 1
          OTHERS         = 2.

      IF sy-subrc = 0.
*            MESSAGE i000(zpos) WITH 'IDoc Created' && ls_edidc-docnum.
      ELSE.
        CLEAR ms_textid.
        ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
        ms_textid-msgno = '026'.
        ms_textid-attr1 = ls_edidc-mestyp.
        ms_textid-attr2 = ls_promo-aktnr.
        mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
        RAISE EXCEPTION mx_exception.
      ENDIF.

      CLEAR: lt_edidd, ls_edidc-docnum.

    ENDLOOP.

  ENDMETHOD.


  METHOD DISCGRP_STEP_HANDLER.


* There can (should) only be one row in the table for a Discount Group
    CHECK mv_is_basket = abap_false.
    LOOP AT it_get INTO DATA(ls_get).

      APPEND INITIAL LINE TO cs_offer-steps ASSIGNING FIELD-SYMBOL(<ls_step>).
      <ls_step>-discount_quantity = ls_get-mquan.
      TRY.
          <ls_step>-discount_amount   = it_reward[ bbynr = ls_get-bbynr rewnr = ls_get-rewnr ]-kbetr.
        CATCH cx_sy_itab_line_not_found.
          CLEAR ms_textid.
          ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ms_textid-msgno = '030'.
          ms_textid-attr1 = ls_get-bbynr.
          mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION mx_exception.

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD ELEMENT_SORT.


*        DiscountGroup elements sorted on DiscountId
*                We only ever have one per file
*
*        Step elements sorted on DiscountQty
*                Need to check this   the Discount group schema
*
*        DiscountItem elements sorted on ItemType  AND ItemId secondly
*                Which Schema are you tlaking about? the Discount group schema
*
*        Basket elements sorted on DiscountId
*                We only ever have one per file
*
*        BasketItem elements sorted on ItemType  AND ItemId secondly
*                Will need to check this
*
*        ScaledPrice elements sorted on ArticleNo
*                We only ever have one per file
*
*        Price elements sorted on DiscountQty
*                They are already sorted this way
*
*        ArticleList elements sorted on ArticleListId
*                Will need to check this  I have just been informed that sorting is not needed at this level
*
*        ListArticle elements sorted on ArticleNo
*                Will need to check this
*
*        Article elements sorted on ArticleNo
*                Fairly sure that they are already sorted this way.

    SORT cs_offer-steps BY discount_quantity ASCENDING.

    SORT cs_offer-items BY itemtype itemid ASCENDING.

    SORT cs_offer-itemlists BY articlelist_id ASCENDING.

    LOOP AT cs_offer-itemlists ASSIGNING FIELD-SYMBOL(<ls_list>).
      SORT <ls_list>-listarticles BY articleid.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_ARTICLELIST_ID.

    DATA: ls_list_id TYPE zpos_vk_list.

    CLEAR ms_message.

    SELECT SINGLE * FROM zpos_vk_list INTO @ls_list_id
            WHERE bbynr        = @is_offer-bbynr
            AND   matgrp_nr    = @iv_matgrp_nr.

    IF sy-subrc = 0.
      rv_articlelist_id = ls_list_id-articlelist_id.
    ELSE.
* We need to get an ID and add to table
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZPOS_LIST'
        IMPORTING
          number                  = ls_list_id-articlelist_id
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.

        rv_articlelist_id = ls_list_id-articlelist_id.

* Add to table
        ls_list_id-bbynr       = is_offer-bbynr.
        ls_list_id-matgrp_nr   = iv_matgrp_nr.
*        ls_list_id-articlelist_id   = see above!
        ls_list_id-valid_to    = is_offer-valid_to(8).
        INSERT zpos_vk_list FROM ls_list_id.
        COMMIT WORK.
      ELSE.
        CLEAR ms_textid.
        ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
        ms_textid-msgno = '020'.
        ms_textid-attr1 = 'ZPOS_LIST'.
        ms_textid-attr2 = '01'.
        ms_textid-attr3 = 'ZPOS_LIST_NR'.
        mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
        RAISE EXCEPTION mx_exception.

      ENDIF.

    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = rv_articlelist_id
      IMPORTING
        output = rv_articlelist_id.

*Move Values to Class Attribute
    APPEND ls_list_id TO mt_list_id.


  ENDMETHOD.


  METHOD GET_BASKET_INDICATOR.

* This is used to determine if we can map a BBY as a Discount Group

* This only handles certain scenarios - awaiting input from the Promotions Team

    rv_is_basket = abap_true.       "Default Value


    IF mo_config_map->is_discgroup_active( ) = abap_true.

      IF lines( mt_header ) GT 1.
* More than one BBY - then we are assuming it is scales handling
        rv_is_basket = abap_false.

      ELSEIF is_header-getcon = 'A' OR is_header-buycon = 'A'.
        IF it_buy IS INITIAL AND is_header-getcon = 'O'.
* If there are no BUY lines and all of the GETs are linked with an OR
          rv_is_basket   = abap_false.
        ELSEIF it_buy IS INITIAL AND lines( it_get ) LT 2.
* Even if the GET has an "AND" but there is only one line
          rv_is_basket   = abap_false.
        ELSE.
* Then it should be a basket .... Hopefully!

        ENDIF.

      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD GET_BBY_ID.

    DATA: ls_new_offer TYPE zpos_vk_offer.

    CLEAR ms_message.

    SELECT SINGLE * FROM zpos_vk_offer
           INTO @DATA(ls_vk_offer)
            WHERE   identifier = @is_offer-bbynr
            AND     category   = @zcl_pos_util=>co_offer_category-bonus_buy.
    IF sy-subrc = 0.
      rv_offer_id = ls_vk_offer-vk_offer_id.
    ELSE.
* We need to get an ID and add to table
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZPOS_BBY'
        IMPORTING
          number                  = ls_new_offer-vk_offer_id
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.

        rv_offer_id           = ls_new_offer-vk_offer_id.

* Add to table
        ls_new_offer-s4_offer_id   = is_offer-s4_offer_id.
        ls_new_offer-identifier    = is_offer-bbynr.
        ls_new_offer-category      = zcl_pos_util=>co_offer_category-bonus_buy.
        ls_new_offer-valid_to      = is_offer-valid_to(8).
        INSERT zpos_vk_offer FROM ls_new_offer.
        COMMIT WORK.
      ELSE.
        CLEAR ms_textid.
        ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
        ms_textid-msgno = '020'.
        ms_textid-attr1 = 'ZPOS_BBY'.
        ms_textid-attr2 = '01'.
        ms_textid-attr3 = 'ZPOS_BBY_NR'.
        mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
        RAISE EXCEPTION mx_exception.

      ENDIF.

    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = rv_offer_id
      IMPORTING
        output = rv_offer_id.

  ENDMETHOD.


  METHOD GET_DISCOUNT_METHOD_VALUE.

    CLEAR ms_message.

    ms_message-msgid = co_zpos_msgid.

* In ViKING the Method is on the header level so only one value is allowed for all get rows

    DATA:lv_btype TYPE rdm_bby_disctype,
         lv_value TYPE kbetr.

    IF NOT is_header-get_rewnr IS INITIAL.

      READ TABLE it_reward INTO DATA(ls_reward) WITH KEY rewnr = is_header-get_rewnr.
      IF sy-subrc = 0.
        IF lv_btype IS INITIAL.
          lv_btype = ls_reward-btype.
          lv_value = ls_reward-kbetr.
        ELSE.
          CLEAR ms_textid.
          ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ms_textid-msgno = '024'.
          ms_textid-attr1 = is_header-bbynr.
          mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION mx_exception.
        ENDIF.
      ENDIF.
    ELSE.
      LOOP AT it_get INTO DATA(ls_get).

        READ TABLE it_reward INTO ls_reward WITH KEY rewnr = ls_get-rewnr.
        IF sy-subrc = 0.
          IF lv_btype IS INITIAL.
            lv_btype = ls_reward-btype.
            lv_value = ls_reward-kbetr.
          ELSE.
            IF lv_btype NE ls_reward-btype OR lv_value NE ls_reward-kbetr.
              CLEAR ms_textid.
              ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
              ms_textid-msgno = '024'.
              ms_textid-attr1 = is_header-bbynr.
              mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
              RAISE EXCEPTION mx_exception.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.


*"----------------------------------------------------------------------
* BTYPE
*"----------------------------------------------------------------------
*P  Discount Price
*R  Discount Amount
*%  Discount Percent
*S  No Discount

*"----------------------------------------------------------------------
* ViKING Method Type
*"----------------------------------------------------------------------
*0  Discount Amount
*1  Total Price
*2  Discount Percentage
*3  Percentage to Pay -Selected Item(s)

    IF NOT lv_btype IS INITIAL.

      CASE lv_btype.
        WHEN 'P'.
          rs_method_value-method = zcl_pos_util=>co_offer_discount_method-total_price.
          rs_method_value-value = lv_value.
        WHEN 'R'.
          CLEAR ms_textid.
          ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ms_textid-msgno = '022'.
          ms_textid-attr1 = is_header-bbynr.
          mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION mx_exception.
        WHEN '%'.
          rs_method_value-method = zcl_pos_util=>co_offer_discount_method-pay_percent.
          rs_method_value-value = 100 - lv_value.     " ViKING needs the percentage to be paid - 100% discount = 0% to be paid
        WHEN OTHERS.
          CLEAR ms_textid.
          ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ms_textid-msgno = '024'.
          ms_textid-attr1 = is_header-bbynr.
          mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION mx_exception.
      ENDCASE.

    ELSE.
      CLEAR ms_textid.
      ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
      ms_textid-msgno = '023'.
      ms_textid-attr1 = is_header-bbynr.
      mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
      RAISE EXCEPTION mx_exception.
    ENDIF.

  ENDMETHOD.


  METHOD GET_MARM_EAN.

    DATA: ls_marm TYPE marm.

    CALL FUNCTION 'MARM_SINGLE_READ'
      EXPORTING
*       KZRFB = ' '
*       MAXTZ = 0
        matnr = iv_matnr
        meinh = iv_meinh
      IMPORTING
        wmarm = ls_marm
*     EXCEPTIONS
*       WRONG_CALL       = 1
*       NOT_FOUND        = 2
*       OTHERS           = 3
      .
    IF sy-subrc = 0 AND ls_marm-ean11 IS NOT INITIAL.
      rv_ean11 = ls_marm-ean11.
    ELSE.
      CLEAR ms_textid.
      ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
      ms_textid-msgno = '021'.
      ms_textid-attr1 = iv_matnr.
      ms_textid-attr2 = iv_meinh.
      mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
      RAISE EXCEPTION mx_exception.

    ENDIF.


  ENDMETHOD.


  METHOD GET_SALESORG_LANGAUGE.

    CASE iv_vkorg.
      WHEN '9700'.
        rv_language = 'V'.      "Swedish
      WHEN '9000'.
        rv_language = 'K'.      "Danish
      WHEN '7400'.
        rv_language = 'O'.      "Norwegian
      WHEN '6800'.
        rv_language = 'b'.      "Icenlandic
      WHEN OTHERS.
        CLEAR ms_textid.
        ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
        ms_textid-msgno = '024'.
        ms_textid-attr1 = iv_vkorg.
        mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
        RAISE EXCEPTION mx_exception.

    ENDCASE.

  ENDMETHOD.


  METHOD ITEMLIST_HANDLER.

    LOOP AT it_matgrp INTO DATA(ls_matgrp) GROUP BY ls_matgrp-grpnr.

*------------------------------- ZPOS_ITEMLIST_HEADER ------------------------------------
      APPEND INITIAL LINE TO cs_offer-itemlists ASSIGNING FIELD-SYMBOL(<ls_itemlist_header>).

      MOVE-CORRESPONDING cs_offer TO <ls_itemlist_header>.


      DATA(lv_grpnr) = ls_matgrp-grpnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_grpnr
        IMPORTING
          output = lv_grpnr.

      <ls_itemlist_header>-description = |BBY-{ cs_offer-bbynr }  Grp-{ lv_grpnr } | .
      REPLACE ALL OCCURRENCES OF '0' IN <ls_itemlist_header>-description WITH ''.
      <ls_itemlist_header>-description = <ls_itemlist_header>-description && '0'.

      TRY.
          <ls_itemlist_header>-articlelist_id = mt_list_id[ bbynr = cs_offer-bbynr matgrp_nr = lv_grpnr ]-articlelist_id.


        CATCH cx_sy_itab_line_not_found.
          clear ms_textid.
          ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ms_textid-msgno = '027'.
          ms_textid-attr1 = cs_offer-bbynr.
          ms_textid-attr2 =  lv_grpnr.
          mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION mx_exception.

      ENDTRY.

*------------------------------- ZPOS_ITEMLIST_HEADER ------------------------------------


      LOOP AT GROUP ls_matgrp INTO DATA(member).

*------------------------------- ZPOS_ITEMLIST_ITEM_01 -----------------------------------

        APPEND INITIAL LINE TO <ls_itemlist_header>-listarticles ASSIGNING FIELD-SYMBOL(<ls_itemlist_item>).

        <ls_itemlist_item>-articleid   = get_marm_ean( EXPORTING iv_matnr = member-matnr iv_meinh = member-vrkme ) .
        <ls_itemlist_item>-article_sku = member-matnr.

*------------------------------- ZPOS_OFFER_ITEM_01 -----------------------------------

      ENDLOOP.

    ENDLOOP.


  ENDMETHOD.


  METHOD MAP_STRUCT_TO_IDOC.


    DATA: ls_idoc_data       TYPE edidd,
          ls_edidc           TYPE edidc,
          ls_offer_header    TYPE zpos_offer_header_01,
          ls_offer_step      TYPE zpos_offer_step_01,
          ls_offer_item      TYPE zpos_offer_item_01,
          ls_itemlist_header TYPE zpos_itemlist_header_01,
          ls_itemlist_item   TYPE zpos_itemlist_item_01.

    DATA: lv_header_segnum   TYPE idocdsgnum VALUE '0',
          lv_itemlist_segnum TYPE idocdsgnum VALUE '0',
          lv_segnum          TYPE idocdsgnum VALUE '0'.


    element_sort( CHANGING cs_offer = cs_offer ).

*------------------------------- ZPOS_OFFER_HEADER ------------------------------------
    ls_idoc_data-segnam = 'ZPOS_OFFER_HEADER_01'.
    ls_idoc_data-hlevel =  '02'.
    lv_segnum           += 1.
    ls_idoc_data-segnum = lv_segnum.

    MOVE-CORRESPONDING cs_offer TO ls_offer_header.
    ls_offer_header-store = iv_store.         " Reusing the same IDoc Data for each store in the BBY

    ls_idoc_data-sdata = ls_offer_header.

    APPEND ls_idoc_data TO rt_edidd.
    CLEAR : ls_idoc_data.
    lv_header_segnum = lv_segnum.
*------------------------------- ZPOS_OFFER_HEADER ------------------------------------



*------------------------------- ZPOS_OFFER_STEP -----------------------------------
    LOOP AT cs_offer-steps INTO DATA(ls_step).

      ls_idoc_data-segnam = 'ZPOS_OFFER_STEP_01'.
      ls_idoc_data-hlevel =  '03'.
      ls_idoc_data-psgnum = lv_header_segnum.
      lv_segnum           += 1.
      ls_idoc_data-segnum = lv_segnum.

      MOVE-CORRESPONDING ls_step TO ls_offer_step.

      ls_idoc_data-sdata = ls_offer_step.
      APPEND ls_idoc_data TO rt_edidd.
      CLEAR : ls_idoc_data.

    ENDLOOP.
*------------------------------- ZPOS_OFFER_STEP -----------------------------------


*------------------------------- ZPOS_OFFER_ITEM -----------------------------------
    LOOP AT cs_offer-items INTO DATA(ls_item).

      ls_idoc_data-segnam = 'ZPOS_OFFER_ITEM_01'.
      ls_idoc_data-hlevel =  '03'.
      ls_idoc_data-psgnum = lv_header_segnum.
      lv_segnum           += 1.
      ls_idoc_data-segnum = lv_segnum.

      MOVE-CORRESPONDING ls_item TO ls_offer_item.

      ls_idoc_data-sdata = ls_offer_item.
      APPEND ls_idoc_data TO rt_edidd.
      CLEAR : ls_idoc_data.

    ENDLOOP.
*------------------------------- ZPOS_OFFER_ITEM -----------------------------------


*------------------------------- ZPOS_ITEMLIST_HEADER ------------------------------------
    LOOP AT cs_offer-itemlists INTO DATA(ls_offer_itemlist).

      ls_idoc_data-segnam = 'ZPOS_ITEMLIST_HEADER_01'.
      ls_idoc_data-hlevel =  '03'.
      ls_idoc_data-psgnum = lv_header_segnum.
      lv_segnum           += 1.
      ls_idoc_data-segnum = lv_segnum.

      MOVE-CORRESPONDING ls_offer_itemlist TO ls_itemlist_header.
      ls_itemlist_header-store = iv_store.         " Reusing the same IDoc Data for each store in the BBY

      ls_idoc_data-sdata = ls_itemlist_header.
      APPEND ls_idoc_data TO rt_edidd.
      CLEAR : ls_idoc_data.
      lv_itemlist_segnum = lv_segnum.


      LOOP AT ls_offer_itemlist-listarticles INTO DATA(ls_offer_itemlist_item).

        ls_idoc_data-segnam = 'ZPOS_ITEMLIST_ITEM_01'.
        ls_idoc_data-hlevel =  '04'.
        ls_idoc_data-psgnum = lv_itemlist_segnum.
        lv_segnum           += 1.
        ls_idoc_data-segnum = lv_segnum.

        MOVE-CORRESPONDING ls_offer_itemlist_item TO ls_itemlist_item.

        ls_idoc_data-sdata = ls_itemlist_item.
        APPEND ls_idoc_data TO rt_edidd.
        CLEAR : ls_idoc_data.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD OFFER_HEADER_HANDLER.


*"----------------------------------------------------------------------
* Get Language
*"----------------------------------------------------------------------
    DATA(lv_language) = get_salesorg_langauge( iv_vkorg ).


*"----------------------------------------------------------------------
*  Update Action
*"----------------------------------------------------------------------
    cs_offer-action      = iv_chgid.


*"----------------------------------------------------------------------
*  Store
*"----------------------------------------------------------------------
    cs_offer-store      = iv_werks.


*"----------------------------------------------------------------------
*  BBY Number
*"----------------------------------------------------------------------
    cs_offer-bbynr  = is_header-bbynr.


*"----------------------------------------------------------------------
*  S4 Offer ID
*"----------------------------------------------------------------------

    IF is_header-offer_id CA '-'.
*SY-FDPOS contains the position of the character
      DATA(lv_offset) = sy-fdpos - 1.
      cs_offer-s4_offer_id  = is_header-offer_id(lv_offset).
    ELSE.
      cs_offer-s4_offer_id  = is_header-offer_id.
    ENDIF.


*"----------------------------------------------------------------------
*  Validity Period
*"----------------------------------------------------------------------
    cs_offer-valid_from  = is_header-datab && '000000'.
    cs_offer-valid_to    = is_header-datbi && '235959'.


*"----------------------------------------------------------------------
*  Offer Description
*"----------------------------------------------------------------------
    TRY.
        cs_offer-description = it_texts[ bbynr = is_header-bbynr spras = lv_language ]-bbytext.
      CATCH cx_sy_itab_line_not_found.

        TRY.
            cs_offer-description = it_texts[ bbynr = is_header-bbynr spras = 'E' ]-bbytext.
          CATCH cx_sy_itab_line_not_found.
            cs_offer-description = 'No Description Found'.
        ENDTRY.
    ENDTRY.

*"----------------------------------------------------------------------
*  Receipt Text - Not Used
*"----------------------------------------------------------------------
*    cs_offer-receipt_text  = 'Some Receipt Text'.


*"----------------------------------------------------------------------
*  ViKING Offer ID
*"----------------------------------------------------------------------
    cs_offer-vk_offer_id  = get_bby_id( EXPORTING is_offer = cs_offer ).



  ENDMETHOD.


  METHOD STEP_PRICING_HANDLER.

    DATA: lv_amount   TYPE rdm_bby_kbetr,
          lv_quantity TYPE rdm_bby_buy_matquan.


    rt_steps[] = it_steps[].

    CHECK mo_config_map->is_offer_extra_scales_active( ).
    CHECK mv_is_basket = abap_false.

* MAXIMUM of 5 STEPS ALLOWED

    DATA(lv_lines) = lines( rt_steps ).

    READ TABLE it_steps INDEX 1 INTO DATA(ls_step_1).
    READ TABLE it_steps INDEX 2 INTO DATA(ls_step_2).

* Step 3 ( Line 1 + Line 2 )
    APPEND INITIAL LINE TO rt_steps ASSIGNING FIELD-SYMBOL(<ls_steps>).
    lv_quantity = ls_step_1-discount_quantity + ls_step_2-discount_quantity  .
    lv_amount   = ls_step_1-discount_amount + ls_step_2-discount_amount .

    <ls_steps>-discount_quantity = lv_quantity.
    <ls_steps>-discount_amount   = lv_amount.

* Step 4 (Either Line 3 + Line 1, or Line  2 * 2
    IF lv_quantity + ls_step_1-discount_quantity < ls_step_2-discount_quantity * 2.

      lv_quantity += ls_step_1-discount_quantity .
      lv_amount   += ls_step_1-discount_amount.

      APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_steps>.
      <ls_steps>-discount_quantity = lv_quantity.
      <ls_steps>-discount_amount   = lv_amount.

    ELSE.

      lv_quantity = ls_step_2-discount_quantity * 2.
      lv_amount   = ls_step_2-discount_amount * 2.

      APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_steps>.
      <ls_steps>-discount_quantity = lv_quantity.
      <ls_steps>-discount_amount   = lv_amount.

    ENDIF.


* Step 5 (Either Line 4 + Line 1, or Line  2 * 3
    IF lv_quantity + ls_step_1-discount_quantity < ls_step_2-discount_quantity * 3.

      lv_quantity += ls_step_1-discount_quantity .
      lv_amount   += ls_step_1-discount_amount.

      APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_steps>.
      <ls_steps>-discount_quantity = lv_quantity.
      <ls_steps>-discount_amount   = lv_amount.

    ELSE.

      lv_quantity = ls_step_2-discount_quantity * 3.
      lv_amount   = ls_step_2-discount_amount * 3.

      APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_steps>.
      <ls_steps>-discount_quantity = lv_quantity.
      <ls_steps>-discount_amount   = lv_amount.

    ENDIF.


  ENDMETHOD.
ENDCLASS.

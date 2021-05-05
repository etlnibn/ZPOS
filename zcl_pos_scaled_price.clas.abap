class ZCL_POS_SCALED_PRICE definition
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

  class-methods CREATE_SCALE_PRICE_IDOC
    importing
      value(IT_ARTICLE_PRICE) type ZPOS_ARTICLE_PRICE_TTY
    raising
      ZCX_POS_EXCEPTION .
  PROTECTED SECTION.
private section.

  constants CO_BOOL_01_TRUE type BOOLEAN_01 value 1 ##NO_TEXT.
  constants CO_BOOL_01_FALSE type BOOLEAN_01 value 0 ##NO_TEXT.
  constants CO_ENGLISH_LANGUAGE type SPRAS value 'E' ##NO_TEXT.
  class-data MS_MESSAGE type SCX_T100KEY .
  class-data MO_CONFIG_MAP type ref to ZCL_POS_CONFIG_TAB_HANLDER .
  class-data MX_EXCEPTION type ref to ZCX_POS_EXCEPTION .
  class-data MS_TEXTID type SCX_T100KEY .

  class-methods GET_SCALE_DISCOUNT_ID
    importing
      !IV_EAN11 type EAN11
      value(IV_MATNR) type MATNR
      !IV_VALID_TO type DATBI
    returning
      value(RV_DISCOUNT_ID) type ZPOS_VK_OFFER_ID
    raising
      ZCX_POS_EXCEPTION .
  class-methods STEP_PRICING_HANDLER
    importing
      !IT_STEPS type ZPOS_PRICELIST_SCALE_TTY
    returning
      value(RT_STEPS) type ZPOS_PRICELIST_SCALE_TTY .
ENDCLASS.



CLASS ZCL_POS_SCALED_PRICE IMPLEMENTATION.


  METHOD create_scale_price_idoc.


    DATA: lt_edidd        TYPE STANDARD TABLE OF edidd,
          ls_idoc_data    TYPE edidd,
          ls_edidc        TYPE edidc,
          ls_offer_header TYPE zpos_offer_header_01,
          ls_offer_step   TYPE zpos_offer_step_01.

    DATA: ls_edids TYPE edids,
          lv_subrc TYPE sysubrc.

    DATA: lv_parent_segnum   TYPE idocdsgnum VALUE '0',
          lv_itemlist_segnum TYPE idocdsgnum VALUE '0',
          lv_segnum          TYPE idocdsgnum VALUE '0',
          lv_counter         TYPE i VALUE 0,
          lv_docnum          TYPE docnum,
          ls_article_price   TYPE zpos_article_price_sty.

    DATA(lt_article_price) = it_article_price.
    DELETE lt_article_price WHERE pos_recipient NE zcl_pos_util=>co_pos_recipient-viking.
    DELETE lt_article_price WHERE eff_scale IS INITIAL.


    CHECK lt_article_price IS NOT INITIAL.
    READ TABLE lt_article_price INDEX 1 INTO ls_article_price.
    SELECT vkorg, land1 FROM t001w INTO TABLE @DATA(lt_country) WHERE vkorg = @ls_article_price-vkorg.
    mo_config_map = NEW #( iv_vkorg = ls_article_price-vkorg ).


*------------------------------- CONTROL record ------------------------------------
    ls_edidc-rcvpor = ls_edidc-sndpor = 'SAP' && sy-sysid.

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = ls_edidc-sndprn
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e007(zpos).
    ENDIF.


    ls_edidc-rcvpor = 'SAP_PO'.
    ls_edidc-rcvprn = 'SAP_PO'.
    ls_edidc-rcvprt = ls_edidc-sndprt = 'LS'.
    ls_edidc-outmod = '2'.          " Pass Immediately - For Testing

    ls_edidc-status = '30'.
    ls_edidc-direct = '1'.
    ls_edidc-mestyp = 'ZPOS_OFFER'.
    ls_edidc-idoctp = 'ZPOS_OFFER_01'.
    TRY.
        ls_edidc-mescod = lt_country[ vkorg = ls_article_price-vkorg ]-land1.
      CATCH cx_sy_itab_line_not_found.
        ls_edidc-mescod = 'XX'.
    ENDTRY.
    ls_edidc-mesfct = 'SCP'.      "Scaled Pricing
*------------------------------- CONTROL RECORD ------------------------------------


    lv_docnum += 1.
    LOOP AT lt_article_price INTO ls_article_price
                  GROUP BY ( werks = ls_article_price-werks ).

*------------------------------- CONTROL record ------------------------------------
      ls_edidc-sndprn = ls_article_price-werks.
*------------------------------- CONTROL RECORD ------------------------------------

      LOOP AT GROUP ls_article_price ASSIGNING FIELD-SYMBOL(<ls_article_price>).

        CLEAR: ls_idoc_data, ls_offer_header, ls_offer_step.
        lv_counter += 1.

*------------------------------- ZPOS_OFFER_HEADER ------------------------------------
        ls_idoc_data-segnam = 'ZPOS_OFFER_HEADER_01'.
        ls_idoc_data-hlevel =  '02'.
        lv_segnum           += 1.
        ls_idoc_data-segnum = lv_segnum.

        ls_offer_header-action      = 'U'.
        ls_offer_header-vk_offer_id = get_scale_discount_id( iv_ean11 = <ls_article_price>-ean11 iv_matnr = <ls_article_price>-matnr iv_valid_to = <ls_article_price>-datbi ).
        ls_offer_header-store       = <ls_article_price>-werks.
        ls_offer_header-itemid      = <ls_article_price>-ean11.
        ls_offer_header-description = <ls_article_price>-description.
        ls_offer_header-valid_from  = <ls_article_price>-datab && '000000'.
        ls_offer_header-valid_to    = <ls_article_price>-datbi && '235959' .

        ls_idoc_data-sdata = ls_offer_header.

        APPEND ls_idoc_data TO lt_edidd.
        CLEAR : ls_idoc_data.
        lv_parent_segnum = lv_segnum.
*------------------------------- ZPOS_OFFER_HEADER ------------------------------------



*------------------------------- ZPOS_OFFER_STEP -----------------------------------
        IF lines( <ls_article_price>-eff_scale ) EQ 2.

          <ls_article_price>-eff_scale = step_pricing_handler( <ls_article_price>-eff_scale ).

        ENDIF.


        LOOP AT <ls_article_price>-eff_scale INTO DATA(ls_scale).

          ls_idoc_data-segnam = 'ZPOS_OFFER_STEP_01'.
          ls_idoc_data-hlevel =  '03'.
          ls_idoc_data-psgnum = lv_parent_segnum.
          lv_segnum           += 1.
          ls_idoc_data-segnum = lv_segnum.

          ls_offer_step-discount_quantity = ls_scale-kstbm .
          ls_offer_step-discount_amount   = ls_scale-kbetr .

          ls_idoc_data-sdata = ls_offer_step.
          APPEND ls_idoc_data TO lt_edidd.
          CLEAR : ls_idoc_data.

        ENDLOOP.
*------------------------------- ZPOS_OFFER_STEP -----------------------------------


* Only process 50 records at a time
        IF ( lv_counter MOD 50 ) = 0 .

          lv_docnum += 1.
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
            ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
            ms_textid-msgno = '100'.
            ms_textid-attr1 = ls_edidc-mestyp.
            mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
            RAISE EXCEPTION mx_exception.
          ENDIF.

          CLEAR: ls_idoc_data, lt_edidd, ls_edidc-docnum, ls_offer_header, ls_offer_step, lv_parent_segnum, lv_segnum.

        ENDIF.

      ENDLOOP.

* Pick up the remains not captured in the Modulus check / This could be the last few records for a store.
      IF NOT lt_edidd IS INITIAL.
        lv_docnum += 1.
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
*          MESSAGE i000(zpos) WITH 'IDoc Created' && ls_edidc-docnum.
        ELSE.
          ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ms_textid-msgno = '100'.
          ms_textid-attr1 = ls_edidc-mestyp.
          mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION mx_exception.
        ENDIF.

        CLEAR: ls_idoc_data, lt_edidd, ls_edidc-docnum, ls_offer_header, ls_offer_step, lv_parent_segnum, lv_segnum.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_scale_discount_id.

* Only placeholder for now
    DATA: ls_new_offer TYPE zpos_vk_offer.

    CLEAR ms_message.

* Sharing this table with the BBYs - they are using a common number range
* for offers for Viking
    SELECT SINGLE * FROM zpos_vk_offer
           INTO @DATA(ls_vk_offer)
            WHERE   identifier = @iv_ean11
            AND     category   = @zcl_pos_util=>co_offer_category-scaled_price.

    IF sy-subrc = 0.
      rv_discount_id = ls_vk_offer-vk_offer_id.
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

        rv_discount_id            = ls_new_offer-vk_offer_id.

* Add to table
        ls_new_offer-s4_offer_id = iv_matnr.
        ls_new_offer-identifier   = iv_ean11.
        ls_new_offer-category     = zcl_pos_util=>co_offer_category-scaled_price.
        ls_new_offer-valid_to = iv_valid_to.
        INSERT zpos_vk_offer FROM ls_new_offer.
        COMMIT WORK.
      ELSE.
        RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e020(zpos) WITH 'ZPOS_BBY' '01' 'ZPOS_BBY_NR'.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = rv_discount_id
      IMPORTING
        output = rv_discount_id.

  ENDMETHOD.


  METHOD step_pricing_handler.

    DATA: lv_amount   TYPE rdm_bby_kbetr,
          lv_quantity TYPE rdm_bby_buy_matquan.


    CHECK mo_config_map->is_price_extra_scales_active( ).

* MAXIMUM of 5 STEPS ALLOWED

    rt_steps = it_steps.

    DATA(lv_lines) = lines( rt_steps ).

    READ TABLE it_steps INDEX 1 INTO DATA(ls_step_1).
    READ TABLE it_steps INDEX 2 INTO DATA(ls_step_2).

* Step 3 ( Line 1 + Line 2 )
    APPEND INITIAL LINE TO rt_steps ASSIGNING FIELD-SYMBOL(<ls_steps>).
    lv_quantity = ls_step_1-kstbm + ls_step_2-kstbm  .
    lv_amount   = ls_step_1-kbetr + ls_step_2-kbetr .

    <ls_steps>-kstbm = lv_quantity.
    <ls_steps>-kbetr   = lv_amount.

* Step 4 (Either Line 3 + Line 1, or Line  2 * 2
    IF lv_quantity + ls_step_1-kstbm < ls_step_2-kstbm * 2.

      lv_quantity += ls_step_1-kstbm .
      lv_amount   += ls_step_1-kbetr.

      APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_steps>.
      <ls_steps>-kstbm = lv_quantity.
      <ls_steps>-kbetr   = lv_amount.

    ELSE.

      lv_quantity = ls_step_2-kstbm * 2.
      lv_amount   = ls_step_2-kbetr * 2.

      APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_steps>.
      <ls_steps>-kstbm = lv_quantity.
      <ls_steps>-kbetr   = lv_amount.

    ENDIF.


* Step 5 (Either Line 4 + Line 1, or Line  2 * 3
    IF lv_quantity + ls_step_1-kstbm < ls_step_2-kstbm * 3.

      lv_quantity += ls_step_1-kstbm .
      lv_amount   += ls_step_1-kbetr.

      APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_steps>.
      <ls_steps>-kstbm = lv_quantity.
      <ls_steps>-kbetr   = lv_amount.

    ELSE.

      lv_quantity = ls_step_2-kstbm * 3.
      lv_amount   = ls_step_2-kbetr * 3.

      APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_steps>.
      <ls_steps>-kstbm = lv_quantity.
      <ls_steps>-kbetr   = lv_amount.

    ENDIF.


  ENDMETHOD.
ENDCLASS.

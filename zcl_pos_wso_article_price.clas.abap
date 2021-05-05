class ZCL_POS_WSO_ARTICLE_PRICE definition
  public
  final
  create public

  global friends ZCL_POS_ARTICLE_2_API
                 ZCL_POS_ARTICLE_API .

public section.

  types:
    ty_t_layout TYPE STANDARD TABLE OF zposi_layout_module WITH DEFAULT KEY .
  types:
    ty_t_pre23 TYPE STANDARD TABLE OF pre23 .
  types:
    BEGIN OF ty_s_idoc_map,
        source_value TYPE docnum,
        target_value TYPE docnum,
      END OF ty_s_idoc_map .
  types:
    ty_t_idoc_map TYPE STANDARD TABLE OF ty_s_idoc_map .

  class-methods COPY_ARTICLE_STATUS_DATA
    importing
      !IV_RECIPIENT type ZPOS_RECIPIENT
      !IV_VTWEG type VTWEG
      !IV_VKORG type VKORG
      !IV_WERKS type WERKS_D .
  class-methods CREATE_OUTBOUND_ARTICLE_IDOC
    importing
      !IV_CREATE_BOM type BOOLE_D optional
      !IV_RECORDS type I default 500
      !IV_STATUS type ZPOS_RECIPIENT_STATUS
      !IT_R_RECIPIENT type ZPOS_R_RECIPIENT_TTY
      !IT_R_VTWEG type WRF_VTWEG_RTTY optional
      !IT_R_VKORG type WRF_VKORG_RTTY optional
      !IT_R_WERKS type WRF_WERKS_RTTY optional
      !IV_VALID_ON type DATS
      !IV_LEADTIME type I default 2
      !IV_VKORG type VKORG
      !IT_R_MATNR type WRF_MATNR_RTTY optional
    raising
      ZCX_POS_EXCEPTION .
  class-methods CREATE_PERSIST_ARTICLE_DATA
    importing
      !IT_PACKAGE type WES_T_MERCHANDISE
    raising
      ZCX_POS_EXCEPTION .
  class-methods CREATE_PERSIST_PRICE_DATA
    importing
      !IV_INITIAL_LOAD type BOOLE_D optional
      !IV_DATAB type DATS optional
      !IV_DATBI type DATS optional
      !IT_R_APPL type WES_TR_APPL optional
      !IT_R_RECIPIENT type ZPOS_R_RECIPIENT_TTY optional
      !IV_UPDATE_WIND_STATUS type BOOLE_D default 'X' .
  class-methods DELETE_ARTICLE_PRICE_DATA
    importing
      !IV_STATUS type ZPOS_RECIPIENT_STATUS
      !IT_R_RECIPIENT type ZPOS_R_RECIPIENT_TTY
      !IT_R_VTWEG type WRF_VTWEG_RTTY optional
      !IT_R_VKORG type WRF_VKORG_RTTY optional
      !IT_R_WERKS type WRF_WERKS_RTTY optional
      !IV_OLDER_THAN type DATS .
  class-methods GET_ARTICLE_PRICE
    importing
      !IV_VTWEG type VTWEG
      !IV_VKORG type VKORG
      !IV_WERKS type WERKS_D
      !IV_MATNR type MATNR
      !IV_VALID_TO type DATS default SY-DATUM
      !IV_VALID_FROM type DATS default SY-DATUM
      !IV_EAN11 type EAN11
    returning
      value(RS_PRICELIST) type ZPOS_ARTICLE_PRICE_STY .
  class-methods GET_ARTICLE_PRICES
    importing
      !IT_R_VTWEG type WRF_VTWEG_RTTY
      !IT_R_VKORG type WRF_VKORG_RTTY
      !IT_R_WERKS type WRF_WERKS_RTTY
      !IT_R_MATNR type WRF_MATNR_RTTY
      !IV_VALID_TO type DATS
      !IV_VALID_FROM type DATS
      !IT_R_EAN11 type WART_TR_EAN11
    returning
      value(RT_PRICELIST) type ZPOS_PRICELIST_RFC_TTY .
  methods APPEND_WESOUT_PRICE
    importing
      !IS_PRICE type WES_S_SALES_PRICES_HEAD
      !IV_WERKS type WERKS_D .
  methods CONSTRUCTOR .
  methods CREATE_CONDITION_RECORD
    importing
      !SR_CO type ref to CL_WES_CONST
    changing
      !CT_MESSAGES type BAL_T_MSG
    raising
      ZCX_POS_EXCEPTION .
  methods DETERMINE_EFFECTIVE_PRICE
    changing
      !CS_COND_ART type ITEM_SACO_MAPPING .
  methods NEW_PACKAGE_RESET .
protected section.

  class-methods GET_ARTICLE_PRICELIST
    importing
      !IV_CREATE_BOM type BOOLE_D optional
      !IV_STATUS type ZPOS_RECIPIENT_STATUS default '10'
      !IT_R_RECIPIENT type ZPOS_R_RECIPIENT_TTY optional
      !IT_R_VTWEG type WRF_VTWEG_RTTY optional
      !IT_R_VKORG type WRF_VKORG_RTTY optional
      !IT_R_WERKS type WRF_WERKS_RTTY optional
      !IV_VALID_ON type DATS
      !IV_LEADTIME type I default 2
      !IV_VKORG type VKORG
      !IT_R_MATNR type WRF_MATNR_RTTY optional
    returning
      value(RT_ARTICLE_PRICE) type ZPOS_ARTICLE_PRICE_TTY
    raising
      ZCX_POS_EXCEPTION .
private section.

  types:
    BEGIN OF ty_zwso_price.
        INCLUDE  TYPE a991.
    TYPES:
        kbetr TYPE kbetr_kond,
        konwa TYPE konwa,
        kpein TYPE kpein,
        kmein TYPE kmein,
      END OF ty_zwso_price .
  types:
    BEGIN OF ty_short_konp.
    TYPES:
      knumh TYPE knumh,
      kopos TYPE kopos,
      kschl TYPE kschl,
      kbetr TYPE kbetr_kond,
      konwa TYPE konwa,
      kpein TYPE kpein,
      kmein TYPE kmein,
      aktnr TYPE waktion,
      END OF ty_short_konp .
  types:
    BEGIN OF ty_short_wakp.
    TYPES:
      aktnr        TYPE waktion,
      offer_id     TYPE wpm_offerid_price,
      offer_id_dsc TYPE wpm_offerid_discount,
      END OF ty_short_wakp .
  types:
    BEGIN OF ty_vkpo_gpa1_map.
    TYPES:
      vkpo_knumh TYPE knumh,
      gpa1_knumh TYPE knumh,
      END OF ty_vkpo_gpa1_map .

*    CONSTANTS co_application TYPE kappl VALUE 'V' ##NO_TEXT.
*    CONSTANTS co_competitor_vkorg_price TYPE kscha VALUE 'ZKC0' ##NO_TEXT.
*    CONSTANTS co_competitor_werks_price TYPE kscha VALUE 'ZKC1' ##NO_TEXT.
*    CONSTANTS co_condition_table_991 TYPE kotabnr VALUE '991' ##NO_TEXT.
*    CONSTANTS zcl_pos_util=>co_english_language TYPE spras VALUE 'E' ##NO_TEXT.
*    CONSTANTS co_detailed_language TYPE spras VALUE 'Z' ##NO_TEXT.
*    CONSTANTS zcl_pos_util=>co_condition_type-discount_amount TYPE kscha VALUE 'GPA1' ##NO_TEXT.
*    CONSTANTS co_effective_comp_price TYPE kscha VALUE 'ZCP1' ##NO_TEXT.
*    CONSTANTS zcl_pos_util=>co_condition_type-effective_price TYPE kscha VALUE 'ZWSO' ##NO_TEXT.
*    CONSTANTS co_kappl TYPE kappl VALUE 'V' ##NO_TEXT.
*    CONSTANTS co_local_price TYPE kscha VALUE 'ZKP1' ##NO_TEXT.
*    CONSTANTS co_promotion_price TYPE kscha VALUE 'VKA0' ##NO_TEXT.
*    CONSTANTS co_standard_comp_price TYPE kscha VALUE 'ZCP0' ##NO_TEXT.
*    CONSTANTS zcl_pos_util=>co_condition_type-standard_price TYPE kscha VALUE 'VKP0' ##NO_TEXT.
  data CO_USAGE type KVEWE value 'A' ##NO_TEXT.
  data MT_COND_ART type TT_ITEM_SACO_MAPPING .
  data:
    mt_vkp0_gpa1_map TYPE STANDARD TABLE OF ty_vkpo_gpa1_map .
  data MT_WESOUT_PRICE type ZPOS_WES_T_SALES_PRICES_HEAD .
  class-data SR_CO type ref to CL_WES_CONST .
  class-data MX_EXCEPTION type ref to ZCX_POS_EXCEPTION .
  class-data MS_TEXTID type SCX_T100KEY .
  constants CO_DELETE type STRING value 'DELETE' ##NO_TEXT.
  class-data MT_ECOM_STATUS type ZECM_ARTICLE_STS_TTY .
  class-data MO_CONFIG_MAP type ref to ZCL_POS_CONFIG_TAB_HANLDER .

  class-methods CALCULATE_NET_PRICE
    importing
      !IV_INPUT_PRICE type KBETR
      !IV_TAX_RATE type KBETR
    returning
      value(RV_NET_PRICE) type KBETR .
  class-methods CREATE_PERSIST_LABEL_DATA
    importing
      !IT_ARTICLE type ZPOS_ARTICLE_TTY .
  class-methods CREATE_STATUS_RECORDS
    importing
      !IT_ARTICLE_PRICE type ZPOS_ARTICLE_PRICE_TTY
      !IT_IDOC_MAP type TY_T_IDOC_MAP optional
      !IV_STATUS type ZPOS_RECIPIENT_STATUS
    returning
      value(RV_COUNT) type SYST_DBCNT
    raising
      ZCX_POS_EXCEPTION .
  class-methods GET_VIKING_TAX_CODE
    changing
      !CV_TAX_CODE type MWSKZ .
  class-methods MAGENTO_PRICING_CHECK
    changing
      value(CT_ARTICLE_PRICE) type ZPOS_ARTICLE_PRICE_TTY .
  class-methods READ_LAYOUT_MODULE
    importing
      !IT_R_MATNR type WRF_MATNR_RTTY
      !IT_R_WERKS type TDT_RG_KUNNR
    returning
      value(RT_LAYOUT) type TY_T_LAYOUT .
  class-methods READ_MARA_DATA
    changing
      !CT_MATNR type WRF_REF_PRE03_TTY
    returning
      value(RT_MARA) type WRF_MARA_TTY .
  class-methods READ_MARC_DATA
    changing
      !CT_MATNR_WERKS type WRF_REF_PRE01_TTY
    returning
      value(RT_MARC) type WRF_REF_MARC_TTY .
  class-methods READ_MAW1_DATA
    changing
      !CT_MATNR type WRF_REF_PRE03_TTY
    returning
      value(RT_MAW1) type WRF_MAW1_TTY .
  class-methods READ_MVKE_DATA
    changing
      !CT_VKORG_VTWEG_MATNR type PRE10_TAB
    returning
      value(RT_MVKE) type MVKE_TT .
  class-methods READ_WIND_POINTER
    importing
      !IT_R_APPL type WES_TR_APPL
    returning
      value(RT_WIND) type TYP_WINDVB .
  class-methods READ_WLK2_DATA
    changing
      !CT_MATNR_VKORG_VTWEG_WERKS type TY_T_PRE23
    returning
      value(RT_WLK2) type WLK2_TABLE .
  class-methods SET_STATUS_WIND_POINTER
    importing
      !IT_WIND_POINTER_ID type WES_T_WIND_ID
      !IT_R_APPL type WES_TR_APPL .
  methods EFFECTIVE_PRICE_PER_DATE
    changing
      !CT_SACO_PRICE type TT_SACO .
  methods GET_PROMOTION_ID
    importing
      !IV_VKORG type VKORG
      !IV_VTWEG type VTWEG
      !IV_WERKS type WERKS_D
      !IV_MATNR type MATNR
      !IV_VRKME type VRKME
    returning
      value(RV_PROMOID) type WAKTION .
  methods MAP_PERSIST_DATA
    returning
      value(RT_PRICES) type ZPOS_PRICELIST_TTY .
  methods MAP_PERSIST_DATA_PER_DATE
    importing
      !IS_SACO_PRICE type ITEM_SACO_MAPPING
    returning
      value(RT_PRICES) type ZPOS_PRICELIST_TTY .
ENDCLASS.



CLASS ZCL_POS_WSO_ARTICLE_PRICE IMPLEMENTATION.


  METHOD set_status_wind_pointer.

    CHECK it_wind_pointer_id IS NOT INITIAL.

    IF sr_co IS INITIAL.
      sr_co = cl_wes_const=>sr_co.
    ENDIF.

*==========================================================
* Taken From CALL FUNCTION 'WES_WIND_STATUS_SET'
*==========================================================

    SELECT * FROM wesd_winds
           INTO TABLE @DATA(lt_winds)
           FOR ALL ENTRIES IN @it_wind_pointer_id
           WHERE appl  IN @it_r_appl
           AND   bltyp = @sr_co->document_category_merch
           AND   knumh = @it_wind_pointer_id-knumh
           AND   kopos = @it_wind_pointer_id-kopos.

    LOOP AT lt_winds ASSIGNING FIELD-SYMBOL(<ls_winds>).
      <ls_winds>-processed = sr_co->x.
    ENDLOOP.

    MODIFY wesd_winds FROM TABLE lt_winds.

  ENDMETHOD.


  METHOD read_wlk2_data.

    CALL FUNCTION 'WLK2_ARRAY_READ'
      TABLES
        ipre23   = ct_matnr_vkorg_vtweg_werks
        wlk2_tab = rt_wlk2.

* We want to keep the plant specific values if they have been maintained - otherwise the distribution chain values.
    SORT rt_wlk2 BY matnr vkorg vtweg ASCENDING werks DESCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_wlk2 COMPARING matnr vkorg vtweg.

    SORT rt_wlk2 BY matnr vkorg vtweg werks.

  ENDMETHOD.


  METHOD read_wind_pointer.
*
    DATA: lt_wesd_winds TYPE TABLE OF wesd_winds.

    IF sr_co IS INITIAL.
      sr_co = cl_wes_const=>sr_co.
    ENDIF.

*--------------------------------------------------------------
* Taken From *      CALL FUNCTION 'WES_WIND_READ'
*--------------------------------------------------------------

    SELECT * FROM wesd_winds
           INTO TABLE lt_wesd_winds
           WHERE appl      IN it_r_appl
           AND   bltyp     = sr_co->document_category_merch
           AND   processed = abap_false.

    IF sy-subrc = 0.
      SORT lt_wesd_winds BY knumh.
      DELETE ADJACENT DUPLICATES FROM lt_wesd_winds.

      IF lt_wesd_winds[] IS INITIAL.
        EXIT.
      ENDIF.

*--------------------------------------------------------------
* Taken From *      CALL FUNCTION 'WES_WIND_READ_RFC'
*--------------------------------------------------------------

      SELECT * FROM wind
             INTO TABLE rt_wind
             FOR ALL ENTRIES IN lt_wesd_winds
             WHERE bltyp   = sr_co->document_category_merch
             AND   kschl   = zcl_pos_util=>co_condition_type-effective_price             " This was added for the effective price records only
             AND   knumh   = lt_wesd_winds-knumh.
*--------------------------------------------------------------

      SORT rt_wind BY knumh kopos.
      DELETE ADJACENT DUPLICATES FROM rt_wind COMPARING knumh.

      SORT rt_wind BY cretime acttime.

    ENDIF. " sy-subrc = 0.

  ENDMETHOD.


  METHOD read_mvke_data.

    CALL FUNCTION 'MVKE_ARRAY_READ'
      TABLES
        ipre10   = ct_vkorg_vtweg_matnr
        mvke_tab = rt_mvke.

    SORT rt_mvke BY matnr vkorg vtweg .

  ENDMETHOD.


  METHOD read_maw1_data.

    CALL FUNCTION 'MAW1_ARRAY_READ'
*      EXPORTING
*       KZRFB    = ' '
*       NEUFLAG  = ' '
      TABLES
        ipre03   = ct_matnr
        maw1_tab = rt_maw1.

    SORT rt_maw1 BY matnr.

  ENDMETHOD.


  METHOD read_marc_data.

    CALL FUNCTION 'MARC_ARRAY_READ'
      TABLES
        ipre01               = ct_matnr_werks
        marc_tab             = rt_marc
      EXCEPTIONS ##FM_SUBRC_OK
        enqueue_mode_changed = 1
        lock_on_marc         = 2
        lock_system_error    = 3
        OTHERS               = 4.

    SORT rt_marc BY matnr werks.

  ENDMETHOD.


  METHOD read_mara_data.

    CALL FUNCTION 'MARA_ARRAY_READ'
      TABLES
        ipre03               = ct_matnr
        mara_tab             = rt_mara
      EXCEPTIONS ##FM_SUBRC_OK
        enqueue_mode_changed = 1
        OTHERS               = 2.

  ENDMETHOD.


  METHOD read_layout_module.

    SELECT * FROM zposi_layout_module
             INTO TABLE @rt_layout
             WHERE article IN @it_r_matnr
             AND  location IN @it_r_werks
             AND  validfrom LE @sy-datum
             AND  validto GE @sy-datum
             AND  source = 'L'        "May need to change in future waves
             AND ( status = ' '
             OR    status = '1'  ).

    SORT rt_layout BY article location ASCENDING validfrom DESCENDING.

  ENDMETHOD.


  METHOD new_package_reset.

    CLEAR: mt_cond_art, mt_vkp0_gpa1_map, mt_wesout_price.

  ENDMETHOD.


  METHOD map_persist_data_per_date.

* Sort for binary search
    SORT mt_wesout_price BY matnr kmein vkorg vtweg werks.
    SORT mt_vkp0_gpa1_map BY vkpo_knumh.

    LOOP AT is_saco_price-saco_table INTO DATA(ls_item_price).

      IF ls_item_price-kschl = zcl_pos_util=>co_condition_type-standard_price.
        APPEND INITIAL LINE TO rt_prices ASSIGNING FIELD-SYMBOL(<ls_output_price>).
        <ls_output_price>-std_price = ls_item_price-kbetr.
        <ls_output_price>-std_price_fkey = ls_item_price-knumh.
      ELSE.
* Effective Price

        IF NOT <ls_output_price> IS ASSIGNED.
* This could only happen if there is no Standard Price
          APPEND INITIAL LINE TO rt_prices ASSIGNING <ls_output_price>.
        ENDIF.

        <ls_output_price>-vkorg           = is_saco_price-vkorg.
        <ls_output_price>-vtweg           = is_saco_price-vtweg.
        <ls_output_price>-werks           = is_saco_price-werks.
        <ls_output_price>-matnr           = is_saco_price-matnr.


* Some date complexity - We can normally use the condition record DATAB_KOND for things which are valid today, but for future prices (tomorrow) we need to use the WESOUT D
        IF ls_item_price-datab = sy-datum AND NOT ls_item_price-datab_kond IS INITIAL.
          <ls_output_price>-datab = ls_item_price-datab_kond.
        ELSE.
          <ls_output_price>-datab = ls_item_price-datab.
        ENDIF.


        <ls_output_price>-datbi           = ls_item_price-datbi.
        <ls_output_price>-eff_price       = ls_item_price-kbetr.

        <ls_output_price>-eff_price_type  = ls_item_price-lifnr.          " We use this to temporarily store the origin condition type


        CASE ls_item_price-kotabnr.
          WHEN '071' OR '803'.
            <ls_output_price>-eff_price_level = 'S'.          " Store Level Pricing
          WHEN '073' OR '801'.
            <ls_output_price>-eff_price_level = 'C'.          " Country Level Pricing
        ENDCASE.


* In this situation the correct condition record could come from the GPA1 mapping table
        IF <ls_output_price>-eff_price_type = zcl_pos_util=>co_condition_type-discount_amount.
          READ TABLE mt_vkp0_gpa1_map WITH KEY vkpo_knumh = ls_item_price-knumh INTO DATA(ls_vkp0_gpa1_map) BINARY SEARCH.
          IF sy-subrc = 0.
            <ls_output_price>-eff_price_fkey  = ls_vkp0_gpa1_map-gpa1_knumh.
          ELSE.
            <ls_output_price>-eff_price_fkey  = ls_item_price-knumh.
          ENDIF.

        ELSE.
          <ls_output_price>-eff_price_fkey  = ls_item_price-knumh.
        ENDIF.


        <ls_output_price>-vrkme           = is_saco_price-vrkme.
        <ls_output_price>-currency        = ls_item_price-waers.


        READ TABLE mt_wesout_price INTO DATA(ls_wesout_head) WITH KEY
                                matnr = is_saco_price-matnr
                                kmein = is_saco_price-vrkme
                                vkorg = is_saco_price-vkorg
                                vtweg = is_saco_price-vtweg
                                werks = is_saco_price-werks BINARY SEARCH.

        IF sy-subrc = 0.
          <ls_output_price>-ean11 = ls_wesout_head-ean11.
        ENDIF.

        UNASSIGN <ls_output_price>.

      ENDIF.                " Standard or Effective Price Loop

    ENDLOOP.

  ENDMETHOD.


  METHOD map_persist_data.

* This mapping does not really make much sense anymore
* It was mapped to conform to the persistent data table structure
* As the original plan was to save the data, but now that is not performed
* It could be more elegnat to go from MT_COND_ART to the IDoc Format

    DATA: lv_datbi TYPE dats.
    CHECK mt_cond_art IS NOT INITIAL.


    LOOP AT mt_cond_art INTO DATA(ls_head_price).

      SORT ls_head_price-saco_table BY datab ASCENDING.
      READ TABLE ls_head_price-saco_table INDEX 1 INTO DATA(ls_low_date).
      READ TABLE ls_head_price-saco_table INDEX lines( ls_head_price-saco_table ) INTO DATA(ls_high_date).

      WHILE ls_low_date-datab <= ls_high_date-datab.

        CLEAR lv_datbi.
        DATA(ls_input_price) = ls_head_price.

* This will be handled by the condition record (hopefully)
**-------------------------------------------------------------------------------------
** We can have overlapping time periods in the ZWSO conditions
**-------------------------------------------------------------------------------------
**     KSCHL   DATAB         DATBI
**-------------------------------------------------------------------------------------
**     ZWSO    20201216      99991231
**     ZWSO    20201217      20201231
** We need to truncate the DATBI to be one day less than the DATAB date of the following record
**-------------------------------------------------------------------------------------
*        SORT ls_input_price-saco_table BY kschl datab DESCENDING.
*        LOOP AT ls_input_price-saco_table ASSIGNING FIELD-SYMBOL(<ls_saco_table>) WHERE kschl = zcl_pos_util=>co_condition_type-effective_price.
*
*          IF NOT <ls_saco_table>-datbi < lv_datbi AND NOT lv_datbi IS INITIAL.
*            <ls_saco_table>-datbi = lv_datbi - 1.
*          ENDIF.
*          lv_datbi = <ls_saco_table>-datab.
*        ENDLOOP.
*
**-------------------------------------------------------------------------------------

        DELETE ls_input_price-saco_table WHERE datab GT ls_low_date-datab OR datbi LT ls_low_date-datab.
        ls_low_date-datab += 1.

        SORT ls_input_price-saco_table BY kschl datab.
        DATA(lt_prices) = map_persist_data_per_date( EXPORTING is_saco_price = ls_input_price ).
        APPEND LINES OF lt_prices TO rt_prices.

      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD magento_pricing_check.

* Checking whether the article has already been sent to magento
* before sending the pricing - this check can be deactivated
* in table ZPOS_CONFIG_MAP

    mo_config_map->is_magento_price_check_active( ).

    LOOP AT ct_article_price ASSIGNING FIELD-SYMBOL(<ls_article_price>)
                 WHERE pos_recipient_status = zcl_pos_util=>co_pos_recipient-magento_idoc.

      READ TABLE mt_ecom_status
                 INTO DATA(ls_ecom_status)
                 WITH KEY vkorg = <ls_article_price>-vkorg
                          vtweg = <ls_article_price>-vtweg
                          werks = <ls_article_price>-werks
                          matnr = <ls_article_price>-matnr
                          BINARY SEARCH.

      IF sy-subrc = 0.

        IF cl_rest_status_code=>is_success( CONV #( ls_ecom_status-http_status ) ) = abap_true.
*        Do Nothing
        ELSE.
          <ls_article_price>-matnr = co_delete.        " Delete all of those that do not already have a successful HTTP Status
        ENDIF.

      ENDIF.

    ENDLOOP.
    DELETE ct_article_price WHERE matnr = co_delete.

  ENDMETHOD.


  METHOD get_viking_tax_code.
  ENDMETHOD.


  METHOD get_promotion_id.

    READ TABLE mt_cond_art WITH KEY
                               vkorg = iv_vkorg
                               vtweg = iv_vtweg
                               werks = iv_werks
                               matnr = iv_matnr
                               vrkme = iv_vrkme BINARY SEARCH INTO DATA(ls_cond_art).

    IF sy-subrc = 0.
      READ TABLE ls_cond_art-saco_table WITH KEY kschl = zcl_pos_util=>co_condition_type-effective_price INTO DATA(ls_saco).
      IF sy-subrc = 0.
        rv_promoid = ls_saco-aktnr.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_article_prices.

    DATA: lt_r_knumh    TYPE RANGE OF knumh,
          lt_eff_scales TYPE STANDARD TABLE OF konm,
          lt_std_scales TYPE STANDARD TABLE OF konm.


    SELECT * FROM zpos_pricelist
      INTO TABLE @DATA(lt_pricelist)
      WHERE vkorg IN @it_r_vkorg
      AND vtweg IN @it_r_vtweg
      AND werks IN @it_r_werks
      AND matnr IN @it_r_matnr
      AND ean11 IN @it_r_ean11
      AND datab LE @iv_valid_from
      AND datbi GE @iv_valid_to.

*====================================================================================================================
* Effective Price Read Scales Information
*====================================================================================================================
    SELECT * FROM konm INTO TABLE lt_eff_scales
                       FOR ALL ENTRIES IN lt_pricelist
                       WHERE knumh EQ lt_pricelist-eff_price_fkey.
*====================================================================================================================
* Standard Price Read Scales Information
*====================================================================================================================
    SELECT * FROM konm INTO TABLE lt_std_scales
                       FOR ALL ENTRIES IN lt_pricelist
                       WHERE knumh EQ lt_pricelist-std_price_fkey.



    LOOP AT lt_pricelist INTO DATA(ls_pricelist_row).

      APPEND INITIAL LINE TO rt_pricelist ASSIGNING FIELD-SYMBOL(<ls_pricelist>).
      MOVE-CORRESPONDING ls_pricelist_row TO <ls_pricelist>.

      LOOP AT lt_eff_scales INTO DATA(ls_scale) WHERE knumh = ls_pricelist_row-eff_price_fkey .
        APPEND INITIAL LINE TO <ls_pricelist>-eff_scale ASSIGNING FIELD-SYMBOL(<ls_scale>).
        MOVE-CORRESPONDING ls_scale TO <ls_scale>.
      ENDLOOP.
      SORT <ls_pricelist>-eff_scale BY klfn1 ASCENDING.

      LOOP AT lt_std_scales INTO ls_scale WHERE knumh = ls_pricelist_row-std_price_fkey .
        APPEND INITIAL LINE TO <ls_pricelist>-std_scale ASSIGNING <ls_scale>.
        MOVE-CORRESPONDING ls_scale TO <ls_scale>.
      ENDLOOP.
      SORT <ls_pricelist>-std_scale BY klfn1 ASCENDING.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_article_pricelist.

    DATA: lt_r_knumh       TYPE RANGE OF knumh,
          lt_scale         TYPE SORTED TABLE OF konm WITH UNIQUE KEY knumh kopos klfn1,
          lv_leadtime_date TYPE dats,
          lr_config_map    TYPE REF TO zcl_pos_config_tab_hanlder.

    IF lr_config_map IS INITIAL.
      lr_config_map = NEW #( iv_vkorg = iv_vkorg ).
    ENDIF.

* The main purpose is to join the key tables together and calculate some columns

    lv_leadtime_date = iv_valid_on + iv_leadtime - 1.         " Two means Today and Tomorrow


    SELECT * FROM zposi_article_price_status
       INTO CORRESPONDING FIELDS OF TABLE @rt_article_price
       WHERE vkorg IN @it_r_vkorg
       AND   vtweg IN @it_r_vtweg
       AND   werks IN @it_r_werks
       AND   matnr IN @it_r_matnr
       AND   pos_recipient IN @it_r_recipient
       AND   datab <= @lv_leadtime_date              " With a Lead Time of 2 - we take away 1 and that means valid on or before Tomorrow
       AND   datbi >= @iv_valid_on.                  " Here we do not use the Lead time as we want prices which are valid today but might actually expire tomorrow



    SORT rt_article_price BY vkorg vtweg werks matnr ean11 vrkme datbi pos_recipient ASCENDING timestamp DESCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_article_price COMPARING vkorg vtweg werks matnr ean11 vrkme datbi pos_recipient.           " Comparing without Status so we get the highest status code
    DELETE rt_article_price WHERE pos_recipient_status > iv_status.                                                               " Keeps only those equal or less than the specified status code

    CHECK rt_article_price IS NOT INITIAL.

    IF iv_create_bom EQ abap_true.

      SELECT * FROM zpos_article_bom
               INTO TABLE @DATA(lt_article_bom)
               FOR ALL ENTRIES IN @rt_article_price
               WHERE vkorg = @rt_article_price-vkorg
               AND   vtweg = @rt_article_price-vtweg
               AND   werks = @rt_article_price-werks
               AND   matnr = @rt_article_price-matnr
               AND   ean11 = @rt_article_price-ean11
               AND   vrkme = @rt_article_price-vrkme.

      SORT lt_article_bom BY vkorg vtweg werks matnr ean11 vrkme.     "Prepare for Binary Search
    ENDIF.



*====================================================================================================================
* The main purpose of the following section is to calculate certain simple colums (calcuated columns which do not need to be persisted
*====================================================================================================================


*====================================================================================================================
* Read Scales Information
*====================================================================================================================
    SELECT * FROM konm INTO TABLE lt_scale
      FOR ALL ENTRIES IN rt_article_price
      WHERE knumh EQ rt_article_price-eff_price_fkey.

*====================================================================================================================
* Perform the calculations
*====================================================================================================================
    LOOP AT rt_article_price ASSIGNING FIELD-SYMBOL(<ls_article_price>).

*=====================================
* Tax Code Mapping for ViKING
*=====================================
      IF <ls_article_price>-pos_recipient = zcl_pos_util=>co_pos_recipient-viking .
        <ls_article_price>-tax_code = lr_config_map->get_vk_tax_code( EXPORTING iv_source2 = CONV #( <ls_article_price>-tax_code ) ).
      ENDIF.


*=====================================
* Effective Pricing --> Net Price
*=====================================
      <ls_article_price>-eff_net_price = calculate_net_price( iv_input_price = <ls_article_price>-eff_price iv_tax_rate = <ls_article_price>-tax_rate ) .

      IF NOT <ls_article_price>-comp_qty IS INITIAL AND NOT <ls_article_price>-net_contents IS INITIAL.
*=====================================
* Effective Pricing --> Comparative Price
*=====================================
        <ls_article_price>-eff_comp_price = ( <ls_article_price>-eff_price * <ls_article_price>-comp_qty ) / <ls_article_price>-net_contents .
*=====================================
* Effective Pricing --> Comparative Net Price
*=====================================
        <ls_article_price>-eff_comp_net_price = calculate_net_price( iv_input_price = <ls_article_price>-eff_comp_price iv_tax_rate = <ls_article_price>-tax_rate ) .
      ENDIF.



*=====================================
* Standard Pricing --> Net Price
*=====================================
      <ls_article_price>-std_net_price = calculate_net_price( iv_input_price = <ls_article_price>-std_price iv_tax_rate = <ls_article_price>-tax_rate ) .

      IF NOT <ls_article_price>-comp_qty IS INITIAL AND NOT <ls_article_price>-net_contents IS INITIAL.
*=====================================
* Standard Pricing --> Comparative Price
*=====================================
        <ls_article_price>-std_comp_price = ( <ls_article_price>-std_price * <ls_article_price>-comp_qty  ) / <ls_article_price>-net_contents .
*=====================================
* Standard Pricing --> Comparative Net Price
*=====================================
        <ls_article_price>-std_comp_net_price = calculate_net_price( iv_input_price = <ls_article_price>-std_comp_price iv_tax_rate = <ls_article_price>-tax_rate )  .
      ENDIF.


*=====================================
* Scale Pricing --> Adds Only the First Real Level of the Scale
*=====================================
      DATA(lt_temp_scale) = FILTER #( lt_scale WHERE knumh EQ <ls_article_price>-eff_price_fkey ).
      READ TABLE lt_temp_scale INDEX 2 INTO DATA(ls_temp_scale).
      IF sy-subrc = 0.
        <ls_article_price>-scale_qty = ls_temp_scale-kstbm.
        <ls_article_price>-scale_price = ls_temp_scale-kbetr.
      ENDIF.

      lt_temp_scale = FILTER #( lt_scale WHERE knumh EQ <ls_article_price>-eff_price_fkey ).
      DELETE lt_temp_scale INDEX 1.     " We don't want the first scale for quantity 0 as this is the standard non-scale price
      IF NOT lt_temp_scale IS INITIAL.

* Performed in the separate ZCL_POS_SCALED_PRICING class
*        <ls_article_price>-scale_id = get_scale_discount_id( iv_ean11 = <ls_article_price>-ean11 iv_matnr = <ls_article_price>-matnr iv_valid_to = <ls_article_price>-datbi ).

        <ls_article_price>-eff_scale = CORRESPONDING #( lt_temp_scale ).

* Maximum 5 Levels allowed in Viking - Will assume that no other consumer wants more than 5 either
        IF lines( <ls_article_price>-eff_scale ) > 5.
          DELETE <ls_article_price>-eff_scale FROM 6.
        ENDIF.
      ENDIF.


      IF lt_article_bom[] IS NOT INITIAL.

        READ TABLE lt_article_bom WITH KEY
               vkorg = <ls_article_price>-vkorg
               vtweg = <ls_article_price>-vtweg
               werks = <ls_article_price>-werks
               matnr = <ls_article_price>-matnr
               ean11 = <ls_article_price>-ean11
               vrkme = <ls_article_price>-vrkme
               BINARY SEARCH TRANSPORTING NO FIELDS.

        IF sy-subrc = 0.
* Performance Optimization
          DATA(lv_tabix) = sy-tabix.

          LOOP AT lt_article_bom INTO DATA(ls_article_bom) FROM lv_tabix.

            IF ls_article_bom-vkorg <> <ls_article_price>-vkorg OR
               ls_article_bom-vtweg <> <ls_article_price>-vtweg OR
               ls_article_bom-werks <> <ls_article_price>-werks OR
               ls_article_bom-matnr <> <ls_article_price>-matnr OR
               ls_article_bom-ean11 <> <ls_article_price>-ean11 OR
               ls_article_bom-vrkme <> <ls_article_price>-vrkme.
              EXIT.
            ENDIF.

            APPEND INITIAL LINE TO <ls_article_price>-bom ASSIGNING FIELD-SYMBOL(<ls_bom>).

            MOVE-CORRESPONDING ls_article_bom TO <ls_bom>.

          ENDLOOP.


        ENDIF.


      ENDIF.


    ENDLOOP.

  ENDMETHOD.


  METHOD effective_price_per_date.

    DATA:
      ls_lowest_price   TYPE saco,
      ls_standard_price TYPE saco,
      ls_discount_price TYPE saco,
      lt_discount       TYPE tt_saco.

    FIELD-SYMBOLS: <ls_lowest_price>  TYPE saco.

    CLEAR: ls_standard_price, ls_lowest_price, lt_discount.

* Special Processing of Discounts
    lt_discount[] = ct_saco_price[].

    DELETE lt_discount WHERE kschl NE zcl_pos_util=>co_condition_type-discount_amount.
    SORT lt_discount BY kotabnr ASCENDING.         " This will give us the VKORG discount before the Store Discount (although mathematically it does not matter so much).
    DELETE ct_saco_price WHERE kschl EQ zcl_pos_util=>co_condition_type-discount_amount.


    READ TABLE ct_saco_price INTO ls_standard_price WITH KEY kschl = zcl_pos_util=>co_condition_type-standard_price.
    SORT ct_saco_price BY kbetr ASCENDING.
    READ TABLE ct_saco_price INDEX 1 INTO ls_lowest_price.


* Calculate the Standard Price minus the Discount
    IF sy-subrc = 0.
      ls_discount_price = ls_standard_price.
* The solution is not really built to support multiple discounts which are active simultaneously - OfferID problems!
      LOOP AT lt_discount INTO DATA(ls_discount).
        ls_discount_price-kbetr = ls_discount_price-kbetr * ( 1 - abs( ls_discount-kbetr / 1000 ) ) .

* Persist Data for Use Later - When Generating the Condition Record as we want to use the GPA record
*        ls_discount_price-knumh = ls_discount-knumh.     " Don't do this or we will lose the scales information

        APPEND INITIAL LINE TO mt_vkp0_gpa1_map ASSIGNING FIELD-SYMBOL(<ls_vkp0_gpa1_map>).
        <ls_vkp0_gpa1_map>-vkpo_knumh = ls_standard_price-knumh.
        <ls_vkp0_gpa1_map>-gpa1_knumh = ls_discount-knumh.
*         This can result in multiple records for the same VKP0 record

        ls_discount_price-kschl = ls_discount-kschl.
        ls_discount_price-aktnr = ls_discount-aktnr.
      ENDLOOP.
    ENDIF.

    IF ls_lowest_price-kbetr >= ls_discount_price-kbetr.
* The discounted VKPO price is now the lowest price and should be used
      ls_lowest_price = ls_discount_price.
    ENDIF.


    IF NOT ls_lowest_price IS INITIAL.
* ---------------------------------------------------------- This is really strange if it is not assigned!

      UNASSIGN <ls_lowest_price>.
      IF ls_lowest_price-kschl EQ zcl_pos_util=>co_condition_type-standard_price OR
         ls_lowest_price-kschl EQ zcl_pos_util=>co_condition_type-discount_amount.
* ----------------------------------------------------------  Then we need to insert a ZWSO record - As we want to have both a standard and lowest price in WESOUT
        APPEND ls_lowest_price TO ct_saco_price ASSIGNING <ls_lowest_price>.
      ELSE.
        READ TABLE ct_saco_price WITH KEY kschl = ls_lowest_price-kschl ASSIGNING <ls_lowest_price>.
      ENDIF.
      IF <ls_lowest_price> IS ASSIGNED.
        <ls_lowest_price>-lifnr = <ls_lowest_price>-kschl.      " Save the Original Input Condition Type in a "spare" field .... ugly!
        <ls_lowest_price>-kschl = zcl_pos_util=>co_condition_type-effective_price.           " Then we change it to ZWSO
      ENDIF.

    ENDIF.

* Just added "AND kschl NE zcl_pos_util=>co_condition_type-discount_amount" without testing ! Need to check later that this was correct
    DELETE ct_saco_price WHERE kschl NE zcl_pos_util=>co_condition_type-standard_price AND kschl NE zcl_pos_util=>co_condition_type-discount_amount AND kschl NE zcl_pos_util=>co_condition_type-effective_price.

  ENDMETHOD.


  METHOD determine_effective_price.

    DATA:
      lt_input_price  TYPE tt_saco,
      lt_output_price TYPE tt_saco.


* ----------------------------------------------------------
*     Step 1: Check if there are multiple dates
* ----------------------------------------------------------
    SORT cs_cond_art-saco_table BY datab ASCENDING.
    READ TABLE cs_cond_art-saco_table INDEX 1 INTO DATA(ls_low_date).
    READ TABLE cs_cond_art-saco_table INDEX lines( cs_cond_art-saco_table ) INTO DATA(ls_high_date).


* ----------------------------------------------------------
*     Step 2: Process each date in the Lead Time Range e.g. 2 or 3 days - and choose the lowest price
* ----------------------------------------------------------
    WHILE ls_low_date-datab <= ls_high_date-datab.

      lt_input_price[] = cs_cond_art-saco_table[].

      DELETE lt_input_price WHERE datab GT ls_low_date-datab OR datbi LT ls_low_date-datab.
      ls_low_date-datab += 1.

      effective_price_per_date( CHANGING ct_saco_price = lt_input_price ).
      APPEND LINES OF lt_input_price TO lt_output_price.

    ENDWHILE.


*     Step 3: The record chosen for each day might be the same for all days, so only create the number of records really needed
    SORT lt_output_price BY kschl datab datbi datab_kond.
    DELETE ADJACENT DUPLICATES FROM lt_output_price COMPARING kschl datab datbi datab_kond.

    CLEAR cs_cond_art-saco_table.
    APPEND LINES OF lt_output_price TO cs_cond_art-saco_table.

* Accumulate the Records to create the persistent records later in Method - Create_Persit_Data
    APPEND cs_cond_art TO mt_cond_art.

  ENDMETHOD.


  METHOD delete_article_price_data.

    DATA: lv_timestamp TYPE hdb_timestamp.

* Delete from Status if older than specified date as long as the status has been reached

    lv_timestamp = iv_older_than && '000000'.

    SELECT
       t1~mandt,
       t1~vkorg,
       t1~vtweg,
       t1~werks,
       t1~matnr,
       t1~ean11,
       t1~vrkme,
       t1~datbi,
       t1~pos_recipient,
       t1~timestamp,
       t1~pos_recipient_status,
       ' ' AS delete
       FROM zpos_artprc_sts AS t1
       INTO TABLE @DATA(lt_status)
       WHERE t1~vkorg IN @it_r_vkorg
       AND t1~vtweg IN @it_r_vtweg
       AND t1~werks IN @it_r_werks
       AND t1~pos_recipient IN @it_r_recipient
      ORDER BY vkorg, vtweg, werks, matnr, ean11, vrkme, datbi, pos_recipient ASCENDING, timestamp DESCENDING.       "TimeStamp Descending to keep most recent

    DATA(lt_single_status) = lt_status.
    DELETE ADJACENT DUPLICATES FROM lt_single_status COMPARING vkorg vtweg werks matnr ean11 vrkme datbi pos_recipient.  " We should have 1 record left with the highest Timestamp regardless of status

    DELETE lt_single_status WHERE pos_recipient_status <= iv_status AND timestamp <= lv_timestamp.


    LOOP AT lt_single_status INTO DATA(ls_single_status).

      LOOP AT lt_status ASSIGNING FIELD-SYMBOL(<ls_status>) WHERE
                           vkorg = ls_single_status-vkorg AND
                           vtweg = ls_single_status-vtweg AND
                           werks = ls_single_status-werks AND
                           matnr = ls_single_status-matnr AND
                           ean11 = ls_single_status-ean11 AND
                           vrkme = ls_single_status-vrkme AND
                           timestamp <= ls_single_status-timestamp.

        IF <ls_status> IS ASSIGNED.
          <ls_status>-delete = abap_true.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    DELETE lt_status WHERE delete = abap_false.

    IF NOT lt_status IS INITIAL.
      DELETE zpos_artprc_sts FROM TABLE lt_status.
      MESSAGE i000(zpos) WITH sy-dbcnt ' entries deleted from table ZPOS_ARTPRC_STS'.

      DELETE lt_status WHERE datbi > sy-datum.        " We don't want to delete prices which are still valid"
      DELETE zpos_pricelist FROM TABLE lt_status.
      MESSAGE i000(zpos) WITH sy-dbcnt ' entries deleted from table ZPOS_PRICELIST'.
    ENDIF.



* This selects the Article Data which is not matched by anything in the pricelist table.
    SELECT
         t1~mandt,
         t1~vkorg,
         t1~vtweg,
         t1~werks,
         t1~matnr,
         t1~ean11,
         t1~vrkme
      FROM zpos_article AS t1
      LEFT OUTER JOIN zpos_pricelist AS t2
         ON  t1~vkorg = t2~vkorg
         AND t1~vtweg = t2~vtweg
         AND t1~werks = t2~werks
         AND t1~matnr = t2~matnr
         AND t1~ean11 = t2~ean11
         AND t1~vrkme = t2~vrkme
      INTO TABLE @DATA(lt_article)
      WHERE t2~vkorg IS NULL
         AND t2~vtweg IS NULL
         AND t2~werks IS NULL
         AND t2~matnr IS NULL
         AND t2~ean11 IS NULL
         AND t2~vrkme IS NULL.


    IF NOT lt_article IS INITIAL.
      DELETE zpos_article FROM TABLE lt_article.
      MESSAGE i000(zpos) WITH sy-dbcnt ' entries deleted from table ZPOS_ARTICLE'.
    ENDIF.

* This selects the Article BOM Data which is not matched by anything in the pricelist table.
    SELECT
         t1~mandt,
         t1~vkorg,
         t1~vtweg,
         t1~werks,
         t1~matnr,
         t1~ean11,
         t1~vrkme,
         t1~stlnr,
         t1~stlkn
      FROM zpos_article_bom AS t1
      LEFT OUTER JOIN zpos_pricelist AS t2
         ON  t1~vkorg = t2~vkorg
         AND t1~vtweg = t2~vtweg
         AND t1~werks = t2~werks
         AND t1~matnr = t2~matnr
         AND t1~ean11 = t2~ean11
         AND t1~vrkme = t2~vrkme
      INTO TABLE @DATA(lt_article_bom)
      WHERE t2~vkorg IS NULL
         AND t2~vtweg IS NULL
         AND t2~werks IS NULL
         AND t2~matnr IS NULL
         AND t2~ean11 IS NULL
         AND t2~vrkme IS NULL.

    IF NOT lt_article_bom IS INITIAL.
      DELETE zpos_article_bom FROM TABLE lt_article_bom.
      MESSAGE i000(zpos) WITH sy-dbcnt ' entries deleted from table ZPOS_ARTICLE_BOM'.
    ENDIF.


  ENDMETHOD.


  METHOD create_status_records.

    DATA: lt_artprice_sts TYPE zpos_artprc_sts_tty.
    DATA(lv_timestamp) = sy-datum && sy-uzeit.

    LOOP AT it_article_price INTO DATA(ls_article_price) GROUP BY ls_article_price-docnum.

      LOOP AT GROUP ls_article_price INTO DATA(member).

        APPEND INITIAL LINE TO lt_artprice_sts ASSIGNING FIELD-SYMBOL(<ls_artprice_sts>).
        <ls_artprice_sts>-key                  = member-key.
        <ls_artprice_sts>-pos_recipient        = member-pos_recipient.
        <ls_artprice_sts>-timestamp            = lv_timestamp.                                " Ensure all updates in a single job get the same timestamp.
        IF iv_status IS INITIAL AND member-pos_recipient_status IS INITIAL.
          RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e033(zpos).
        ENDIF.
        <ls_artprice_sts>-pos_recipient_status = COND #( WHEN iv_status IS NOT INITIAL THEN iv_status ELSE member-pos_recipient_status ).
        TRY.
            IF NOT it_idoc_map IS INITIAL.
              <ls_artprice_sts>-docnum               = it_idoc_map[ source_value = member-docnum ]-target_value .
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e031(zpos).
        ENDTRY.
        <ls_artprice_sts>-created_by = sy-uname.

      ENDLOOP.

    ENDLOOP.

    IF NOT lt_artprice_sts IS INITIAL.
      MODIFY zpos_artprc_sts FROM TABLE lt_artprice_sts.
      rv_count = sy-dbcnt.
*      MESSAGE i000(zpos) WITH sy-dbcnt ' entries added to table ZPOS_ARTPRC_STS'.
    ENDIF.

  ENDMETHOD.


  METHOD create_persist_price_data.

*---------------------------------------------------------------*
    DATA: lt_zwso_price    TYPE STANDARD TABLE OF ty_zwso_price,
          lt_eff_price     TYPE STANDARD TABLE OF ty_short_konp,
          lt_std_price     TYPE STANDARD TABLE OF ty_short_konp,
          lt_offer_id      TYPE STANDARD TABLE OF ty_short_wakp,
          lt_persist_price TYPE zpos_pricelist_tty,
          lt_r_aktnr       TYPE RANGE OF waktion,
          lt_wind_pointer  TYPE wes_t_wind_id,
          lt_short_marm    TYPE wes_to_short_marm,
          ls_short_marm    TYPE wes_s_short_marm,
          lt_artprc_status TYPE STANDARD TABLE OF zpos_artprc_sts,
          lv_rect_check    TYPE i VALUE 0,
          lt_wind          TYPE typ_windvb,
          lv_werks         TYPE werks_d,
          lv_channel       TYPE vtweg,
          lv_ref_channel   TYPE vtweg.

*---------------------------------------------------------------*

    SELECT * FROM zpos_store_rcpt
       INTO TABLE @DATA(lt_store_rcpt)
       ORDER BY vkorg, werks, pos_recipient.


    IF iv_initial_load = abap_true.

*--------------------- ZWSO PRICE DETAILS INITIAL LOAD ---------------------*
      SELECT
         t1~vkorg,
         t1~vtweg,
         t1~werks,
         t1~matnr,
         t1~vrkme,
         t1~datbi,
         t1~datab,
         t1~zeff_price_type,
         t1~zeff_price_fkey,
         t1~zeff_price_level,
         t1~zstd_price_fkey,
         t1~knumh,
         t2~kbetr,
         t2~konwa,
         t2~kpein,
         t2~kmein
        INTO  CORRESPONDING FIELDS OF TABLE @lt_zwso_price
        FROM  a991 AS t1
         INNER JOIN konp AS t2
         ON t1~knumh = t2~knumh
         WHERE t1~kschl = @zcl_pos_util=>co_condition_type-effective_price
           AND t1~datab <= @iv_datbi            " These look the wrong way round but they are correct!
           AND t1~datbi >= @iv_datab            " These look the wrong way round but they are correct!
           AND t2~loevm_ko NE @abap_true.

    ELSE.

      lt_wind = read_wind_pointer( it_r_appl ).
      IF lt_wind[] IS INITIAL.
        MESSAGE i000(zpos) WITH 'No relevant entries found in table WIND'.
        RETURN.
      ENDIF.

* Can be more entries in lt_wind than we find in lt_zwso_price --> this is okay

*--------------------- ZWSO PRICE DETAILS VIA WIND ---------------------*
      SELECT
         t1~vkorg,
         t1~vtweg,
         t1~werks,
         t1~matnr,
         t1~vrkme,
         t1~datbi,
         t1~datab,
         t1~zeff_price_type,
         t1~zeff_price_fkey,
         t1~zeff_price_level,
         t1~zstd_price_fkey,
         t1~knumh,
         t2~kbetr,
         t2~konwa,
         t2~kpein,
         t2~kmein
        INTO  CORRESPONDING FIELDS OF TABLE @lt_zwso_price
        FROM  a991 AS t1
         INNER JOIN konp AS t2
         ON t1~knumh = t2~knumh
         FOR ALL ENTRIES IN @lt_wind
         WHERE t1~kschl = @zcl_pos_util=>co_condition_type-effective_price
           AND t1~knumh = @lt_wind-knumh
           AND t2~loevm_ko NE @abap_true.

    ENDIF.

    IF lt_zwso_price[] IS INITIAL.
      MESSAGE i000(zpos) WITH 'No entries found in Tables A991 & KONP'.
      RETURN.
    ENDIF.

* Can be more entries in lt_zwso_price than we find in lt_eff_price --> this is okay. the same source price can be used for multiple target prices

*--------------------- EFFECTIVE PRICE DETAILS ---------------------*
    SELECT
       t1~knumh,
       t1~kopos,
       t1~kschl,
       t1~kbetr,
       t1~konwa,
       t1~kpein,
       t1~kmein,
       t2~aktnr
      INTO  CORRESPONDING FIELDS OF TABLE @lt_eff_price
      FROM  konp AS t1
       INNER JOIN konh AS t2
       ON t1~knumh = t2~knumh
       FOR ALL ENTRIES IN @lt_zwso_price
       WHERE t1~knumh = @lt_zwso_price-zeff_price_fkey.

    SORT lt_eff_price BY knumh.


*--------------------- OFFERID ---------------------*
    IF NOT lt_eff_price[] IS INITIAL.

      LOOP AT lt_eff_price INTO DATA(ls_eff_price).

        IF ls_eff_price-aktnr IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_r_aktnr ASSIGNING FIELD-SYMBOL(<ls_r_aktnr>).
          <ls_r_aktnr>-low = ls_eff_price-aktnr.
          <ls_r_aktnr>-sign   = 'I'.
          <ls_r_aktnr>-option = 'EQ'.
        ENDIF.

      ENDLOOP.
      SORT lt_r_aktnr BY low ASCENDING. DELETE ADJACENT DUPLICATES FROM lt_r_aktnr COMPARING low.
      SORT lt_wind_pointer BY knumh kopos ASCENDING. DELETE ADJACENT DUPLICATES FROM lt_wind_pointer COMPARING knumh kopos.

      IF NOT lt_r_aktnr IS INITIAL.
        SELECT
           t1~aktnr,
           t1~offer_id,
           t1~offer_id_dsc
          INTO  CORRESPONDING FIELDS OF TABLE @lt_offer_id
          FROM  wakp AS t1
           WHERE t1~aktnr IN @lt_r_aktnr.

        SORT lt_offer_id BY aktnr.
      ENDIF.

    ENDIF.

* Can be more entries in lt_zwso_price than we find in lt_std_price --> this is okay. the same source price can be used for multiple target prices

*---------------------- STANDARD PRICE DETAILS ----------------------*
    SELECT
       t1~knumh,
       t1~kschl,
       t1~kopos,
       t1~kbetr,
       t1~konwa,
       t1~kpein,
       t1~kmein
      INTO  CORRESPONDING FIELDS OF TABLE @lt_std_price
      FROM  konp AS t1
       FOR ALL ENTRIES IN @lt_zwso_price
       WHERE t1~kschl = @zcl_pos_util=>co_condition_type-standard_price
       AND t1~knumh = @lt_zwso_price-zstd_price_fkey.

    SORT lt_std_price BY knumh.

*---------------------- MAP to PERSISTENT TABLE ----------------------*




    LOOP AT lt_zwso_price INTO DATA(ls_zwso_price).


*"----------------------------------------------------------------------
*  Here we store the data in the non-reference channel (used for eComm)
*  So the master data references prices as in 10, but here we will store them in 20
*"----------------------------------------------------------------------
*  Read the reference Channel Data and use that instead of the default channel
      IF lv_werks <> ls_zwso_price-werks.
        lv_werks = ls_zwso_price-werks.
        READ TABLE lt_store_rcpt INTO DATA(ls_store_rcpt) WITH KEY vkorg = ls_zwso_price-vkorg werks = ls_zwso_price-werks BINARY SEARCH.
        IF sy-subrc = 0.
          lv_ref_channel = ls_store_rcpt-persist_channel.
        ELSE.
          CLEAR lv_ref_channel.
        ENDIF.
      ENDIF.
*"----------------------------------------------------------------------


      APPEND INITIAL LINE TO lt_persist_price ASSIGNING FIELD-SYMBOL(<ls_persist_price>).
      <ls_persist_price>-mandt = sy-mandt.
      <ls_persist_price>-vkorg = ls_zwso_price-vkorg.
      <ls_persist_price>-vtweg = COND #( WHEN lv_ref_channel IS NOT INITIAL THEN lv_ref_channel ELSE ls_zwso_price-vtweg ).
      <ls_persist_price>-werks = ls_zwso_price-werks.
      <ls_persist_price>-matnr = ls_zwso_price-matnr.
      <ls_persist_price>-vrkme = ls_zwso_price-vrkme.
      <ls_persist_price>-datbi = ls_zwso_price-datbi.
      <ls_persist_price>-datab = ls_zwso_price-datab.
      <ls_persist_price>-wso_price_key = ls_zwso_price-knumh.
      <ls_persist_price>-currency = ls_zwso_price-konwa.

*         --------------- EFFECTIVE PRICE DETAILS
      <ls_persist_price>-eff_price = ls_zwso_price-kbetr.
      <ls_persist_price>-eff_price_type = ls_zwso_price-zeff_price_type.
      <ls_persist_price>-eff_price_fkey = ls_zwso_price-zeff_price_fkey.
      <ls_persist_price>-eff_price_level = ls_zwso_price-zeff_price_level.
      <ls_persist_price>-kpein = ls_zwso_price-kpein.
      <ls_persist_price>-kmein = ls_zwso_price-kmein.

      READ TABLE lt_eff_price WITH KEY knumh = ls_zwso_price-zeff_price_fkey BINARY SEARCH INTO ls_eff_price.
      IF sy-subrc = 0.
        <ls_persist_price>-aktnr = ls_eff_price-aktnr.
        IF NOT <ls_persist_price>-aktnr IS INITIAL.
          READ TABLE lt_offer_id WITH KEY aktnr = <ls_persist_price>-aktnr INTO DATA(ls_offer_id).
          IF sy-subrc = 0.
            <ls_persist_price>-offer_id = ls_offer_id-offer_id.
            <ls_persist_price>-offer_id_dsc = ls_offer_id-offer_id_dsc.
          ENDIF.
        ENDIF.
      ENDIF.

*         --------------- STANDARD PRICE DETAILS
      <ls_persist_price>-std_price_fkey = ls_zwso_price-zstd_price_fkey.
      READ TABLE lt_std_price WITH KEY knumh = ls_zwso_price-zstd_price_fkey BINARY SEARCH INTO DATA(ls_std_price).
      IF sy-subrc = 0.
        <ls_persist_price>-std_price = ls_std_price-kbetr.
      ENDIF.


*         --------------- EAN CODE
      CLEAR lt_short_marm[].
      CALL FUNCTION 'WES_MARM_SELECT'
        EXPORTING
          iv_check_ean   = abap_true
          iv_matnr       = <ls_persist_price>-matnr
          iv_meinh       = <ls_persist_price>-vrkme
        IMPORTING
          eto_short_marm = lt_short_marm.

      READ TABLE lt_short_marm INTO ls_short_marm INDEX 1.

      IF sy-subrc = 0.
        <ls_persist_price>-ean11 = ls_short_marm-ean11.
*        <ls_persist_price>-numtp = ls_short_marm-numtp.
      ENDIF.


    ENDLOOP.

* Delete Records without an EAN code
    DELETE lt_persist_price WHERE ean11 IS INITIAL.
    SORT lt_persist_price BY vkorg werks vtweg matnr ean11.

    IF NOT lt_persist_price IS INITIAL.

      DATA(lt_filter) = lt_store_rcpt.
      DELETE lt_filter WHERE filter IS INITIAL.

      IF NOT lt_filter IS INITIAL.
        SELECT matnr,
               material_group_1
               FROM zpos_article
               INTO TABLE @DATA(lt_article_filter)
               FOR ALL ENTRIES IN @lt_filter
               WHERE vkorg          = @lt_filter-vkorg
               AND werks            = @lt_filter-werks
               AND material_group_1 = @lt_filter-filter.

        SORT lt_article_filter BY matnr ASCENDING. DELETE ADJACENT DUPLICATES FROM lt_article_filter COMPARING matnr.

      ENDIF.

      DATA(lv_timestamp) = sy-datum && sy-uzeit.
      LOOP AT lt_persist_price INTO DATA(ls_persist_price).

        CLEAR lv_rect_check.
        LOOP AT lt_store_rcpt
                     INTO ls_store_rcpt
                     WHERE vkorg = ls_persist_price-vkorg
                     AND   werks = ls_persist_price-werks.

* For Bizerba we might have a filter value like ZSC (Scales)
* Now when creating a status record we should check that the article exists in the filter table - the article data is the same for all recipients / stores
          IF NOT ls_store_rcpt-filter IS INITIAL.
            READ TABLE lt_article_filter INTO DATA(ls_article) WITH KEY matnr = ls_persist_price-matnr BINARY SEARCH.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.
          ENDIF.

          lv_rect_check += 1.
          APPEND INITIAL LINE TO lt_artprc_status ASSIGNING FIELD-SYMBOL(<ls_artprc_status>).
          <ls_artprc_status>-key                  = ls_persist_price-key.
          <ls_artprc_status>-pos_recipient        = ls_store_rcpt-pos_recipient.
          <ls_artprc_status>-timestamp            = lv_timestamp.                   "Same value for all records created at one time
          <ls_artprc_status>-pos_recipient_status = zcl_pos_util=>co_recipient_status-created.
          <ls_artprc_status>-created_by           = sy-uname.

        ENDLOOP.

        IF lv_rect_check IS INITIAL.
* CHECK IF WE HAVE DATA FOR A STORE BUT NO RECIPIENT APPLICATION DEFINED
          MESSAGE i000(zpos) WITH 'No config found for store ' ls_persist_price-werks ' in table ZPOS_STORE_RCPT. Processing Stopped'.
          RETURN.
        ENDIF.
      ENDLOOP.

      IF NOT lt_artprc_status IS INITIAL.
        MODIFY zpos_pricelist FROM TABLE lt_persist_price.
        MESSAGE i000(zpos) WITH sy-dbcnt ' entries added to table ZPOS_PRICELIST'.

        MODIFY zpos_artprc_sts FROM TABLE lt_artprc_status.
        MESSAGE i000(zpos) WITH sy-dbcnt ' entries added to table ZPOS_ARTPRC_STATUS'.

      ENDIF.

      IF iv_update_wind_status = abap_true.
        LOOP AT lt_persist_price INTO ls_persist_price.
          APPEND INITIAL LINE TO lt_wind_pointer ASSIGNING FIELD-SYMBOL(<ls_wind_pointer>).
          <ls_wind_pointer>-bltyp = '80'.
          <ls_wind_pointer>-knumh = ls_persist_price-wso_price_key.
          <ls_wind_pointer>-kopos = '01'.                 "Is this right? Value always seems to be 01
        ENDLOOP.
        set_status_wind_pointer( EXPORTING it_wind_pointer_id = lt_wind_pointer it_r_appl = it_r_appl ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD create_persist_label_data.

    DATA: lt_shelf_label TYPE STANDARD TABLE OF zpos_shelf_lbl.

    LOOP AT it_article INTO DATA(ls_article).

      APPEND INITIAL LINE TO lt_shelf_label ASSIGNING FIELD-SYMBOL(<ls_shelf_label>).
      MOVE-CORRESPONDING ls_article TO <ls_shelf_label>.
      <ls_shelf_label>-label_template = 001.
      <ls_shelf_label>-label_quantity = 1.

    ENDLOOP.

* We do NOT want to overwrite any existing entries as values may hav been manually entered by users.
    INSERT zpos_shelf_lbl FROM TABLE lt_shelf_label ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.


  METHOD create_persist_article_data.

* TODO -----> LAYOUT_MODULE  ---> MALG-LAYGR

    DATA: lt_output_article TYPE STANDARD TABLE OF zpos_article,
          lt_output_bom     TYPE STANDARD TABLE OF zpos_article_bom,
          lv_ean11          TYPE ean11,
          ls_maw1           TYPE maw1,
          ls_mara           TYPE mara,
          ls_marc           TYPE marc,
          ls_mvke           TYPE mvke,
          ls_wlk2           TYPE wlk2,
          ls_layout         TYPE zposi_layout_module,
          lt_makt           TYPE STANDARD TABLE OF makt,

          lt_r_matnr        TYPE wrf_matnr_rtty,
          lt_r_werks        TYPE tdt_rg_kunnr.
    DATA:lv_werks           TYPE kunnr.

    DATA: lt_matnr                   TYPE wrf_ref_pre03_tty,
          lt_matnr_werks             TYPE wrf_ref_pre01_tty,
          lt_vkorg_vtweg_matnr       TYPE pre10_tab,
          lt_matnr_vkorg_vtweg_werks TYPE ty_t_pre23.



    LOOP AT it_package INTO DATA(ls_package).
      APPEND INITIAL LINE TO lt_matnr ASSIGNING FIELD-SYMBOL(<ls_matnr>).
      <ls_matnr>-matnr = ls_package-matnr.

      APPEND INITIAL LINE TO lt_matnr_werks ASSIGNING FIELD-SYMBOL(<ls_matnr_werks>).
      <ls_matnr_werks>-matnr = ls_package-matnr.
      <ls_matnr_werks>-werks = ls_package-werks.

      APPEND INITIAL LINE TO lt_vkorg_vtweg_matnr ASSIGNING FIELD-SYMBOL(<ls_mvke>).
      <ls_mvke>-vkorg = ls_package-vkorg.
      <ls_mvke>-vtweg = ls_package-vtweg.
      <ls_mvke>-matnr = ls_package-matnr.

* With Werks
      APPEND INITIAL LINE TO lt_matnr_vkorg_vtweg_werks ASSIGNING FIELD-SYMBOL(<ls_wlk2>).
      <ls_wlk2>-matnr = ls_package-matnr.
      <ls_wlk2>-vkorg = ls_package-vkorg.
      <ls_wlk2>-vtweg = ls_package-vtweg.
      <ls_wlk2>-werks = ls_package-werks.

* Without Werks
      APPEND INITIAL LINE TO lt_matnr_vkorg_vtweg_werks ASSIGNING <ls_wlk2>.
      <ls_wlk2>-matnr = ls_package-matnr.
      <ls_wlk2>-vkorg = ls_package-vkorg.
      <ls_wlk2>-vtweg = ls_package-vtweg.

* Range Table
      APPEND INITIAL LINE TO lt_r_matnr ASSIGNING FIELD-SYMBOL(<ls_r_matnr>).
      <ls_r_matnr>-sign   = zcl_pos_util=>co_selopt_sign_i.
      <ls_r_matnr>-option = zcl_pos_util=>co_selopt_opt_eq.
      <ls_r_matnr>-low = ls_package-matnr.

      APPEND INITIAL LINE TO lt_r_werks ASSIGNING FIELD-SYMBOL(<ls_r_werks>).
      <ls_r_werks>-sign   = zcl_pos_util=>co_selopt_sign_i.
      <ls_r_werks>-option = zcl_pos_util=>co_selopt_opt_eq.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_package-werks
        IMPORTING
          output = <ls_r_werks>-low.

    ENDLOOP.
    SORT lt_matnr                   BY matnr.                   DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING ALL FIELDS.
    SORT lt_matnr_werks             BY matnr werks.             DELETE ADJACENT DUPLICATES FROM lt_matnr_werks COMPARING ALL FIELDS.
    SORT lt_vkorg_vtweg_matnr       BY vkorg vtweg matnr.       DELETE ADJACENT DUPLICATES FROM lt_vkorg_vtweg_matnr COMPARING ALL FIELDS.
    SORT lt_matnr_vkorg_vtweg_werks BY matnr vkorg vtweg werks. DELETE ADJACENT DUPLICATES FROM lt_matnr_vkorg_vtweg_werks COMPARING ALL FIELDS.
    SORT lt_r_matnr                 BY low.                     DELETE ADJACENT DUPLICATES FROM lt_r_matnr COMPARING low.
    SORT lt_r_werks                 BY low.                     DELETE ADJACENT DUPLICATES FROM lt_r_werks COMPARING low.

*====================================================================================================================
* Read Master Data
*====================================================================================================================

    DATA(lt_maw1) = read_maw1_data( CHANGING ct_matnr = lt_matnr ).
    DATA(lt_mara) = read_mara_data( CHANGING ct_matnr = lt_matnr ).
    DATA(lt_marc) = read_marc_data( CHANGING ct_matnr_werks = lt_matnr_werks ).
    DATA(lt_mvke) = read_mvke_data( CHANGING ct_vkorg_vtweg_matnr = lt_vkorg_vtweg_matnr ).
    DATA(lt_wlk2) = read_wlk2_data( CHANGING ct_matnr_vkorg_vtweg_werks = lt_matnr_vkorg_vtweg_werks ).
    DATA(lt_layout) = read_layout_module( EXPORTING it_r_matnr = lt_r_matnr it_r_werks = lt_r_werks ).
*====================================================================================================================


*    SORT mt_cond_art BY vkorg vtweg werks matnr vrkme.


    LOOP AT it_package INTO ls_package.

      IF ls_package-matnr NE ls_maw1-matnr.
        CLEAR ls_maw1.
        READ TABLE lt_maw1 WITH KEY matnr = ls_package-matnr INTO ls_maw1 BINARY SEARCH.
      ENDIF.

      IF ls_package-matnr NE ls_mara-matnr.
        CLEAR ls_mara.
        READ TABLE lt_mara WITH KEY matnr = ls_package-matnr INTO ls_mara BINARY SEARCH.
      ENDIF.

      IF ls_package-matnr NE ls_marc-matnr OR
        ls_package-werks NE ls_marc-werks.
        CLEAR ls_marc.
        READ TABLE lt_marc WITH KEY matnr = ls_package-matnr werks = ls_package-werks INTO ls_marc BINARY SEARCH.
      ENDIF.

      IF ls_package-vkorg NE ls_mvke-vkorg OR
        ls_package-vtweg NE ls_mvke-vtweg OR
        ls_package-matnr NE ls_mvke-matnr.
        CLEAR ls_mvke.
        READ TABLE lt_mvke WITH KEY matnr = ls_package-matnr vkorg = ls_package-vkorg vtweg = ls_package-vtweg INTO ls_mvke BINARY SEARCH.
      ENDIF.

      IF ls_package-matnr NE ls_wlk2-matnr OR
         ls_package-vkorg NE ls_wlk2-vkorg OR
         ls_package-vtweg NE ls_wlk2-vtweg.

        CLEAR ls_wlk2.
        READ TABLE lt_wlk2 WITH KEY matnr = ls_package-matnr vkorg = ls_package-vkorg vtweg = ls_package-vtweg INTO ls_wlk2 BINARY SEARCH.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_package-werks
        IMPORTING
          output = lv_werks.

      IF ls_package-matnr NE ls_layout-article OR
         lv_werks         NE ls_layout-location.
        CLEAR ls_layout.
        READ TABLE lt_layout WITH KEY article = ls_package-matnr location = lv_werks INTO ls_layout.
      ENDIF.

      CLEAR lv_ean11.

      LOOP AT ls_package-sales_prices_tab INTO DATA(ls_sales_price) WHERE ean11 IS NOT INITIAL.

        IF ls_sales_price-ean11 NE lv_ean11.


*=============================================================================================================
* Create Output Record - Right cardinality ?
*=============================================================================================================
          APPEND INITIAL LINE TO lt_output_article ASSIGNING FIELD-SYMBOL(<ls_output_article>).



*=============================================================================================================
* Get Tax Percentage - Assumes ONLY EVER 1 SINGLE VALID TAX RECORD - How about future tax rate changes???
*=============================================================================================================
          READ TABLE ls_package-tax_tab INDEX 1 INTO DATA(ls_tax).
          <ls_output_article>-tax_classification = ls_tax-taxkm.
          <ls_output_article>-tax_code           = ls_tax-mwskz.
          <ls_output_article>-tax_rate           = ls_tax-kbetr / 10 .


          IF lines( ls_package-tax_tab ) > 1 OR ls_tax IS INITIAL.
            CLEAR ms_textid.
            ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
            ms_textid-msgno = '040'.
            ms_textid-attr1 = ls_package-matnr.
            mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
            RAISE EXCEPTION mx_exception.

          ENDIF.


*=============================================================================================================
* General Key Fields
*=============================================================================================================
          <ls_output_article>-mandt             = sy-mandt.
          <ls_output_article>-vkorg             = ls_package-vkorg.
          <ls_output_article>-vtweg             = ls_package-vtweg.
          <ls_output_article>-werks             = ls_package-werks.
          <ls_output_article>-matnr             = ls_package-matnr.
          <ls_output_article>-material_type     = ls_package-mtart.
          <ls_output_article>-material_category = ls_package-attyp.

          <ls_output_article>-ean11 = ls_sales_price-ean11.
          <ls_output_article>-vrkme = ls_sales_price-kmein.

*=============================================================================================================
* Description Texts
*=============================================================================================================
          DATA(lv_language) = zcl_pos_util=>get_language_by_salesorg( ls_package-vkorg ).


          TRY.
              <ls_output_article>-description = ls_package-maktx_tab[ spras = lv_language ]-maktx.
            CATCH cx_sy_itab_line_not_found.

              TRY.
                  <ls_output_article>-description = ls_package-maktx_tab[ spras = zcl_pos_util=>co_english_language ]-maktx.
                CATCH cx_sy_itab_line_not_found.
                  <ls_output_article>-description = 'No Description'.
              ENDTRY.
          ENDTRY.

          TRY.
              <ls_output_article>-detailed_desc = ls_package-maktx_tab[ spras = zcl_pos_util=>co_detailed_language ]-maktx.
            CATCH cx_sy_itab_line_not_found.
* Do Nothing !
          ENDTRY.

          TRY.
              <ls_output_article>-pos_text = ls_package-mamt_tab[ meinh = <ls_output_article>-vrkme spras = lv_language ]-maktm.
            CATCH cx_sy_itab_line_not_found.

              TRY.
                  <ls_output_article>-pos_text = ls_package-mamt_tab[ meinh = <ls_output_article>-vrkme spras = zcl_pos_util=>co_english_language ]-maktm.
                CATCH cx_sy_itab_line_not_found.
* Do Nothing !
              ENDTRY.
          ENDTRY.


*=============================================================================================================
* Material Group - Are these restrictions correct?
*=============================================================================================================
          READ TABLE ls_package-prod_tab WITH KEY hier_id = '1' hier_type = '3' INTO DATA(ls_prod).
          IF sy-subrc = 0.
            <ls_output_article>-material_group = ls_prod-matkl.
          ENDIF.

*=============================================================================================================
* Material Group 1 - Used as a Filter e.g. Bizerba
*=============================================================================================================
          <ls_output_article>-material_group_1 = ls_mvke-mvgr1.


*=============================================================================================================
* Vendor
*=============================================================================================================
          READ TABLE ls_package-proc_prices_tab INDEX 1 INTO DATA(ls_proc_prices).
          IF sy-subrc = 0.
            <ls_output_article>-vendor = ls_proc_prices-lifnr.
          ENDIF.

          IF lines( ls_package-proc_prices_tab ) > 1.
* Want to see if this scenario can ever happen!
            RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e041(zpos).
          ENDIF.



*=============================================================================================================
* Comparison Units
*=============================================================================================================
          <ls_output_article>-net_contents  = ls_mara-inhal.
          <ls_output_article>-comp_qty     = ls_mara-vpreh.
          <ls_output_article>-comp_uom     = ls_mara-inhme.


*=============================================================================================================
* Procurement Status
*=============================================================================================================
          <ls_output_article>-purch_status   = ls_marc-mmsta.

*=============================================================================================================
* Assortment Grade
*=============================================================================================================
          <ls_output_article>-grade = ls_maw1-sstuf.

*=============================================================================================================
* Weight Info
*=============================================================================================================
          <ls_output_article>-gross_weight = ls_mara-brgew.
          <ls_output_article>-net_weight = ls_mara-ntgew.
          <ls_output_article>-weight_unit = ls_mara-gewei.

*=============================================================================================================
* Commodity Code
*=============================================================================================================
          <ls_output_article>-commodity_code = ls_marc-stawn.

*=============================================================================================================
* Country of Origin
*=============================================================================================================
          <ls_output_article>-country_origin = ls_maw1-wherl.


*=============================================================================================================
* Material Item Category
*=============================================================================================================
          <ls_output_article>-material_item_cat = ls_mvke-mtpos.


*=============================================================================================================
* WLK2 Values - Mainly Viking Article Values
*=============================================================================================================
          <ls_output_article>-pos_entry_code = ls_wlk2-zz_entry_code.

          <ls_output_article>-pos_staff_disc = ls_wlk2-zz_staff_disc.

          <ls_output_article>-pos_add_text = ls_wlk2-zz_posmsg.

          <ls_output_article>-pos_multibuy = ls_wlk2-zz_multibuy_code.

          IF ls_wlk2-kwdht = 1.
            <ls_output_article>-pos_repeat_key = 0.
          ELSE.
            <ls_output_article>-pos_repeat_key = 1.
          ENDIF.

* Not clear yet how to fill this value
          <ls_output_article>-pos_add_item = ''.

          <ls_output_article>-pos_decimals = ls_wlk2-zz_decimals.

          IF <ls_output_article>-pos_entry_code = 'P'.
            <ls_output_article>-pos_fixedprice = 0.
          ELSEIF <ls_output_article>-pos_entry_code = 'E'.
            <ls_output_article>-pos_fixedprice = 1.
          ENDIF.


*=============================================================================================================
* MARC - Freight Group
*=============================================================================================================
          <ls_output_article>-freight_group   = ls_marc-mfrgr.



*=============================================================================================================
* Layout Module - Still to be decided
*=============================================================================================================
*        See CDS view - ZPOSI_LAYOUT_MODULE - Using Current Date in the Select.
          <ls_output_article>-layout_module = ls_layout-assortment.


*=============================================================================================================
* Bill of Materials
*=============================================================================================================
          LOOP AT ls_package-mmbom INTO DATA(ls_bom_header).


            LOOP AT ls_bom_header-variant-item INTO DATA(ls_bom_item).

              APPEND INITIAL LINE TO lt_output_bom ASSIGNING FIELD-SYMBOL(<ls_output_bom>).

              <ls_output_bom>-key       = <ls_output_article>-key.
              <ls_output_bom>-stlnr     =  ls_bom_header-stlnr  .
              <ls_output_bom>-stlkn     =  ls_bom_item-stlkn.
              <ls_output_bom>-idnrk     =  ls_bom_item-idnrk.
              <ls_output_bom>-bom_ean11 =  ls_bom_item-ean11.
              <ls_output_bom>-bom_numtp =  ls_bom_item-numtp.
              <ls_output_bom>-meins     =  ls_bom_item-meins.
              <ls_output_bom>-menge     =  ls_bom_item-menge.

              IF NOT <ls_output_bom>-idnrk IS INITIAL AND NOT lv_language IS INITIAL.
*   get text out of buffer
                CALL FUNCTION 'MAKT_GENERIC_READ_WITH_MATNR'
                  EXPORTING
*                   KZRFB      = ' '
                    matnr      = <ls_output_bom>-idnrk
                  TABLES
*                   KTEXT      =
                    makt_tab   = lt_makt
                  EXCEPTIONS
                    wrong_call = 1
                    not_found  = 2
                    OTHERS     = 3.

                IF sy-subrc = 0.

                  TRY.
                      <ls_output_bom>-description = lt_makt[ spras = lv_language ]-maktx.
                    CATCH cx_sy_itab_line_not_found.

                      TRY.
                          <ls_output_bom>-description = lt_makt[ spras = zcl_pos_util=>co_english_language ]-maktx.
                        CATCH cx_sy_itab_line_not_found.
*                          Do Nothing
                      ENDTRY.
                  ENDTRY.

                ENDIF.


              ENDIF.

            ENDLOOP.

          ENDLOOP.

        ENDIF.

*     For next check loop.
        lv_ean11 = ls_sales_price-ean11.

      ENDLOOP.

    ENDLOOP.

    IF NOT lt_output_article[] IS INITIAL.
      MODIFY zpos_article FROM TABLE lt_output_article.

* And update the Shelf Label Table used by ESL
      create_persist_label_data( lt_output_article ).

    ENDIF.



    IF NOT lt_output_bom[] IS INITIAL.
      MODIFY zpos_article_bom FROM TABLE lt_output_bom.
    ENDIF.


  ENDMETHOD.


  METHOD create_outbound_article_idoc.


    DATA: lt_edidd        TYPE STANDARD TABLE OF edidd,
          ls_idoc_data    TYPE edidd,
          ls_edidc        TYPE edidc,
          ls_idoc_article TYPE zpos_article_01,
          ls_idoc_scale   TYPE zpos_article_scale_01,
          ls_idoc_bom     TYPE zpos_article_bom_01,
          lt_idoc_map     TYPE ty_t_idoc_map,
          ls_idoc_map     TYPE ty_s_idoc_map,
          lt_artprice_sts TYPE STANDARD TABLE OF zpos_artprc_sts.

    DATA: lv_parent_segnum TYPE idocdsgnum VALUE '0',
          lv_segnum        TYPE idocdsgnum VALUE '0',
          lv_scale_segnum  TYPE idocdsgnum VALUE '0',
          lv_counter       TYPE i VALUE 0,
          lv_docnum        TYPE docnum,
          ls_article_price TYPE zpos_article_price_sty.

    DATA: ls_edids TYPE edids,
          lv_subrc TYPE sysubrc.

    mo_config_map = NEW #( iv_vkorg = iv_vkorg ).

    SELECT vkorg, land1 FROM t001w INTO TABLE @DATA(lt_country) WHERE vkorg = @iv_vkorg.


    IF zcl_pos_util=>co_pos_recipient-magento_idoc IN it_r_recipient.

*   Should only get this data if we are sending pricing to eComm
      SELECT vkorg,
             vtweg,
             werks,
             matnr,
             ean11,
             vrkme,
             http_status
        FROM zecm_article_sts
             INTO CORRESPONDING FIELDS OF
             TABLE @mt_ecom_status
             WHERE vkorg = @iv_vkorg
             AND   vtweg IN @it_r_vtweg
             AND   werks IN @it_r_werks.

*   Might not even need it at all now
      SELECT * FROM zpos_website
               INTO TABLE @DATA(lt_website)
               WHERE vkorg = @iv_vkorg
               AND   werks IN @it_r_werks
               ORDER BY vkorg, werks.
    ENDIF.


    DATA(lt_article_price) = get_article_pricelist(
                              EXPORTING iv_create_bom  = iv_create_bom
                                        iv_status      = iv_status
                                        iv_valid_on    = iv_valid_on
                                        iv_leadtime    = iv_leadtime
                                        it_r_recipient = it_r_recipient
                                        iv_vkorg       = iv_vkorg
                                        it_r_vtweg     = it_r_vtweg
                                        it_r_werks     = it_r_werks
                                        it_r_matnr     = it_r_matnr ).

    IF lt_article_price IS INITIAL.
      MESSAGE i000(zpos) WITH 'No relevant entries found - No IDocs Created'.
      RETURN.
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
    ENDIF.

    ls_edidc-rcvpor = 'SAP_PO'.
    ls_edidc-rcvprn = 'SAP_PO'.
    ls_edidc-rcvprt = ls_edidc-sndprt = 'LS'.
    ls_edidc-outmod = '2'.          " Pass Immediately - For Testing


    ls_edidc-status = '30'.
    ls_edidc-direct = '1'.
    ls_edidc-mestyp = 'ZPOS_ARTICLE'.
    ls_edidc-idoctp = 'ZPOS_ARTICLE_01'.
    TRY.
        ls_edidc-mescod = lt_country[ vkorg = iv_vkorg ]-land1.
      CATCH cx_sy_itab_line_not_found.
        ls_edidc-mescod = 'XX'.
    ENDTRY.
*------------------------------- CONTROL RECORD ------------------------------------



*------------------------------- IDocs are Recípient and Store Specific ------------------------------------
    lv_docnum += 1.
    LOOP AT lt_article_price INTO ls_article_price GROUP BY ( werks = ls_article_price-werks ).


      ls_edidc-mesfct = ls_article_price-pos_recipient.
*------------------------------- CONTROL RECORD ------------------------------------

      LOOP AT GROUP ls_article_price ASSIGNING FIELD-SYMBOL(<ls_article_price>).

        CLEAR: ls_idoc_data, ls_idoc_article, ls_idoc_bom, ls_idoc_scale.

        <ls_article_price>-docnum = lv_docnum.
        ls_idoc_map-source_value = lv_docnum.
        lv_counter += 1.

*------------------------------- ZPOS_ARTICLE_01 ------------------------------------
        ls_idoc_data-segnam = 'ZPOS_ARTICLE_01'.
        ls_idoc_data-hlevel =  '02'.
        lv_segnum           += 1.
        ls_idoc_data-segnum = lv_segnum.

        MOVE-CORRESPONDING <ls_article_price> TO ls_idoc_article.       " This should move just about everything

        IF ls_article_price-pos_recipient = zcl_pos_util=>co_pos_recipient-magento_idoc.
          ls_idoc_article-offer_id = <ls_article_price>-eff_price_fkey.       "For Magento Price Integration the KNUMH key for the Effective price should be the "offerID"
        ENDIF.


*------------------------------- WEBSITE DATA ------------------------------------
        READ TABLE lt_website INTO DATA(ls_website)
                              WITH KEY vkorg = ls_idoc_article-vkorg
                                       werks = ls_idoc_article-werks BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_website TO ls_idoc_article.
        ENDIF.

*------------------------------- WEBSITE DATA ------------------------------------


        ls_idoc_data-sdata = ls_idoc_article.

        APPEND ls_idoc_data TO lt_edidd.
        CLEAR : ls_idoc_data.
        lv_parent_segnum = lv_segnum.

*------------------------------- ZPOS_ARTICLE_01 ------------------------------------



        LOOP AT <ls_article_price>-eff_scale INTO DATA(ls_article_scale) WHERE kbetr IS NOT INITIAL.

*------------------------------- ZPOS_ARTICLE_SCALE_01 ------------------------------------
          ls_idoc_data-segnam = 'ZPOS_ARTICLE_SCALE_01'.
          ls_idoc_data-hlevel =  '03'.
          ls_idoc_data-psgnum = lv_parent_segnum.
          lv_segnum           += 1.
          ls_idoc_data-segnum = lv_segnum.

          MOVE-CORRESPONDING ls_article_scale TO ls_idoc_scale.

          ls_idoc_data-sdata = ls_idoc_scale.
          APPEND ls_idoc_data TO lt_edidd.
          CLEAR : ls_idoc_data.
*------------------------------- ZPOS_ARTICLE_SCALE_01 ------------------------------------

        ENDLOOP.




        LOOP AT <ls_article_price>-bom INTO DATA(ls_article_bom).

*------------------------------- ZPOS_ARTICLE_BOM_01 ------------------------------------
          ls_idoc_data-segnam = 'ZPOS_ARTICLE_BOM_01'.
          ls_idoc_data-hlevel =  '03'.
          ls_idoc_data-psgnum = lv_parent_segnum.
          lv_segnum           += 1.
          ls_idoc_data-segnum = lv_segnum.

          MOVE-CORRESPONDING ls_article_bom TO ls_idoc_bom.

          ls_idoc_data-sdata = ls_idoc_bom.
          APPEND ls_idoc_data TO lt_edidd.
          CLEAR : ls_idoc_data.
*------------------------------- ZPOS_ARTICLE_BOM_01 ------------------------------------

        ENDLOOP.

* Only process 50 records at a time
        IF ( lv_counter MOD iv_records ) = 0 .

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
            ls_idoc_map-target_value = ls_edidc-docnum.
            APPEND ls_idoc_map TO lt_idoc_map.

          ELSE.
            ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
            ms_textid-msgno = '100'.
            ms_textid-attr1 = ls_edidc-mestyp.
            mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
            RAISE EXCEPTION mx_exception.
          ENDIF.

          CLEAR: ls_idoc_data, lt_edidd, ls_edidc-docnum, ls_idoc_article, ls_idoc_bom, lv_parent_segnum, lv_segnum.

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
          ls_idoc_map-target_value = ls_edidc-docnum.
          APPEND ls_idoc_map TO lt_idoc_map.
        ELSE.

          ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ms_textid-msgno = '100'.
          ms_textid-attr1 = ls_edidc-mestyp.
          mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION mx_exception.

        ENDIF.

        CLEAR: ls_idoc_data, lt_edidd, ls_edidc-docnum, ls_idoc_article, ls_idoc_bom, lv_parent_segnum, lv_segnum.

      ENDIF.

    ENDLOOP.

    IF lt_idoc_map IS NOT INITIAL.
      DATA(lv_lines) = lines( lt_idoc_map ).
      READ TABLE lt_idoc_map INDEX 1 INTO DATA(ls_idoc_from).
      READ TABLE lt_idoc_map INDEX lv_lines INTO DATA(ls_idoc_to).
      MESSAGE i004(zpos) WITH lv_lines ls_idoc_from-target_value ls_idoc_to-target_value.
    ENDIF.

    DATA(lv_count) = create_status_records( it_article_price = lt_article_price iv_status = zcl_pos_util=>co_recipient_status-sent it_idoc_map = lt_idoc_map ).
    MESSAGE i000(zpos) WITH lv_count ' entries added to table ZPOS_ARTPRC_STS'.

* Create the Scaled Price IDoc
    IF NOT lt_article_price IS INITIAL.
      zcl_pos_scaled_price=>create_scale_price_idoc( lt_article_price ).
    ENDIF.

* Create the Sales Set (Offer) IDoc
    IF NOT lt_article_price IS INITIAL.
      zcl_pos_sales_set=>create_sales_set_idoc( lt_article_price ).
    ENDIF.


  ENDMETHOD.


  METHOD create_condition_record.

* Change this value in debugging if you want the IDocs to be persisted. Perhaps a User Parameter would be better!
    DATA: lv_idoc_test TYPE boole_d VALUE abap_false.


    DATA: lt_edidd        TYPE STANDARD TABLE OF edidd,
          ls_idoc_data    TYPE edidd,
          lt_edidc        TYPE STANDARD TABLE OF edidc,
          ls_edidc        TYPE edidc,
          lt_idoc_status  TYPE STANDARD TABLE OF bdidocstat,
          lt_return_var   TYPE STANDARD TABLE OF bdwfretvar,
          lt_serial_info  TYPE STANDARD TABLE OF bdi_ser,
          ls_idoc_e1komg  TYPE e1komg,
          ls_idoc_e1komg1 TYPE e1komg1,
          ls_idoc_e1konh  TYPE e1konh,
          ls_idoc_e1konp  TYPE e1konp,
          ls_idoc_e1konm  TYPE e1konm.

    DATA: lv_counter        TYPE i VALUE 0,
          lv_docnum         TYPE edi_docnum VALUE '0000000000000000',
          lv_uom            TYPE char3,
          lv_matnr_vakey    TYPE char18,
          lv_e1komg_segnum  TYPE idocdsgnum,
          lv_e1komg1_segnum TYPE idocdsgnum,
          lv_e1konh_segnum  TYPE idocdsgnum,
          lv_e1konp_segnum  TYPE idocdsgnum,
          lv_e1konm_segnum  TYPE idocdsgnum.

    DATA: ls_edids TYPE edids,
          lv_subrc TYPE sysubrc.

    DATA: lt_delete_prices TYPE zpos_pricelist_tty,
          lt_insert_prices TYPE zpos_pricelist_tty.


    lt_insert_prices = map_persist_data( ).

    IF NOT lt_insert_prices IS INITIAL.

*------------------------------- CONTROL RECORD ------------------------------------
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

      ls_edidc-rcvprn = ls_edidc-sndprn.
      ls_edidc-rcvprt = ls_edidc-sndprt = 'LS'.

      ls_edidc-status = '62'.
      ls_edidc-direct = '2'.
      ls_edidc-mestyp = 'COND_A'.
      ls_edidc-idoctp = 'COND_A04'.
      ls_edidc-mesfct = 'WES'.

      ls_edidc-credat = sy-datum.
      ls_edidc-cretim = sy-uzeit.
*------------------------------- CONTROL RECORD ------------------------------------


* Prepare for a Binary Read
      SORT mt_wesout_price BY matnr kmein vkorg vtweg werks.


      LOOP AT lt_insert_prices ASSIGNING FIELD-SYMBOL(<ls_price_record>).

* Create IDocs - One Per Condition Record
        CLEAR: lv_matnr_vakey,ls_idoc_data.
        lv_e1komg_segnum = '09'.
        lv_e1konh_segnum = '19'.
        lv_e1konp_segnum = '29'.
        lv_e1konm_segnum = '39'.

        lv_docnum = lv_docnum + 1.

        ls_edidc-docnum = lv_docnum.
        APPEND ls_edidc TO lt_edidc.

* The IDoc needs to have ISO units of Measure, where are later on converted back to internal
        CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
          EXPORTING
            sap_code    = <ls_price_record>-vrkme
          IMPORTING
            iso_code    = lv_uom
          EXCEPTIONS
            not_found   = 1
            no_iso_code = 2
            OTHERS      = 3.
        IF sy-subrc <> 0.
          CLEAR ms_textid.
          ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
          ms_textid-msgno = '008'.
          ms_textid-attr1 = <ls_price_record>-vrkme.
          mx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
          RAISE EXCEPTION mx_exception.
        ENDIF.


*------------------------------- E1KOMG ------------------------------------

        ls_idoc_data-docnum = lv_docnum.
        ls_idoc_data-segnam = 'E1KOMG'.
        ls_idoc_data-hlevel =  '02'.
        lv_e1komg_segnum += 1.
        ls_idoc_data-segnum = lv_e1komg_segnum.
        ls_idoc_e1komg-kvewe = co_usage.
        ls_idoc_e1komg-kotabnr = zcl_pos_util=>co_condition_table_991.
        ls_idoc_e1komg-kappl = zcl_pos_util=>co_application.
        ls_idoc_e1komg-kschl = zcl_pos_util=>co_condition_type-effective_price.

        "Set the varkey - The number of BLANK SPACES here is incredibly important - It is 18 Characters Long!
*      CONCATENATE '1090' '90' 'C003' 'M_CR_3012         ' 'ST' INTO DATA(lv_varkey) RESPECTING BLANKS.
        OVERLAY lv_matnr_vakey WITH <ls_price_record>-matnr.
        CONCATENATE <ls_price_record>-vkorg
                    <ls_price_record>-vtweg
                    <ls_price_record>-werks
                    lv_matnr_vakey           "Needs to be 18 characters long in total
                    lv_uom
                    INTO ls_idoc_e1komg-vakey RESPECTING BLANKS.

        ls_idoc_e1komg-vakey_long = ls_idoc_e1komg-vakey.

        ls_idoc_e1komg-vrkme = lv_uom.
        ls_idoc_data-sdata = ls_idoc_e1komg.
        APPEND ls_idoc_data TO lt_edidd.
        CLEAR : ls_idoc_data.
*------------------------------- E1KOMG ------------------------------------


*------------------------------- E1KOMG1 ------------------------------------

        ls_idoc_data-docnum = lv_docnum.
        ls_idoc_data-segnam = 'E1KOMG1'.
        ls_idoc_data-hlevel =  '03'.
        ls_idoc_data-psgnum = lv_e1komg_segnum.
        lv_e1komg1_segnum += 1.
        ls_idoc_data-segnum = lv_e1komg1_segnum.
        ls_idoc_e1komg1-vadat_255 =  <ls_price_record>-eff_price_type && <ls_price_record>-eff_price_fkey && <ls_price_record>-std_price_fkey && <ls_price_record>-eff_price_level.
        ls_idoc_data-sdata = ls_idoc_e1komg1.
        APPEND ls_idoc_data TO lt_edidd.
        CLEAR : ls_idoc_data.

*------------------------------- E1KOMG1 ------------------------------------


*------------------------------- E1KOMH ------------------------------------

        ls_idoc_data-docnum = lv_docnum.
        ls_idoc_data-segnam = 'E1KONH'.
        ls_idoc_data-hlevel =  '03'.
        ls_idoc_data-psgnum = lv_e1komg_segnum.
        lv_e1konh_segnum += 1.
        ls_idoc_data-segnum = lv_e1konh_segnum.
        ls_idoc_e1konh-datab = <ls_price_record>-datab.
        ls_idoc_e1konh-datbi = <ls_price_record>-datbi.
        ls_idoc_data-sdata = ls_idoc_e1konh.
        APPEND ls_idoc_data TO lt_edidd.
        CLEAR : ls_idoc_data.

*------------------------------- E1KOMH ------------------------------------


*------------------------------- E1KOMP ------------------------------------

        ls_idoc_data-docnum = lv_docnum.
        ls_idoc_data-segnam = 'E1KONP'.
        ls_idoc_data-hlevel =  '04'.
        ls_idoc_data-psgnum = lv_e1konh_segnum.
        lv_e1konp_segnum += 1.
        ls_idoc_data-segnum = lv_e1konp_segnum.

        ls_idoc_e1konp-kschl = zcl_pos_util=>co_condition_type-effective_price.
        ls_idoc_e1konp-stfkz = 'A'.
        ls_idoc_e1konp-kzbzg = 'C'.
        ls_idoc_e1konp-konws = <ls_price_record>-currency.
        ls_idoc_e1konp-krech = 'C'.
        ls_idoc_e1konp-kbetr = <ls_price_record>-eff_price.
        ls_idoc_e1konp-konwa = <ls_price_record>-currency.
        ls_idoc_e1konp-kpein = '1'.
        ls_idoc_e1konp-kmein = lv_uom.
        ls_idoc_e1konp-kwaeh = <ls_price_record>-currency.

        ls_idoc_data-sdata = ls_idoc_e1konp.
        APPEND ls_idoc_data TO lt_edidd.
        CLEAR : ls_idoc_data.

*------------------------------- E1KOMH ------------------------------------

*------------------------------------- SCALES -----------------------------------------------------------

* Optimization in case of large volumes of data
        READ TABLE mt_wesout_price WITH KEY
                                    matnr = <ls_price_record>-matnr
                                    kmein = <ls_price_record>-vrkme
                                    vkorg = <ls_price_record>-vkorg
                                    vtweg = <ls_price_record>-vtweg
                                    werks = <ls_price_record>-werks TRANSPORTING NO FIELDS.

* ==================================================================================================
*   No entry found - Strange Scenario which should never occur!
* ==================================================================================================
        IF sy-subrc <> 0.
          ASSERT 1 = 2.
          "Want to understand when this scenario could occur first.
        ELSE.
          DATA(lv_wesout_index) = sy-tabix.

* Optimization - Read forward from the tabix pointer
          LOOP AT mt_wesout_price INTO DATA(ls_wesout_head) FROM lv_wesout_index .

            IF ls_wesout_head-matnr <> <ls_price_record>-matnr OR
               ls_wesout_head-kmein <> <ls_price_record>-vrkme OR
               ls_wesout_head-vkorg <> <ls_price_record>-vkorg OR
               ls_wesout_head-vtweg <> <ls_price_record>-vtweg OR
               ls_wesout_head-werks <> <ls_price_record>-werks.
*            All prices for current article / store have been procesed return and continue with the next article
              EXIT.
            ENDIF.

            READ TABLE ls_wesout_head-item WITH KEY kschl = zcl_pos_util=>co_condition_type-effective_price INTO DATA(ls_wesout_item).

            IF sy-subrc = 0.

              LOOP AT ls_wesout_item-scale INTO DATA(ls_scale).

*------------------------------- E1KONM ------------------------------------

                ls_idoc_data-docnum = lv_docnum.
                ls_idoc_data-segnam = 'E1KONM'.
                ls_idoc_data-hlevel =  '05'.
                ls_idoc_data-psgnum = lv_e1konp_segnum.
                lv_e1konm_segnum += 1.
                ls_idoc_data-segnum = lv_e1konm_segnum.
                ls_idoc_e1konm-kstbm = ls_scale-kstbm.
                ls_idoc_e1konm-kbetr = ls_scale-kbetr.
                ls_idoc_data-sdata = ls_idoc_e1konm.
                APPEND ls_idoc_data TO lt_edidd.
                CLEAR : ls_idoc_data.

*------------------------------- E1KONM ------------------------------------

              ENDLOOP.        " Looping over the scale lines

            ENDIF.            " Did we find the relevant condition type in the WESOUT Item table

          ENDLOOP.            " Did we find the matching matnr / price record in WESOUT Header table

        ENDIF.                " From BINARY READ

        lv_counter += 1.
* Only process 50 records at a time
        IF ( lv_counter MOD 50 ) = 0 .

          CALL FUNCTION 'IDOC_INPUT_COND_A'
            EXPORTING
              input_method          = 'A'
              mass_processing       = 'X'
            TABLES
              idoc_contrl           = lt_edidc
              idoc_data             = lt_edidd
              idoc_status           = lt_idoc_status
              return_variables      = lt_return_var
              serialization_info    = lt_serial_info
            EXCEPTIONS
              wrong_function_called = 1
              OTHERS                = 2.

          IF sy-subrc <> 0.
            CALL FUNCTION 'RDM_OPT_MESSAGE_ADD'
              EXPORTING
                i_probclass = sr_co->msg_probclass_1
                i_sortfield = sr_co->sortfield_merchandise
                i_msgid     = 'ZPOS'
                i_msgty     = sr_co->msg_type_error
                i_msgno     = '001'
*               i_msgv1     = lv_matnr
*               i_msgv2     = <ls_package>-werks
*               i_msgv3     = <ls_package>-vkorg
*               i_msgv4     = <ls_package>-vtweg
              CHANGING
                xt_messages = ct_messages.
            EXIT.
          ENDIF.

          CLEAR: lt_edidc, lt_edidd, lt_idoc_status, lt_return_var, lt_serial_info.

        ENDIF.

* Testing IDoc Problems.
        IF lv_idoc_test = abap_true.
          CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
            EXPORTING
              pi_status_message      = ls_edids
            IMPORTING
              pe_state_of_processing = lv_subrc
            TABLES
              t_data_records         = lt_edidd
            CHANGING
              pc_control_record      = ls_edidc
            EXCEPTIONS
              idoc_not_saved         = 1
              OTHERS                 = 2.

          IF sy-subrc = 0 AND NOT ls_edidc-docnum IS INITIAL.
            MESSAGE i000(zdta) WITH 'IDoc Created' && ls_edidc-docnum.
          ENDIF.
        ENDIF.

      ENDLOOP.

* Process any final reccords not caught above.
      IF NOT lt_edidd IS INITIAL.
        CALL FUNCTION 'IDOC_INPUT_COND_A'
          EXPORTING
            input_method          = 'A'
            mass_processing       = 'X'
          TABLES
            idoc_contrl           = lt_edidc
            idoc_data             = lt_edidd
            idoc_status           = lt_idoc_status
            return_variables      = lt_return_var
            serialization_info    = lt_serial_info
          EXCEPTIONS
            wrong_function_called = 1
            OTHERS                = 2.

        IF sy-subrc <> 0.
          CALL FUNCTION 'RDM_OPT_MESSAGE_ADD'
            EXPORTING
              i_probclass = sr_co->msg_probclass_1
              i_sortfield = sr_co->sortfield_merchandise
              i_msgid     = 'ZPOS'
              i_msgty     = sr_co->msg_type_error
              i_msgno     = '001'
*             i_msgv1     = lv_matnr
*             i_msgv2     = <ls_package>-werks
*             i_msgv3     = <ls_package>-vkorg
*             i_msgv4     = <ls_package>-vtweg
            CHANGING
              xt_messages = ct_messages.
          EXIT.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD copy_article_status_data.

    DATA: lt_persist_price TYPE zpos_pricelist_tty,
          lt_artprc_status TYPE STANDARD TABLE OF zpos_artprc_sts.

* It could be the case that pricing and article data was created for a store but that the recipient information was not maintained correctly.
* There is a utility to retrospectively create the necessary status records.

* Inputs are the store and the target application
* The receipient data needs to have been added to table ZPOS_STORE_RCPT
* This will then create the missing status records

    SELECT * FROM zpos_store_rcpt
           INTO TABLE @DATA(lt_recipient)
           WHERE vkorg         = @iv_vkorg
           AND   werks         = @iv_werks
           AND   pos_recipient = @iv_recipient.

    IF lt_recipient IS INITIAL.
      MESSAGE TEXT-001 TYPE 'E'.
    ENDIF.

    LOOP AT lt_recipient INTO DATA(ls_recipient). " Can really only be one.

      SELECT
         t1~mandt,
         t1~vkorg,
         t1~vtweg,
         t1~werks,
         t1~matnr,
         t1~ean11,
         t1~vrkme,
         t1~datbi
         FROM zpos_pricelist AS t1
         INTO TABLE @lt_persist_price
         WHERE t1~vkorg EQ @ls_recipient-vkorg
         AND t1~werks EQ @ls_recipient-werks
         ORDER BY vkorg, vtweg, werks, matnr, ean11, vrkme, datbi.

      DATA(lv_timestamp) = sy-datum && sy-uzeit.
      LOOP AT lt_persist_price INTO DATA(ls_persist_price).

        APPEND INITIAL LINE TO lt_artprc_status ASSIGNING FIELD-SYMBOL(<ls_artprc_status>).
        <ls_artprc_status>-key                  = ls_persist_price-key.
        <ls_artprc_status>-pos_recipient        = ls_recipient-pos_recipient.
        <ls_artprc_status>-timestamp            = lv_timestamp.                   "Same value for all records created at one time
        <ls_artprc_status>-pos_recipient_status = zcl_pos_util=>co_recipient_status-created.
        <ls_artprc_status>-created_by           = sy-uname.
      ENDLOOP.

    ENDLOOP.

    IF NOT lt_artprc_status IS INITIAL.
      MODIFY zpos_artprc_sts FROM TABLE lt_artprc_status.
      MESSAGE i000(zpos) WITH sy-dbcnt ' entries added to table ZPOS_ARTPRC_STATUS'.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

  ENDMETHOD.


  METHOD calculate_net_price.

    rv_net_price = iv_input_price / ( 1 + abs( iv_tax_rate / 100 ) ).

  ENDMETHOD.


  METHOD append_wesout_price.

    APPEND INITIAL LINE TO mt_wesout_price ASSIGNING FIELD-SYMBOL(<ls_wesout_price>).
    MOVE-CORRESPONDING is_price TO <ls_wesout_price>.
    <ls_wesout_price>-werks = iv_werks.

  ENDMETHOD.


  METHOD get_article_price.

    SELECT SINGLE vkorg, vtweg, werks, matnr, ean11, vrkme, datbi, datab,
                  eff_price, std_price, comp_qty, comp_uom, net_contents, is_promo_price,
                  currency
       FROM zposi_article_price
       INTO CORRESPONDING FIELDS OF @rs_pricelist
       WHERE vkorg = @iv_vkorg
       AND   vtweg = @iv_vtweg
       AND   werks = @iv_werks
       AND   matnr = @iv_matnr
       AND   ean11 = @iv_ean11
       AND   datab <= @iv_valid_from
       AND   datbi >= @iv_valid_to.

*=====================================
* Effective Pricing --> Comparative Price
*=====================================
    IF NOT rs_pricelist-net_contents IS INITIAL.
      rs_pricelist-eff_comp_price = ( rs_pricelist-eff_price * ( rs_pricelist-comp_qty ) / rs_pricelist-net_contents ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

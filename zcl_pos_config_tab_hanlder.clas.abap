class ZCL_POS_CONFIG_TAB_HANLDER definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_VKORG type VKORG .
  class-methods DELETE_ALL_ENTRIES .
  class-methods INSERT_ENTRIES
    importing
      !IT_TABLE type ZPOS_CONFIG_MAP_TTY .
  methods IS_SET_DISCOUNT_ACTIVE
    returning
      value(RV_ACTIVE) type ABAP_BOOL .
  methods IS_PRICE_EXTRA_SCALES_ACTIVE
    returning
      value(RV_ACTIVE) type ABAP_BOOL .
  methods IS_MAGENTO_PRICE_CHECK_ACTIVE
    returning
      value(RV_ACTIVE) type ABAP_BOOL .
  methods IS_OFFER_EXTRA_SCALES_ACTIVE
    returning
      value(RV_ACTIVE) type ABAP_BOOL .
  methods IS_DISCGROUP_ACTIVE
    returning
      value(RV_ACTIVE) type ABAP_BOOL .
  methods GET_VK_TAX_CODE
    importing
      !IV_SOURCE2 type ZPOS_SOURCE2
    returning
      value(RV_TAX_CODE) type MWSKZ
    raising
      ZCX_POS_EXCEPTION .
  PROTECTED SECTION.
*"* protected components of class ZCL_POS_MAP_TAB1_HANDLER
*"* do not include other source files here!!!
private section.

  constants CO_ACTIVE type ZPOS_SOURCE1 value 'ACTIVE' ##NO_TEXT.
  constants CO_DSR type ZPOS_GRPID value 'ZDSR' ##NO_TEXT.
  constants CO_DUMMY type ZPOS_SOURCE1 value 'DUMMY' ##NO_TEXT.
  data MS_TABLE_LINE type ZPOS_CONFIG_MAP_STY .
  data MT_TABLE type ZPOS_CONFIG_MAP_TTY .
  data MV_SELECTION_STRING type STRING .

  methods IS_ACTIVE
    importing
      !IV_GROUPID type ZPOS_GRPID
      !IV_SOURCEVAL1 type ZPOS_SOURCE1 optional
      !IV_SOURCEVAL2 type ZPOS_SOURCE2 optional
      !IV_SOURCEVAL3 type ZPOS_SOURCE3 optional
    returning
      value(RV_ACTIVE) type BOOLE_D .
*"* private components of class ZCL_POS_CONFIG_TAB_HANDLER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_POS_CONFIG_TAB_HANLDER IMPLEMENTATION.


  METHOD constructor.

    DATA:          lt_salesorg   TYPE vkorg_ran_itab.
    FIELD-SYMBOLS: <ls_table_line> TYPE zpos_config_map_sty.

*==========================================================================================
* All entries where the full profile match the table entries
*==========================================================================================
    APPEND INITIAL LINE TO lt_salesorg ASSIGNING FIELD-SYMBOL(<ls_r_salesorg>).

    <ls_r_salesorg>-sign   = zcl_pos_util=>co_selopt_sign_i."'I'.
    <ls_r_salesorg>-option = zcl_pos_util=>co_selopt_opt_eq."'EQ'.
    <ls_r_salesorg>-low = iv_vkorg.

*==========================================================================================
* All entries where the vkorg is blank should also be included.
*==========================================================================================
    APPEND INITIAL LINE TO lt_salesorg ASSIGNING <ls_r_salesorg>.
    <ls_r_salesorg>-sign   = zcl_pos_util=>co_selopt_sign_i."'I'.
    <ls_r_salesorg>-option = zcl_pos_util=>co_selopt_opt_eq."'EQ'.
    <ls_r_salesorg>-low = zcl_pos_util=>co_all.

    CLEAR mt_table.
    SELECT * FROM zpos_config_map INTO TABLE mt_table WHERE vkorg IN lt_salesorg.

* This is handled in the IS_ACTIVE method

*    SORT mt_table BY groupid sourceval1 sourceval2 sourceval3 vkorg.
*    DELETE ADJACENT DUPLICATES FROM mt_table COMPARING groupid sourceval1 sourceval2 sourceval3.

    IF sy-subrc IS INITIAL.
      LOOP AT mt_table ASSIGNING <ls_table_line>.
        TRANSLATE <ls_table_line>-sourceval1 TO UPPER CASE.
        TRANSLATE <ls_table_line>-sourceval2 TO UPPER CASE.
        TRANSLATE <ls_table_line>-sourceval3 TO UPPER CASE.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD DELETE_ALL_ENTRIES.
* This method removes all entries from the table zdta_config_map.
* Currently this is only used in the generic upload programme.
    DELETE FROM zpos_config_map.

  ENDMETHOD.


  METHOD get_vk_tax_code.

    DATA: lt_table TYPE zpos_config_map_tty.

    LOOP AT mt_table INTO DATA(ls_table_line)
                          WHERE groupid    = zcl_pos_util=>co_viking
                          AND   sourceval1 = zcl_pos_util=>co_tax
                          AND   sourceval2 = iv_source2           "SAP Tax Code
                          AND   active     = abap_true.

      APPEND ls_table_line TO lt_table .
    ENDLOOP.

    IF lt_table IS NOT INITIAL.
      SORT lt_table BY sourceval1 vkorg DESCENDING.

      READ TABLE lt_table INDEX 1 INTO  ls_table_line.
      IF sy-subrc IS INITIAL.
        rv_tax_code = ls_table_line-targetval.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e042(zpos) with iv_source2.
    ENDIF.

  ENDMETHOD.


  METHOD INSERT_ENTRIES.

* This method is used to populate the zpos_config_map table and is currently only used in the generic upload programme.
* The method receives a table of entries which are inserted into the DB table.
* This method is used in programme ZPOS_GENERIC_UPLOAD_UTIL_IMP in method upd_map_tab1.

    CHECK it_table IS NOT INITIAL.
    INSERT zpos_config_map FROM TABLE it_table ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.


  METHOD is_active.

* This is a generic method to check if a functionality as governed by a group id in config mapping table.
* For example if logging of UMS aggregation is active or not

    DATA: lt_table TYPE STANDARD TABLE OF zpos_config_map.

    CLEAR: rv_active,
           lt_table.

* First set it to false, later it can be set to true if needed.
    rv_active = abap_false.

    LOOP AT mt_table INTO DATA(ls_table_line) WHERE groupid = iv_groupid
                                              AND sourceval1 = iv_sourceval1
                                              AND sourceval2 = iv_sourceval2
                                              AND sourceval3 = iv_sourceval3.
      APPEND ls_table_line TO lt_table.
    ENDLOOP.

    IF lt_table IS NOT INITIAL.
      SORT lt_table BY groupid ASCENDING vkorg DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_table COMPARING groupid.

      CLEAR ls_table_line.
      READ TABLE lt_table INTO ls_table_line INDEX 1.
      IF sy-subrc IS INITIAL.
* Return the Active Flag (True or False)
        rv_active = ls_table_line-active.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD is_discgroup_active.

* Used to determine if the mapping of BBYs to Discount Groups is Active or not

* This might be used in Sweden to map certain types of simple BBY as Discount Groups instead of Baskets

    rv_active = is_active( iv_groupid    = zcl_pos_util=>co_viking
                           iv_sourceval1 = zcl_pos_util=>co_offer
                           iv_sourceval2 = zcl_pos_util=>co_discount_group ).

  ENDMETHOD.


  METHOD IS_SET_DISCOUNT_ACTIVE.

* Used to determine if the setting <SetDiscount>1</SetDiscount> should be active for a Sales Org e.g. Sweden

* This optino is used in Discount Groups and controls what happens with scale pricing, so the customer only
* get discount for the fixed intervals (true), or should the scale continue when you reach the threshold

    rv_active = is_active( iv_groupid    = zcl_pos_util=>co_viking
                           iv_sourceval1 = zcl_pos_util=>co_offer
                           iv_sourceval2 = zcl_pos_util=>co_set_discount ).

  ENDMETHOD.


  METHOD is_magento_price_check_active.

* We have a requirement to check that article data has already been sent to magento
* before creating the pricing records - here we have the ability to turn this on/off
* via the configuration table ZPOS_CONFIG_MAP

    rv_active = is_active( iv_groupid    = zcl_pos_util=>co_magento
                           iv_sourceval1 = zcl_pos_util=>co_price
                           iv_sourceval2 = zcl_pos_util=>co_article_check ).

  ENDMETHOD.


  METHOD IS_OFFER_EXTRA_SCALES_ACTIVE.

* There may be situations where the generation of extra pricing scales is useful
* Input  --> 3 = 200, 6 = 350
* Output --> 3 = 200, 6 = 350, 9 = 550, 12 = 700 ..etc
* This function checks if that feature is active or not

    rv_active = is_active( iv_groupid    = zcl_pos_util=>co_viking
                           iv_sourceval1 = zcl_pos_util=>co_offer
                           iv_sourceval2 = zcl_pos_util=>co_extra_scales ).

  ENDMETHOD.


  METHOD IS_PRICE_EXTRA_SCALES_ACTIVE.

* There may be situations where the generation of extra pricing scales is useful
* Input  --> 3 = 200, 6 = 350
* Output --> 3 = 200, 6 = 350, 9 = 550, 12 = 700 ..etc
* This function checks if that feature is active or not

    rv_active = is_active( iv_groupid    = zcl_pos_util=>co_viking
                           iv_sourceval1 = zcl_pos_util=>co_offer
                           iv_sourceval2 = zcl_pos_util=>co_extra_scales ).

  ENDMETHOD.
ENDCLASS.

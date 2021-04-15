CLASS zcl_pos_article_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_report_data,
        vkorg          TYPE vkorg,
        werks          TYPE werks_d,
        matnr          TYPE matnr,
        description    TYPE maktx,
        uom            TYPE vrkme,
        changed_on     TYPE hdb_timestamp,
        success        TYPE boole_d,
        is_bundle      TYPE boole_d,
        http_status    TYPE zpos_http_status,
        http_message   TYPE zpos_http_message,
        assign_status  TYPE zpos_http_status,
        assign_message TYPE zpos_http_message,
        color          TYPE lvc_t_scol,               " ALV OO type
        style          TYPE lvc_t_styl,
      END OF ty_s_report_data .
    TYPES:
      ty_t_report_data TYPE STANDARD TABLE OF ty_s_report_data .

    CONSTANTS:
      BEGIN OF co_endpoint,
        product  TYPE char1 VALUE 'P',
        bundle   TYPE char1 VALUE 'B',
        websites TYPE char1 VALUE 'W',
      END OF co_endpoint .


    CLASS-METHODS article_collective_handler
      IMPORTING
        !iv_status   TYPE zpos_recipient_status
        !it_r_werks  TYPE wrf_werks_rtty OPTIONAL
        !iv_valid_on TYPE dats
        !iv_vkorg    TYPE vkorg
        !it_r_matnr  TYPE wrf_matnr_rtty OPTIONAL
      RAISING
        zcx_pos_exception .
  PROTECTED SECTION.
private section.

  constants:
    BEGIN OF co_alv_color,
        yellow TYPE lvc_col VALUE '3', "
        green  TYPE lvc_col VALUE '5', "
        red    TYPE lvc_col VALUE '6', "
      END OF co_alv_color .
  class-data MO_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  class-data MO_RESPONSE type ref to IF_REST_ENTITY .
  class-data MO_REST_CLIENT type ref to CL_REST_HTTP_CLIENT .
  class-data MR_BCS_EXCEPTION type ref to CX_BCS .
  class-data MR_COLUMN type ref to CL_SALV_COLUMN_TABLE .
  class-data MR_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  class-data MR_DISPLAY type ref to CL_SALV_DISPLAY_SETTINGS .
  class-data MR_DOCUMENT type ref to CL_DOCUMENT_BCS .
  class-data MR_EVENTS type ref to CL_SALV_EVENTS_TABLE .
  class-data MR_FUNCTIONAL_SETTINGS type ref to CL_SALV_FUNCTIONAL_SETTINGS .
  class-data MR_FUNCTIONS type ref to CL_SALV_FUNCTIONS_LIST .
  class-data MR_INT_SENDER type ref to CL_CAM_ADDRESS_BCS .
  class-data MR_OVERFLOW type ref to CX_SY_ARITHMETIC_OVERFLOW .
  class-data MR_TABLE type ref to CL_SALV_TABLE .
  class-data MR_TOOLTIPS type ref to CL_SALV_TOOLTIPS .
  class-data MR_ZERODIVIDE type ref to CX_SY_ZERODIVIDE .
  class-data MS_COLOR type LVC_S_SCOL .
  class-data:
    BEGIN OF ms_error_response,
        message TYPE string,
      END OF ms_error_response .
  class-data MS_SELTAB type RSPARAMS .
  class-data MS_TEXTID type SCX_T100KEY .
  class-data MT_ECOM_STATUS type ZECM_ARTICLE_STS_TTY .
  class-data MT_HEX type SOLIX_TAB .
  class-data MT_REPORT_DATA type TY_T_REPORT_DATA .
  class-data:
    mt_website TYPE STANDARD TABLE OF zpos_website .
  class-data MV_TOKEN type STRING value 'Bearer j86uwiz4vlul7ifmo6c8ja0xx62762q4' ##NO_TEXT.

  class-methods ALV_GET_ARTICLE_JSON
    importing
      !IV_SKU type STRING
    raising
      ZCX_POS_EXCEPTION .
  class-methods ARTICLE_CHECK_STATUS
    importing
      !IS_ARTICLE_PRICE type ZPOS_ARTICLE_PRICE_STY
    returning
      value(RS_STATUS) type ZECM_ARTICLE_STS
    raising
      ZCX_POS_EXCEPTION .
  class-methods ARTICLE_SINGLE_HANDLER
    importing
      !IS_ARTICLE_PRICE type ZPOS_ARTICLE_PRICE_STY
    returning
      value(RS_STATUS) type ZECM_ARTICLE_STS
    raising
      ZCX_POS_EXCEPTION .
  class-methods ASSIGN_ARTICLE_TO_WEBSITE
    importing
      !IS_ARTICLE_PRICE type ZPOS_ARTICLE_PRICE_STY
    returning
      value(RS_STATUS) type ZECM_ARTICLE_STS
    raising
      ZCX_POS_EXCEPTION .
  class-methods BUNDLE_CHECK_CHILDREN
    importing
      !IS_ARTICLE_PRICE type ZPOS_ARTICLE_PRICE_STY
    returning
      value(RV_EXISTS) type BOOLE_D
    raising
      ZCX_POS_EXCEPTION .
  class-methods BUNDLE_DELETE_CHILD
    importing
      !IV_SKU type STRING
      !IV_OPTION_ID type STRING
    returning
      value(RV_BODY) type STRING
    raising
      ZCX_POS_EXCEPTION .
  class-methods BUNDLE_GET_CHILDREN
    importing
      !IV_SKU type STRING
    returning
      value(RT_CHILDREN) type ZPOS_MAG_PROD_LINKS_TTY
    raising
      ZCX_POS_EXCEPTION .
  class-methods DISPLAY_STATUS_ALV .
  class-methods GET_RECORDS_FOR_PROCESSING
    importing
      !IV_STATUS type ZPOS_RECIPIENT_STATUS
      !IT_R_WERKS type WRF_WERKS_RTTY optional
      !IV_VALID_ON type DATS
      !IV_VKORG type VKORG
      !IT_R_MATNR type WRF_MATNR_RTTY optional
    returning
      value(RT_ARTICLE_PRICE) type ZPOS_ARTICLE_PRICE_TTY
    raising
      ZCX_POS_EXCEPTION .
  class-methods MAP_ARTICLE_DATA
    importing
      !IS_ARTICLE_PRICE type ZPOS_ARTICLE_PRICE_STY
      !IV_BUNDLE type BOOLE_D
      !IV_EXISTS type BOOLE_D
    returning
      value(RV_BODY) type STRING
    raising
      ZCX_POS_EXCEPTION .
  class-methods ON_ALV_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
  class-methods UPDATE_ARTICLE_STATUS
    importing
      !IT_ARTICLE_STATUS type ZECM_ARTICLE_STS_TTY
    returning
      value(RV_COUNT) type SYST_DBCNT .
  class-methods UPDATE_ARTPRC_STATUS
    importing
      !IT_ARTPRC_STATUS type ZPOS_ARTPRC_STS_TTY
    returning
      value(RV_COUNT) type SYST_DBCNT .
ENDCLASS.



CLASS ZCL_POS_ARTICLE_API IMPLEMENTATION.


  METHOD article_collective_handler.

*"----------------------------------------------------------------------
*  This is the main entry point for processing articles and sending the
*  updates to Magento.
*"----------------------------------------------------------------------

    DATA: ls_ecom_status   TYPE zecm_article_sts,
          lt_ecom_status   TYPE zecm_article_sts_tty,
          lt_artprc_status TYPE zpos_artprc_sts_tty,
          lt_artprice_sts  TYPE STANDARD TABLE OF zpos_artprc_sts.


    TRY.
*"----------------------------------------------------------------------
*  Get the relevant records which need to be sent
*"---------------------------------------------------------------------
        DATA(lt_article_price) = get_records_for_processing(
          EXPORTING
            iv_status        = iv_status
            it_r_werks       = it_r_werks
            iv_valid_on      = iv_valid_on
            iv_vkorg         = iv_vkorg
            it_r_matnr       = it_r_matnr ).

      CATCH zcx_pos_exception. " POS Exception Class
        MESSAGE i000(zpos) WITH 'No relevant entries found - Processing Terminated'.
        RETURN.
    ENDTRY.


    DATA(lv_total_records) = lines( lt_article_price ).


*"----------------------------------------------------------------------
*  Loop on the list of articles - processing one at a time
*"---------------------------------------------------------------------
    LOOP AT lt_article_price ASSIGNING FIELD-SYMBOL(<ls_article_price>).

      CLEAR: ls_ecom_status.


*"----------------------------------------------------------------------
*  Progress indicator as this method is called exclusively from a report
*"---------------------------------------------------------------------
      cl_progress_indicator=>progress_indicate(
                i_text = |Processing Article: { <ls_article_price>-matnr } - Record { sy-tabix }/{ lv_total_records }|
                i_output_immediately = abap_true ).



*"----------------------------------------------------------------------
*  This is where the individual article is processed and the updates to
*  Magento are sent
*"---------------------------------------------------------------------
*    >>>>>>>>
      ls_ecom_status = article_single_handler( is_article_price = <ls_article_price> ).



*"----------------------------------------------------------------------
*  Here we come back from the Magento updates and we update various status
*  tables
*"---------------------------------------------------------------------



      IF cl_rest_status_code=>is_success( CONV #( ls_ecom_status-http_status ) ) = abap_true AND ls_ecom_status-http_message IS INITIAL.
        ls_ecom_status-http_message = cl_rest_status_code=>get_reason_phrase( CONV #( ls_ecom_status-http_status ) ).
      ENDIF.

      IF cl_rest_status_code=>is_success( CONV #( ls_ecom_status-website_http_status ) ) = abap_true AND ls_ecom_status-website_http_message IS INITIAL.
        ls_ecom_status-website_http_message = cl_rest_status_code=>get_reason_phrase( CONV #( ls_ecom_status-website_http_status ) ).
      ENDIF.

*"----------------------------------------------------------------------
*    Common Article Price Status Table
*"---------------------------------------------------------------------
      IF cl_rest_status_code=>is_success( CONV #( ls_ecom_status-http_status ) ) = abap_true.
        <ls_article_price>-pos_recipient_status = zcl_pos_util=>co_recipient_status-confirmed.
      ELSE.
        <ls_article_price>-pos_recipient_status = zcl_pos_util=>co_recipient_status-error.
      ENDIF.


      APPEND INITIAL LINE TO lt_artprc_status ASSIGNING FIELD-SYMBOL(<ls_artprc_status>).
      MOVE-CORRESPONDING <ls_article_price> TO <ls_artprc_status>.
      <ls_artprc_status>-timestamp    = sy-datum && sy-uzeit.
      <ls_artprc_status>-created_by   = sy-uname.



*"----------------------------------------------------------------------
*  Specific Ecom Status table
*"---------------------------------------------------------------------
      APPEND ls_ecom_status TO lt_ecom_status ASSIGNING FIELD-SYMBOL(<ls_ecom_status>).
      <ls_ecom_status>-changed_on   = sy-datum && sy-uzeit.
      <ls_ecom_status>-changed_by   = sy-uname.

      APPEND INITIAL LINE TO mt_report_data ASSIGNING FIELD-SYMBOL(<ls_report_data>).
      MOVE-CORRESPONDING <ls_article_price> TO <ls_report_data>.
      <ls_report_data>-changed_on      = <ls_ecom_status>-changed_on.
      <ls_report_data>-uom             = <ls_article_price>-vrkme.
      <ls_report_data>-is_bundle       = <ls_ecom_status>-is_bundle.
      <ls_report_data>-http_status     = <ls_ecom_status>-http_status.
      <ls_report_data>-http_message    = <ls_ecom_status>-http_message.
      <ls_report_data>-assign_status   = <ls_ecom_status>-website_http_status.
      <ls_report_data>-assign_message  = <ls_ecom_status>-website_http_message.
      <ls_report_data>-success         = cl_rest_status_code=>is_success( CONV #( <ls_ecom_status>-http_status ) ).

    ENDLOOP.


*"----------------------------------------------------------------------
*  Perform the updates
*"---------------------------------------------------------------------
    DATA(lv_count) = update_artprc_status( lt_artprc_status ).
    IF sy-batch = abap_false.
      MESSAGE i000(zpos) WITH lv_count ' entries added to ZPOS_ARTPRC_STS'.
    ENDIF.
    lv_count = update_article_status( lt_ecom_status ).
    IF sy-batch = abap_false.
      MESSAGE i000(zpos) WITH lv_count ' entries added / updated in ZECM_ARTICLE_STS'.
    ENDIF.


*"----------------------------------------------------------------------
*  Display the ALV results
*"---------------------------------------------------------------------
    IF sy-batch = abap_false.
      display_status_alv( ).
    ENDIF.


  ENDMETHOD.


  METHOD article_single_handler.

* Things we need to check here
*    --> Is it a bundle or a Simple Article
*    --> Does the article itself exist already  (update or create)
*    --> Is the article already assigned to the website?
*    --> If it is a bundle do all of the children exist as articles in their own right - they must exist first


    DATA: lv_sku    TYPE string,
*          lv_endpoint TYPE string,
          lv_exists_previously TYPE boole_d VALUE abap_true.




*"---------------------------------------------------------------------
*  Check whether the article exists in Magento
*"---------------------------------------------------------------------
    DATA(ls_existence_status) = article_check_status( is_article_price ).

    IF cl_rest_status_code=>gc_client_error_not_found = ls_existence_status-http_status.

* This is used to determine the body which is created below.
      lv_exists_previously = abap_false.

*     404: In case we specifically want to check the text for the message
*      LS_EXISTENCE_STATUS-HTTP_MESSAGE = The product that was requested doesn't exist. Verify the product and try again.

    ENDIF.
















*"---------------------------------------------------------------------
*  Bundle Precondition Check
*"---------------------------------------------------------------------
    IF is_article_price-material_category = zcl_pos_util=>co_material_category-bundle.

* This is defined as a bundle but has no BOM items - raise exception
      IF is_article_price-bom IS INITIAL.
        CLEAR ms_textid.
        ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
        ms_textid-msgno = '066'.
        ms_textid-attr1 = is_article_price-matnr. SHIFT ms_textid-attr1 LEFT DELETING LEADING '0'.
        DATA(lx_exception) = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
        RAISE EXCEPTION lx_exception.
      ELSE.

*  Check the existence of each of the children in the bundle
        IF bundle_check_children( is_article_price ) = abap_false.

* If the children do not exist yet then we need to exit the process.
          rs_status-http_status = cl_rest_status_code=>gc_client_error_precond_failed.
          rs_status-http_message = 'Some bundle items do not exist'.
          RETURN.
        ENDIF.

        rs_status-is_bundle = abap_true.

      ENDIF.

    ELSEIF is_article_price-bom IS NOT INITIAL.

* This is not defined as a bundle but has BOM items - raise exception
      CLEAR ms_textid.
      ms_textid-msgid = zcl_pos_util=>co_pos_msgid.
      ms_textid-msgno = '067'.
      ms_textid-attr1 = is_article_price-matnr. SHIFT ms_textid-attr1 LEFT DELETING LEADING '0'.
      lx_exception = NEW zcx_pos_exception( textid = ms_textid msgty = zcl_pos_util=>co_msgty-error ).
      RAISE EXCEPTION lx_exception.

    ENDIF.






*"---------------------------------------------------------------------
*  Ready to process next stage
*  We know whether the main article exists or not
*  If it is a bundle we know that the children must exist
*"---------------------------------------------------------------------
    MOVE-CORRESPONDING is_article_price TO rs_status.
    lv_sku = is_article_price-matnr. SHIFT lv_sku LEFT DELETING LEADING '0'.

* The body which is created depends on whether the article already exists in the global scope
    DATA(lv_body) = map_article_data( EXPORTING is_article_price = is_article_price iv_bundle = rs_status-is_bundle iv_exists = lv_exists_previously ).

* Create / Update the Main Article for the specific Country / Sales Org
    DATA(lv_endpoint) = zcl_pos_magento_rest_api=>build_endpoint_string( iv_data  = lv_sku
                                                                   iv_type  = co_endpoint-product
                                                                   iv_vkorg = is_article_price-vkorg ).

    zcl_pos_magento_rest_api=>rest_operation( EXPORTING iv_body      = lv_body
                                                        iv_endpoint  = lv_endpoint
                                                        iv_operation = zcl_pos_magento_rest_api=>co_http_operation-put
                           IMPORTING ev_http_message = rs_status-http_message ev_http_status = rs_status-http_status ).


* Check whether we need to assign the article to a website



    IF cl_rest_status_code=>is_success( CONV #( rs_status-http_status ) ) = abap_true.
* Did we do our update successfully - in this case we should only have to do the assignment if we were extending from one scope e.g. SV to another e.g. NO


      IF lv_exists_previously = abap_true.     " Did it already exist?
* If the article already exists then we MIGHT need to do the website assignment - this is different than creating
* When creating the article in the global scope it will automatically be assigned to the correct website ID.


        IF cl_rest_status_code=>is_success( CONV #( ls_existence_status-website_http_status  ) ) = abap_false.
* If the article is already assigned then the website_http_status should be 200 so no need to do an assignment

          DATA(ls_assignment_status) = assign_article_to_website( is_article_price ).
          rs_status-website_http_status  = ls_assignment_status-website_http_status.
          rs_status-website_http_message = ls_assignment_status-website_http_message.

        ELSE.
* Article is already assigned, lets make sure that this status is included in the rs_status
          rs_status-website_http_status  = ls_existence_status-website_http_status.
          rs_status-website_http_message = ls_existence_status-website_http_message.

        ENDIF.

      ELSE.
* If it did not previously exist and was successfully created then it will have been assgined to the website

        rs_status-website_http_status  = cl_rest_status_code=>gc_success_ok.
        rs_status-website_http_message = cl_rest_status_code=>get_reason_phrase( cl_rest_status_code=>gc_success_ok ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD bundle_check_children.

    DATA: lv_sku           TYPE string,
          ls_article_price TYPE zpos_article_price_sty,
          lt_status        TYPE zecm_article_sts_tty.

    ls_article_price = is_article_price.

* Loops through the articles in a sales set - and checks for their existence in Magento

    rv_exists = abap_true.    "default
    LOOP AT ls_article_price-bom INTO DATA(ls_child).

      lv_sku = ls_child-idnrk.
      SHIFT lv_sku LEFT DELETING LEADING '0'.

*"---------------------------------------------------------------------
*  Check the existence of each child in the bundle
*"---------------------------------------------------------------------
      ls_article_price-matnr = ls_child-idnrk.
      ls_article_price-ean11 = ls_child-bom_ean11.
      ls_article_price-vrkme = ls_child-meins.

      DATA(ls_status) = article_check_status( is_article_price ).



      IF ls_status-changed_on IS INITIAL. "Then no existing record was found

        APPEND ls_status TO lt_status ASSIGNING FIELD-SYMBOL(<ls_status>).
        <ls_status>-changed_on   = sy-datum && sy-uzeit.
        <ls_status>-changed_by   = sy-uname.
      ENDIF.

      IF cl_rest_status_code=>is_success( CONV #( ls_status-http_status ) ) = abap_false.
        rv_exists = abap_false.
        EXIT.     " Exit Loop early if a child article does not exist in Magento
      ENDIF.
    ENDLOOP.

    IF NOT lt_status IS INITIAL.
      update_article_status( lt_status ).
    ENDIF.

  ENDMETHOD.


  METHOD bundle_delete_child.

** //rest/V1/bundle-products/{sku}/options/{optionId}
*
*    DATA(lv_endpoint) = zcl_pos_magento_rest_api=>build_endpoint_string( iv_data = iv_sku iv_type = co_endpoint-bundle iv_suffix = '/options/' && iv_option_id ).
*
*    zcl_pos_magento_rest_api=>rest_operation( EXPORTING iv_endpoint = lv_endpoint iv_operation = zcl_pos_magento_rest_api=>co_http_operation-delete
*                           IMPORTING ev_http_status = DATA(lv_http_status) ev_http_message = DATA(lv_http_message) ev_body = rv_body ).
*
**  Error handling



  ENDMETHOD.


  METHOD bundle_get_children.

*    DATA(lv_endpoint) = zcl_pos_magento_rest_api=>build_endpoint_string( iv_data = iv_sku iv_type = co_endpoint-bundle iv_suffix = '/children' ).
*
*    zcl_pos_magento_rest_api=>rest_operation( EXPORTING iv_endpoint = lv_endpoint iv_operation = zcl_pos_magento_rest_api=>co_http_operation-get
*                           IMPORTING ev_http_status = DATA(lv_http_status) ev_http_message = DATA(lv_http_message) ev_body = DATA(lv_body) ).
*
*    CALL METHOD /ui2/cl_json=>deserialize( EXPORTING json = lv_body CHANGING data = rt_children ).


  ENDMETHOD.


  METHOD map_article_data.

    DATA:
      ls_article     TYPE        zpos_magento_product_sty,
      ls_custom_attr TYPE        zpos_mag_cust_attr_sty.

    ls_article-product-sku = is_article_price-matnr. SHIFT ls_article-product-sku LEFT DELETING LEADING '0'.

*"----------------------------------------------------------------------
*  Article Creation Block - Element only allowed in Creation
*"----------------------------------------------------------------------

    IF iv_exists = abap_false.

      READ TABLE mt_website
            INTO DATA(ls_website)
            WITH KEY vkorg = is_article_price-vkorg
                     werks = is_article_price-werks
                     BINARY SEARCH.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e034(zpos).
      ENDIF.

* This is now handled in a separate assignment method
*      APPEND INITIAL LINE TO ls_article-product-extension_attributes-website_ids ASSIGNING FIELD-SYMBOL(<ls_website_id>).
*      <ls_website_id> = ls_website-website_id.

      ls_article-product-visibility = 1.      "9 Will be replace with 0 later - after the conversion to JSON

      ls_article-product-attribute_set_id = ls_website-attr_set_id.



      APPEND INITIAL LINE TO ls_article-product-extension_attributes-category_links ASSIGNING FIELD-SYMBOL(<ls_category_link>).
      <ls_category_link>-position = 1.
      <ls_category_link>-category_id = ls_website-category_id.

    ENDIF.

    IF iv_bundle = abap_true.

      LOOP AT is_article_price-bom INTO DATA(ls_child).

        APPEND INITIAL LINE TO ls_article-product-extension_attributes-bundle_product_options ASSIGNING FIELD-SYMBOL(<ls_bundle_child>).
        <ls_bundle_child>-title = ls_child-description.
        <ls_bundle_child>-required = abap_true.
        <ls_bundle_child>-type = 'checkbox'.
        <ls_bundle_child>-position = ls_child-stlkn.

        APPEND INITIAL LINE TO <ls_bundle_child>-product_links ASSIGNING FIELD-SYMBOL(<ls_prod_link>).

        <ls_prod_link>-sku        = ls_child-idnrk. SHIFT <ls_prod_link>-sku LEFT DELETING LEADING '0'.
        <ls_prod_link>-qty        = ls_child-menge.
        <ls_prod_link>-position   = 1.        "Always 1
        <ls_prod_link>-is_default = abap_true.
        <ls_prod_link>-price      = 9.
        <ls_prod_link>-price_type = 9.
        <ls_prod_link>-can_change_quantity = 9.

      ENDLOOP.

    ENDIF.

    ls_article-product-name = is_article_price-description && '-' && sy-uzeit.
    ls_article-product-price = is_article_price-eff_price.
    ls_article-product-status = 1.
    ls_article-product-type_id = COND #( WHEN iv_bundle = abap_true THEN zcl_pos_util=>co_bundle ELSE zcl_pos_util=>co_simple ).

    ls_article-product-weight = is_article_price-gross_weight.
*    ls_article-product-extension_attributes-stock_item-qty = 100.


    IF NOT is_article_price-country_origin IS INITIAL.
      ls_custom_attr-attribute_code = 'country_of_manufacture'.
      ls_custom_attr-value = is_article_price-country_origin.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-net_weight IS INITIAL.
      ls_custom_attr-attribute_code = 'net_weight'.
      ls_custom_attr-value = is_article_price-net_weight.  SHIFT ls_custom_attr-value LEFT DELETING LEADING space.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF is_article_price-eff_price LT is_article_price-std_price.
      ls_custom_attr-attribute_code = 'ordpris'.
      ls_custom_attr-value = '9'.           "9 Will be replace with 0 later - after the conversion to JSON
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

*ls_custom_attr-attribute_code = 'dirdelfreight'.
*ls_custom_attr-value = '199.00'.
*APPEND ls_custom_attr TO ls_article-product-custom_attributes.
*
*ls_custom_attr-attribute_code = 'dirdeltime'.
*ls_custom_attr-value = '2 veckor'.
*APPEND ls_custom_attr TO ls_article-product-custom_attributes.
*
    IF NOT is_article_price-material_item_cat IS INITIAL.
      ls_custom_attr-attribute_code = 'dirdelyn'.
      ls_custom_attr-value = COND #( WHEN is_article_price-material_item_cat = 'BANS' THEN '1' ELSE '9' ).                "9 Will be replace with 0 later - after the conversion to JSON
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-vendor IS INITIAL.
      ls_custom_attr-attribute_code = 'vendor'.
      ls_custom_attr-value = is_article_price-vendor. SHIFT ls_custom_attr-value LEFT DELETING LEADING '0'.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-ean11 IS INITIAL.
      ls_custom_attr-attribute_code = 'ean'.
      ls_custom_attr-value = is_article_price-ean11.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-vrkme IS INITIAL.
      ls_custom_attr-attribute_code = 'unit'.
      ls_custom_attr-value = is_article_price-vrkme.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-comp_qty IS INITIAL.
      ls_custom_attr-attribute_code = 'ifactor'.
      ls_custom_attr-value = is_article_price-comp_qty.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-std_comp_price IS INITIAL.
      ls_custom_attr-attribute_code = 'ifactor_ordpris'.
      ls_custom_attr-value = is_article_price-std_comp_price.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-eff_comp_price IS INITIAL.
      ls_custom_attr-attribute_code = 'ifactor_price'.
      ls_custom_attr-value = is_article_price-eff_comp_price.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-comp_uom IS INITIAL.
      ls_custom_attr-attribute_code = 'ifactor_unit'.
      ls_custom_attr-value = is_article_price-comp_uom.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-freight_group IS INITIAL.
      ls_custom_attr-attribute_code = 'freightcat'.
      ls_custom_attr-value = is_article_price-freight_group.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-material_group IS INITIAL.
      ls_custom_attr-attribute_code = 'b2b_article'.
      ls_custom_attr-value = is_article_price-material_group.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.
*
*ls_custom_attr-attribute_code = 'hazard'.
*ls_custom_attr-value = 'H315,H317,H318,H335'.
*APPEND ls_custom_attr TO ls_article-product-custom_attributes.

    IF NOT is_article_price-commodity_code IS INITIAL.
      ls_custom_attr-attribute_code = 'customsnumber'.
      ls_custom_attr-value = is_article_price-commodity_code.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    IF NOT is_article_price-grade IS INITIAL.
      ls_custom_attr-attribute_code = 'art_assortmet'.
      ls_custom_attr-value = is_article_price-grade.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

* Mandatory element for bundles
    IF iv_bundle = abap_false.
      ls_custom_attr-attribute_code = 'price_view'.
      ls_custom_attr-value = '9'.
      APPEND ls_custom_attr TO ls_article-product-custom_attributes.
    ENDIF.

    rv_body = /ui2/cl_json=>serialize( data = ls_article compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

* Problem with Initial Values being removed in the conversion to JSON - the compress = abap_false option does not solve the problem
    REPLACE ALL OCCURRENCES OF '"visibility":9' IN rv_body WITH '"visibility":0'.
    REPLACE ALL OCCURRENCES OF '"position":9' IN rv_body WITH '"position":0'.
    REPLACE ALL OCCURRENCES OF '"attribute_code":"dirdelyn","value":"9"' IN rv_body WITH '"attribute_code":"dirdelyn","value":"0"'.
    REPLACE ALL OCCURRENCES OF '"can_change_quantity":9' IN rv_body WITH '"can_change_quantity":0'.
    REPLACE ALL OCCURRENCES OF '"price_type":9' IN rv_body WITH '"price_type":0'.
    REPLACE ALL OCCURRENCES OF '"price":9.00' IN rv_body WITH '"price":0'.
    REPLACE ALL OCCURRENCES OF 'price_view","value":"9"' IN rv_body WITH 'price_view","value":"0"'.

*    DATA(out) = cl_demo_output=>new( )->begin_section( 'Article JSON' ).
*    out->write_json( rv_body ).
*    out->display( ).

  ENDMETHOD.


  METHOD update_article_status.

    IF NOT it_article_status IS INITIAL.
      MODIFY zecm_article_sts FROM TABLE it_article_status.
      rv_count = sy-dbcnt.
    ENDIF.

  ENDMETHOD.


  METHOD update_artprc_status.

    IF NOT it_artprc_status IS INITIAL.
      MODIFY zpos_artprc_sts FROM TABLE it_artprc_status.
      rv_count = sy-dbcnt.
    ENDIF.

  ENDMETHOD.


  METHOD display_status_alv.


    DATA: lv_color            TYPE lvc_s_colo,
          lv_n                TYPE i VALUE 1,
          lv_long_text        TYPE scrtext_l,
          lv_medium_text      TYPE scrtext_m,
          lv_short_text       TYPE scrtext_s,
          lv_iterator         TYPE i VALUE 0,
          lv_half_hour_string TYPE string,
          lv_fieldname        TYPE string,
          lv_columname        TYPE lvc_fname.

    FIELD-SYMBOLS: <lv_field>        TYPE any.

    CHECK mt_report_data IS NOT INITIAL.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mr_table
          CHANGING
            t_table      = mt_report_data ).
      CATCH cx_salv_msg.
    ENDTRY.

    mr_columns = mr_table->get_columns( ).
    mr_columns->set_optimize( abap_true ).
    mr_functions = mr_table->get_functions( ).
    mr_functions->set_all( abap_true ).

    mr_events = mr_table->get_event( ).

    SET HANDLER zcl_pos_article_api=>on_alv_double_click FOR mr_events.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> KEY FIELDS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*============================================================================
* VKORG
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'VKORG' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-001.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_optimized( 'X' ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.

*============================================================================
* DISTRIBUTION CENTER
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'WERKS' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-002.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_optimized( 'X' ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.

*======================================================================*
* ARTICLE
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'MATNR' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-003.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_optimized( 'X' ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.


*============================================================================
* DESCRIPTION
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'DESCRIPTION' ).
        lv_short_text = lv_medium_text = lv_long_text = TEXT-005.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_optimized( 'X' ).
*        mr_column->set_visible( abap_false ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.


*============================================================================
* UNIT of MEASURE
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'UOM' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-005.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_optimized( 'X' ).
*        mr_column->set_visible( abap_false ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> KEY FIELDS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


*======================================================================*
* IS_BUNDLE Indicator
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'IS_BUNDLE' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-010.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_cell_type( 1 ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_visible( abap_true ).
        mr_column->set_zero( abap_false ).
*        mr_column->set_currency( mt_report_data[ 1 ]-transcurrency ).
      CATCH cx_salv_not_found.
    ENDTRY.


*======================================================================*
* SUCCESS Indicator
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'SUCCESS' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-006.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_cell_type( 1 ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_visible( abap_true ).
        mr_column->set_zero( abap_false ).
*        mr_column->set_currency( mt_report_data[ 1 ]-transcurrency ).
      CATCH cx_salv_not_found.
    ENDTRY.




*======================================================================*
* TIMESTAMP
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'CREATED_ON' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-007.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_optimized( 'X' ).
*        mr_column->set_visible( abap_true ).
        mr_column->set_edit_mask('____-__-__ __:__' ).
      CATCH cx_salv_not_found.
    ENDTRY.

*======================================================================*
* HTTP_STATUS
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'HTTP_STATUS' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-008.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.


*======================================================================*
* HTTP_MESSAGE
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'HTTP_MESSAGE' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-009.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_visible( abap_true ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.


*======================================================================*
* WEBSITE_HTTP_STATUS (assignment status)
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'ASSIGN_STATUS' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-011.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.


*======================================================================*
* WEBSITE_HTTP_MESSAGE (assignment message)
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'ASSIGN_MESSAGE' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-012.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_visible( abap_true ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.


*======================================================================*
* Set Column for Color Codes
*======================================================================*
    TRY.
        mr_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.


*    mr_table->set_screen_status(
*          EXPORTING
*            report        = 'BCALV_EDIT_02'
*            pfstatus      = 'MAIN100'
*            set_functions = mr_table->c_functions_all ).


    mr_display = mr_table->get_display_settings( ).
    mr_display->set_striped_pattern( cl_salv_display_settings=>true ).

    mr_table->display( ).

  ENDMETHOD.


  METHOD alv_get_article_json.

    DATA: lv_endpoint TYPE string,
          lv_body     TYPE string.

    lv_endpoint = zcl_pos_magento_rest_api=>build_endpoint_string( iv_data = iv_sku iv_type = co_endpoint-product ).


    zcl_pos_magento_rest_api=>rest_operation( EXPORTING iv_endpoint = lv_endpoint iv_operation = zcl_pos_magento_rest_api=>co_http_operation-get
                           IMPORTING ev_http_message = DATA(lv_http_message)
                                     ev_http_status = DATA(lv_http_status)
                                     ev_body = lv_body  ).


    DATA(out) = cl_demo_output=>new( )->begin_section( 'Article ' && iv_sku  ).
    out->write_json( lv_body ).
    out->display( ).

  ENDMETHOD.


  METHOD on_alv_double_click.

    DATA: lv_sku TYPE string.
    TRY.
        DATA(ls_report_line) = mt_report_data[ row ].     " This should selected the Index Row passed in the variable.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    lv_sku = ls_report_line-matnr.
    SHIFT lv_sku LEFT DELETING LEADING '0'.
    CHECK lv_sku IS NOT INITIAL.


    CASE  column.

      WHEN 'MATNR' OR 'GTIN'.
        TRY.
            alv_get_article_json( lv_sku ).
          CATCH zcx_pos_exception INTO DATA(lx_exception).
        ENDTRY.

    ENDCASE.

  ENDMETHOD.


  METHOD article_check_status.

    DATA:lv_sku              TYPE string,
         ls_article_response TYPE zpos_mag_article_main_sty.

    DATA: lv_country   TYPE string.

*"---------------------------------------------------------------------
*  Check whether we have information already stored locally on the
*  status of the article
*"---------------------------------------------------------------------

* Here we check using the global scope for the basic existence check
    LOOP AT mt_ecom_status INTO rs_status WHERE
                        matnr = is_article_price-matnr AND
                        ean11 = is_article_price-ean11 AND
                        vrkme = is_article_price-vrkme .
* Looping because there could theoretically be more than one

      IF cl_rest_status_code=>is_success( CONV #( rs_status-http_status ) ) = abap_true.
* The record might exist - but we only set the status if it is a status 200
        DATA(lv_exists_locally) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.



    IF lv_exists_locally = abap_true.
* So now we know that it must exist in the global scope - let's check for the specific country

      IF rs_status-vkorg NE is_article_price-vkorg OR
         rs_status-werks NE is_article_price-werks.
* No need to do a second read if we already have the right record

* This must mean that we found the record for another country e.g. we found the Swedish record but wanted to know about Norway
        READ TABLE mt_ecom_status
                   INTO DATA(ls_status2)
                   WITH KEY
                        vkorg = is_article_price-vkorg
                        vtweg = is_article_price-vtweg
                        werks = is_article_price-werks
                        matnr = is_article_price-matnr
                        ean11 = is_article_price-ean11
                        vrkme = is_article_price-vrkme
                        BINARY SEARCH.

* So now we know whether it exists for the specific country.
* A record might exist, but the HTTP status may not be a sucess - so we have to be careful

* And what do we really want to know? We want to know whether it has been assigned to the website
        IF sy-subrc = 0.
          IF cl_rest_status_code=>is_success( CONV #( ls_status2-http_message ) ) = abap_true.
* Here we will overwrite the rs_status information if we found that it existed
            rs_status = ls_status2.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.



*"---------------------------------------------------------------------
*  By this stage we should know whether an article exists in the global
*  scope in Magento, and we should know the website assignment status
*"---------------------------------------------------------------------


* In this case we do not know the status of the article
* it might exist or might not - so we will have to check
    IF lv_exists_locally = abap_false.

      MOVE-CORRESPONDING is_article_price TO rs_status.

      lv_sku = is_article_price-matnr. SHIFT lv_sku LEFT DELETING LEADING '0'.

* The endpoint can take Sales Org as a parameter but in this case we want to check the global scope - GET operation
      DATA(lv_endpoint) = zcl_pos_magento_rest_api=>build_endpoint_string( iv_data = lv_sku iv_type = co_endpoint-product ).

* Perform the operation
      zcl_pos_magento_rest_api=>rest_operation( EXPORTING iv_endpoint = lv_endpoint iv_operation = zcl_pos_magento_rest_api=>co_http_operation-get
                             IMPORTING ev_http_message = rs_status-http_message ev_http_status = rs_status-http_status ev_body = DATA(lv_body) ).


* If this is a success then the article exists
      IF cl_rest_status_code=>is_success( CONV #( rs_status-http_status ) ) = abap_true.


* Here we get the article from the global scope and can see which websites it is assigned to
* Using this data we can potentially reduce the number of calls we need to make later.

* Deserialize the result payload into an ABAP structure
        CALL METHOD /ui2/cl_json=>deserialize( EXPORTING json = lv_body CHANGING data = ls_article_response ).


        READ TABLE mt_website
              INTO DATA(ls_website)
              WITH KEY vkorg = is_article_price-vkorg
                       werks = is_article_price-werks
                       BINARY SEARCH.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e034(zpos).
        ENDIF.


* So here we could look at the websites and set the HTTP status to 200
        LOOP AT ls_article_response-extension_attributes-website_ids INTO DATA(lv_website).

          IF lv_website = ls_website-website_id.
*                  e.g. If this is 9700 and the ID is 1 then we know we are already assigned.
            rs_status-website_http_status = cl_rest_status_code=>gc_success_ok.
          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD assign_article_to_website.

    DATA:lv_sku TYPE string.
    TYPES: BEGIN OF tp_check_payload,
             sku        TYPE char20,
             website_id TYPE i,
           END OF tp_check_payload.

    DATA: BEGIN OF ls_check_payload,
            productwebsitelink TYPE tp_check_payload,
          END OF ls_check_payload.

    READ TABLE mt_website
          INTO DATA(ls_website)
          WITH KEY vkorg = is_article_price-vkorg
                   werks = is_article_price-werks
                   BINARY SEARCH.


    IF sy-subrc = 0 AND ls_website-website_id IS NOT INITIAL.

      lv_sku = is_article_price-matnr.
      SHIFT lv_sku LEFT DELETING LEADING '0'.
      ls_check_payload-productwebsitelink-sku        = is_article_price-matnr.
      ls_check_payload-productwebsitelink-website_id = ls_website-website_id.

      DATA(lv_body) = /ui2/cl_json=>serialize( data = ls_check_payload compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

      REPLACE ALL OCCURRENCES OF 'productwebsitelink' IN lv_body WITH 'productWebsiteLink'.

      DATA(lv_endpoint) = zcl_pos_magento_rest_api=>build_endpoint_string( iv_data = lv_sku iv_type = co_endpoint-websites ).

      zcl_pos_magento_rest_api=>rest_operation( EXPORTING iv_body = lv_body iv_endpoint = lv_endpoint iv_operation = zcl_pos_magento_rest_api=>co_http_operation-put
                             IMPORTING ev_http_message = rs_status-website_http_message ev_http_status = rs_status-website_http_status ).

    ELSE.
      RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e034(zpos).
    ENDIF.

  ENDMETHOD.


  METHOD get_records_for_processing.

    DATA: lt_r_recipient TYPE zpos_r_recipient_tty,
          lt_r_vtweg     TYPE wrf_vtweg_rtty.

*          ls_textid    TYPE scx_t100key,
*          lx_exception TYPE REF TO zcx_pos_exception.


    CLEAR mt_report_data.

    APPEND INITIAL LINE TO lt_r_recipient ASSIGNING FIELD-SYMBOL(<ls_r_recipient>).
    <ls_r_recipient>-sign = zcl_pos_util=>co_selopt_sign_i.
    <ls_r_recipient>-option = zcl_pos_util=>co_selopt_opt_eq.
    <ls_r_recipient>-low = zcl_pos_util=>co_pos_recipient-magento_api.       " Needs to be changed when we have data available
*    IF sy-uname = 'BRENNANN'.
*      <ls_r_recipient>-low = zcl_pos_util=>co_pos_recipient-viking.            " Needs to be changed when we have data available
*    ENDIF.

    APPEND INITIAL LINE TO lt_r_vtweg ASSIGNING FIELD-SYMBOL(<ls_r_vtweg>).
    <ls_r_vtweg>-sign = zcl_pos_util=>co_selopt_sign_i.
    <ls_r_vtweg>-option = zcl_pos_util=>co_selopt_opt_eq.
    <ls_r_vtweg>-low = zcl_pos_util=>co_distribution_channel-web.             " Needs to be changed when we have data available
*    IF sy-uname = 'BRENNANN'.
*      <ls_r_vtweg>-low = zcl_pos_util=>co_distribution_channel-store.           " Needs to be changed when we have data available
*    ENDIF.

    rt_article_price = zcl_pos_wso_article_price=>get_article_pricelist(
                              EXPORTING iv_create_bom  = abap_true
                                        iv_status      = iv_status
                                        iv_valid_on    = iv_valid_on
                                        iv_leadtime    = 0
                                        it_r_recipient = lt_r_recipient
                                        iv_vkorg       = iv_vkorg
                                        it_r_vtweg     = lt_r_vtweg
                                        it_r_werks     = it_r_werks
                                        it_r_matnr     = it_r_matnr ).

    IF rt_article_price IS INITIAL.
      RAISE EXCEPTION TYPE zcx_pos_exception.
    ELSE.

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
             FOR ALL ENTRIES IN @rt_article_price
             WHERE vkorg = @iv_vkorg
             AND   vtweg IN @lt_r_vtweg
             AND   werks IN @it_r_werks
             AND   matnr = @rt_article_price-matnr.


      SELECT * FROM zpos_website
               INTO TABLE @mt_website
               WHERE vkorg = @iv_vkorg
               AND   werks IN @it_r_werks
               ORDER BY vkorg, werks.

      IF mt_website IS INITIAL.
        RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e034(zpos).
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

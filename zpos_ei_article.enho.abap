CLASS lcl_zpos_ei_article DEFINITION DEFERRED.
CLASS cl_wes_merchandise_repl DEFINITION LOCAL FRIENDS lcl_zpos_ei_article.
CLASS lcl_zpos_ei_article DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zpos_ei_article.         "#EC NEEDED
    DATA core_object TYPE REF TO cl_wes_merchandise_repl .  "#EC NEEDED
 INTERFACES  IOW_ZPOS_EI_ARTICLE.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO cl_wes_merchandise_repl OPTIONAL.
ENDCLASS.
CLASS lcl_zpos_ei_article IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD iow_zpos_ei_article~fill_sales_prices.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods FILL_SALES_PRICES
*"  importing
*"    !IV_DATE_FROM type SY-DATUM
*"    !IV_DATE_TO type SY-DATUM
*"  changing
*"    !CT_PACKAGE type WES_T_MERCHANDISE
*"    !CT_MESSAGES type BAL_T_MSG
*"  raising
*"    CX_NO_SALES_PRICE_FOUND .
*"------------------------------------------------------------------------*

    DATA lt_scales_art         TYPE cond_scale_t.
    DATA ls_scales_art         TYPE condscale.
    DATA lt_cond_art           TYPE tt_item_saco_mapping.
    DATA ls_cond_art           TYPE item_saco_mapping.
    DATA lv_cond_art_index     TYPE sy-tabix.
    DATA lv_scales_art_index   TYPE sy-tabix.
    DATA ls_matnr_pmata        TYPE core_object->st_matnr_pmata.
    DATA lv_matnr              TYPE matnr.
    DATA ls_mara_read          TYPE mara.
    DATA ls_sales_price        TYPE wes_s_sales_prices_head.
    DATA ls_sales_price_item   TYPE wes_s_sales_prices_item.
    DATA ls_sales_price_scales TYPE wes_s_sales_prices_scale.
    DATA ls_saco               TYPE saco.
    DATA lv_pltyp              TYPE pltyp VALUE IS INITIAL.
    DATA lv_ean_check          TYPE xfeld.
    DATA lt_short_marm         TYPE wes_to_short_marm.
    DATA ls_short_marm         TYPE wes_s_short_marm.
    DATA lo_error              TYPE REF TO zcx_pos_exception.

    FIELD-SYMBOLS <ls_package> TYPE wes_s_merchandise.

    IF core_object->sr_co IS INITIAL.
      core_object->sr_co = cl_wes_const=>sr_co.
    ENDIF.

* ==================================================================================================
*    Are there articles at all?
* ==================================================================================================
*   Package seems to be always store specific - including the articles per store

    IF ct_package IS NOT INITIAL.

      "First read sales prices for all articles
      CALL FUNCTION 'SALES_COND_MULTI_ACCESS'
        EXPORTING
          datab_i             = iv_date_from
          datbi_i             = iv_date_to
          kvewe_i             = core_object->sr_co->condition_usage_pricing
          kappl_i             = core_object->sr_co->conditions_appl_sales
          scale_read_i        = core_object->sr_co->x
          pi_check_diff_kmein = core_object->sr_co->x
        IMPORTING
          pe_t_scale          = lt_scales_art
          pe_t_item_saco      = lt_cond_art
        EXCEPTIONS
          no_conditions       = 1
          OTHERS              = 2.

* ==================================================================================================
*    Are there any prices at all?
* ==================================================================================================
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_no_sales_price_found.
      ENDIF.



* ==================================================================================================
*    So we know we have articles and pricing - worth to continue
* ==================================================================================================
      core_object->mr_article_price->new_package_reset( ).
      LOOP AT ct_package ASSIGNING <ls_package>.

* ==================================================================================================
*       Do we need to read the conditions with pricinf ref. material
* ==================================================================================================
        READ TABLE core_object->mt_matnr_pmata WITH KEY matnr = <ls_package>-matnr INTO ls_matnr_pmata BINARY SEARCH.

        IF sy-subrc = 0.
          lv_matnr = ls_matnr_pmata-pmata.
        ELSE.
          lv_matnr = <ls_package>-matnr.
        ENDIF.





* ==================================================================================================
* Find first entry for this vkorg/vtweg/werks/article combination (there can be more than one for different dates)
* ==================================================================================================
* This gives us a sy-tabix value used below to improve the looping - could be thousands of records I presume
* ==================================================================================================

        READ TABLE lt_cond_art WITH KEY vkorg = <ls_package>-vkorg
                                        vtweg = core_object->mv_pricing_channel
                                        pltyp = lv_pltyp
                                        werks = <ls_package>-werks
                                        matnr = lv_matnr
                                        BINARY SEARCH TRANSPORTING NO FIELDS.




* ==================================================================================================
*   No entry found - and could not therefore clean-up and ocntinue
* ==================================================================================================
        IF sy-subrc <> 0.
          "article has no sales price
          CALL FUNCTION 'RDM_OPT_MESSAGE_ADD'
            EXPORTING
              i_probclass = core_object->sr_co->msg_probclass_1
              i_sortfield = core_object->sr_co->sortfield_merchandise
              i_msgid     = core_object->sr_co->msg_id_common
              i_msgty     = core_object->sr_co->msg_type_error
              i_msgno     = '116'
              i_msgv1     = lv_matnr
              i_msgv2     = <ls_package>-werks
              i_msgv3     = <ls_package>-vkorg
              i_msgv4     = core_object->mv_pricing_channel
            CHANGING
              xt_messages = ct_messages.

          IF core_object->ms_runtime_param-dlmod = core_object->sr_co->mode_change.
            "no sales price in case of change mode, delete article from destination system
            <ls_package>-chgid = core_object->sr_co->delete.
            CONTINUE.
          ELSE.
            "no sales price in case of manuel request or initialization
            "delete article from distribution list
            DELETE TABLE ct_package FROM <ls_package>.
            CONTINUE.
          ENDIF.
* ==================================================================================================
*   No entry found - and could not therefore clean-up and ocntinue
* ==================================================================================================











        ELSE.
* ==================================================================================================
*    Entry found - This is where we really start prepaing data for the package
* ==================================================================================================
          lv_cond_art_index = sy-tabix.


* Get Additional MARA Data

          IF <ls_package>-matnr NE ls_mara_read-matnr.

            CALL FUNCTION 'MARA_SINGLE_READ'
              EXPORTING
*               KZRFB             = ' '
*               MAXTZ             = 0
                matnr             = lv_matnr
*               SPERRMODUS        = ' '
*               STD_SPERRMODUS    = ' '
*               OUTPUT_NO_MESSAGE =
              IMPORTING
                wmara             = ls_mara_read
              EXCEPTIONS
                lock_on_material  = 1
                lock_system_error = 2
                wrong_call        = 3
                not_found         = 4
                OTHERS            = 5.
            IF sy-subrc <> 0.
* Implement suitable error handling here
              ASSERT 1 = 2.
            ENDIF.

          ENDIF.



          "check corresponding cond art item entries for this vkorg/vtweg/werks/article combination (there can be more than one for different dates)
          LOOP AT lt_cond_art INTO ls_cond_art FROM lv_cond_art_index.


*    Cursor positioned previously with the following statement
*    ------------------------------------------------------------------------------------
*        READ TABLE lt_cond_art WITH KEY vkorg = <ls_package>-vkorg
*                                        vtweg = mv_pricing_channel
*                                        pltyp = lv_pltyp
*                                        werks = <ls_package>-werks
*                                        matnr = lv_matnr
*                                        BINARY SEARCH TRANSPORTING NO FIELDS.
*    ------------------------------------------------------------------------------------

            " The first loop would always work but then in subsequent loops .....
            " All prices for current article are already read, return and continue with the next article

            IF ls_cond_art-matnr <> lv_matnr.
              EXIT.
            ENDIF.


* ----------- CUSTOM - PREPROCESSING OF CONDITION DATA  ---------------------------------------------------------

            TRY.
                core_object->mr_article_price->determine_effective_price( CHANGING cs_cond_art = ls_cond_art  ).

              CATCH zcx_pos_exception INTO lo_error.

                CALL METHOD core_object->mo_bal->add_msg
                  EXPORTING
                    iv_msgty = core_object->sr_co->msg_type_error
                    iv_msgno = lo_error->if_t100_message~t100key-msgno
                    iv_msgid = lo_error->if_t100_message~t100key-msgid
                    iv_msgv1 = lo_error->if_t100_message~t100key-attr1
                    iv_msgv2 = lo_error->if_t100_message~t100key-attr2
                    iv_msgv3 = lo_error->if_t100_message~t100key-attr3
                    iv_msgv4 = lo_error->if_t100_message~t100key-attr4.

            ENDTRY.

* ----------- CUSTOM - PREPROCESSING OF CONDITION DATA  ---------------------------------------------------------



            "loop over the konp results in saco_table and take over the relevant data
            LOOP AT ls_cond_art-saco_table INTO ls_saco.

* ----------- HEADER DATA ---------------------------------------------------------

              "header data
              ls_sales_price-matnr = <ls_package>-matnr.
              ls_sales_price-vkorg = <ls_package>-vkorg.
              ls_sales_price-vtweg = core_object->mv_pricing_channel.
              ls_sales_price-kmein = ls_saco-kmein.
              ls_sales_price-datab = ls_saco-datab.
              ls_sales_price-datbi = ls_saco-datbi.


* ----------- EAN DATA -------------------
              "determine EAN11 for each unit of measure from the buffer (if requested)

*   ------------------------ EAN's DO NOT EXIST FOR THE COMPARATIVE PRICING RECORDS ----------------------------

              IF NOT ls_saco-knumh IS INITIAL.          " This will always be blank for the Comparative Pricing Records

                IF core_object->mv_no_ean_check IS INITIAL.
                  lv_ean_check = 'X'.
                ENDIF.

                CALL FUNCTION 'WES_MARM_SELECT'
                  EXPORTING
                    iv_check_ean   = lv_ean_check
                    iv_matnr       = <ls_package>-matnr
                    iv_meinh       = ls_saco-kmein
                  IMPORTING
                    eto_short_marm = lt_short_marm.

                READ TABLE lt_short_marm INTO ls_short_marm INDEX 1.

                IF sy-subrc = 0.
                  ls_sales_price-ean11 = ls_short_marm-ean11.
                  ls_sales_price-numtp = ls_short_marm-numtp.
                ENDIF.

              ENDIF.

* ----------- EAN DATA -------------------

* ----------- HEADER DATA ---------------------------------------------------------




* ----------- ITEM DATA -------------------
              CLEAR ls_sales_price_item.
              ls_sales_price_item-kschl = ls_saco-kschl.
              ls_sales_price_item-kbetr = ls_saco-kbetr.
              ls_sales_price_item-waers = ls_saco-waers.
              ls_sales_price_item-kpein = ls_saco-kpein.
              ls_sales_price_item-kmein = ls_saco-kmein.
              ls_sales_price_item-krech = ls_saco-krech.
* ----------- ITEM DATA -------------------




* ----------- SCALES -------------------

*   ------------------------ EAN's DO NOT EXIST FOR THE COMPARATIVE PRICING RECORDS ----------------------------

              IF NOT ls_saco-knumh IS INITIAL.          " This will always be blank for the Comparative Pricing Records

                "determine the relevant scales for the sales price
                READ TABLE lt_scales_art WITH KEY knumh = ls_saco-knumh kopos = ls_saco-kopos BINARY SEARCH TRANSPORTING NO FIELDS.

                IF sy-subrc = 0.
                  lv_scales_art_index = sy-tabix.

                  LOOP AT lt_scales_art INTO ls_scales_art FROM lv_scales_art_index.

                    IF ls_scales_art-knumh <> ls_saco-knumh.
                      EXIT.
                    ENDIF.

                    "KONP-values does not have to be overtaken, because they already
                    "have been overtaken by the corresponding item entry
                    IF ( ls_scales_art-kstbm IS INITIAL AND
                         ls_scales_art-kzbzg = core_object->sr_co->scale_type_unit )
                    OR ( ls_scales_art-kstbw IS INITIAL AND
                         ls_scales_art-kzbzg = core_object->sr_co->scale_type_value ).
                      CONTINUE.
                    ENDIF.

                    CLEAR ls_sales_price_scales.

                    ls_sales_price_scales-kzbzg = ls_scales_art-kzbzg.
                    ls_sales_price_scales-kstbw = ls_scales_art-kstbw.
                    ls_sales_price_scales-kmein = ls_saco-kmein.
                    ls_sales_price_scales-kstbm = ls_scales_art-kstbm.
                    ls_sales_price_scales-waers = ls_saco-waers.
                    ls_sales_price_scales-kbetr = ls_scales_art-kbetr.
                    APPEND ls_sales_price_scales TO ls_sales_price_item-scale.

                  ENDLOOP.
                ENDIF.

              ENDIF.
* ----------- SCALES -------------------




* ----------- APPEND to PACKAGE     ---------------------------------------------------------

              CLEAR  ls_sales_price-item.
              APPEND ls_sales_price_item TO ls_sales_price-item.
              APPEND ls_sales_price TO <ls_package>-sales_prices_tab.


              TRY.
                  core_object->mr_article_price->append_wesout_price( is_price = ls_sales_price iv_werks = <ls_package>-werks ).

                CATCH zcx_pos_exception INTO lo_error.

                  CALL METHOD core_object->mo_bal->add_msg
                    EXPORTING
                      iv_msgty = core_object->sr_co->msg_type_error
                      iv_msgno = lo_error->if_t100_message~t100key-msgno
                      iv_msgid = lo_error->if_t100_message~t100key-msgid
                      iv_msgv1 = lo_error->if_t100_message~t100key-attr1
                      iv_msgv2 = lo_error->if_t100_message~t100key-attr2
                      iv_msgv3 = lo_error->if_t100_message~t100key-attr3
                      iv_msgv4 = lo_error->if_t100_message~t100key-attr4.

              ENDTRY.


* ----------- APPEND to PACKAGE     ---------------------------------------------------------


            ENDLOOP.                      " Konp Results        ----> LOOP AT ls_cond_art-saco_table

          ENDLOOP.                        " Pricing Conditions ----> LOOP AT lt_cond_art

        ENDIF.                            " Do Prices Exist ----> READ TABLE lt_cond_art

      ENDLOOP.                            " Articles  -----> LOOP AT ct_package ASSIGNING <ls_package>.

      TRY.
          core_object->mr_article_price->create_condition_record( EXPORTING sr_co = core_object->sr_co CHANGING ct_messages = ct_messages ).
        CATCH zcx_pos_exception INTO lo_error.

          CALL METHOD core_object->mo_bal->add_msg
            EXPORTING
              iv_msgty = core_object->sr_co->msg_type_error
              iv_msgno = lo_error->if_t100_message~t100key-msgno
              iv_msgid = lo_error->if_t100_message~t100key-msgid
              iv_msgv1 = lo_error->if_t100_message~t100key-attr1
              iv_msgv2 = lo_error->if_t100_message~t100key-attr2
              iv_msgv3 = lo_error->if_t100_message~t100key-attr3
              iv_msgv4 = lo_error->if_t100_message~t100key-attr4.

      ENDTRY.

    ENDIF.

  ENDMETHOD.
  METHOD iow_zpos_ei_article~send_data.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods SEND_DATA
*"  importing
*"    !IT_PACKAGE type WES_T_MERCHANDISE
*"  exporting
*"    !EV_PI_MESSAGE_ID type SXMSMGUID
*"  changing
*"    !CT_MESSAGES type BAL_T_MSG .
*"------------------------------------------------------------------------*

    IF NOT it_package[] IS INITIAL.

      TRY.
          core_object->mr_article_price->create_persist_article_data( EXPORTING it_package = it_package ).
        CATCH zcx_pos_exception INTO DATA(lo_error).

          CALL METHOD core_object->mo_bal->add_msg
            EXPORTING
              iv_msgty = core_object->sr_co->msg_type_error
              iv_msgno = lo_error->if_t100_message~t100key-msgno
              iv_msgid = lo_error->if_t100_message~t100key-msgid
              iv_msgv1 = lo_error->if_t100_message~t100key-attr1
              iv_msgv2 = lo_error->if_t100_message~t100key-attr2
              iv_msgv3 = lo_error->if_t100_message~t100key-attr3
              iv_msgv4 = lo_error->if_t100_message~t100key-attr4.

      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

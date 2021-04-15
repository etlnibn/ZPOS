CLASS lcl_zpos_ei_retail_event DEFINITION DEFERRED.
CLASS cl_wpm_re_erp_str_rpl_rqu_v1 DEFINITION LOCAL FRIENDS lcl_zpos_ei_retail_event.
CLASS lcl_zpos_ei_retail_event DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zpos_ei_retail_event.    "#EC NEEDED
    DATA core_object TYPE REF TO cl_wpm_re_erp_str_rpl_rqu_v1 . "#EC NEEDED
 INTERFACES  IOW_ZPOS_EI_RETAIL_EVENT.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO cl_wpm_re_erp_str_rpl_rqu_v1 OPTIONAL.
ENDCLASS.
CLASS lcl_zpos_ei_retail_event IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD iow_zpos_ei_retail_event~map_retail_event_offer_bby.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods MAP_RETAIL_EVENT_OFFER_BBY
*"  importing
*"    !IV_AKTNR type WAKTION
*"    !ITO_KONBBYH type WES_TO_KONBBYH
*"  changing
*"    !CT_OFFER type RTCO_RESRV1_OF_T
*"  raising
*"    CX_SMT_ERROR
*"    CX_WES_PROCESS_MESSAGES
*"    CX_WES_EXCEPTION .
*"------------------------------------------------------------------------*
    DATA: ls_wa_header             TYPE rdm_s_bby_wa_header,
          lto_wa_buy               TYPE rdm_t_bby_wa_buy,
          lto_wa_get               TYPE rdm_t_bby_wa_get,
          lto_wa_reward            TYPE rdm_t_bby_wa_reward,
          lto_wa_matgrp            TYPE rdm_t_bby_wa_matgrp,
          lto_wa_orgitems          TYPE rdm_t_bby_wa_orgitems,
          lto_wa_orgsites          TYPE rdm_t_bby_wa_orgsites,
          lto_wa_texts             TYPE rdm_t_bby_wa_texts,
          lt_conditions            TYPE rdm_t_bby_conditions,
          lt_bby_sent              TYPE wes_t_bby_sent,
          lo_engine                TYPE REF TO cl_smt_engine,
          ls_offer                 TYPE rtco_resrv1_of,
          lx_smt_error             TYPE REF TO cx_smt_error,
          lv_offer_id              TYPE wes_offer_id,
          lv_org_level_plant       TYPE xfeld,
          lv_org_level_plant_group TYPE xfeld,
          lv_org_level_price_list  TYPE xfeld,
          lv_org_level_distr_chain TYPE xfeld,
          lt_short_storeinfo       TYPE wes_t_store_pricelist_data,
          lr_pos_offer             TYPE REF TO zcl_pos_wso_promotion.

    lr_pos_offer = NEW #( ).

    FIELD-SYMBOLS: <ls_konbbyh>    LIKE LINE OF ito_konbbyh.

    BREAK-POINT ID wes_re_erp_str_rpl_rqu_v1.

* 4 kinds of offers
*   (item prices:                   MTO_WAKPD)
*   (item discounts:                MTO_WAKPD_DSC)
*   (discounts on aggregated level: MTO_WAKRD)
*   bonus buys:                    ITO_KONBBYH

* create instance of mapping tool
    CREATE OBJECT lo_engine
      EXPORTING
        i_mapping      = core_object->co_smt_mapping
        i_mapping_step = 'RE_OFFER_BBY'
*       i_context      =
*       i_extended_xml_handling =
*       i_with_prefetch         =
*       i_dynamic_mode =
      .

    LOOP AT ito_konbbyh ASSIGNING <ls_konbbyh>.

      TRY.

*       read data from db
          CLEAR: ls_wa_header,
                 lto_wa_buy,
                 lto_wa_get,
                 lto_wa_reward,
                 lto_wa_matgrp,
                 lto_wa_orgitems,
                 lto_wa_orgsites,
                 lto_wa_texts,
                 lt_conditions,
                 lt_bby_sent,
                 lv_offer_id,
                 lv_org_level_plant,
                 lv_org_level_plant_group,
                 lv_org_level_price_list,
                 lv_org_level_distr_chain,
                 lt_short_storeinfo.

          CALL METHOD core_object->read_bby_data
            EXPORTING
              iv_bbynr       = <ls_konbbyh>-bbynr
            IMPORTING
              es_wa_header   = ls_wa_header
              et_wa_buy      = lto_wa_buy
              et_wa_get      = lto_wa_get
              et_wa_reward   = lto_wa_reward
              et_wa_matgrp   = lto_wa_matgrp
              et_wa_orgitems = lto_wa_orgitems
              et_wa_orgsites = lto_wa_orgsites
              et_wa_texts    = lto_wa_texts
              et_conditions  = lt_conditions.
*       save conditions for later mapping
          CALL FUNCTION 'WPM_BBY_SLS_COND_INSERT'
            EXPORTING
              it_conditions = lt_conditions.

          CALL FUNCTION 'WES_STORES_FOR_BONUSBUY'
            EXPORTING
*             IV_APPL                  =
*             IV_SERV_IMPL             =
              iv_bbynr                 = <ls_konbbyh>-bbynr
              iv_aktnr                 = iv_aktnr
              ito_bby_wa_orgitems      = lto_wa_orgitems
              ito_bby_wa_orgsites      = lto_wa_orgsites
*             IV_OFFERID               =
            IMPORTING
              et_bby_sent              = lt_bby_sent
              et_short_storeinfo       = lt_short_storeinfo
              ev_org_level_plant       = lv_org_level_plant
              ev_org_level_plant_group = lv_org_level_plant_group
              ev_org_level_price_list  = lv_org_level_price_list
              ev_org_level_distr_chain = lv_org_level_distr_chain
            EXCEPTIONS
              no_stores_found          = 0
              OTHERS                   = 0.

*       MATKL not of interest
          SORT lt_short_storeinfo BY werks pltyp_p.
          DELETE ADJACENT DUPLICATES FROM lt_short_storeinfo
            COMPARING werks pltyp_p.

          CLEAR ls_offer.

*       map offer - header
          CALL METHOD lo_engine->execute
            EXPORTING
              i_source  = ls_wa_header
*             i_predecessor1            =
*             i_predecessor2            =
*             i_predecessor3            =
*             i_predecessor4            =
*             i_add1    =
*             i_add2    =
*             i_add3    =
*             i_add4    =
*             i_add5    =
*             i_add6    =
*             i_add7    =
*             i_add8    =
*             i_add9    =
*             i_add10   =
*             i_add11   =
*             i_add12   =
*             i_add13   =
*             i_add14   =
*             i_add15   =
*             i_add16   =
*             i_add17   =
*             i_add18   =
*             i_add19   =
*             i_add20   =
*             i_do_not_set_change_field = CL_SMT_CON=>FALSE
*             i_overwrite_check         =
*             i_no_data_loss_check      =
            CHANGING
*             ch_change =
              ch_target = ls_offer.

*       map descriptions
          CALL METHOD core_object->map_re_offer_bby_des
            EXPORTING
              ito_bby_wa_texts = lto_wa_texts
            CHANGING
              cs_offer         = ls_offer.

*       map buy side
          IF NOT lto_wa_buy[] IS INITIAL OR
             NOT ls_wa_header-buy_prqval_min IS INITIAL.
            CALL METHOD core_object->map_re_offer_bby_buy
              EXPORTING
                is_bby_wa_header  = ls_wa_header
                ito_bby_wa_buy    = lto_wa_buy
                ito_bby_wa_matgrp = lto_wa_matgrp
                ito_bby_wa_reward = lto_wa_reward
              CHANGING
                cs_offer          = ls_offer.
          ENDIF.

*       map get side
          CALL METHOD core_object->map_re_offer_bby_get
            EXPORTING
              is_bby_wa_header  = ls_wa_header
              ito_bby_wa_reward = lto_wa_reward
              ito_bby_wa_get    = lto_wa_get
              ito_bby_wa_matgrp = lto_wa_matgrp
            CHANGING
              cs_offer          = ls_offer.

*       determine condition records per store for later mapping
          CALL METHOD core_object->det_bby_conds_per_store
            EXPORTING
              it_store_pricelist_data  = lt_short_storeinfo
              it_bby_sent              = lt_bby_sent
              it_conditions            = lt_conditions
              iv_offer_id              = ls_offer-id
              iv_org_level_plant       = lv_org_level_plant
              iv_org_level_plant_group = lv_org_level_plant_group
              iv_org_level_price_list  = lv_org_level_price_list
              iv_org_level_distr_chain = lv_org_level_distr_chain.

*       map stores
          CALL METHOD core_object->map_re_offer_bby_store
            EXPORTING
              it_bby_sent = lt_bby_sent
            CHANGING
              cs_offer    = ls_offer.

          APPEND ls_offer TO ct_offer.


* Pass all of the values to the handler class so it gets all of the BBYs for one promotion.

* We only want the BBYs for the specific promotion
          IF <ls_konbbyh>-aktnr = core_object->mv_aktnr AND ls_wa_header-status = cl_rdm_opt_const=>sr_co->bby_status_activated.       " Initial Means Activated

            CALL METHOD lr_pos_offer->append_bby_data
              EXPORTING
                is_header         = ls_wa_header
                it_buy            = lto_wa_buy
                it_get            = lto_wa_get
                it_reward         = lto_wa_reward
                it_matgrp         = lto_wa_matgrp
                it_orgitems       = lto_wa_orgitems
                it_orgsites       = lto_wa_orgsites
                it_texts          = lto_wa_texts
*               is_sent_ext       = ls_bby_sent_ext
                it_bby_sent       = lt_bby_sent
                it_conditions     = lt_conditions
                it_promo_sent_ext = core_object->mto_promo_sent_ext.
          ENDIF.

        CATCH cx_smt_error INTO lx_smt_error.

          CALL FUNCTION 'RDM_BUILD_OFFER_ID_BBY'
            EXPORTING
              is_bby_header = ls_wa_header
            IMPORTING
              ev_offerid    = lv_offer_id.

*       handle error
          CALL METHOD core_object->handle_smt_error_offer
            EXPORTING
              iv_offerid   = lv_offer_id
              ix_smt_error = lx_smt_error.
*       raise exception
          RAISE EXCEPTION TYPE cx_wes_process_messages.

      ENDTRY.

    ENDLOOP.

* Create the IDoc for th Promotion
    TRY.
        CALL METHOD lr_pos_offer->create_retail_events( ).


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
*           iv_msgv1_src =
*           iv_msgv2_src =
*           iv_msgv3_src =
*           iv_msgv4_src =
*           is_context   =
*           is_params    =
    ENDTRY.

  ENDMETHOD.
  METHOD iow_zpos_ei_retail_event~send_message.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods SEND_MESSAGE
*"  importing
*"    !IV_OBJECT_COUNT type WES_PACKAGE_SIZE
*"  raising
*"    CX_WES_PROCESS_MESSAGES
*"    CX_WES_EXCEPTION .
*"------------------------------------------------------------------------*

*"----------------------------------------------------------------------
* We Do Not Want to Send Anything - IDoc Output Instead!
    ASSERT 1 = 1.
*"----------------------------------------------------------------------


  ENDMETHOD.
ENDCLASS.

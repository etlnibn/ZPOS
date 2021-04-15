CLASS lcl_zpos_ei_bonus_buy DEFINITION DEFERRED.
CLASS cl_wpm_ri_erp_str_o_rpl_blk_v1 DEFINITION LOCAL FRIENDS lcl_zpos_ei_bonus_buy.
CLASS lcl_zpos_ei_bonus_buy DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zpos_ei_bonus_buy.       "#EC NEEDED
    DATA core_object TYPE REF TO cl_wpm_ri_erp_str_o_rpl_blk_v1 . "#EC NEEDED
 INTERFACES  IOW_ZPOS_EI_BONUS_BUY.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO cl_wpm_ri_erp_str_o_rpl_blk_v1 OPTIONAL.
ENDCLASS.
CLASS lcl_zpos_ei_bonus_buy IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD iow_zpos_ei_bonus_buy~call_badi.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods CALL_BADI
*"  importing
*"    !IV_BBYNR type BBYNR
*"    !IV_CHGID type WES_CHGID
*"    !IS_BBY_WA_HEADER type RDM_S_BBY_WA_HEADER
*"    !ITO_BBY_WA_BUY type RDM_T_BBY_WA_BUY
*"    !ITO_BBY_WA_GET type RDM_T_BBY_WA_GET
*"    !ITO_BBY_WA_REWARD type RDM_T_BBY_WA_REWARD
*"    !ITO_BBY_WA_SCALES type RDM_T_BBY_WA_SCALES
*"    !ITO_BBY_WA_MATGRP type RDM_T_BBY_WA_MATGRP
*"    !ITO_BBY_WA_ORGITEMS type RDM_T_BBY_WA_ORGITEMS
*"    !ITO_BBY_WA_TEXTS type RDM_T_BBY_WA_TEXTS
*"    !IS_BBY_SENT_EXT type WES_BBY_SENT_EXT
*"  changing
*"    !CS_OUTPUT type RTCO_RISR_MSG_V1
*"  raising
*"    CX_WES_PROCESS_MESSAGES
*"    CX_WES_EXCEPTION .
*"------------------------------------------------------------------------*

    DATA: lo_error TYPE REF TO zcx_pos_exception,
          lv_msgid TYPE symsgid,
          lv_msgno TYPE symsgno,
          lv_msgv1 TYPE symsgv,
          lv_msgv2 TYPE symsgv,
          lv_msgv3 TYPE symsgv,
          lv_msgv4 TYPE symsgv.

    BREAK-POINT ID wes_ri_erp_str_offr_rpl_blk.

    IF NOT core_object->ms_bby_wa_header-status = cl_rdm_opt_const=>sr_co->bby_status_activated.       " Initial Means Active
      TRY.

          CALL METHOD zcl_pos_wso_bonus_buy=>create_offer_idoc
            EXPORTING
              iv_bbynr    = core_object->mv_bbynr
              iv_chgid    = core_object->mv_chgid
              is_header   = core_object->ms_bby_wa_header
              it_buy      = core_object->mto_bby_wa_buy
              it_get      = core_object->mto_bby_wa_get
              it_reward   = core_object->mto_bby_wa_reward
              it_scales   = core_object->mto_bby_wa_scales
              it_matgrp   = core_object->mto_bby_wa_matgrp
              it_orgitems = core_object->mto_bby_wa_orgitems
              it_orgsites = core_object->mto_bby_wa_orgsites
              it_texts    = core_object->mto_bby_wa_texts
              it_sent_ext = core_object->mto_bby_sent_ext
*             is_sent_ext = core_object->ls_bby_sent_ext.
            .

        CATCH zcx_pos_exception INTO lo_error.

*      CATCH cx_wes_process_messages INTO DATA(lo_error).
*         handle error

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
*
*        IF 1 = 0. MESSAGE e306 WITH '' ''. ENDIF.
**     add message and raise exception
*        CALL METHOD core_object->mo_bal->add_msg_re
*          EXPORTING
*            iv_msgno     = '306'
**                              Exception &1 in BAdI &2 ausgelöst
*            iv_msgid     = core_object->sr_co->msg_id_common
*            iv_msgv1     = core_object->sr_co->cx_proxy_badi_processing
*            iv_msgv2     = core_object->sr_co->badi_retail_incntv_out
**           iv_msgv3     =
**           iv_msgv4     =
**           iv_msgv1_src =
**           iv_msgv2_src =
**           iv_msgv3_src =
**           iv_msgv4_src =
**           is_context   =
**           is_params    =
*            ix_exception = core_object->mx_process_messages.

      ENDTRY.

    ENDIF.



  ENDMETHOD.
  METHOD iow_zpos_ei_bonus_buy~send_message.
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

* We do not want to Send Anything - IDoc Output Instead

    ASSERT 1 = 1.

  ENDMETHOD.
ENDCLASS.

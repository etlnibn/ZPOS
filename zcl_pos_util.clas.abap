CLASS zcl_pos_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS co_all TYPE string VALUE '' ##NO_TEXT.
    CONSTANTS co_bundle TYPE string VALUE 'bundle' ##NO_TEXT.
    CONSTANTS co_car_pos_search TYPE rvari_vnam VALUE 'CAR_POS_SEARCH' ##NO_TEXT.
    CONSTANTS co_discount_group TYPE zpos_source2 VALUE 'DISCGROUP' ##NO_TEXT.


    CONSTANTS:
      BEGIN OF co_condition_type,
        competitor_vkorg_price TYPE kscha VALUE 'ZKC0',
        competitor_werks_price TYPE kscha VALUE 'ZKC1',
        store_price            TYPE kscha VALUE 'ZKP1',
        promotion_price        TYPE kscha VALUE 'VKA0',
        standard_price         TYPE kscha VALUE 'VKP0',
        effective_price        TYPE kscha VALUE 'ZWSO',
        discount_amount        TYPE kscha VALUE 'GPA1',
      END OF co_condition_type .


    CONSTANTS co_application TYPE kappl VALUE 'V' ##NO_TEXT.
    CONSTANTS co_condition_table_991 TYPE kotabnr VALUE '991' ##NO_TEXT.
    CONSTANTS co_english_language TYPE spras VALUE 'E' ##NO_TEXT.
    CONSTANTS co_detailed_language TYPE spras VALUE 'Z' ##NO_TEXT.
    CONSTANTS co_kappl TYPE kappl VALUE 'V' ##NO_TEXT.


    CONSTANTS:
      BEGIN OF co_offer_category,
        bonus_buy    TYPE zpos_offer_category VALUE 'B',
        sales_set    TYPE zpos_offer_category VALUE 'S',
        scaled_price TYPE zpos_offer_category VALUE 'P',
      END OF co_offer_category .

    CONSTANTS:
      BEGIN OF co_offer_discount_method,
        discount_amount  TYPE zpos_vk_discount_method VALUE '0',
        total_price      TYPE zpos_vk_discount_method VALUE '1',
        discount_percent TYPE zpos_vk_discount_method VALUE '2',
        pay_percent      TYPE zpos_vk_discount_method VALUE '3',     " Pay Percentage of select item(s)
      END OF co_offer_discount_method.


    CONSTANTS:
      BEGIN OF co_distribution_channel,
        store TYPE vtweg VALUE '10',
        web   TYPE vtweg VALUE '20',
      END OF co_distribution_channel .
    CONSTANTS co_extra_scales TYPE zpos_source2 VALUE 'EXTRASCALES' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF co_material_category,
        bundle TYPE attyp VALUE '10',
      END OF co_material_category .
    CONSTANTS:
      BEGIN OF co_msgty,
        error       TYPE msgty_co VALUE 'E',
        info        TYPE msgty_co VALUE 'I',
        success     TYPE msgty_co VALUE 'S',
        termination TYPE msgty_co VALUE 'A',
        warning     TYPE msgty_co VALUE 'W',
      END OF co_msgty .
    CONSTANTS co_offer TYPE zpos_source1 VALUE 'OFFER' ##NO_TEXT.
    CONSTANTS co_pos_msgid TYPE msgid VALUE 'ZPOS' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF co_pos_recipient,
        viking        TYPE zpos_recipient VALUE 'VK',
        labelprinting TYPE zpos_recipient VALUE 'LP',
        esl           TYPE zpos_recipient VALUE 'ES',
        mybau         TYPE zpos_recipient VALUE 'MB',
        bizerba       TYPE zpos_recipient VALUE 'BZ',
        magento_idoc  TYPE zpos_recipient VALUE 'MG',
        magento_api   TYPE zpos_recipient VALUE 'AP',
      END OF co_pos_recipient .
    CONSTANTS co_price TYPE zpos_source1 VALUE 'PRICE' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF co_recipient_status,
        created   TYPE zpos_recipient_status VALUE '10',
        sent      TYPE zpos_recipient_status VALUE '20',
        confirmed TYPE zpos_recipient_status VALUE '30',
        error     TYPE zpos_recipient_status VALUE '99',
      END OF co_recipient_status .
    CONSTANTS co_selopt_opt_eq TYPE ddoption VALUE 'EQ' ##NO_TEXT.
    CONSTANTS co_selopt_sign_i TYPE ddsign VALUE 'I' ##NO_TEXT.
    CONSTANTS co_set_discount TYPE zpos_source2 VALUE 'SETDISCOUNT' ##NO_TEXT.
    CONSTANTS co_sign_comma TYPE char1 VALUE ',' ##NO_TEXT.
    CONSTANTS co_sign_dot TYPE char1 VALUE '.' ##NO_TEXT.
    CONSTANTS co_sign_space TYPE char1 VALUE '' ##NO_TEXT.
    CONSTANTS co_simple TYPE string VALUE 'simple' ##NO_TEXT.
    CONSTANTS co_spras_e TYPE spras VALUE 'E' ##NO_TEXT.
    CONSTANTS co_tax TYPE zpos_source1 VALUE 'TAX' ##NO_TEXT.
    CONSTANTS co_viking TYPE zpos_grpid VALUE 'VIKING' ##NO_TEXT.
    CONSTANTS co_magento TYPE zpos_grpid VALUE 'MAGENTO' ##NO_TEXT.
    CONSTANTS co_article_check TYPE zpos_source2 VALUE 'ARTICLECHECK' ##NO_TEXT.

    CONSTANTS co_zeroblank TYPE char2 VALUE '0 ' ##NO_TEXT.

    CLASS-METHODS remove_dec_separator
      CHANGING
        !cv_fieldvalue TYPE any .
    CLASS-METHODS write_application_log
      IMPORTING
        !iv_logobj TYPE balobj_d
        !iv_subobj TYPE balsubobj
        !iv_msgid  TYPE symsgid
        !iv_msgno  TYPE symsgno
        !iv_msgty  TYPE symsgty
        !iv_msgv1  TYPE symsgv OPTIONAL
        !iv_msgv2  TYPE symsgv OPTIONAL
        !iv_msgv3  TYPE symsgv OPTIONAL
        !iv_msgv4  TYPE symsgv OPTIONAL .
    CLASS-METHODS get_car_rfc_destination
      RETURNING
        VALUE(rv_destination) TYPE rfcdest
      RAISING
        zcx_pos_exception .
    CLASS-METHODS create_appl_log
      IMPORTING
        !it_message          TYPE bapiret2_t
        !iv_object           TYPE balobj_d
        !iv_subobject        TYPE balsubobj
      RETURNING
        VALUE(rt_lognumbers) TYPE bal_t_lgnm .
protected section.
*"* protected components of class ZCL_DTA_UTIL
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_POS_UTIL IMPLEMENTATION.


  METHOD create_appl_log.

*    TYPE-POOLS: abap.

    DATA: lv_log_handle  TYPE balloghndl,
          lv_timestamp   TYPE tzntstmps,
          lv_timezone    TYPE timezone VALUE 'UTC',
          ls_str_log     TYPE bal_s_log,
          ls_str_balmsg  TYPE bal_s_msg,
          ls_str_message TYPE bapiret2,
          lv_msg_logged  TYPE boolean,
          lt_message     TYPE bapiret2_t.

*-Building messages
    LOOP AT it_message INTO ls_str_message.
      CLEAR ls_str_message.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = ls_str_message-type
          cl     = ls_str_message-id
          number = ls_str_message-number
          par1   = ls_str_message-message_v1
          par2   = ls_str_message-message_v2
          par3   = ls_str_message-message_v3
          par4   = ls_str_message-message_v4
        IMPORTING
          return = ls_str_message.
      APPEND ls_str_message TO lt_message.

    ENDLOOP.

    CHECK lt_message IS NOT INITIAL.

*-Logging messages
    CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP lv_timestamp TIME ZONE lv_timezone.

    ls_str_log-extnumber = lv_timestamp.
    CONDENSE ls_str_log-extnumber.
    ls_str_log-object = iv_object.
    ls_str_log-subobject = iv_subobject.
    ls_str_log-aldate_del = sy-datum + 5.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_str_log
      IMPORTING
        e_log_handle            = lv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_str_message-message.
*      WRITE: 'type',sy-msgty, 'message',l_str_message-message.
    ELSE.
      LOOP AT lt_message INTO ls_str_message.
        MOVE: ls_str_message-type    TO ls_str_balmsg-msgty,
        ls_str_message-id            TO ls_str_balmsg-msgid,
        ls_str_message-number        TO ls_str_balmsg-msgno,
        ls_str_message-message_v1    TO ls_str_balmsg-msgv1,
        ls_str_message-message_v2    TO ls_str_balmsg-msgv2,
        ls_str_message-message_v3    TO ls_str_balmsg-msgv3,
        ls_str_message-message_v4    TO ls_str_balmsg-msgv4.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle     = lv_log_handle
            i_s_msg          = ls_str_balmsg
          IMPORTING
            e_msg_was_logged = lv_msg_logged
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.

        IF sy-subrc <> 0.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_str_message-message.
*          WRITE: 'type',sy-msgty, 'message',ls_str_message-message.
        ENDIF.
      ENDLOOP
      .
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            i_save_all       = abap_true
          IMPORTING
            e_new_lognumbers = rt_lognumbers
          EXCEPTIONS
            log_not_found    = 1
            save_not_allowed = 2
            numbering_error  = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_str_message-message.
*          WRITE: 'type',sy-msgty, 'message',ls_str_message-message.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_car_rfc_destination.

    SELECT SINGLE low
          INTO  rv_destination
          FROM  tvarvc
          WHERE name = co_car_pos_search
          AND   type = 'P'
          AND   numb = '0000'.

    IF rv_destination IS INITIAL.
      RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e032(zpos).
    ENDIF.

  ENDMETHOD.


  METHOD remove_dec_separator.
    REPLACE ALL OCCURRENCES OF co_sign_dot IN cv_fieldvalue WITH co_sign_space.
    REPLACE ALL OCCURRENCES OF co_sign_comma IN cv_fieldvalue WITH co_sign_space.
    CONDENSE cv_fieldvalue NO-GAPS.
  ENDMETHOD.                    "REMOVE_DEC_SEPARATOR


  METHOD write_application_log.

    DATA: lf_obj        TYPE balobj_d,
          ls_header     TYPE balhdri,
          lf_log_handle TYPE balloghndl,
          lf_log_number TYPE balognr,
          lt_msg        TYPE balmi_t,
          ls_msg        TYPE balmi,
          lt_lognum     TYPE TABLE OF balnri,
          ls_lognum     TYPE balnri.


* Header information for the log
    ls_header-object     = iv_logobj.
    ls_header-subobject  = iv_subobj.
    ls_header-aldate     = sy-datum.
    ls_header-altime     = sy-uzeit.
    ls_header-aluser     = sy-uname.
    ls_header-aldate_del = sy-datum + 7.

*
* Get the Log handle using the header

    CALL FUNCTION 'APPL_LOG_WRITE_HEADER'
      EXPORTING
        header              = ls_header
      IMPORTING
        e_log_handle        = lf_log_handle
      EXCEPTIONS
        object_not_found    = 1
        subobject_not_found = 2
        error               = 3
        OTHERS              = 4.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno

              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

*
* Get the next avaliable Log number

    CALL FUNCTION 'BAL_DB_LOGNUMBER_GET'
      EXPORTING
        i_client                 = sy-mandt
        i_log_handle             = lf_log_handle
      IMPORTING
        e_lognumber              = lf_log_number
      EXCEPTIONS
        log_not_found            = 1
        lognumber_already_exists = 2
        numbering_error          = 3
        OTHERS                   = 4.

*
* Fill the Application Log messages.

    ls_msg-msgty = iv_msgty.
    ls_msg-msgid = iv_msgid.
    ls_msg-msgno = iv_msgno.
    ls_msg-msgv1 = iv_msgv1.
    ls_msg-msgv2 = iv_msgv2.
    ls_msg-msgv3 = iv_msgv3.
    ls_msg-msgv4 = iv_msgv4.

    APPEND ls_msg TO lt_msg.

* Write the Log mesages to the memory

    CALL FUNCTION 'APPL_LOG_WRITE_MESSAGES'
      EXPORTING
        object              = iv_logobj
        subobject           = iv_subobj
        log_handle          = lf_log_handle
      TABLES
        messages            = lt_msg
      EXCEPTIONS
        object_not_found    = 1
        subobject_not_found = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

* write the log message to Database which can be later analyzed
* from transaction SLG1

    MOVE-CORRESPONDING ls_header TO ls_lognum.
    ls_lognum-lognumber = lf_log_number.
    APPEND ls_lognum TO lt_lognum.



    CALL FUNCTION 'APPL_LOG_WRITE_DB'
      EXPORTING
        object                = iv_logobj
        subobject             = iv_subobj
        log_handle            = lf_log_handle
      TABLES
        object_with_lognumber = lt_lognum
      EXCEPTIONS
        object_not_found      = 1
        subobject_not_found   = 2
        internal_error        = 3
        OTHERS                = 4.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

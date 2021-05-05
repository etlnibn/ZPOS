class ZCL_POS_UTIL definition
  public
  final
  create public .

public section.

  constants CO_ALL type STRING value '' ##NO_TEXT.
  constants CO_BUNDLE type STRING value 'bundle' ##NO_TEXT.
  constants CO_CAR_POS_SEARCH type RVARI_VNAM value 'CAR_POS_SEARCH' ##NO_TEXT.
  constants CO_DISCOUNT_GROUP type ZPOS_SOURCE2 value 'DISCGROUP' ##NO_TEXT.
  constants:
    BEGIN OF co_condition_type,
        competitor_vkorg_price TYPE kscha VALUE 'ZKC0',
        competitor_werks_price TYPE kscha VALUE 'ZKC1',
        store_price            TYPE kscha VALUE 'ZKP1',
        promotion_price        TYPE kscha VALUE 'VKA0',
        standard_price         TYPE kscha VALUE 'VKP0',
        effective_price        TYPE kscha VALUE 'ZWSO',
        discount_amount        TYPE kscha VALUE 'GPA1',
      END OF co_condition_type .
  constants CO_APPLICATION type KAPPL value 'V' ##NO_TEXT.
  constants CO_CONDITION_TABLE_991 type KOTABNR value '991' ##NO_TEXT.
  constants CO_ENGLISH_LANGUAGE type SPRAS value 'E' ##NO_TEXT.
  constants CO_DETAILED_LANGUAGE type SPRAS value 'Z' ##NO_TEXT.
  constants CO_KAPPL type KAPPL value 'V' ##NO_TEXT.
  constants:
    BEGIN OF co_offer_category,
        bonus_buy    TYPE zpos_offer_category VALUE 'B',
        sales_set    TYPE zpos_offer_category VALUE 'S',
        scaled_price TYPE zpos_offer_category VALUE 'P',
      END OF co_offer_category .
  constants:
    BEGIN OF co_offer_discount_method,
        discount_amount  TYPE zpos_vk_discount_method VALUE '0',
        total_price      TYPE zpos_vk_discount_method VALUE '1',
        discount_percent TYPE zpos_vk_discount_method VALUE '2',
        pay_percent      TYPE zpos_vk_discount_method VALUE '3',     " Pay Percentage of select item(s)
      END OF co_offer_discount_method .
  constants:
    BEGIN OF co_distribution_channel,
        store TYPE vtweg VALUE '10',
        web   TYPE vtweg VALUE '20',
      END OF co_distribution_channel .
  constants CO_EXTRA_SCALES type ZPOS_SOURCE2 value 'EXTRASCALES' ##NO_TEXT.
  constants:
    BEGIN OF co_material_category,
        bundle TYPE attyp VALUE '10',
      END OF co_material_category .
  constants:
    BEGIN OF co_msgty,
        error       TYPE msgty_co VALUE 'E',
        info        TYPE msgty_co VALUE 'I',
        success     TYPE msgty_co VALUE 'S',
        termination TYPE msgty_co VALUE 'A',
        warning     TYPE msgty_co VALUE 'W',
      END OF co_msgty .
  constants CO_OFFER type ZPOS_SOURCE1 value 'OFFER' ##NO_TEXT.
  constants CO_POS_MSGID type MSGID value 'ZPOS' ##NO_TEXT.
  constants:
    BEGIN OF co_pos_recipient,
        viking        TYPE zpos_recipient VALUE 'VK',
        labelprinting TYPE zpos_recipient VALUE 'LP',
        esl           TYPE zpos_recipient VALUE 'ES',
        mybau         TYPE zpos_recipient VALUE 'MB',
        bizerba       TYPE zpos_recipient VALUE 'BZ',
        magento_idoc  TYPE zpos_recipient VALUE 'MG',
        magento_api   TYPE zpos_recipient VALUE 'AP',
      END OF co_pos_recipient .
  constants CO_PRICE type ZPOS_SOURCE1 value 'PRICE' ##NO_TEXT.
  constants:
    BEGIN OF co_recipient_status,
        created   TYPE zpos_recipient_status VALUE '10',
        sent      TYPE zpos_recipient_status VALUE '20',
        confirmed TYPE zpos_recipient_status VALUE '30',
        error     TYPE zpos_recipient_status VALUE '99',
      END OF co_recipient_status .
  constants CO_SELOPT_OPT_EQ type DDOPTION value 'EQ' ##NO_TEXT.
  constants CO_SELOPT_SIGN_I type DDSIGN value 'I' ##NO_TEXT.
  constants CO_SET_DISCOUNT type ZPOS_SOURCE2 value 'SETDISCOUNT' ##NO_TEXT.
  constants CO_SIGN_COMMA type CHAR1 value ',' ##NO_TEXT.
  constants CO_SIGN_DOT type CHAR1 value '.' ##NO_TEXT.
  constants CO_SIGN_SPACE type CHAR1 value '' ##NO_TEXT.
  constants CO_SIMPLE type STRING value 'simple' ##NO_TEXT.
  constants CO_SPRAS_E type SPRAS value 'E' ##NO_TEXT.
  constants CO_TAX type ZPOS_SOURCE1 value 'TAX' ##NO_TEXT.
  constants CO_VIKING type ZPOS_GRPID value 'VIKING' ##NO_TEXT.
  constants CO_MAGENTO type ZPOS_GRPID value 'MAGENTO' ##NO_TEXT.
  constants CO_ARTICLE_CHECK type ZPOS_SOURCE2 value 'ARTICLECHECK' ##NO_TEXT.
  constants CO_ZEROBLANK type CHAR2 value '0 ' ##NO_TEXT.

  class-methods REMOVE_DEC_SEPARATOR
    changing
      !CV_FIELDVALUE type ANY .
  class-methods WRITE_APPLICATION_LOG
    importing
      !IV_LOGOBJ type BALOBJ_D
      !IV_SUBOBJ type BALSUBOBJ
      !IV_MSGID type SYMSGID
      !IV_MSGNO type SYMSGNO
      !IV_MSGTY type SYMSGTY
      !IV_MSGV1 type SYMSGV optional
      !IV_MSGV2 type SYMSGV optional
      !IV_MSGV3 type SYMSGV optional
      !IV_MSGV4 type SYMSGV optional .
  class-methods GET_CAR_RFC_DESTINATION
    returning
      value(RV_DESTINATION) type RFCDEST
    raising
      ZCX_POS_EXCEPTION .
  class-methods CREATE_APPL_LOG
    importing
      !IT_MESSAGE type BAPIRET2_T
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
    returning
      value(RT_LOGNUMBERS) type BAL_T_LGNM .
  class-methods GET_LANGUAGE_BY_COUNTRY
    importing
      !IV_COUNTRY type LAND1
    returning
      value(RV_LANGUAGE) type SPRAS
    raising
      ZCX_POS_EXCEPTION .
  class-methods GET_LANGUAGE_BY_SALESORG
    importing
      !IV_VKORG type VKORG
    returning
      value(RV_LANGUAGE) type SPRAS .
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


  METHOD get_language_by_country.

    CASE iv_country.
      WHEN 'SE'.
        rv_language = 'V'.      "Swedish
      WHEN 'DK'.
        rv_language = 'K'.      "Danish
      WHEN 'NO'.
        rv_language = 'O'.      "Norwegian
      WHEN 'IS'.
        rv_language = 'b'.      "Icenlandic
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_pos_exception MESSAGE e025(zpos).

    ENDCASE.

  ENDMETHOD.


  METHOD get_language_by_salesorg.

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
        rv_language = zcl_pos_util=>co_english_language.      "English for all other cases

    ENDCASE.

  ENDMETHOD.
ENDCLASS.

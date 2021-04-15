CLASS zcl_pos_search_return DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    TYPES:
      BEGIN OF tp_s_invoice_header,
        documentnumber     TYPE zposi_billdoc_partner-documentnumber,
        documentdate       TYPE zposi_billdoc_partner-documentdate,
        soldtoparty        TYPE zposi_billdoc_partner-soldtoparty,
        name               TYPE zposi_billdoc_partner-name,
        invoicegrossamount TYPE zposi_billdoc_partner-invoicegrossamount,
        invoicerowcount    TYPE zposi_billdoc_partner-invoicerowcount,
        vatnumber          TYPE zposi_billdoc_partner-vatnumber,
        currency           TYPE zposi_billdoc_partner-currency,
      END OF tp_s_invoice_header .

    TYPES: tp_t_billdoc_partner TYPE STANDARD TABLE OF zposi_billdoc_partner.
    TYPES:
      tp_t_invoice_details TYPE STANDARD TABLE OF zposi_billdoc_partner .
    TYPES:
      tp_t_invoice_header TYPE STANDARD TABLE OF tp_s_invoice_header .
    TYPES:
      tp_r_email TYPE RANGE OF ad_smtpadr .
    TYPES:
      tp_r_name TYPE RANGE OF /sapce/fkua_txbpnamf .
    TYPES:
      tp_r_phone TYPE RANGE OF ad_tlnmbr .
    TYPES:
      tp_r_vatreg   TYPE RANGE OF stceg .
    TYPES:
      BEGIN OF tp_report_data,
        retailstoreid     TYPE zpos_retailstoreid,
        businessdaydate   TYPE zpos_businessdaydate,
        time              TYPE tims,
        transindex        TYPE zpos_transindex,
        transnumber       TYPE zpos_transactionsequencenumb,
        workstationid     TYPE zpos_workstationid,
        operatorid        TYPE zpos_operatorid,
        department        TYPE zpos_department,
        turnover          TYPE zpos_transturnover,
        transcurrency     TYPE zpos_transactioncurrency,
        receipt_linecount TYPE zpos_itemcount,
        tender_cash       TYPE zpos_amount,
        tender_ccard      TYPE zpos_amount,
        tender_gcard      TYPE zpos_amount,
        tender_iou        TYPE zpos_amount,
        tender_bank       TYPE zpos_amount,
        tender_invoice    TYPE zpos_amount,
        receipt_status    TYPE zpos_retail_trans_status,
        color             TYPE lvc_t_scol,               " ALV OO type
        style             TYPE lvc_t_styl,
      END OF tp_report_data .

    DATA mt_invoice_details TYPE tp_t_invoice_details .
    DATA mt_invoice_header TYPE tp_t_invoice_header .
    DATA mt_pos_result_list TYPE zpos_search_transaction_tty .
    DATA mt_r_fkarv TYPE RANGE OF fkarv.

    METHODS execute_invoice_search
      IMPORTING
        !it_r_vbeln        TYPE fip_t_vbeln_range OPTIONAL
        !it_r_date         TYPE range_t_dats OPTIONAL
        !it_r_invoicegross TYPE zpos_rt_amount OPTIONAL
        !it_r_name         TYPE tp_r_name OPTIONAL
        !it_r_taxreg       TYPE tp_r_vatreg OPTIONAL
        !it_r_ean11        TYPE wart_tr_ean11 OPTIONAL
        !it_r_kunnr        TYPE wrf_kunnr_rtty OPTIONAL
        !it_r_itemgross    TYPE zpos_rt_amount OPTIONAL
        !iv_records        TYPE int4 OPTIONAL
        !it_r_email        TYPE tp_r_email OPTIONAL
        !it_r_mobile       TYPE tp_r_phone OPTIONAL
        !iv_vkorg          TYPE vkorg OPTIONAL
        !iv_currency       TYPE waers OPTIONAL
        !iv_zero_qty       TYPE boole_d OPTIONAL
      RETURNING
        VALUE(rt_message)  TYPE zpos_bapi_message_tty .
    METHODS execute_pos_search
      IMPORTING
        !iv_destination       TYPE rfcdest
        !it_r_retailstoreid   TYPE zpos_rt_retailstoreid
        !it_r_businessdaydate TYPE zpos_rt_businessdaydate
        !it_r_workstationid   TYPE zpos_rt_workstationid OPTIONAL
        !it_r_receipt         TYPE zpos_rt_transnumber OPTIONAL
        !it_r_operatorid      TYPE zpos_rt_operatorid OPTIONAL
        !it_r_department      TYPE zpos_rt_department OPTIONAL
        !it_r_itemid          TYPE zpos_rt_itemid OPTIONAL
        !it_r_itemcount       TYPE zpos_rt_count OPTIONAL
        !it_r_transamount     TYPE zpos_rt_amount OPTIONAL
        !iv_ccard_start       TYPE char6 OPTIONAL
        !iv_ccard_end         TYPE char4 OPTIONAL
        !iv_other_card        TYPE zpos_referenceid OPTIONAL
        !iv_records           TYPE int4 OPTIONAL
        !iv_currency          TYPE waers
      RETURNING
        VALUE(rt_message)     TYPE zpos_bapi_message_tty .
    METHODS display_search_results .
    CLASS-METHODS update_receipt_in_car
      IMPORTING
        !iv_destination        TYPE rfcdest
        !is_return_transaction TYPE zpos_search_transaction_sty
      EXPORTING
        !et_message            TYPE bapiret2_tt .
    CLASS-METHODS close_dekort_order
      IMPORTING
        !iv_docnum  TYPE vbeln_va DEFAULT '0000000336'
      EXPORTING
        !et_message TYPE bapiret2_tt .
    CLASS-METHODS create_inv_adv_retur
      IMPORTING
        !is_head_comv TYPE msr_rfc_ro_head_comv
        !it_item_comv TYPE tdt_msr_rfc_ro_item_comv
      EXPORTING
        !ev_docnum    TYPE vbeln_va
        !et_message   TYPE bapiret2_tt .
    METHODS create_pos_adv_return
      IMPORTING
        !is_pos_transaction TYPE zpos_search_transaction_sty
      EXPORTING
        !ev_docnum          TYPE vbeln_va
        !et_message         TYPE bapiret2_tt .
    CLASS-METHODS view_document_flow
      IMPORTING
        !iv_vbeln TYPE vbeln .
    CLASS-METHODS view_invoice
      IMPORTING
        !iv_vbeln TYPE vbeln .
    CLASS-METHODS view_receipt_in_car
      IMPORTING
        !iv_destination     TYPE rfcdest
        !iv_retailstoreid   TYPE zpos_retailstoreid
        !iv_businessdaydate TYPE zpos_businessdaydate
        !iv_transindex      TYPE zpos_transindex .

    CLASS-METHODS invoice_search_tf
        FOR TABLE FUNCTION zpostf_billdoc_partner.
*    CLASS-METHODS crmd_partner_but000 FOR TABLE FUNCTION zpostf_billdoc_partner.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_table TYPE REF TO cl_salv_table .
    DATA mr_bcs_exception TYPE REF TO cx_bcs .
    DATA mr_column TYPE REF TO cl_salv_column_table .
    DATA mr_columns TYPE REF TO cl_salv_columns_table .
    DATA mr_display TYPE REF TO cl_salv_display_settings .
    DATA mr_document TYPE REF TO cl_document_bcs .
    DATA mr_events TYPE REF TO cl_salv_events_table .
    DATA mr_functional_settings TYPE REF TO cl_salv_functional_settings .
    DATA mr_functions TYPE REF TO cl_salv_functions_list .
    DATA mr_int_sender TYPE REF TO cl_cam_address_bcs .
    DATA mr_overflow TYPE REF TO cx_sy_arithmetic_overflow .
    DATA mr_tooltips TYPE REF TO cl_salv_tooltips .
    DATA mr_zerodivide TYPE REF TO cx_sy_zerodivide .
    DATA ms_color TYPE lvc_s_scol .
    DATA ms_seltab TYPE rsparams .
    DATA mt_hex TYPE solix_tab .
    DATA:
      mt_report_data TYPE STANDARD TABLE OF tp_report_data .
    CONSTANTS:
      BEGIN OF co_alv_color,
        yellow TYPE lvc_col VALUE '3', "
        green  TYPE lvc_col VALUE '5', "
        red    TYPE lvc_col VALUE '6', "
      END OF co_alv_color .
    CONSTANTS co_car_system TYPE string VALUE 'CA1CLNT300' ##NO_TEXT.
    CONSTANTS co_return_type TYPE auart VALUE 'RE2' ##NO_TEXT.

    METHODS on_alv_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        !row
        !column .
    METHODS alv_show_pos_workbench
      IMPORTING
        !is_report_data TYPE tp_report_data .
    METHODS util_search_result_to_alv .

    METHODS set_relevant_invoice_types.
ENDCLASS.



CLASS zcl_pos_search_return IMPLEMENTATION.

  METHOD set_relevant_invoice_types.

    IF mt_r_fkarv IS INITIAL.

      SELECT fkarv
             FROM  tvcpa
             INTO TABLE @DATA(lt_tvcpa)
             WHERE ( auarn = 'RE2'
             OR auarn = 'ZE2').

      LOOP AT lt_tvcpa INTO DATA(ls_tvcpa) WHERE fkarv IS NOT INITIAL.
        APPEND INITIAL LINE TO mt_r_fkarv ASSIGNING FIELD-SYMBOL(<ls_fkarv>).
        <ls_fkarv>-sign = 'I'.
        <ls_fkarv>-option = 'EQ'.
        <ls_fkarv>-low = ls_tvcpa-fkarv.
      ENDLOOP.
      SORT mt_r_fkarv BY low ASCENDING. DELETE ADJACENT DUPLICATES FROM mt_r_fkarv COMPARING low.

    ENDIF.

  ENDMETHOD.

  METHOD invoice_search_tf
          BY DATABASE FUNCTION FOR HDB
          LANGUAGE SQLSCRIPT
          OPTIONS READ-ONLY
          USING zposi_billdoc_partner.

    lt_invoice_details = select bp.client as client,
                  bp.documentnumber      as documentnumber,
                  bp.salesorg            as salesorg,
                  bp.distchannel         as distchannel,
                  bp.division            as division,
                  bp.documentdate        as documentdate,
                  bp.soldtoparty         as soldtoparty,
                  bp.currency            as currency,
                  bp.ordercategory       as ordercategory,
                  bp.billingcategory     as billingcategory,
                  bp.billingtype         as billingtype,
                  bp.iscancelled         as iscancelled,
                  bp.invoicenetamount    as invoicenetamount,
                  bp.invoicetaxamount    as invoicetaxamount,
                  bp.invoicegrossamount  as invoicegrossamount,
                  bp.vatnumber           as vatnumber,
                  bp.documentitem        as documentitem,
                  bp.article             as article,
                  bp.gtin                as gtin,
                  bp.articledescription  as articledescription,
                  bp.unit                as unit,
                  bp.quantity            as quantity,
                  bp.openquantity        as openquantity,
                  bp.netamount           as netamount,
                  bp.taxamount           as taxamount,
                  bp.grossamount         as grossamount,
                  bp.invoicerowcount     as invoicerowcount,
                  bp.search1             as search1,
                  bp.search2             as search2,
                  bp.name                as name,
                  bp.personnumber        as personnumber,
                  bp.addressid           as addressid,
                  bp.emailaddress        as emailaddress,
                  bp.mobile              as mobile
                  from zposi_billdoc_partner as bp ;

*    lt_sddocumentflow =
*               select
*               flow.PrecedingDocument       as invoicedocumentnumber,
*               flow.PrecedingDocumentitem   as invoicedocumentitem,
*               flow.subsequentdocument      as returndocumentnumber,
*               flow.subsequentdocumentitem  as returnedocumentitem
*               from zposi_sddocumentflow    as flow
*               inner join :lt_invoice_details as details
*               on   flow.subsequentdocument = details.documentnumber
*               and  flow.subsequentdocumentitem = details.documentitem ;
**              Returns Order Category
*               where flow.subsequentdocumentcategory = 'H'
**              Billing Category
*               and   flow.precedingdocumentcategory = 'M'
*               and   flow.salesdocumenttype = 'RE2';

            return select * from :lt_invoice_details;

  endmethod.

  METHOD alv_show_pos_workbench.

*    DATA: ls_selection     TYPE /posdw/mon_selection,
**          ls_mon_filter    TYPE /posdw/mon_fs_filter,
*          ls_rng_store     TYPE /posdw/s_retailstoreid,
*          lt_rng_store     TYPE /posdw/rt_retailstoreid,
*          ls_rng_date      TYPE /posdw/r_businessdaydate,
*          lt_rng_date      TYPE /posdw/rt_businessdaydate,
*          ls_rng_bust      TYPE /posdw/r_businesstype,
*          lt_rng_bust      TYPE /posdw/rt_bus_type,
*          ls_rng_transtype TYPE /posdw/r_transtypecode,
*          lt_rng_transtype TYPE /posdw/rt_transtypecode,
*          lt_transaction   TYPE /posdw/tt_transaction_int,
*          ls_rng_index     TYPE /posdw/r_transindex,
*          lt_rng_index     TYPE /posdw/rt_transindex.
*
*
*    IF NOT is_report_data-retailstoreid IS INITIAL AND
*            is_report_data-no_data = abap_false.
*
** Fill workbench selection criterian
** 1. Store ID
*      ls_rng_store-sign = 'I'.
*      ls_rng_store-option = 'EQ'.
*      ls_rng_store-low = is_report_data-retailstoreid.
*      APPEND ls_rng_store TO lt_rng_store.
*      ls_selection-general-selection-retailstoreid = lt_rng_store.
**      ls_mon_filter-retailstoreid = lt_rng_store.
*
** 2. Business day Date
*      ls_rng_date-sign = 'I'.
*      ls_rng_date-option = 'EQ'.
*      ls_rng_date-low = sel-r_bdd->*.
*      APPEND ls_rng_date TO lt_rng_date.
*      ls_mon_filter-businessdaydate = lt_rng_date.
*
** Not Good from Performance Perspective
** 3. Business Type -> Only Sales Transactions
*      IF iv_show_eod = abap_false.
*
*        ls_rng_bust-sign = 'I'.
*        ls_rng_bust-option = 'EQ'.
*        ls_rng_bust-low = /posdw/if_gen_constants=>businesstype_retail.
*        APPEND ls_rng_bust TO lt_rng_bust.
*        ls_mon_filter-businesstype = lt_rng_bust.
*
*
*      ELSE.
*
*        ls_rng_bust-sign = 'I'.
*        ls_rng_bust-option = 'EQ'.
*        ls_rng_bust-low = /posdw/if_gen_constants=>businesstype_control.
*        APPEND ls_rng_bust TO lt_rng_bust.
*        ls_mon_filter-businesstype = lt_rng_bust.
*
*
** Not Good from Performance Perspective
** 3a. Transaction Type -> EoD Only
*
*        ls_rng_transtype-sign = 'I'.
*        ls_rng_transtype-option = 'EQ'.
*        ls_rng_transtype-low = zcl_pos_util=>co_trans_prelim_eod.
*        APPEND ls_rng_transtype TO lt_rng_transtype.
*        ls_mon_filter-transtypecode = lt_rng_transtype.
*      ENDIF.
*
** 4. Display parameters
*      ls_selection-general-parameters-call_trl = abap_true.
*      ls_selection-general-parameters-call_editor = abap_true.
*      ls_selection-general-parameters-display_postr = abap_true.
*      ls_selection-general-parameters-display_aggr = abap_false.
*      ls_selection-postr-selection-businessdaydate = lt_rng_date.
*      ls_selection-postr-parameters-max_count_trl = 5000.
*      ls_selection-postr-parameters-time_intv = 3600.
*      ls_selection-postr-selection-businesstype = lt_rng_bust.        " Bad for Performance
*      IF iv_show_eod = abap_true.
*        ls_selection-postr-filter-transtypecode = lt_rng_transtype.   " Bad for Performance
*      ENDIF.
*
** Show Everything
** 4. Call to POS workbench
*      CALL FUNCTION '/POSDW/CALL_MONITOR_WITH_SEL' DESTINATION co_car_system
*        EXPORTING
*          is_selection          = ls_selection
*          i_no_selection_screen = abap_true.
*

*    ENDIF.

  ENDMETHOD.


  METHOD close_dekort_order.

    DATA: ls_header_in      TYPE bapisdh1,
          ls_header_inx     TYPE bapisdh1x,
          lt_order_item_in  TYPE bapisditm_tt,
          lt_order_item_inx TYPE bapisditmx_tt,
          lt_message        TYPE bapiret2_tt.

*----------------------------------------------------------------------------
* ORDER HEADER - START
*----------------------------------------------------------------------------

*    ls_header_in-ord_reason = '1'.
*    ls_header_in-purch_no_c = 'Dekort Closed'.
*    ls_header_in-dlv_block  = '01'.
*    ls_header_in-bill_block = '01'.

    ls_header_inx-updateflag  = 'U'.
*    ls_header_in-ord_reason  = abap_true.
*    ls_header_inx-purch_no_c = abap_true.
*    ls_header_inx-dlv_block  = abap_true.
*    ls_header_inx-bill_block = abap_true.

    SELECT posnr FROM vbap INTO TABLE @DATA(lt_posnr) WHERE vbeln = @iv_docnum.

    LOOP AT lt_posnr INTO DATA(ls_posnr).
      APPEND INITIAL LINE TO lt_order_item_in ASSIGNING FIELD-SYMBOL(<ls_order_item_in>).
      <ls_order_item_in>-itm_number = ls_posnr-posnr.
      <ls_order_item_in>-reason_rej = 'Z1'.

      APPEND INITIAL LINE TO lt_order_item_inx ASSIGNING FIELD-SYMBOL(<ls_order_item_inx>).
      <ls_order_item_inx>-updateflag = 'U'.
      <ls_order_item_inx>-itm_number = abap_true.
      <ls_order_item_inx>-reason_rej = abap_true.

    ENDLOOP.


*----------------------------------------------------------------------------
* ORDER HEADER - START
*----------------------------------------------------------------------------

*      ORDER_HEADER_IN        LIKE  BAPISDH1
*      ORDER_HEADER_INX       LIKE  BAPISDH1X
*      SIMULATION             LIKE  BAPIFLAG-BAPIFLAG
*      BEHAVE_WHEN_ERROR      LIKE  BAPIFLAG-BAPIFLAG
*      INT_NUMBER_ASSIGNMENT  LIKE  BAPIFLAG-BAPIFLAG
*      LOGIC_SWITCH           LIKE  BAPISDLS
*      NO_STATUS_BUF_INIT     LIKE  BAPIFLAG-BAPIFLAG


    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = iv_docnum
*       order_header_in  = ls_header_in
        order_header_inx = ls_header_inx
      TABLES
        return           = et_message
        order_item_in    = lt_order_item_in
        order_item_inx   = lt_order_item_inx.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDMETHOD.


  METHOD create_inv_adv_retur.

    DATA: ls_head_comx TYPE  msr_rfc_ro_head_comc,
          lt_item_comx TYPE  tdt_msr_rfc_ro_item_comx.

*----------------------------------------------------------------------------
* ORDER HEADER - START
*----------------------------------------------------------------------------

    ls_head_comx-doc_type    = abap_true.
    ls_head_comx-salesorg    = abap_true.
    ls_head_comx-distr_chan  = abap_true.
    ls_head_comx-division    = abap_true.
    ls_head_comx-sold_to     = abap_true.
    ls_head_comx-ship_to     = abap_true.

*----------------------------------------------------------------------------
* ORDER HEADER - END
*----------------------------------------------------------------------------


*----------------------------------------------------------------------------
* ORDER ITEMS - START
*----------------------------------------------------------------------------
    LOOP AT it_item_comv INTO DATA(ls_lineitem).

      APPEND INITIAL LINE TO lt_item_comx ASSIGNING FIELD-SYMBOL(<ls_lineitem_x>).
      <ls_lineitem_x>-itm_number = ls_lineitem-itm_number.
      <ls_lineitem_x>-req_qty    = abap_true.
      <ls_lineitem_x>-ref_doc    = abap_true.
      <ls_lineitem_x>-ref_doc_it = abap_true.

    ENDLOOP.
*----------------------------------------------------------------------------
* ORDER ITEMS - END
*----------------------------------------------------------------------------

    IF NOT it_item_comv[] IS INITIAL.

      CALL FUNCTION 'SD_ADV_RETURNS_CREATE'
        EXPORTING
          is_head_comv     = is_head_comv
          is_head_comx     = ls_head_comx
          it_rfc_item_comv = it_item_comv
          it_rfc_item_comx = lt_item_comx
        IMPORTING
          ev_salesdocument = ev_docnum
          et_return        = et_message.

      IF NOT ev_docnum IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        DATA(lv_counter) =  0.
        DATA(lv_max)     =  10. " Wait for 10 seconds in maximum
        WHILE lv_counter < lv_max.
          CALL FUNCTION 'ENQUEUE_EVVBAKE'
            EXPORTING
              vbeln          = ev_docnum
            EXCEPTIONS
              foreign_lock   = 02
              system_failure = 03.
          IF sy-subrc = 0.
            CALL FUNCTION 'DEQUEUE_EVVBAKE'
              EXPORTING
                vbeln = ev_docnum.
            lv_counter = lv_max + 1.
          ELSE.
            lv_counter = lv_counter + 1.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDWHILE.
        IF lv_counter = lv_max.
          APPEND INITIAL LINE TO et_message ASSIGNING FIELD-SYMBOL(<ls_message>).
          <ls_message>-id      = 'ZPOS'.
          <ls_message>-type    = 'E'.
          <ls_message>-message = 'Tech Error: Not possible to create Sales Order Lock'.
        ENDIF.

      ELSE.
        LOOP AT et_message INTO DATA(ls_message) WHERE type = 'E'.
          MESSAGE ls_message-message TYPE 'I'.
        ENDLOOP.

        APPEND INITIAL LINE TO et_message ASSIGNING <ls_message>.
        <ls_message>-id      = 'ZPOS'.
        <ls_message>-type    = 'E'.
        <ls_message>-message = 'Tech Error: Return could not be created'.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD create_pos_adv_return.

    DATA: ls_header_in     TYPE bapisdhead1,
          ls_header_inx    TYPE bapisdhead1x,
          lt_items_in      TYPE STANDARD TABLE OF bapisditem,
          lt_items_inx     TYPE STANDARD TABLE OF bapisditemx,
          lt_partners      TYPE STANDARD TABLE OF bapipartnr,
          lt_schedules_in  TYPE STANDARD TABLE OF bapischedule,
          lt_schedules_inx TYPE STANDARD TABLE OF bapischedulex,
          lt_conditions_in TYPE STANDARD TABLE OF bapicondition,
          lv_item_number   TYPE posnr_va VALUE '000010',
          lv_bstkd         TYPE bstkd.   " Purchase Order Reference

*----------------------------------------------------------------------------
* ORDER HEADER - START
*----------------------------------------------------------------------------

    ls_header_in-doc_type = co_return_type.
    ls_header_in-sales_org  = '1090'.
    ls_header_in-distr_chan = '90'.
    ls_header_in-division   = '90'.
    ls_header_in-purch_date = sy-datum.
    ls_header_in-req_date_h = sy-datum.
    ls_header_in-doc_date   = sy-datum.
    ls_header_in-price_date = sy-datum.
    ls_header_in-ord_reason = '101'.

    ls_header_in-purch_no_c = is_pos_transaction-retailstoreid && '-' &&
                              is_pos_transaction-businessdaydate && '-'.

    lv_bstkd = is_pos_transaction-workstationid.
    SHIFT lv_bstkd LEFT DELETING LEADING '0'.
    ls_header_in-purch_no_c = ls_header_in-purch_no_c && lv_bstkd && '-'.

    lv_bstkd = is_pos_transaction-transnumber.
    SHIFT lv_bstkd LEFT DELETING LEADING '0'.
    ls_header_in-purch_no_c = ls_header_in-purch_no_c && lv_bstkd.


    ls_header_inx-updateflag  = 'I'.
    ls_header_inx-sales_org  = abap_true.
    ls_header_inx-distr_chan = abap_true.
    ls_header_inx-division   = abap_true.
    ls_header_inx-purch_date = abap_true.
    ls_header_inx-req_date_h = abap_true.
    ls_header_inx-doc_date   = abap_true.
    ls_header_inx-price_date = abap_true.
    ls_header_in-ord_reason  = abap_true.
    ls_header_inx-purch_no_c = abap_true.


*----------------------------------------------------------------------------
* ORDER HEADER - END
*----------------------------------------------------------------------------


*----------------------------------------------------------------------------
* ORDER PARTNERS - START
*----------------------------------------------------------------------------
    APPEND INITIAL LINE TO lt_partners ASSIGNING FIELD-SYMBOL(<ls_partner>).
    <ls_partner>-partn_role = 'AG'.
    <ls_partner>-partn_numb = 'PAYMENT'.

    APPEND INITIAL LINE TO lt_partners ASSIGNING <ls_partner>.
    <ls_partner>-partn_role = 'RE'.
    <ls_partner>-partn_numb = 'PAYMENT'.

    APPEND INITIAL LINE TO lt_partners ASSIGNING <ls_partner>.
    <ls_partner>-partn_role = 'RG'.
    <ls_partner>-partn_numb = 'PAYMENT'.

    APPEND INITIAL LINE TO lt_partners ASSIGNING <ls_partner>.
    <ls_partner>-partn_role = 'WE'.
    <ls_partner>-partn_numb = 'PAYMENT'.

*----------------------------------------------------------------------------
* ORDER PARTNERS - END
*----------------------------------------------------------------------------



*----------------------------------------------------------------------------
* ORDER ITEMS - START
*----------------------------------------------------------------------------
    LOOP AT is_pos_transaction-receipt_lineitem INTO DATA(ls_lineitem) WHERE retailquantity > 0 .

      APPEND INITIAL LINE TO lt_items_in ASSIGNING FIELD-SYMBOL(<ls_item_in>).
      <ls_item_in>-itm_number = lv_item_number.
      <ls_item_in>-material   = ls_lineitem-itemid.
      <ls_item_in>-plant      = is_pos_transaction-retailstoreid.
      <ls_item_in>-target_qty = ls_lineitem-returnquantity.
      <ls_item_in>-target_qu  = ls_lineitem-salesuom.

      APPEND INITIAL LINE TO lt_items_inx ASSIGNING FIELD-SYMBOL(<ls_item_inx>).
      <ls_item_inx>-itm_number = lv_item_number.
      <ls_item_inx>-updateflag = 'I'.
      <ls_item_inx>-material   = abap_true.
      <ls_item_inx>-plant      = abap_true.
      <ls_item_inx>-target_qty = abap_true.
      <ls_item_inx>-target_qu  = abap_true.

      APPEND INITIAL LINE TO lt_schedules_in ASSIGNING FIELD-SYMBOL(<ls_schedule_in>).
      <ls_schedule_in>-itm_number = lv_item_number.
      <ls_schedule_in>-req_qty = <ls_item_in>-target_qty.

      APPEND INITIAL LINE TO lt_schedules_inx ASSIGNING FIELD-SYMBOL(<ls_schedule_inx>).
      <ls_schedule_inx>-itm_number = lv_item_number.
      <ls_schedule_inx>-req_qty = abap_true.


      APPEND INITIAL LINE TO lt_conditions_in ASSIGNING FIELD-SYMBOL(<ls_condition_in>).
      <ls_condition_in>-itm_number = lv_item_number.
      <ls_condition_in>-cond_type = 'VKP0'.
* This is only a very simplistic solution and not at all good enough for the final solution
      <ls_condition_in>-cond_value = ls_lineitem-returnamount / 10 .         "BAPI Needs values divided by 10.

      lv_item_number += 10.

    ENDLOOP.
*----------------------------------------------------------------------------
* ORDER ITEMS - END
*----------------------------------------------------------------------------

    IF NOT lt_items_in[] IS INITIAL.

      CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA1'
        EXPORTING
          sales_header_in     = ls_header_in
          sales_header_inx    = ls_header_inx
        IMPORTING
          salesdocument_ex    = ev_docnum
        TABLES
          return              = et_message
          sales_items_in      = lt_items_in
          sales_items_inx     = lt_items_inx
          sales_partners      = lt_partners
          sales_schedules_in  = lt_schedules_in
          sales_schedules_inx = lt_schedules_inx
          sales_conditions_in = lt_conditions_in.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.


  ENDMETHOD.


  METHOD display_search_results.

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

    IF mt_pos_result_list[] IS NOT INITIAL.
      util_search_result_to_alv( ).
    ENDIF.

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

    SET HANDLER me->on_alv_double_click FOR mr_events.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> KEY FIELDS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*============================================================================
* RETAILSTOREID
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'RETAILSTOREID' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-001.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
*        mr_column->set_tooltip( 'Double-Click to view Workbench' ).
        mr_column->set_optimized( 'X' ).
*        mr_column->set_output_length( 10 ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.

*============================================================================
* BUSINESSDAYDATE
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'BUSINESSDAYDATE' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-002.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
*        mr_column->set_tooltip( 'Double-Click to view Workbench' ).
        mr_column->set_optimized( 'X' ).
*        mr_column->set_output_length( 5 ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.

*======================================================================*
* TIME
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'TIME' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-007.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
*        mr_column->set_short_text( lv_short_text  ).
*        mr_column->set_tooltip( 'Double-Click to view Workbench' ).
        mr_column->set_optimized( 'X' ).
*        mr_column->set_output_length( 5 ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.

*============================================================================
* SESSION ID
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'DEPARTMENT' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-003.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
*        mr_column->set_short_text( lv_short_text  ).
*        mr_column->set_tooltip( 'Double-Click to view Workbench' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_visible( abap_false ).
*        mr_column->set_output_length( 5 ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.



*============================================================================
* TRANSACTION INDEX
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'TRANSINDEX' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-002.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
*        mr_column->set_short_text( lv_short_text  ).
*        mr_column->set_tooltip( 'Double-Click to view Workbench' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_visible( abap_false ).
*        mr_column->set_output_length( 5 ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.

*============================================================================
* WORKSTATION ID
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'WORKSTATIONID' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-003.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
*        mr_column->set_short_text( lv_short_text  ).
*        mr_column->set_tooltip( 'Double-Click to view Workbench' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_visible( abap_true ).
*        mr_column->set_output_length( 5 ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.


*============================================================================
* TRANSACTION NUMBER
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'TRANSNUMBER' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-004.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
*        mr_column->set_short_text( lv_short_text  ).
*        mr_column->set_tooltip( 'Double-Click to view Workbench' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_visible( abap_true ).
*        mr_column->set_output_length( 5 ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.


*============================================================================
* OPERATOR ID
*============================================================================
    TRY.
        mr_column ?= mr_columns->get_column( 'OPERATORID' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-005.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
*        mr_column->set_short_text( lv_short_text  ).
*        mr_column->set_tooltip( 'Double-Click to view Workbench' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_visible( abap_true ).
*        mr_column->set_output_length( 5 ).
        lv_color-col = 1.
        lv_color-int = 0.
        lv_color-inv = 0.
        mr_column->set_color( lv_color ).
      CATCH cx_salv_not_found.
    ENDTRY.


*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> KEY FIELDS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


*======================================================================*
* TURNOVER - RECEIPT TOTAL
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'TURNOVER' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-006.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_tooltip( 'Receipt Total' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_decimals( '0' ).                     "Not currently working
        mr_column->set_visible( abap_false ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
        mr_column->set_currency( mt_report_data[ 1 ]-transcurrency ).
      CATCH cx_salv_not_found.
    ENDTRY.


**======================================================================*
** BEGIN TIMESTAMP
**======================================================================*
*    TRY.
*        mr_column ?= mr_columns->get_column( 'BEGINTIMESTAMP' ).
*        lv_long_text = lv_medium_text = lv_short_text = TEXT-007.
*        mr_column->set_long_text( lv_long_text ).
*        mr_column->set_medium_text( lv_medium_text  ).
*        mr_column->set_short_text( lv_short_text  ).
*        mr_column->set_optimized( 'X' ).
*        mr_column->set_zero( abap_false ).
*        mr_column->set_visible( abap_true ).
*        mr_column->set_edit_mask('____-__-__ __:__' ).
*      CATCH cx_salv_not_found.
*    ENDTRY.

*======================================================================*
* RETAIL_STATUS
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'RECEIPT_STATUS' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-009.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*======================================================================*
* RETAIL_STATUS
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'RECEIPT_LINECOUNT' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-010.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.


*======================================================================*
* TENDER CASH
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'TENDER_CASH' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-011.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_tooltip( 'Amount paid with cash' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_decimals( '0' ).                     "Not currently working
        mr_column->set_visible( abap_false ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
        mr_column->set_currency( mt_report_data[ 1 ]-transcurrency ).
      CATCH cx_salv_not_found.
    ENDTRY.

*======================================================================*
* TENDER CREDIT CARD
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'TENDER_CCARD' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-012.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_tooltip( 'Amount paid with cedit card' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_decimals( '0' ).                     "Not currently working
        mr_column->set_visible( abap_false ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
        mr_column->set_currency( mt_report_data[ 1 ]-transcurrency ).
      CATCH cx_salv_not_found.
    ENDTRY.

*======================================================================*
* TENDER GIFT CARD
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'TENDER_GCARD' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-013.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_tooltip( 'Amount paid with gift card' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_decimals( '0' ).                     "Not currently working
        mr_column->set_visible( abap_false ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
        mr_column->set_currency( mt_report_data[ 1 ]-transcurrency ).
      CATCH cx_salv_not_found.
    ENDTRY.

*======================================================================*
* TENDER IOU / TIllgodo / Store Credit
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'TENDER_IOU' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-014.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_tooltip( 'Amount paid with tillgodo / store credit' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_decimals( '0' ).                     "Not currently working
        mr_column->set_visible( abap_false ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
        mr_column->set_currency( mt_report_data[ 1 ]-transcurrency ).
      CATCH cx_salv_not_found.
    ENDTRY.

*======================================================================*
* TENDER BANK
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'TENDER_BANK' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-015.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_tooltip( 'Amount paid via bank transfer' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_decimals( '0' ).                     "Not currently working
        mr_column->set_visible( abap_false ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
        mr_column->set_currency( mt_report_data[ 1 ]-transcurrency ).
      CATCH cx_salv_not_found.
    ENDTRY.

*======================================================================*
* TENDER INVOICE
*======================================================================*
    TRY.
        mr_column ?= mr_columns->get_column( 'TENDER_BANK' ).
        lv_long_text = lv_medium_text = lv_short_text = TEXT-016.
        mr_column->set_long_text( lv_long_text ).
        mr_column->set_medium_text( lv_medium_text  ).
        mr_column->set_short_text( lv_short_text  ).
        mr_column->set_tooltip( 'Amount paid via invoice' ).
        mr_column->set_optimized( 'X' ).
        mr_column->set_decimals( '0' ).                     "Not currently working
        mr_column->set_visible( abap_false ).
        mr_column->set_zero( abap_false ).
        mr_column->set_visible( abap_true ).
        mr_column->set_currency( mt_report_data[ 1 ]-transcurrency ).
      CATCH cx_salv_not_found.
    ENDTRY.


*======================================================================*
* Set Column for Color Codes
*======================================================================*
    TRY.
        mr_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.


    mr_table->set_screen_status(
          EXPORTING
            report        = 'ZPOS_RECEIPT_SEARCH'
            pfstatus      = 'SALV_TABLE_STANDARD'
            set_functions = mr_table->c_functions_all ).


    mr_display = mr_table->get_display_settings( ).
    mr_display->set_striped_pattern( cl_salv_display_settings=>true ).


    mr_table->display( ).


  ENDMETHOD.


  METHOD execute_invoice_search.

    set_relevant_invoice_types(  ).

    CLEAR: mt_invoice_header, mt_invoice_details.

    SELECT * FROM zposi_billdoc_partner
           UP TO 1000 ROWS
           INTO TABLE @mt_invoice_details
           WHERE documentnumber     IN @it_r_vbeln
           AND   salesorg           = @iv_vkorg
*           AND   ( distchannel        = '10'
*           OR      distchannel        = '20' )
           AND   soldtoparty        IN @it_r_kunnr
           AND   iscancelled        = @abap_false
           AND   documentdate       IN @it_r_date
           AND   invoicegrossamount IN @it_r_invoicegross
           AND   name               IN @it_r_name
*          and   search1      in ???
*          and   search2      in ???
           AND   vatnumber          IN @it_r_taxreg
           AND   mobile             IN @it_r_mobile
           AND   emailaddress       IN @it_r_email
           AND   gtin               IN @it_r_ean11
           AND   grossamount        IN @it_r_itemgross
           AND   invoicegrossamount GT 0
           AND   grossamount        GT 0
           AND   gtin               NE ''
           AND   billingtype        IN @mt_r_fkarv
           ORDER BY documentnumber, documentitem.

    IF lines( mt_invoice_details ) > 0.
      SELECT * FROM zposi_sddocumentflow
             INTO TABLE @DATA(lt_flow)
             FOR ALL ENTRIES IN @mt_invoice_details
             WHERE precedingdocument = @mt_invoice_details-documentnumber
             AND   salesdocumenttype = 'RE2'
             AND   precedingdocumentcategory = 'M'
             AND   precedingdocumentitem NE '000000'.

      LOOP AT lt_flow INTO DATA(ls_flow).

        READ TABLE mt_invoice_details ASSIGNING FIELD-SYMBOL(<ls_invoice_details>) WITH KEY documentnumber = ls_flow-precedingdocument documentitem = ls_flow-precedingdocumentitem .

        IF sy-subrc = 0.
* Modify the Open Qty - Reduce by the return qty
          <ls_invoice_details>-openquantity -= ls_flow-orderquantity.
        ENDIF.

      ENDLOOP.

      IF iv_zero_qty = abap_false.
        DELETE mt_invoice_details WHERE openquantity LE 0.
      ENDIF.

      LOOP AT mt_invoice_details INTO DATA(ls_details)
              GROUP BY ( documentnumber = ls_details-documentnumber
                         size  = GROUP SIZE ) ASCENDING
        REFERENCE INTO DATA(details_group).

        LOOP AT GROUP details_group ASSIGNING FIELD-SYMBOL(<detail>).
          APPEND INITIAL LINE TO mt_invoice_header ASSIGNING FIELD-SYMBOL(<ls_invoice_header>).
          MOVE-CORRESPONDING <detail> TO <ls_invoice_header>.
          <ls_invoice_header>-invoicerowcount = details_group->size.
        ENDLOOP.

      ENDLOOP.
      SORT mt_invoice_header BY documentnumber ASCENDING. DELETE ADJACENT DUPLICATES FROM mt_invoice_header COMPARING documentnumber.
      SORT mt_invoice_header BY documentdate DESCENDING soldtoparty ASCENDING.
      DELETE mt_invoice_header FROM iv_records + 1 TO lines( mt_invoice_header ).

    ENDIF.

  ENDMETHOD.


  METHOD execute_pos_search.

    CALL FUNCTION 'Z_DTA_RECEIPT_SEARCH' DESTINATION iv_destination
      EXPORTING
        it_r_retailstoreid   = it_r_retailstoreid
        it_r_businessdaydate = it_r_businessdaydate
        it_r_workstationid   = it_r_workstationid
        it_r_receipt         = it_r_receipt
        it_r_operatorid      = it_r_operatorid
        it_r_department      = it_r_department
        it_r_itemid          = it_r_itemid
        it_r_itemcount       = it_r_itemcount
        it_r_transamount     = it_r_transamount
        iv_ccard_start       = iv_ccard_start
        iv_ccard_end         = iv_ccard_end
        iv_other_card        = iv_other_card
        iv_records           = iv_records
        iv_currency          = iv_currency
      IMPORTING
        et_result            = mt_pos_result_list
        et_message           = rt_message.

  ENDMETHOD.


  METHOD on_alv_double_click.

    TRY.
        DATA(ls_report_line) = mt_report_data[ row ].     " This should selected the Index Row passed in the variable.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    CHECK NOT ls_report_line IS INITIAL.

    CASE  column.

      WHEN 'COUNTY' OR 'BRAND' OR 'RETAILSTOREID' OR 'TURNOVER'.

        alv_show_pos_workbench( is_report_data = ls_report_line ).

    ENDCASE.


  ENDMETHOD.                    "on_double_click


  METHOD update_receipt_in_car.

    CALL FUNCTION 'Z_DTA_RECEIPT_RETURN_UPD' DESTINATION iv_destination
      EXPORTING
        is_return_transaction = is_return_transaction
      IMPORTING
        et_message            = et_message.

  ENDMETHOD.


  METHOD util_search_result_to_alv.

    CLEAR mt_report_data.
    LOOP AT mt_pos_result_list INTO DATA(ls_result_line).
      APPEND INITIAL LINE TO mt_report_data ASSIGNING FIELD-SYMBOL(<ls_report_line>).
      MOVE-CORRESPONDING ls_result_line TO <ls_report_line>.
      <ls_report_line>-time = ls_result_line-begintimestamp+8(6).
    ENDLOOP.

  ENDMETHOD.


  METHOD view_document_flow.

    DATA: lt_r_docnum TYPE RANGE OF vbeln.
*    DATA: lt_seltab TYPE STANDARD TABLE OF rsparams.
*
*    CALL FUNCTION 'RS_VARIANT_CONTENTS'
*      EXPORTING
*        report               = 'RIBELF20'
*        variant              = 'DEFAULT'
*      TABLES
*        valutab              = lt_seltab
*      EXCEPTIONS
*        variant_non_existent = 1
*        variant_obsolete     = 2
*        OTHERS               = 3.
*
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*      MESSAGE i011(zpos).
*    ELSE.
*
*      READ TABLE lt_seltab ASSIGNING FIELD-SYMBOL(<ls_seltab>) WITH KEY selname = 'S_VBELN7' BINARY SEARCH.
*
*      IF sy-subrc = 0.
*        <ls_seltab>-low = iv_vbeln.
*        SUBMIT ribelf20 WITH SELECTION-TABLE lt_seltab AND RETURN.
*      ENDIF.
*
*    ENDIF.

    APPEND INITIAL LINE TO lt_r_docnum ASSIGNING FIELD-SYMBOL(<ls_docnum>).
    <ls_docnum>-sign   = zcl_pos_util=>co_selopt_sign_i.
    <ls_docnum>-option = zcl_pos_util=>co_selopt_opt_eq.
    <ls_docnum>-low    = iv_vbeln.
    SUBMIT ribelf20 WITH s_vbeln7 IN lt_r_docnum AND RETURN.


  ENDMETHOD.


  METHOD view_invoice.

    SET PARAMETER ID 'VF' FIELD iv_vbeln.
    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

  ENDMETHOD.


  METHOD view_receipt_in_car.

    CALL FUNCTION 'Z_DTA_RECEIPT_DISPLAY' DESTINATION iv_destination
      EXPORTING
        iv_retailstoreid   = iv_retailstoreid
        iv_businessdaydate = iv_businessdaydate
        iv_transindex      = iv_transindex.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Include MZPOS_RECEIPT_SEARCH_TOP                 - Module Pool      SAPMZPOS_RECEIPT_SEARCH
*&---------------------------------------------------------------------*
PROGRAM sapmzpos_receipt_search.

INCLUDE mzpos_search_return_typ.


*"----------------------------------------------------------------------
* POS Related Data
*"----------------------------------------------------------------------

DATA: gt_pos_receipt          TYPE STANDARD TABLE OF tp_pos_receipt,
      wa_pos_receipt          TYPE tp_pos_receipt,
      wa_pos_selected_receipt TYPE tp_pos_receipt,
      gt_pos_line             TYPE STANDARD TABLE OF tp_pos_line,
      gs_pos_line             TYPE tp_pos_line,
      wa_pos_line             TYPE tp_pos_line,
      gt_pos_scan_line        TYPE STANDARD TABLE OF tp_scan_line,
      wa_pos_scan_line        TYPE tp_scan_line,
      gt_pos_tender           TYPE STANDARD TABLE OF tp_pos_tender,
      wa_pos_tender           TYPE tp_pos_tender,
      gt_r_retailstoreid      TYPE zpos_rt_retailstoreid,
      gt_r_businessdaydate    TYPE zpos_rt_businessdaydate,
      gt_r_workstationid      TYPE zpos_rt_workstationid,
      gt_r_transnumber        TYPE zpos_rt_transnumber,
      gt_r_operatorid         TYPE zpos_rt_operatorid,
      gt_r_department         TYPE zpos_rt_department,
      gt_r_itemid             TYPE zpos_rt_itemid,
      gt_r_itemcount          TYPE zpos_rt_count,
      gt_r_transamount        TYPE zpos_rt_amount,
      gv_pos                  TYPE boole_d VALUE abap_false,
      gv_bdd                  TYPE zpos_businessdaydate,
      gv_store                TYPE zpos_retailstoreid,
      gv_workstationid        TYPE zpos_workstationid,
      gv_operatorid           TYPE zpos_operatorid,
      gv_receipt              TYPE zpos_transactionsequencenumb,
      gv_department           TYPE zpos_department,
      gv_turnover             TYPE zpos_salesamount,
      gv_itemid               TYPE zpos_itemid,
      gv_itemamount           TYPE zpos_salesamount,
      gv_itemcount            TYPE zpos_itemcount,
      gv_ccard_start          TYPE char6,
      gv_ccard_end            TYPE char4,
      gv_other_card           TYPE zpos_referenceid,
*&SPWIZARD: LINES OF TABLECONTROL 'TC_scan_line'
      g_tc_scan_line_lines    LIKE sy-loopc.

CONTROLS: tc_pos_receipt   TYPE TABLEVIEW USING SCREEN 9001,
          tc_pos_line      TYPE TABLEVIEW USING SCREEN 9002,
          tc_pos_tender    TYPE TABLEVIEW USING SCREEN 9002,
          tc_pos_scan_line TYPE TABLEVIEW USING SCREEN 9003.


*"----------------------------------------------------------------------
* Shared Data
*"----------------------------------------------------------------------
DATA:
  gr_search      TYPE REF TO zcl_pos_search_return,
  ok_code        TYPE sy-ucomm,
  gv_check_code  TYPE xfeld,
  gv_area        TYPE char50,
  gv_records     TYPE int4,
  gt_makt        TYPE STANDARD TABLE OF makt,
  gv_currency    TYPE waers,
  gt_message     TYPE bapiret2_t,
  gs_message     TYPE bapiret2,
  gv_field       TYPE char50,
  gv_pick_line   TYPE syst_tabix,
  gv_destination TYPE rfcdest.

CONSTANTS: co_car_pos_search TYPE rvari_vnam VALUE 'CAR_POS_SEARCH'.

*"----------------------------------------------------------------------
* Invoice Related Data
*"----------------------------------------------------------------------
DATA:
  gv_kunnr  TYPE kunnr,
  gv_date   TYPE dats,
  gv_vkorg  TYPE vkorg,
  gv_vbeln  TYPE vbeln,
  gv_name   TYPE /sapce/fkua_txbpnamf,
  gv_orgnbr TYPE stceg,
  gv_gtin   TYPE ean11,
  gv_phone  TYPE ad_tlnmbr1,
  gv_email  TYPE ad_smtpadr,
  gv_multi  TYPE boole_d VALUE abap_false.

DATA: gt_invoice_details         TYPE tp_t_invoice_details,
      wa_invoice_details         TYPE tp_s_invoice_details,
      wa_selected_invoice        TYPE tp_s_invoice_details,
      gt_refresh_invoice_details TYPE tp_t_invoice_details,
      gt_return_basket           TYPE tp_t_invoice_details,
      wa_return_basket           TYPE tp_s_invoice_details.

DATA: gt_invoice_header TYPE tp_t_invoice_header,
      wa_invoice_header TYPE tp_s_invoice_header.

DATA: gt_soldtoparty TYPE tp_t_soldtoparty,
      wa_soldtoparty TYPE tp_s_soldtoparty.


DATA: gs_head_comv TYPE  msr_rfc_ro_head_comv,
      gt_item_comv TYPE  tdt_msr_rfc_ro_item_comv.


CONTROLS: tc_invoice_head TYPE TABLEVIEW USING SCREEN 9101.
CONTROLS: tc_invoice_line TYPE TABLEVIEW USING SCREEN 9102.
CONTROLS: tc_return_bask TYPE TABLEVIEW USING SCREEN 9103.
DATA:     g_tc_return_bask_lines  LIKE sy-loopc.

DATA:     g_tc_invoice_head_lines  LIKE sy-loopc.


*"----------------------------------------------------------------------
* Tab Strip Controls
*"----------------------------------------------------------------------

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TABSTRIP_CNTRL'
CONSTANTS: BEGIN OF c_tabstrip_cntrl,
             tab1 LIKE sy-ucomm VALUE 'TABSTRIP_CNTRL_FC1',
             tab2 LIKE sy-ucomm VALUE 'TABSTRIP_CNTRL_FC2',
           END OF c_tabstrip_cntrl.
*&SPWIZARD: DATA FOR TABSTRIP 'TABSTRIP_CNTRL'
CONTROLS:  tabstrip_cntrl TYPE TABSTRIP.
DATA: BEGIN OF g_tabstrip_cntrl,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'SAPMZPOS_SEARCH_RETURN',
        pressed_tab LIKE sy-ucomm VALUE c_tabstrip_cntrl-tab2,
      END OF g_tabstrip_cntrl.


*"----------------------------------------------------------------------
* Selection Screen
*"----------------------------------------------------------------------
INCLUDE mzpos_search_return_sel.

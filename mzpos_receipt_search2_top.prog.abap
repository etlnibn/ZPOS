*&---------------------------------------------------------------------*
*& Include MZPOS_RECEIPT_SEARCH_TOP                 - Module Pool      SAPMZPOS_RECEIPT_SEARCH
*&---------------------------------------------------------------------*
PROGRAM sapmzpos_receipt_search.

*TABLES: zpos_search_trans_flat_sty.

CONSTANTS: BEGIN OF co_selection_options,
             include TYPE ddsign VALUE 'I',
             equal   TYPE ddoption VALUE 'EQ',
           END OF co_selection_options.

TYPES: BEGIN OF tp_receipt_barcode,
         store   TYPE char5,
         date    TYPE char6,
         till    TYPE char3,
         receipt TYPE char4,
       END OF tp_receipt_barcode.

TYPES: BEGIN OF tp_receipt,
         mark              TYPE ce_mark,
         retailstoreid     TYPE zpos_retailstoreid,
         businessdaydate   TYPE zpos_businessdaydate,
         transtime         TYPE tims,
         transindex        TYPE zpos_transindex,
         transnumber       TYPE zpos_transactionsequencenumb,
         workstationid     TYPE zpos_workstationid,
         operatorid        TYPE zpos_operatorid,
         department        TYPE zpos_department,
         turnover          TYPE zpos_transturnover,
         transcurrency     TYPE zpos_transactioncurrency,
         begintimestamp    TYPE zpos_begindatetimestamp,
         endtimestamp      TYPE zpos_enddatetimestamp,
         receipt_linecount TYPE zpos_itemcount,
       END OF tp_receipt.

TYPES: BEGIN OF tp_line,
         mark            TYPE ce_mark,
         retailnumber    TYPE  zpos_retailsequencenumber,
         retailtypecode  TYPE  zpos_retailtypecode,
         itemid          TYPE  zpos_itemid,
         description     TYPE  maktx,
         retailquantity  TYPE  zpos_retailquantity,
         returnquantity  TYPE  zpos_retailquantity,
         salesuom        TYPE  zpos_salesunitofmeasure,
         salesamount     TYPE  zpos_salesamount,
         normalsalesamt  TYPE  zpos_normalsalesamount,
         actualunitprice TYPE  zpos_actualunitprice,
         lineitem_void   TYPE  boole_d,
       END OF tp_line.

TYPES: BEGIN OF tp_tender,
         tendernumber    TYPE  zpos_tendersequencenumber,
         tendertypegroup TYPE  zpos_tendertypegroup,
         tendertypecode  TYPE  zpos_tendertypecode,
         tenderamount    TYPE  zpos_tenderamount,
         tendercurrency  TYPE  zpos_tendercurrency,
         tenderid        TYPE  zpos_tenderid,
         accountnumber   TYPE  zpos_accountnumber,
         referenceid     TYPE  zpos_referenceid,
         category        TYPE  char20,
       END OF tp_tender.


DATA: gt_receipt           TYPE STANDARD TABLE OF tp_receipt,
      wa_receipt           TYPE tp_receipt,
      wa_selected_receipt  TYPE tp_receipt,
      gt_line              TYPE STANDARD TABLE OF tp_line,
      wa_line              TYPE tp_line,
      gt_tender            TYPE STANDARD TABLE OF tp_tender,
      wa_tender            TYPE tp_tender,
      gr_search            TYPE REF TO zcl_pos_search_return,
      ok_code              TYPE sy-ucomm,
      gv_check_code        TYPE xfeld,
      gt_r_retailstoreid   TYPE zpos_rt_retailstoreid,
      gt_r_businessdaydate TYPE zpos_rt_businessdaydate,
      gt_r_workstationid   TYPE zpos_rt_workstationid,
      gt_r_transnumber     TYPE zpos_rt_transnumber,
      gt_r_operatorid      TYPE zpos_rt_operatorid,
      gt_r_department      TYPE zpos_rt_department,
      gt_r_itemid          TYPE zpos_rt_itemid,
      gt_r_itemcount       TYPE zpos_rt_count,
      gt_r_transamount     TYPE zpos_rt_amount,
      gv_records           TYPE int4,
      gv_currency          TYPE waers,
      gt_message           TYPE zpos_bapi_message_tty,
      gv_field             TYPE char50,
      gv_pick_line         TYPE syst_tabix,
      gv_bdd               TYPE zpos_businessdaydate,
      gv_store             TYPE zpos_retailstoreid,
      gv_workstationid     TYPE zpos_workstationid,
      gv_operatorid        TYPE zpos_operatorid,
      gv_receipt           TYPE zpos_transactionsequencenumb,
      gv_department        TYPE zpos_department,
      gv_turnover          TYPE zpos_salesamount,
      gv_itemid            TYPE zpos_itemid,
      gv_gtin              TYPE ean11,
      gv_itemamount        TYPE zpos_salesamount,
      gv_itemcount         TYPE zpos_itemcount,
      gv_ccard_start       TYPE  char6,
      gv_ccard_end         TYPE  char4,
      gv_other_card        TYPE  zpos_referenceid,
      gt_makt              TYPE STANDARD TABLE OF makt.

CONTROLS: tc_receipt TYPE TABLEVIEW USING SCREEN 9001,
          tc_line    TYPE TABLEVIEW USING SCREEN 9002,
          tc_tender  TYPE TABLEVIEW USING SCREEN 9002.

SELECTION-SCREEN BEGIN OF SCREEN 9010 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-b00.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c00 FOR FIELD p_bcode MODIF ID gp3.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_bcode TYPE tp_receipt_barcode MODIF ID gp3.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN END OF BLOCK b0.


  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.

    SELECT-OPTIONS: s_store       FOR  gv_store MODIF ID gp1.
    SELECT-OPTIONS: s_date        FOR  gv_bdd MODIF ID gp1.
    SELECT-OPTIONS: s_rcpt        FOR  gv_receipt MODIF ID gp1.
    SELECT-OPTIONS: s_wrkstn      FOR  gv_workstationid MODIF ID gp1.
    SELECT-OPTIONS: s_dept        FOR  gv_department MODIF ID gp1.
    SELECT-OPTIONS: s_opera       FOR  gv_operatorid MODIF ID gp1.
    SELECT-OPTIONS: s_total       FOR  gv_turnover MODIF ID gp1.

  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c01 FOR FIELD p_cend MODIF ID gp3.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_cend TYPE char4 MODIF ID gp3.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c02 FOR FIELD p_cstart MODIF ID gp3.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_cstart TYPE char6 MODIF ID gp3.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(27) TEXT-c03 FOR FIELD p_cnumbr MODIF ID gp3.
      SELECTION-SCREEN    POSITION 33.
      PARAMETERS: p_cnumbr TYPE zpos_referenceid MODIF ID gp3.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN END OF BLOCK b3.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.

    SELECT-OPTIONS: s_item    FOR  gv_itemid  MODIF ID gp2.
    SELECT-OPTIONS: s_itamt   FOR  gv_itemamount  MODIF ID gp2.
    SELECT-OPTIONS: s_count   FOR  gv_itemcount  MODIF ID gp2.

  SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN END OF SCREEN 9010.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.


*----------------------------------------------------------------------*
* SELECTION-SCREEN.                                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT.                                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.



*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.



*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

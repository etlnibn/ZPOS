*&---------------------------------------------------------------------*
*& Include MZPOS_RECEIPT_SEARCH_TYP                 - Module Pool      SAPMZPOS_RECEIPT_SEARCH
*&---------------------------------------------------------------------*

*"----------------------------------------------------------------------
* POS Related Data Types
*"----------------------------------------------------------------------

CONSTANTS co_multiple TYPE string VALUE 'Multiple' ##NO_TEXT.

CONSTANTS: BEGIN OF co_selection_options,
             include TYPE ddsign VALUE 'I',
             equal   TYPE ddoption VALUE 'EQ',
           END OF co_selection_options.

CONSTANTS: BEGIN OF co_scanner_status,
             new  TYPE char1 VALUE 'N',
             used TYPE char1 VALUE 'U',
           END OF co_scanner_status.

TYPES: BEGIN OF tp_receipt_barcode,
         store   TYPE char5,
         date    TYPE char6,
         till    TYPE char3,
         receipt TYPE char4,
       END OF tp_receipt_barcode.

TYPES: BEGIN OF tp_pos_receipt,
         mark TYPE ce_mark.
         INCLUDE TYPE zpos_search_trans_flat_sty .
TYPES END OF tp_pos_receipt.

TYPES: BEGIN OF tp_pos_line,
         mark            TYPE ce_mark,
         retailnumber    TYPE  zpos_retailsequencenumber,
         retailtypecode  TYPE  zpos_retailtypecode,
         itemid          TYPE  zpos_itemid,
         materialnumber  type  matnr,
         description     TYPE  maktx,
         retailquantity  TYPE  zpos_retailquantity,
         returnquantity  TYPE  zpos_retailquantity,
         openquantity    TYPE  zpos_retailquantity,
         salesuom        TYPE  zpos_salesunitofmeasure,
         salesamount     TYPE  zpos_salesamount,
         returnamount    TYPE  zpos_salesamount,
         returndocnum    TYPE  vbeln,
         openamount      TYPE  zpos_salesamount,
         normalsalesamt  TYPE  zpos_normalsalesamount,
         actualunitprice TYPE  zpos_actualunitprice,
         lineitem_void   TYPE  boole_d,
       END OF tp_pos_line.

TYPES: BEGIN OF tp_scan_line,
         mark   TYPE ce_mark,
         ean11  TYPE ean11,
         status TYPE char1,
       END OF tp_scan_line.


TYPES: BEGIN OF tp_pos_tender,
         tendernumber    TYPE  zpos_tendersequencenumber,
         tendertypegroup TYPE  zpos_tendertypegroup,
         tendertypecode  TYPE  zpos_tendertypecode,
         tenderamount    TYPE  zpos_tenderamount,
         tendercurrency  TYPE  zpos_tendercurrency,
         tenderid        TYPE  zpos_tenderid,
         accountnumber   TYPE  zpos_accountnumber,
         referenceid     TYPE  zpos_referenceid,
         category        TYPE  char20,
       END OF tp_pos_tender.


*"----------------------------------------------------------------------
* Invoice Related Data Types
*"----------------------------------------------------------------------

TYPES:
  BEGIN OF tp_s_invoice_header,
    mark TYPE ce_mark.
    INCLUDE TYPE zcl_pos_search_return=>tp_s_invoice_header.
TYPES: END OF tp_s_invoice_header.

TYPES: tp_t_invoice_header TYPE STANDARD TABLE OF tp_s_invoice_header.

TYPES: BEGIN OF tp_s_invoice_details,
         mark           TYPE ce_mark.
         INCLUDE TYPE zposi_billdoc_partner.
TYPES:
         returnquantity TYPE  zposi_billdoc_partner-quantity,
*         openquantity   TYPE  zposi_billdoc_partner-quantity,
*         returnamount   TYPE  zposi_billdoc_partner-grossamount,
*         openamount     TYPE  zposi_billdoc_partner-grossamount,
         returndocnum   TYPE  vbeln.
TYPES END OF tp_s_invoice_details.

TYPES: tp_t_invoice_details TYPE STANDARD TABLE OF tp_s_invoice_details.


TYPES: BEGIN OF tp_s_soldtoparty,
         soldtoparty TYPE zposi_billdoc_partner-soldtoparty,
       END OF tp_s_soldtoparty.

TYPES: tp_t_soldtoparty TYPE STANDARD TABLE OF tp_s_soldtoparty WITH NON-UNIQUE KEY soldtoparty.

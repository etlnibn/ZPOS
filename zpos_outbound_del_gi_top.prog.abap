*&---------------------------------------------------------------------*
*& Include          ZPOS_PERSIST_PRICELIST_TOP
*&---------------------------------------------------------------------*

 TABLES: sscrfields, wes_s_appl_serv.

 DATA: g_repid         TYPE syrepid,
       gv_ucomm        TYPE sy-ucomm,
       gs_help_info    TYPE help_info,
       gt_dynpselect   TYPE STANDARD TABLE OF dselc,
       gt_dynpvaluetab TYPE STANDARD TABLE OF dval,
       gt_r_docnum     TYPE RANGE OF edi_docnum.

 TYPES: ty_partner  TYPE edi_rcvprn,
        ty_delivery TYPE vbeln.

 CLASS lcl_report DEFINITION.
   PUBLIC SECTION.
     CLASS-METHODS :
* Handler for initialization event in report
       initialization_handler,
* Handler for AT selection screen event in report
       selection_screen_handler,
* Handler for At screen output event in report
       screen_output_handler,
       check_mandatory_data. "Check for mandatory selection screen input

     METHODS:
       report_initialization,"Selection comparison options
       constructor.          "Constructor

 ENDCLASS.

 DATA: gr_report       TYPE REF TO zcl_pos_ship_api.

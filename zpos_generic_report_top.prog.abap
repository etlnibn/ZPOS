*&---------------------------------------------------------------------*
*& Include          ZPOS_GENERIC_REPORT_TOP
*&---------------------------------------------------------------------*

 TABLES sscrfields.
 DATA: g_repid      TYPE syrepid,
       gv_ucomm     TYPE sy-ucomm,
       gs_help_info TYPE help_info.

 CLASS lcl_report DEFINITION.
   PUBLIC SECTION.
     CLASS-METHODS :
* Handler for initialization event in report
       initialization_handler,
* Handler for AT selection screen event in report
       selection_screen_handler,
* Handler for At screen output event in report
       screen_output_handler.

     METHODS:
       check_mandatory_data, "Check for mandatory selection screen input
       report_initialization,"Selection comparison options
       constructor.          "Constructor

 ENDCLASS.

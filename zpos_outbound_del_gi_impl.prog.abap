*&---------------------------------------------------------------------*
*& Include          ZPOS_PERSIST_PRICELIST_IMPL
*&---------------------------------------------------------------------*



*===================================================================
* lcl_report (Implementation)
*===================================================================

CLASS lcl_report IMPLEMENTATION.
* Creat ICons for various push buttons
  METHOD constructor.
  ENDMETHOD.
  METHOD report_initialization.
  ENDMETHOD.
  METHOD initialization_handler.

** More Infomation ICON 1
*    CALL FUNCTION 'ICON_CREATE'
*      EXPORTING
*        name   = icon_information
*        text   = 'Info'
*        info   = 'More information'
*      IMPORTING
*        result = p_help1
*      EXCEPTIONS
*        OTHERS = 0.



  ENDMETHOD.
  METHOD selection_screen_handler.
*    gv_ucomm = sscrfields-ucomm.
*    CLEAR gs_help_info.
*    gs_help_info-call      = 'D'.
*    gs_help_info-object    = 'N'.
*    gs_help_info-dynpro    = '1000'.
*    gs_help_info-spras     = 'E'.
*    gs_help_info-messageid = 'ZPOS'.
*    gs_help_info-pfkey     = 'INFO'.
*    gs_help_info-docuid    = 'NA'.
*
*    CASE sscrfields.
*
*      WHEN 'HELP1'.
*
*        gs_help_info-messagenr = '101'.
*
*        CALL FUNCTION 'HELP_START'
*          EXPORTING
*            help_infos   = gs_help_info
*          TABLES
*            dynpselect   = gt_dynpselect
*            dynpvaluetab = gt_dynpvaluetab.
*
*    ENDCASE.

  ENDMETHOD.
  METHOD screen_output_handler.
    LOOP AT SCREEN INTO DATA(ls_screen).
**==============================================
** Hide Application Option when using Full Load
**==============================================
*      IF p_full = abap_true AND ls_screen-group1 = 'APL' .
*        ls_screen-active = '0'.
*      ENDIF.
*
*      IF p_delta = abap_true AND ls_screen-group1 = 'DAT' .
*        ls_screen-active = '0'.
*      ENDIF.
*
*
*      MODIFY SCREEN FROM ls_screen.

    ENDLOOP.
  ENDMETHOD.

  METHOD  check_mandatory_data.

*    IF p_delv = abap_true AND s_appl[] IS INITIAL.
*      MESSAGE i002(zpos).
*      LEAVE LIST-PROCESSING.
*    ELSEIF s_recp[] IS NOT INITIAL AND p_wind = abap_true.
**Full Mode - specific recipients - no WIND updates.
**Delta Mode - specific recipients - no WIND updates.
*      MESSAGE i003(zpos).
*      CLEAR p_wind.
*      LEAVE LIST-PROCESSING.
*    ELSEIF p_full = abap_true.
*      CLEAR s_appl[].
*    ENDIF.

  ENDMETHOD.

ENDCLASS.

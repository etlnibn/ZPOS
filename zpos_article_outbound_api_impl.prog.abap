*&---------------------------------------------------------------------*
*& Include          ZPOS_ARTICLE_OUTBOUND_IMPL
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

* More Infomation ICON 1
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = icon_information
        text   = 'Info'
        info   = 'More information'
      IMPORTING
        result = p_help1
      EXCEPTIONS
        OTHERS = 0.


    DATA: restrict TYPE sscr_restrict,
          opt_list TYPE sscr_opt_list,
          ass      TYPE sscr_ass.
* Défine select-options modes (aka option list)
* - ALL standard - all options allowed
    CLEAR opt_list.
    MOVE 'ALL' TO opt_list-name.
    MOVE 'X' TO: opt_list-options-bt,
                 opt_list-options-cp,
                 opt_list-options-eq,
                 opt_list-options-ge,
                 opt_list-options-gt,
                 opt_list-options-le,
                 opt_list-options-lt,
                 opt_list-options-nb,
                 opt_list-options-ne,
                 opt_list-options-np.
    APPEND opt_list TO restrict-opt_list_tab.
* - EQU only equality allowed (list of values)
    CLEAR opt_list.
    MOVE 'EQU' TO opt_list-name.
    MOVE 'X' TO opt_list-options-eq.
    APPEND opt_list TO restrict-opt_list_tab.
* Affect modes to parameters or block of parameters
* ALL by default
    CLEAR ass.
    MOVE: 'A'          TO ass-kind,
          '*'          TO ass-sg_main,
          'ALL'        TO ass-op_main.
    APPEND ass TO restrict-ass_tab.
* EQU to internal material number
    CLEAR ass.
    MOVE: 'S'          TO ass-kind,
          'S_RECP'     TO ass-name,
          'I'          TO ass-sg_main, " no exclusion
          'EQU'        TO ass-op_main. " only value list
    APPEND ass TO restrict-ass_tab.
* Call  FM
    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        restriction = restrict
      EXCEPTIONS
        OTHERS      = 1.

  ENDMETHOD.
  METHOD selection_screen_handler.
    gv_ucomm = sscrfields-ucomm.
    CLEAR gs_help_info.
    gs_help_info-call      = 'D'.
    gs_help_info-object    = 'N'.
    gs_help_info-dynpro    = '1000'.
    gs_help_info-spras     = 'E'.
    gs_help_info-messageid = 'ZPOS'.
    gs_help_info-pfkey     = 'INFO'.
    gs_help_info-docuid    = 'NA'.

    CASE sscrfields.

      WHEN 'HELP1'.

        gs_help_info-messagenr = '102'.

        CALL FUNCTION 'HELP_START'
          EXPORTING
            help_infos   = gs_help_info
          TABLES
            dynpselect   = gt_dynpselect
            dynpvaluetab = gt_dynpvaluetab.

    ENDCASE.

  ENDMETHOD.
  METHOD screen_output_handler.


*    LOOP AT SCREEN INTO DATA(ls_screen).
*
***==============================================
*** Hide Application Option when using Full Load
***==============================================
**      IF p_full = abap_true AND ls_screen-group1 = 'APL' .
**        ls_screen-active = '0'.
**      ENDIF.
*
*      MODIFY SCREEN FROM ls_screen.
*
*    ENDLOOP.


  ENDMETHOD.

  METHOD  check_mandatory_data.

    IF p_vkorg IS INITIAL.
      MESSAGE i050(zpos) WITH 'Sales Org'.
      LEAVE LIST-PROCESSING.
    ELSE.
        IF p_vkorg = '*'.
          MESSAGE i051(zpos) WITH 'Sales Org'.
          LEAVE LIST-PROCESSING.
        ENDIF.
    ENDIF.

    IF p_sts IS INITIAL.
      MESSAGE i050(zpos) WITH 'Status'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

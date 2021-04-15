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


*    define the object to be passed to the restriction parameter
    DATA ls_restrict TYPE sscr_restrict.

* Auxiliary objects for filling RESTRICT
    DATA ls_opt_list TYPE sscr_opt_list.
    DATA ls_ass      TYPE sscr_ass.

* NOINTERVLS: BT and NB not allowed
    CLEAR ls_opt_list.
    MOVE 'NOINTERVLS' TO ls_opt_list-name.
    MOVE 'X' TO: ls_opt_list-options-cp,
    ls_opt_list-options-eq,
    ls_opt_list-options-ge,
    ls_opt_list-options-gt,
    ls_opt_list-options-le,
    ls_opt_list-options-lt,
    ls_opt_list-options-ne,
    ls_opt_list-options-np.
    APPEND ls_opt_list TO ls_restrict-opt_list_tab.

* EQ_AND_CP: only EQ and CP allowed
    CLEAR ls_opt_list.
    MOVE 'EQ_AND_CP'  TO ls_opt_list-name.
    MOVE 'X'          TO: ls_opt_list-options-eq,
    ls_opt_list-options-cp.
    APPEND ls_opt_list TO ls_restrict-opt_list_tab.

    CLEAR ls_ass.
    MOVE: 'S'    TO ls_ass-kind,
    'S_APPL'     TO ls_ass-name,
    'I'          TO ls_ass-sg_main,
    '*'          TO ls_ass-sg_addy,
    'EQ_AND_CP'  TO ls_ass-op_main,
    'NOINTERVLS' TO ls_ass-op_addy.
    APPEND ls_ass TO ls_restrict-ass_tab.

    CLEAR ls_ass.
    MOVE: 'S'          TO ls_ass-kind,
    'S_RECP'     TO ls_ass-name,
    'I'          TO ls_ass-sg_main,
    '*'          TO ls_ass-sg_addy,
    'EQ_AND_CP'  TO ls_ass-op_main,
    'NOINTERVLS' TO ls_ass-op_addy.
    APPEND ls_ass TO ls_restrict-ass_tab.


* Call function module
    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        restriction                = ls_restrict
*       DB                         = ' '
      EXCEPTIONS
        too_late                   = 1
        repeated                   = 2
        not_during_submit          = 3
        db_call_after_report_call  = 4
        selopt_without_options     = 5
        selopt_without_signs       = 6
        invalid_sign               = 7
        report_call_after_db_error = 8
        empty_option_list          = 9
        invalid_kind               = 10
        repeated_kind_a            = 11
        OTHERS                     = 12.

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

        gs_help_info-messagenr = '101'.

        CALL FUNCTION 'HELP_START'
          EXPORTING
            help_infos   = gs_help_info
          TABLES
            dynpselect   = gt_dynpselect
            dynpvaluetab = gt_dynpvaluetab.

    ENDCASE.

  ENDMETHOD.
  METHOD screen_output_handler.
    LOOP AT SCREEN INTO DATA(ls_screen).
*==============================================
* Hide Application Option when using Full Load
*==============================================
      IF p_full = abap_true AND ls_screen-group1 = 'APL' .
        ls_screen-active = '0'.
      ENDIF.

      IF p_delta = abap_true AND ls_screen-group1 = 'DAT' .
        ls_screen-active = '0'.
      ENDIF.


      MODIFY SCREEN FROM ls_screen.

    ENDLOOP.
  ENDMETHOD.

  METHOD  check_mandatory_data.



    IF p_delta = abap_true AND s_appl[] IS INITIAL.
      MESSAGE i002(zpos).
      LEAVE LIST-PROCESSING.
    ELSEIF s_recp[] IS NOT INITIAL AND p_wind = abap_true.
*Full Mode - specific recipients - no WIND updates.
*Delta Mode - specific recipients - no WIND updates.
      MESSAGE i003(zpos).
      CLEAR p_wind.
      LEAVE LIST-PROCESSING.
    ELSEIF p_full = abap_true.
      CLEAR s_appl[].
    ENDIF.

  ENDMETHOD.

ENDCLASS.

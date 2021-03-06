*&---------------------------------------------------------------------*
*& Include          ZPOS_PERSIST_PRICELIST_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK screen.

  SELECTION-SCREEN: BEGIN OF BLOCK main WITH FRAME TITLE TEXT-tf0.

    SELECTION-SCREEN     PUSHBUTTON 1(30) p_help1 USER-COMMAND help1 VISIBLE LENGTH 10.
    SELECTION-SCREEN SKIP 1.

    SELECTION-SCREEN:    BEGIN OF LINE.
      SELECTION-SCREEN    COMMENT 1(12) TEXT-t01 FOR FIELD p_full .
      SELECTION-SCREEN    POSITION 18.
      PARAMETERS: p_full RADIOBUTTON GROUP grp1 USER-COMMAND btn1.
      SELECTION-SCREEN    POSITION 30.
      SELECTION-SCREEN    COMMENT 30(12) TEXT-t02 FOR FIELD p_delta .
      SELECTION-SCREEN    POSITION 48.
      PARAMETERS: p_delta RADIOBUTTON GROUP grp1 DEFAULT 'X'.
      SELECTION-SCREEN    POSITION 55.
      SELECTION-SCREEN    COMMENT 55(15) TEXT-t03 FOR FIELD p_wind.
      PARAMETERS: p_wind AS CHECKBOX TYPE xfeld DEFAULT 'X'.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    SKIP 1.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t04 FOR FIELD s_appl MODIF ID apl.
    SELECT-OPTIONS: s_appl FOR wes_s_appl_serv-appl MEMORY ID wesappl MODIF ID apl.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t07 FOR FIELD s_recp.
    SELECT-OPTIONS: s_recp FOR gv_recipient .
    SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN: END OF BLOCK main.


  SELECTION-SCREEN: BEGIN OF BLOCK additional WITH FRAME TITLE TEXT-tf1.
    SELECTION-SCREEN:  BEGIN OF LINE,
    COMMENT 1(15) TEXT-t05 FOR FIELD p_datab MODIF ID dat.
    SELECTION-SCREEN    POSITION 38.
    PARAMETERS: p_datab TYPE dats DEFAULT sy-datum  MODIF ID dat.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:  BEGIN OF LINE,
    COMMENT 1(15) TEXT-t06 FOR FIELD p_datbi  MODIF ID dat.
    SELECTION-SCREEN    POSITION 38.
    PARAMETERS: p_datbi TYPE dats DEFAULT '99991231'  MODIF ID dat.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN: END OF BLOCK additional.

SELECTION-SCREEN: END OF BLOCK screen.

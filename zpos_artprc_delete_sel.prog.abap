*&---------------------------------------------------------------------*
*& Include          ZPOS_ARTPRC_DELETE_SEL
*&---------------------------------------------------------------------*



SELECTION-SCREEN: BEGIN OF BLOCK screen.

  SELECTION-SCREEN: BEGIN OF BLOCK main WITH FRAME TITLE TEXT-tf0.

    SELECTION-SCREEN     PUSHBUTTON 1(30) p_help1 USER-COMMAND help1 VISIBLE LENGTH 10.
    SELECTION-SCREEN SKIP 1.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t01 FOR FIELD s_vkorg.
    SELECT-OPTIONS: s_vkorg FOR zpos_artprc_sts-vkorg .
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t02 FOR FIELD s_vtweg.
    SELECT-OPTIONS: s_vtweg FOR zpos_artprc_sts-vtweg .
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t03 FOR FIELD s_werks.
    SELECT-OPTIONS: s_werks FOR zpos_artprc_sts-werks .
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t04 FOR FIELD p_dat.
    SELECTION-SCREEN    POSITION 38.
    PARAMETERS: p_dat TYPE dats OBLIGATORY.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t06 FOR FIELD p_sts.
    SELECTION-SCREEN    POSITION 38.
    PARAMETERS: p_sts TYPE zpos_recipient_status OBLIGATORY DEFAULT '20'.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t05 FOR FIELD s_recp.
    SELECT-OPTIONS: s_recp FOR gv_recipient.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN: END OF BLOCK main.

SELECTION-SCREEN: END OF BLOCK screen.
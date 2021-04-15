*&---------------------------------------------------------------------*
*& Include          ZPOS_ARTPRC_DELETE_SEL
*&---------------------------------------------------------------------*



SELECTION-SCREEN: BEGIN OF BLOCK screen.

  SELECTION-SCREEN: BEGIN OF BLOCK main WITH FRAME TITLE TEXT-tf0.

    SELECTION-SCREEN     PUSHBUTTON 1(30) p_help1 USER-COMMAND help1 VISIBLE LENGTH 10.
    SELECTION-SCREEN SKIP 1.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t01 FOR FIELD p_vkorg.
    PARAMETERS: p_vkorg TYPE vkorg .
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t02 FOR FIELD p_vtweg.
    PARAMETERS: p_vtweg TYPE vtweg .
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t03 FOR FIELD p_werks.
    PARAMETERS: p_werks TYPE werks_d.
    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:    BEGIN OF LINE,
    COMMENT 1(33) TEXT-t04 FOR FIELD p_recpt.
    PARAMETERS: p_recpt TYPE zpos_recipient.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN: END OF BLOCK main.

SELECTION-SCREEN: END OF BLOCK screen.

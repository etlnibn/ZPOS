*&---------------------------------------------------------------------*
*& Include          ZPOS_PERSIST_PRICELIST_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK screen.

  SELECTION-SCREEN: BEGIN OF BLOCK main WITH FRAME TITLE TEXT-tf0.

    SELECTION-SCREEN:  BEGIN OF LINE,
    COMMENT 1(15) TEXT-t01 FOR FIELD p_delv.
    SELECTION-SCREEN    POSITION 38.
    PARAMETERS: p_delv TYPE ty_delivery OBLIGATORY.
    SELECTION-SCREEN: END OF LINE.

*    SELECTION-SCREEN:  BEGIN OF LINE,
*    COMMENT 1(15) TEXT-t02 FOR FIELD p_part.
*    SELECTION-SCREEN    POSITION 38.
*    PARAMETERS: p_part TYPE ty_partner OBLIGATORY DEFAULT 'LEIDW8292'.
*    SELECTION-SCREEN: END OF LINE.

    SELECTION-SCREEN:  BEGIN OF LINE,
    COMMENT 1(15) TEXT-t03 FOR FIELD p_invt.
    SELECTION-SCREEN    POSITION 38.
    PARAMETERS: p_invt AS CHECKBOX DEFAULT ' '.
    SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN: END OF BLOCK main.

SELECTION-SCREEN: END OF BLOCK screen.

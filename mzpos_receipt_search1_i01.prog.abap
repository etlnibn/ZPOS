*&---------------------------------------------------------------------*
*& Include          MZPOS_RECEIPT_SEARCH_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE PROGRAM.
    WHEN 'SRCH' OR 'ENTR'.
      CLEAR gv_check_code.
      PERFORM check_mandatory_data USING gv_check_code.

      IF gv_check_code NE 'E'.
        PERFORM receipt_search.
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      SET SCREEN 0.
      LEAVE TO SCREEN 0.
    WHEN 'ENTR'.
      ASSERT 1 = 1.
    WHEN 'SRTU'.
*      Sort Acedning
    WHEN 'SRTD'.
*      Sort Descending
    WHEN 'DETL'.

      GET CURSOR FIELD gv_field LINE gv_pick_line.
      gv_pick_line = tc_receipt-top_line + gv_pick_line - 1.            " Selecting the row even after scrolling
      READ TABLE gt_receipt INDEX gv_pick_line INTO wa_receipt.
      PERFORM refresh_receipt_details USING wa_receipt.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_GT_RECEIPT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_gt_receipt INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_GT_LINE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_gt_line INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9003 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_GT_TENDER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_gt_tender INPUT.

ENDMODULE.

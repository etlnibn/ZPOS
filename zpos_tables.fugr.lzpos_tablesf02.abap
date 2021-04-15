*----------------------------------------------------------------------*
***INCLUDE LZPOS_TABLESF02.
*----------------------------------------------------------------------*
FORM update_uname.

  DATA: f_indx LIKE sy-tabix. "Index to note the lines found


* Variable TOTAL is not transparent, this structure is how we access the individual fields.

  DATA BEGIN OF l_row.
  INCLUDE STRUCTURE zpos_artprc_sts.
  INCLUDE STRUCTURE vimtbflags.
  DATA END OF l_row.

  DATA: lv_timestamp TYPE hdb_timestamp.

  lv_timestamp = sy-datum && sy-uzeit.

  LOOP AT total.

    lv_timestamp += 1.
    CHECK <action> = 'U' OR <action> = 'N'.
    READ TABLE extract WITH KEY <vim_xtotal_key>.

    IF sy-subrc EQ 0.
      f_indx = sy-tabix.
    ELSE.
      CLEAR f_indx.
    ENDIF.

    l_row = total.
    IF <action> = 'N'.
      l_row-created_by = sy-uname.
      l_row-timestamp = lv_timestamp.
    ENDIF.

    total = l_row.
    MODIFY total.
    CHECK f_indx GT 0.
    extract = total.
    MODIFY extract INDEX f_indx.

  ENDLOOP.
  sy-subrc = 0.

ENDFORM.

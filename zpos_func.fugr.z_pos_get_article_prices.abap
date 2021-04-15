FUNCTION z_pos_get_article_prices .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_R_VTWEG) TYPE  WRF_VTWEG_RTTY OPTIONAL
*"     VALUE(IT_R_VKORG) TYPE  WRF_VKORG_RTTY OPTIONAL
*"     VALUE(IT_R_WERKS) TYPE  WRF_WERKS_RTTY
*"     VALUE(IT_R_MATNR) TYPE  WRF_MATNR_RTTY OPTIONAL
*"     VALUE(IV_VALID_FROM) TYPE  DATS
*"     VALUE(IT_R_EAN11) TYPE  WART_TR_EAN11 OPTIONAL
*"     VALUE(IV_VALID_TO) TYPE  DATS
*"  EXPORTING
*"     VALUE(ET_PRICELIST) TYPE  ZPOS_PRICELIST_RFC_TTY
*"----------------------------------------------------------------------

  et_pricelist = zcl_pos_wso_article_price=>get_article_prices(
                                        it_r_vtweg = it_r_vtweg
                                        it_r_vkorg = it_r_vkorg
                                        it_r_werks = it_r_werks
                                        it_r_matnr = it_r_matnr
                                        it_r_ean11 = it_r_ean11
                                        iv_valid_from = iv_valid_from
                                        iv_valid_to = iv_valid_to
                                         ).

ENDFUNCTION.

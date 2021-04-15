*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 08.04.2021 at 14:46:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMAG_CARRIERS...................................*
DATA:  BEGIN OF STATUS_ZMAG_CARRIERS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMAG_CARRIERS                 .
CONTROLS: TCTRL_ZMAG_CARRIERS
            TYPE TABLEVIEW USING SCREEN '0012'.
*...processing: ZPOS_ARTICLE....................................*
DATA:  BEGIN OF STATUS_ZPOS_ARTICLE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_ARTICLE                  .
CONTROLS: TCTRL_ZPOS_ARTICLE
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZPOS_ARTICLE_BOM................................*
DATA:  BEGIN OF STATUS_ZPOS_ARTICLE_BOM              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_ARTICLE_BOM              .
CONTROLS: TCTRL_ZPOS_ARTICLE_BOM
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZPOS_ARTPRC_STS.................................*
DATA:  BEGIN OF STATUS_ZPOS_ARTPRC_STS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_ARTPRC_STS               .
CONTROLS: TCTRL_ZPOS_ARTPRC_STS
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZPOS_CONFIG_MAP.................................*
DATA:  BEGIN OF STATUS_ZPOS_CONFIG_MAP               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_CONFIG_MAP               .
CONTROLS: TCTRL_ZPOS_CONFIG_MAP
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: ZPOS_PRICELIST..................................*
DATA:  BEGIN OF STATUS_ZPOS_PRICELIST                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_PRICELIST                .
CONTROLS: TCTRL_ZPOS_PRICELIST
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZPOS_SHELF_LBL..................................*
DATA:  BEGIN OF STATUS_ZPOS_SHELF_LBL                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_SHELF_LBL                .
CONTROLS: TCTRL_ZPOS_SHELF_LBL
            TYPE TABLEVIEW USING SCREEN '0008'.
*...processing: ZPOS_STORE_RCPT.................................*
DATA:  BEGIN OF STATUS_ZPOS_STORE_RCPT               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_STORE_RCPT               .
CONTROLS: TCTRL_ZPOS_STORE_RCPT
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZPOS_VK_LIST....................................*
DATA:  BEGIN OF STATUS_ZPOS_VK_LIST                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_VK_LIST                  .
CONTROLS: TCTRL_ZPOS_VK_LIST
            TYPE TABLEVIEW USING SCREEN '0010'.
*...processing: ZPOS_VK_OFFER...................................*
DATA:  BEGIN OF STATUS_ZPOS_VK_OFFER                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_VK_OFFER                 .
CONTROLS: TCTRL_ZPOS_VK_OFFER
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZPOS_WEBSITE....................................*
DATA:  BEGIN OF STATUS_ZPOS_WEBSITE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOS_WEBSITE                  .
CONTROLS: TCTRL_ZPOS_WEBSITE
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZMAG_CARRIERS                 .
TABLES: *ZPOS_ARTICLE                  .
TABLES: *ZPOS_ARTICLE_BOM              .
TABLES: *ZPOS_ARTPRC_STS               .
TABLES: *ZPOS_CONFIG_MAP               .
TABLES: *ZPOS_PRICELIST                .
TABLES: *ZPOS_SHELF_LBL                .
TABLES: *ZPOS_STORE_RCPT               .
TABLES: *ZPOS_VK_LIST                  .
TABLES: *ZPOS_VK_OFFER                 .
TABLES: *ZPOS_WEBSITE                  .
TABLES: ZMAG_CARRIERS                  .
TABLES: ZPOS_ARTICLE                   .
TABLES: ZPOS_ARTICLE_BOM               .
TABLES: ZPOS_ARTPRC_STS                .
TABLES: ZPOS_CONFIG_MAP                .
TABLES: ZPOS_PRICELIST                 .
TABLES: ZPOS_SHELF_LBL                 .
TABLES: ZPOS_STORE_RCPT                .
TABLES: ZPOS_VK_LIST                   .
TABLES: ZPOS_VK_OFFER                  .
TABLES: ZPOS_WEBSITE                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

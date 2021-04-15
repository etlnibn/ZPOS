class ZCL_POS_MAG_ORDER_TEST definition
  public
  final
  create public .

public section.

  class-methods CREATE_DELIVERY
    importing
      !IV_ORDER type VBELN
    returning
      value(RV_DELIVERY) type VBELN .
  class-methods PERFORM_DELIVERY_PGI
    importing
      !IV_DELIVERY type VBELN
      !IV_INCREASE_INVENTORY type BOOLE_D
    returning
      value(RV_DOCNUM) type EDI_DOCNUM .
  class-methods CREATE_BILLING_DOCUMENT
    importing
      !IV_DELIVERY type VBELN
    returning
      value(RV_BILLING) type VBELN .
  class-methods GET_DELIVERY_VIA_DOCFLOW
    importing
      !IV_ORDER type VBELN
    returning
      value(RV_DELIVERY) type VBELN .
protected section.
private section.

  class-data:
    mt_r_docnum TYPE RANGE OF edi_docnum .

  class-methods INCREASE_INVENTORY
    importing
      !IT_ITEMS type TAB_LIPS
      !IV_PARTNER type EDI_RCVPRN .
ENDCLASS.



CLASS ZCL_POS_MAG_ORDER_TEST IMPLEMENTATION.


  METHOD create_billing_document.

    DATA: lt_bapivbrk TYPE STANDARD TABLE OF bapivbrk,
          lt_return   TYPE STANDARD TABLE OF bapiret1,
          lt_success  TYPE STANDARD TABLE OF bapivbrksuccess.

    APPEND INITIAL LINE TO lt_bapivbrk ASSIGNING FIELD-SYMBOL(<ls_bapivbrk>).
    <ls_bapivbrk>-ref_doc    = iv_delivery.
    <ls_bapivbrk>-ref_doc_ca = 'J'.

    CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
*     EXPORTING
*       CREATORDATAIN         =
*       TESTRUN               =
*       POSTING               =
      TABLES
        billingdatain = lt_bapivbrk
*       CONDITIONDATAIN       =
*       CCARDDATAIN   =
*       TEXTDATAIN    =
*       ERRORS        =
        return        = lt_return
        success       = lt_success
*       EXTENSIONIN   =
*       NFMETALLITMS  =
      .

    READ TABLE lt_return WITH KEY type = 'E' INTO DATA(ls_return).

    IF sy-subrc = 0.
      MESSAGE i000(zpos) WITH 'Billing Document creation error'.
    ELSE.
      READ TABLE lt_success INDEX 1 INTO DATA(ls_success).
      rv_billing = ls_success-bill_doc.

      IF NOT rv_billing IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        DATA(lv_counter) =  0.
        DATA(lv_max)     =  10. " Wait for 10 seconds in maximum
        WHILE lv_counter < lv_max.
          CALL FUNCTION 'ENQUEUE_EVVBAKE'
            EXPORTING
              vbeln          = rv_billing
            EXCEPTIONS
              foreign_lock   = 02
              system_failure = 03.
          IF sy-subrc = 0.
            CALL FUNCTION 'DEQUEUE_EVVBAKE'
              EXPORTING
                vbeln = rv_billing.
            lv_counter = lv_max + 1.
          ELSE.
            lv_counter = lv_counter + 1.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDWHILE.
        IF lv_counter = lv_max.
          MESSAGE i000(zpos) WITH 'Could not create lock on Billing document' && rv_billing.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD CREATE_DELIVERY.

*    DATA: BEGIN OF ls_vbap OCCURS 0,
*            vbeln  LIKE vbap-vbeln,
*            posnr  LIKE vbap-posnr,
*            kwmeng LIKE vbap-kwmeng,
*            matnr  LIKE vbap-matnr,
*            werks  LIKE vbap-werks,
*          END OF ls_vbap.

    DATA: lt_request TYPE STANDARD TABLE OF bapideliciousrequest.
    DATA: lt_created TYPE STANDARD TABLE OF bapideliciouscreateditems.
    DATA: lt_return TYPE STANDARD TABLE OF bapiret2.

*    SELECT SINGLE vbeln FROM vbkd INTO @DATA(lv_order)
*      WHERE bstkd = @iv_mag_order.

    SELECT vbeln, posnr, kwmeng, matnr, werks
       INTO TABLE @DATA(lt_vbap)
       FROM vbap
       WHERE vbeln = @iv_order.

    LOOP AT lt_vbap INTO DATA(ls_vbap).

      APPEND INITIAL LINE TO lt_request ASSIGNING FIELD-SYMBOL(<ls_request>).
      <ls_request>-document_numb = ls_vbap-vbeln.
      <ls_request>-document_item = ls_vbap-posnr.
      <ls_request>-quantity_sales_uom = ls_vbap-kwmeng.
*        <ls_request>-quantity_sales_uom = ls_vbap-zmeng.
*        <ls_request>-quantity_base__uom = ls_vbap-zmeng.
      <ls_request>-id = 1.
      <ls_request>-document_type = 'A'.
      <ls_request>-delivery_date = sy-datum.
      <ls_request>-material = ls_vbap-matnr.
      <ls_request>-plant = ls_vbap-werks.
      <ls_request>-date = sy-datum.
      <ls_request>-goods_issue_date = sy-datum.
      <ls_request>-goods_issue_time = sy-uzeit.

    ENDLOOP.

    CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXEC'
      TABLES
        request      = lt_request
        createditems = lt_created
        return       = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' INTO DATA(ls_return).

    IF sy-subrc = 0.
      MESSAGE i000(zpos) WITH 'Delivery creation error'.
    ELSE.

      COMMIT WORK AND WAIT.
      READ TABLE lt_created INDEX 1 INTO DATA(ls_created).
      rv_delivery = ls_created-document_numb.
    ENDIF.

* Need the Delivery Number for the next stage


  ENDMETHOD.


  METHOD get_delivery_via_docflow.

    DATA lt_docflow TYPE tdt_docflow.

    CALL FUNCTION 'SD_DOCUMENT_FLOW_GET'
      EXPORTING
        iv_docnum  = iv_order
*       IV_ITEMNUM =
*       IV_ALL_ITEMS           =
*       IV_SELF_IF_EMPTY       = ' '
      IMPORTING
        et_docflow = lt_docflow.


    READ TABLE lt_docflow INTO DATA(ls_docflow) WITH KEY vbtyp = 'J'.   " J is Delivery.

* Could there be more than 1
    IF sy-subrc = 0.
      rv_delivery = ls_docflow-docnum.
    ELSE.
      MESSAGE i000(zpos) WITH 'Could not find delivery document for this order'.
    ENDIF.

  ENDMETHOD.


  METHOD INCREASE_INVENTORY.

    DATA: lt_edidd     TYPE STANDARD TABLE OF edidd,
          ls_idoc_data TYPE edidd,
          ls_edidc     TYPE edidc,
          ls_edids     TYPE edids,
          lv_subrc     TYPE sysubrc.

    DATA: lv_parent_segnum TYPE idocdsgnum VALUE '0',
          lv_segnum        TYPE idocdsgnum VALUE '0'.

*   Segments
    DATA: ls_e1mbxyh TYPE e1mbxyh,
          ls_e1mbxyi TYPE e1mbxyi.


*------------------------------- CONTROL record ------------------------------------
    ls_edidc-rcvpor = ls_edidc-sndpor = 'SAP' && sy-sysid.

    ls_edidc-rcvpor = 'SAP' && sy-sysid.
    ls_edidc-rcvprn = ls_edidc-sndprn = iv_partner.
    ls_edidc-rcvprt = ls_edidc-sndprt = 'LS'.

    ls_edidc-sndpor = 'SAP_PO'.
    ls_edidc-status = '64'.
    ls_edidc-direct = '2'.
    ls_edidc-mestyp = 'WMMBXY'.
    ls_edidc-idoctp = 'WMMBID02'.
    ls_edidc-mescod = 'BKP'.
*------------------------------- CONTROL RECORD ------------------------------------


*------------------------------- E1MBXYH------------------------------------
    ls_idoc_data-segnam = 'E1MBXYH'.
    ls_idoc_data-hlevel =  '02'.
    lv_segnum           = 1.
    ls_idoc_data-segnum = lv_segnum.

    ls_e1mbxyh-bldat = sy-datum.
    ls_e1mbxyh-budat = sy-datum.
    ls_e1mbxyh-tcode = 'MB11'.

    ls_idoc_data-sdata = ls_e1mbxyh.
    APPEND ls_idoc_data TO lt_edidd.
    CLEAR : ls_idoc_data.
    lv_parent_segnum = lv_segnum.

*------------------------------- E1MBXYH ------------------------------------

    LOOP AT it_items INTO DATA(ls_item).

*------------------------------- E1MBXYI ------------------------------------
      ls_idoc_data-segnam = 'E1MBXYI'.
      ls_idoc_data-hlevel =  '03'.
      ls_idoc_data-psgnum = lv_parent_segnum.
      lv_segnum           += 1.
      ls_idoc_data-segnum = lv_segnum.

      ls_e1mbxyi-matnr      = ls_item-matnr.
      ls_e1mbxyi-werks      = ls_item-werks.
      ls_e1mbxyi-lgort      = ls_item-lgort.
      ls_e1mbxyi-bwart      = '712'.
      ls_e1mbxyi-erfmg      = ls_item-lfimg.
      ls_e1mbxyi-erfme      = ls_item-meins.
      ls_e1mbxyi-kostl      = '0000197118'.
      ls_e1mbxyi-grund      = '0010'.

      ls_idoc_data-sdata = ls_e1mbxyi.
      APPEND ls_idoc_data TO lt_edidd.
      CLEAR : ls_idoc_data.
*------------------------------- E1BPOBDLVITEMCON ------------------------------------

    ENDLOOP.

    IF it_items IS NOT INITIAL.

      CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
        EXPORTING
          pi_status_message      = ls_edids
        IMPORTING
          pe_state_of_processing = lv_subrc
        TABLES
          t_data_records         = lt_edidd
        CHANGING
          pc_control_record      = ls_edidc
        EXCEPTIONS
          idoc_not_saved         = 1
          OTHERS                 = 2.

      IF sy-subrc = 0 AND NOT ls_edidc-docnum IS INITIAL.
        COMMIT WORK AND WAIT.

        CLEAR mt_r_docnum.
        APPEND INITIAL LINE TO mt_r_docnum ASSIGNING FIELD-SYMBOL(<ls_docnum>).
        <ls_docnum>-sign   = zcl_pos_util=>co_selopt_sign_i.
        <ls_docnum>-option = zcl_pos_util=>co_selopt_opt_eq.
        <ls_docnum>-low    = ls_edidc-docnum.

*RBDAPP01 DCN
        SET PARAMETER ID: ls_edidc-docnum FIELD 'DCN'  .    " Not Used anymore
        SUBMIT rbdapp01 WITH docnum IN mt_r_docnum AND RETURN.

*        MESSAGE i000(zpos) WITH 'Inventory Update via IDoc :- ' && ls_edidc-docnum.
      ELSE.
        MESSAGE i000(zpos) WITH 'Error Creating IDoc'.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD perform_delivery_pgi.

    DATA: lt_edidd     TYPE STANDARD TABLE OF edidd,
          ls_idoc_data TYPE edidd,
          ls_edidc     TYPE edidc,
          ls_edids     TYPE edids,
          lv_subrc     TYPE sysubrc.

    DATA: lv_parent_segnum TYPE idocdsgnum VALUE '0',
          lv_segnum        TYPE idocdsgnum VALUE '0',
          lt_items         TYPE tab_lips,
          lv_partner       TYPE edi_rcvprn.

*   Segments
    DATA: ls_confirm_decentr      TYPE e1shp_obdlv_confirm_decentr,
          ls_e1bpobdlvhdrcon      TYPE e1bpobdlvhdrcon,
          ls_e1bpobdlvhdrctrlcon  TYPE e1bpobdlvhdrctrlcon,
          ls_e1bpobdlvitemcon     TYPE e1bpobdlvitemcon,
          ls_e1bpobdlvitemctrlcon TYPE e1bpobdlvitemctrlcon.

    SELECT * FROM lips
             INTO CORRESPONDING FIELDS OF TABLE @lt_items
             WHERE vbeln = @iv_delivery.

    IF lt_items IS NOT INITIAL.

      READ TABLE lt_items INDEX 1 INTO DATA(ls_item).
*LEIDW8292
      lv_partner = 'LEIDW' && ls_item-werks.

      IF iv_increase_inventory = abap_true AND lt_items IS NOT INITIAL.

        increase_inventory( it_items = lt_items iv_partner = lv_partner ).

      ENDIF.

*------------------------------- CONTROL record ------------------------------------
      ls_edidc-rcvpor = ls_edidc-sndpor = 'SAP' && sy-sysid.

*    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
*      IMPORTING
*        own_logical_system             = ls_edidc-rcvpor
*      EXCEPTIONS
*        own_logical_system_not_defined = 1
*        OTHERS                         = 2.
*    IF sy-subrc <> 0.
*    ENDIF.

      ls_edidc-rcvpor = 'SAP' && sy-sysid.
      ls_edidc-rcvprn = ls_edidc-sndprn = lv_partner.
      ls_edidc-rcvprt = ls_edidc-sndprt = 'LS'.

      ls_edidc-sndpor = 'SAP_PO'.
      ls_edidc-status = '64'.
      ls_edidc-direct = '2'.
      ls_edidc-mestyp = 'SHP_OBDLV_CONFIRM_DECENTRAL'.
      ls_edidc-idoctp = 'SHP_OBDLV_CONFIRM_DECENTRAL05'.
      ls_edidc-mescod = 'BKP'.
*------------------------------- CONTROL RECORD ------------------------------------


*------------------------------- E1SHP_OBDLV_CONFIRM_DECENTR ------------------------------------
      ls_idoc_data-segnam = 'E1SHP_OBDLV_CONFIRM_DECENTR'.
      ls_idoc_data-hlevel =  '02'.
      lv_segnum           = 1.
      ls_idoc_data-segnum = lv_segnum.

      ls_confirm_decentr-delivery = iv_delivery.

      ls_idoc_data-sdata = ls_confirm_decentr.
      APPEND ls_idoc_data TO lt_edidd.
      CLEAR : ls_idoc_data.
      lv_parent_segnum = lv_segnum.

*------------------------------- E1BPOBDLVHDRCON ------------------------------------


*------------------------------- E1BPOBDLVHDRCON ------------------------------------
      ls_idoc_data-segnam = 'E1BPOBDLVHDRCON'.
      ls_idoc_data-hlevel =  '03'.
      ls_idoc_data-psgnum = lv_parent_segnum.
      lv_segnum           += 1.
      ls_idoc_data-segnum = lv_segnum.

      ls_e1bpobdlvhdrcon-deliv_numb = iv_delivery.

      ls_idoc_data-sdata = ls_e1bpobdlvhdrcon.
      APPEND ls_idoc_data TO lt_edidd.
      CLEAR : ls_idoc_data.
*------------------------------- E1BPOBDLVHDRCTRLCON ------------------------------------


*------------------------------- E1BPOBDLVHDRCTRLCON ------------------------------------
      ls_idoc_data-segnam = 'E1BPOBDLVHDRCTRLCON'.
      ls_idoc_data-hlevel =  '03'.
      ls_idoc_data-psgnum = lv_parent_segnum.
      lv_segnum           += 1.
      ls_idoc_data-segnum = lv_segnum.

      ls_e1bpobdlvhdrctrlcon-deliv_numb = iv_delivery.
      ls_e1bpobdlvhdrctrlcon-post_gi_flg = abap_true.

      ls_idoc_data-sdata = ls_e1bpobdlvhdrctrlcon.
      APPEND ls_idoc_data TO lt_edidd.
      CLEAR : ls_idoc_data.
*------------------------------- E1BPOBDLVHDRCTRLCON ------------------------------------




      LOOP AT lt_items INTO ls_item.
*------------------------------- E1BPOBDLVITEMCON ------------------------------------
        ls_idoc_data-segnam = 'E1BPOBDLVITEMCON'.
        ls_idoc_data-hlevel =  '03'.
        ls_idoc_data-psgnum = lv_parent_segnum.
        lv_segnum           += 1.
        ls_idoc_data-segnum = lv_segnum.

        ls_e1bpobdlvitemcon-deliv_numb      = ls_item-vbeln.
        ls_e1bpobdlvitemcon-deliv_item      = ls_item-posnr.
        ls_e1bpobdlvitemcon-material        = ls_item-matnr.
        ls_e1bpobdlvitemcon-dlv_qty         = ls_item-lfimg.
        ls_e1bpobdlvitemcon-dlv_qty_imunit  = ls_item-lfimg.
        ls_e1bpobdlvitemcon-fact_unit_nom   = 1.
        ls_e1bpobdlvitemcon-fact_unit_denom = 1.
        ls_e1bpobdlvitemcon-base_uom        = ls_item-meins.

        CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
          EXPORTING
            sap_code = ls_item-meins
          IMPORTING
            iso_code = ls_e1bpobdlvitemcon-base_uom_iso
*       EXCEPTIONS
*           NOT_FOUND         = 1
*           NO_ISO_CODE       = 2
*           OTHERS   = 3
          .
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        ls_idoc_data-sdata = ls_e1bpobdlvitemcon.
        APPEND ls_idoc_data TO lt_edidd.
        CLEAR : ls_idoc_data.
*------------------------------- E1BPOBDLVITEMCON ------------------------------------

      ENDLOOP.


      LOOP AT lt_items INTO ls_item.
*------------------------------- E1BPOBDLVITEMCTRLCON ------------------------------------
        ls_idoc_data-segnam = 'E1BPOBDLVITEMCTRLCON'.
        ls_idoc_data-hlevel =  '03'.
        ls_idoc_data-psgnum = lv_parent_segnum.
        lv_segnum           += 1.
        ls_idoc_data-segnum = lv_segnum.

        ls_e1bpobdlvitemctrlcon-deliv_numb      = ls_item-vbeln.
        ls_e1bpobdlvitemctrlcon-deliv_item      = ls_item-posnr.

        ls_idoc_data-sdata = ls_e1bpobdlvitemctrlcon.
        APPEND ls_idoc_data TO lt_edidd.
        CLEAR : ls_idoc_data.
*------------------------------- E1BPOBDLVITEMCON ------------------------------------
      ENDLOOP.




      CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
        EXPORTING
          pi_status_message      = ls_edids
        IMPORTING
          pe_state_of_processing = lv_subrc
        TABLES
          t_data_records         = lt_edidd
        CHANGING
          pc_control_record      = ls_edidc
        EXCEPTIONS
          idoc_not_saved         = 1
          OTHERS                 = 2.

      IF sy-subrc = 0 AND NOT ls_edidc-docnum IS INITIAL.

        CLEAR mt_r_docnum.
        APPEND INITIAL LINE TO mt_r_docnum ASSIGNING FIELD-SYMBOL(<ls_docnum>).
        <ls_docnum>-sign   = zcl_pos_util=>co_selopt_sign_i.
        <ls_docnum>-option = zcl_pos_util=>co_selopt_opt_eq.
        <ls_docnum>-low    = ls_edidc-docnum.
*
**RBDAPP01 DCN
*        SET PARAMETER ID: ls_edidc-docnum FIELD 'DCN'  .    " Not Used anymore
        COMMIT WORK AND WAIT.

        SUBMIT rbdapp01 WITH docnum IN mt_r_docnum AND RETURN.
*        MESSAGE i000(zpos) WITH 'Goods Issue via IDoc :- ' && ls_edidc-docnum.
      ELSE.
        MESSAGE i000(zpos) WITH 'Error Creating IDoc'.

      ENDIF.


    ELSE.
      MESSAGE i000(zpos) WITH 'No Delivery Items Found - Check Delivery Number' && ls_edidc-docnum.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

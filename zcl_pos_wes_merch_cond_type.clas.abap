class ZCL_POS_WES_MERCH_COND_TYPE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_WES_MERCHANDISE_COND_TYPE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_POS_WES_MERCH_COND_TYPE IMPLEMENTATION.


  METHOD if_wes_merchandise_cond_type~filter_condition_types.

********************************************************************************
* Defaul Implementation:
* Within Service Implementation 0100 of service operation
* MerchandiseERPReplicationBulkRequest_Out transfers
* Condition records only of the Sales Condition Type e.g. VKP0
* If another or several condition type(s) (e.g. VKP0 and VKA0)
* should get taken into account an own implementation needs to done to overrule
* this default implementation

* CT_CONDITION_TYPES contains all defined condition types of
* table T6I1 (Document Index) with BLTYP = 80, KAPPL = 'V', KVEWE = 'A'.
* That table getd filtered and reduced by this method
* If CT_CONDITION_TYPES remains unchanged (e.g. via an empty implementation)
* all maintained condition types get evaluated and the corresponding condition
* records (if available) transferred within the service operation

* IS_PACACKAGE contains the header information of the current data package
*********************************************************************************

*    DATA:  ls_pespr         TYPE          pespr.
*
** get sales condition type for combination distribution chain and store
*    CALL FUNCTION 'SALES_PRICE_COND_TYPE_GET'
*      EXPORTING
*        pi_vkorg                    = is_package-vkorg
*        pi_vtweg                    = is_package-vtweg
*        pi_werks                    = is_package-werks
*      IMPORTING
*        pe_i_spr                    = ls_pespr
*      EXCEPTIONS
*        plant_not_found             = 1
*        org_structure_not_completed = 2
*        vkorg_not_found             = 3
*        no_calculation_type_found   = 4
*        no_condition_types_found    = 5
*        invalid_import              = 6
*        customer_is_no_plant        = 7
*        OTHERS                      = 8.
*
*    IF sy-subrc <> 0     OR
*       ls_pespr-vksch    IS INITIAL.
*      CLEAR ct_condition_types.
*    ENDIF.
*
** delete condition types that are not equal to
** determined sales condition type
*    DELETE ct_condition_types WHERE kschl <> ls_pespr-vksch.


*                BAUHAUS - No Filtering Required

  ENDMETHOD.
ENDCLASS.

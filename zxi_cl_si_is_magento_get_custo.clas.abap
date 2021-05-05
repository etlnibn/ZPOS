class ZXI_CL_SI_IS_MAGENTO_GET_CUSTO definition
  public
  create public .

public section.

  interfaces ZXI_II_SI_IS_MAGENTO_GET_CUSTO .
protected section.
private section.
ENDCLASS.



CLASS ZXI_CL_SI_IS_MAGENTO_GET_CUSTO IMPLEMENTATION.


  METHOD zxi_ii_si_is_magento_get_custo~si_is_get_customer_projects.
*** **** INSERT IMPLEMENTATION HERE **** ***


    SELECT SINGLE *
      FROM zmagi_customer_info_detail
      INTO @DATA(ls_customer_info)
     WHERE businesspartner EQ @input-customer_id.

    APPEND INITIAL LINE TO output-get_customer_projects_result-project ASSIGNING FIELD-SYMBOL(<ls_project>).


    <ls_project>-address = ls_customer_info-streetname.
    <ls_project>-address_supplement_info = ls_customer_info-streetsuffixname.
    <ls_project>-attention_person = ls_customer_info-personfullname.
    <ls_project>-city = ls_customer_info-cityname.
    <ls_project>-name = ls_customer_info-organizationbpname1.
    CONDENSE ls_customer_info-postalcode NO-GAPS.
    <ls_project>-postal_code = ls_customer_info-postalcode.
    <ls_project>-status = 'Active'.



*CONTROLLER
*ADDRESS
*ADDRESS_SUPPLEMENT_INFO
*ATTENTION_PERSON
*CITY
*ID
*INTERNAL_ID
*LOCK_ID
*NAME
*POSTAL_CODE
*STATUS




  ENDMETHOD.
ENDCLASS.

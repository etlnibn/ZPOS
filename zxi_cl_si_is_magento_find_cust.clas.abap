CLASS zxi_cl_si_is_magento_find_cust DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zxi_ii_si_is_magento_find_cust .
  PROTECTED SECTION.
private section.

  constants CO_RIGHTS_STEM type STRING value 'http://bauhaus.net/webshopb2b/' ##NO_TEXT.
  constants CO_RIGHTS_SUBACCOUNT type STRING value 'subaccount' ##NO_TEXT.
  constants CO_RIGHTS_ACCOUNT type STRING value 'account' ##NO_TEXT.
ENDCLASS.



CLASS ZXI_CL_SI_IS_MAGENTO_FIND_CUST IMPLEMENTATION.


  METHOD zxi_ii_si_is_magento_find_cust~si_is__magento_find_customer_i.
*** **** INSERT IMPLEMENTATION HERE **** ***

* Find the customers linked to this contact via the relationship (search based on the Social Security Number)
    SELECT * FROM zmagi_customer_info_list INTO TABLE @DATA(lt_customers) WHERE contactidentitynumber = @input-ssn.

* Did we find any data?
    DATA(lv_lines) = lines( lt_customers ).

    IF lv_lines GT 1.
* If we found more than 1 company then return a list of possible companies
      LOOP AT lt_customers INTO DATA(ls_customer).
        APPEND INITIAL LINE TO output-find_customer_info_result-possible_customers-customer_entry ASSIGNING FIELD-SYMBOL(<ls_customer_entry>).
        <ls_customer_entry>-customer_id   = ls_customer-organizationbpid.
        <ls_customer_entry>-customer_name = ls_customer-organizationname1.
      ENDLOOP.
      output-find_customer_info_result-result_type = 'Multiple'.

    ELSEIF lv_lines = 1.

* If we found exactly 1 - then find the detailed information
      TRY.
          ls_customer = lt_customers[ 1 ].

          SELECT * FROM zmagi_customer_info_detail INTO TABLE @DATA(lt_organization_detail) WHERE businesspartner = @ls_customer-organizationbpid.

* It would be really strange if no detailed data is returned ... not checking for now!

          DATA(ls_organization_detail) = lt_organization_detail[ 1 ].

          output-find_customer_info_result-customer-customer-address_supplement_info = ls_organization_detail-streetsuffixname.
          output-find_customer_info_result-customer-customer-city                    = ls_organization_detail-cityname.
          output-find_customer_info_result-customer-customer-company_name            = ls_organization_detail-organizationbpname1.

          output-find_customer_info_result-customer-customer-organisation_number     = ls_organization_detail-bptaxnumber.

          CONDENSE ls_organization_detail-postalcode NO-GAPS.
          output-find_customer_info_result-customer-customer-postal_code             = ls_organization_detail-postalcode.
          output-find_customer_info_result-customer-customer-street                  = ls_organization_detail-streetname.
          output-find_customer_info_result-customer-customer-telephone               = ls_organization_detail-phonenumber.

* Now get the Contact details of the owner/controller

* This gets the ContactID for the owner
          SELECT * FROM  zmagi_customer_info_list INTO TABLE @lt_customers
                   WHERE contactidentitynumber = @ls_organization_detail-bpidentificationnumber
                   AND   organizationbpid      = @ls_organization_detail-businesspartner.

* There could be an exception here but it should already be caught
          ls_customer = lt_customers[ 1 ].

* Now get the details for the owner
          SELECT * FROM zmagi_customer_info_detail INTO TABLE @DATA(lt_owner_detail) WHERE businesspartner = @ls_customer-contactbpid.

* There could be an exception here but it should already be caught
          DATA(ls_owner_detail) = lt_owner_detail[ 1 ].

          output-find_customer_info_result-customer-customer-email       = ls_owner_detail-emailaddress.
          output-find_customer_info_result-customer-customer-first_name  = ls_owner_detail-firstname.
          output-find_customer_info_result-customer-customer-last_name   = ls_owner_detail-lastname.
          output-find_customer_info_result-customer-customer-full_name   = ls_owner_detail-personfullname.

* IF we somehow end up with multiple Identification records on the same BP then we need to try and see if the person searching is a "controller / owner" for the company

          APPEND INITIAL LINE TO output-find_customer_info_result-customer-rights-string ASSIGNING FIELD-SYMBOL(<ls_right>).
          <ls_right> = 'http://bauhaus.net/webshopb2b/subaccount' .
          LOOP AT lt_organization_detail INTO ls_organization_detail WHERE bpidentificationnumber = input-ssn.
            <ls_right> = 'http://bauhaus.net/webshopb2b/account' .
            EXIT.
          ENDLOOP.


          output-find_customer_info_result-customer-social_security_number            = input-ssn.

          output-find_customer_info_result-result_type = 'One'.

        CATCH cx_sy_itab_line_not_found.

      ENDTRY.

    ELSE.
* Nothing Found!
    ENDIF.



  ENDMETHOD.
ENDCLASS.

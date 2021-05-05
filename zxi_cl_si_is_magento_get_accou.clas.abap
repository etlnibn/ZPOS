class ZXI_CL_SI_IS_MAGENTO_GET_ACCOU definition
  public
  create public .

public section.

  interfaces ZXI_II_SI_IS_MAGENTO_GET_ACCOU .
protected section.
private section.
ENDCLASS.



CLASS ZXI_CL_SI_IS_MAGENTO_GET_ACCOU IMPLEMENTATION.


  METHOD zxi_ii_si_is_magento_get_accou~si_is_magento_get_account_ifor.

*** **** TEST MOCK INTERFACE **** ***

*** **** TEST MOCK INTERFACE **** ***

*** **** TEST MOCK INTERFACE **** ***

*** **** TEST MOCK INTERFACE **** ***


* Fixed Values for Testing
    output-get_account_information_result-rules-invoicing_rule  = 'Daily'.

    output-get_account_information_result-rules-card_number_at_cash_register_c  = abap_true.
    output-get_account_information_result-rules-project_number_at_cash_registe  = abap_true.
    output-get_account_information_result-rules-requisition_number_at_cash_reg  = abap_true.

*Credit Balance
    output-get_account_information_result-balance-available_balance   = 3000.
    output-get_account_information_result-balance-balance             = 0.
    output-get_account_information_result-balance-credit_max          = 3000.
    output-get_account_information_result-balance-waiting_for_billing = 0.

* The Customer ID will not be the Store Number - It will be the main B2B organization - Just for testing right now
    SELECT SINGLE * FROM zmag_business_partner INTO @DATA(ls_business_partner) WHERE businesspartner = @input-customer_id.
    output-get_account_information_result-home_store-store_id    = ls_business_partner-businesspartner.
    output-get_account_information_result-home_store-name        = ls_business_partner-organizationbpname1.
    output-get_account_information_result-home_store-street      = ls_business_partner-streetname.
    output-get_account_information_result-home_store-city        = ls_business_partner-cityname.
    output-get_account_information_result-home_store-postal_code = ls_business_partner-postalcode.
    output-get_account_information_result-home_store-phone       = ls_business_partner-phonenumber.

  ENDMETHOD.
ENDCLASS.

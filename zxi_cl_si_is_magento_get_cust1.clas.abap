class ZXI_CL_SI_IS_MAGENTO_GET_CUST1 definition
  public
  create public .

public section.

  interfaces ZXI_II_SI_IS_MAGENTO_GET_CUST1 .
protected section.
private section.
ENDCLASS.



CLASS ZXI_CL_SI_IS_MAGENTO_GET_CUST1 IMPLEMENTATION.


  METHOD zxi_ii_si_is_magento_get_cust1~si_is_magento_get_customer_car.

    SELECT *
      FROM zmagi_customer_card_detail
      INTO TABLE @DATA(lt_cards)
     WHERE company  EQ @input-customer_id.
    LOOP AT lt_cards INTO DATA(ls_card).

      APPEND INITIAL LINE TO output-get_customer_cards_result-customer_card ASSIGNING FIELD-SYMBOL(<ls_card>).
      <ls_card>-email                  = ls_card-emailaddress.
      <ls_card>-expiration_date        = ls_card-expiration.
      <ls_card>-first_name             = ls_card-firstname.
      <ls_card>-last_name              = ls_card-lastname.
      <ls_card>-full_name              = ls_card-fullname.
      <ls_card>-lock_id                = ''.
      <ls_card>-number                 = ls_card-cardid.
      <ls_card>-social_security_number = ls_card-bpidentificationnumber.
      <ls_card>-status                 = ls_card-status.

    ENDLOOP.

*ZMD_B2BCARD
*COMPANY
*CONTACT
*CPSTATUS
*CARDID
*CARDSTATUS
*EXPIRATION
*B2BCOMMENT

  ENDMETHOD.
ENDCLASS.

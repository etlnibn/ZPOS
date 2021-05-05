class ZXI_CL_SI_IS_GET_DISCOUNT_RATE definition
  public
  create public .

public section.

  interfaces ZXI_II_SI_IS_GET_DISCOUNT_RATE .
protected section.
private section.
ENDCLASS.



CLASS ZXI_CL_SI_IS_GET_DISCOUNT_RATE IMPLEMENTATION.


  method ZXI_II_SI_IS_GET_DISCOUNT_RATE~SI_IS_GET_DISCOUNT_RATES_MAGEN.
*** **** INSERT IMPLEMENTATION HERE **** ***


APPEND INITIAL LINE TO output-get_discount_rates_result-discount_rate ASSIGNING FIELD-SYMBOL(<ls_rates>).
    <ls_rates>-article_group_name = 'Test'.
    <ls_rates>-rate = 3.

  endmethod.
ENDCLASS.

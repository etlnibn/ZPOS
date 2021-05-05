class ZXI_CO_SI_OS_PROXY_TO_MAGENTO definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !DESTINATION type ref to IF_PROXY_DESTINATION optional
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    preferred parameter LOGICAL_PORT_NAME
    raising
      CX_AI_SYSTEM_FAULT .
  methods SI_OS_PROXY_TO_MAGENTO_BILLING
    importing
      !OUTPUT type ZXI_MT_BILLING_NOTIFICATION
    exporting
      !INPUT type ZXI_MT_BILLING_NOTIFICATION_RE
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZXI_CO_SI_OS_PROXY_TO_MAGENTO IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZXI_CO_SI_OS_PROXY_TO_MAGENTO'
    logical_port_name   = logical_port_name
    destination         = destination
  ).

  endmethod.


  method SI_OS_PROXY_TO_MAGENTO_BILLING.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'OUTPUT' kind = '0' value = ref #( OUTPUT ) )
    ( name = 'INPUT' kind = '1' value = ref #( INPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'SI_OS_PROXY_TO_MAGENTO_BILLING'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.

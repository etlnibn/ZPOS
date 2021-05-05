interface ZXI_II_SI_IS_PROXY_TO_MAGENTO
  public .


  methods SI_IS_PROXY_TO_MAGENTO_BILLING
    importing
      !INPUT type ZXI_MT_BILLING_NOTIFICATION_SE
    exporting
      !OUTPUT type ZXI_MT_BILLING_NOTIFICATION_RE .
endinterface.

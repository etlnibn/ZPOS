@AbapCatalog.sqlViewName: 'ZMAG_CUSTLIST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Magento Business Partner View'
define view ZMAGI_CUSTOMER_INFO_LIST

  as select from ZMAGI_CONTACT_LINK

  association [1..*] to I_BuPaIdentificationTP as _ContactIdentity on $projection.ContactBPID = _ContactIdentity.BusinessPartner

{
  key ZMAGI_CONTACT_LINK.OrganizationBPID,
  key ZMAGI_CONTACT_LINK.ContactBPID,
  key _ContactIdentity.BPIdentificationType,
      ZMAGI_CONTACT_LINK.OrganizationName1,
      ZMAGI_CONTACT_LINK.OrganizationName2,
      _ContactIdentity.BPIdentificationNumber as ContactIdentityNumber

}

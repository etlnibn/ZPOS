@AbapCatalog.sqlViewName: 'ZMAG_CONTACTLINK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Magento Contact Person Link'
define view ZMAGI_CONTACT_LINK
  as select from I_BPCustomerContactLink

{
  key BusinessPartnerUUID,
  key PersonUUID,
  key CustomerContact,
      CreatedByUser,
      CreationDate,
      CreationTime,

      _BusinessPartnerCompany.BusinessPartner     as OrganizationBPID,
      _BusinessPartnerCompany.OrganizationBPName1 as OrganizationName1,
      _BusinessPartnerCompany.OrganizationBPName2 as OrganizationName2,
      _BusinessPartnerPerson.BusinessPartner      as ContactBPID


}

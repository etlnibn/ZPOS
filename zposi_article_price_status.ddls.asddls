@ClientHandling.algorithm: #SESSION_VARIABLE
@VDM: {
  viewType: #BASIC
 }

@AbapCatalog.sqlViewName: 'ZPOS_ARTPRICE1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'POS - Article/PriceList/Status'
define view ZPOSI_ARTICLE_PRICE_STATUS
  as select from zpos_pricelist  as price
    inner join   zpos_article    as article on  price.vkorg = article.vkorg
    //    and price.vtweg = article.vtweg
                                            and price.werks = article.werks
                                            and price.matnr = article.matnr
                                            and price.ean11 = article.ean11
                                            and price.vrkme = article.vrkme
    inner join   zpos_artprc_sts as status  on  price.vkorg = status.vkorg
    //    and price.vtweg = status.vtweg
                                            and price.werks = status.werks
                                            and price.matnr = status.matnr
                                            and price.ean11 = status.ean11
                                            and price.vrkme = status.vrkme
                                            and price.datbi = status.datbi

  /*

  association[1..*] to zpos_artprc_sts as _Status
      on  $projection.vkorg = _Status.vkorg
      and $projection.vtweg = _Status.vtweg
      and $projection.werks = _Status.werks
      and $projection.matnr = _Status.matnr
      and $projection.ean11 = _Status.ean11
      and $projection.vrkme = _Status.vrkme
  //    and $projection.datbi = _Status.datbi



  --- association
  association [1..*] to zpos_store_rcpt as _Recipient
      on  $projection.vkorg = _Recipient.vkorg
      and $projection.vtweg = _Recipient.vtweg
      and $projection.werks = _Recipient.werks

  */

{

  key price.vkorg                 as vkorg,
  key price.vtweg                 as vtweg,
  key price.werks                 as werks,
  key price.matnr                 as matnr,
  key price.ean11                 as ean11,
      @Semantics.unitOfMeasure: true
  key price.vrkme                 as vrkme,
      @Semantics.businessDate.to: true
  key price.datbi                 as datbi,
      @Semantics.businessDate.from: true
      price.datab                 as datab,
      @Semantics.amount.currencyCode: 'currency'
      price.eff_price             as eff_price,
      price.eff_price_fkey        as eff_price_fkey,
      @Semantics.amount.currencyCode: 'currency'
      price.std_price             as std_price,
      @Semantics.currencyCode: true
      price.currency              as currency,

      @Semantics.quantity.unitOfMeasure: 'comp_uom'
      article.net_contents        as net_contents,
      article.comp_qty            as comp_qty,
      @Semantics.unitOfMeasure: true
      article.comp_uom            as comp_uom,
      @Semantics.text: true
      article.description         as description,
      @Semantics.text: true
      article.detailed_desc       as detailed_desc,
      article.pos_text            as pos_text,
      article.material_type       as material_type,
      article.material_category   as material_category,
      article.material_group      as material_group,
      article.material_group_1    as material_group_1,
      article.material_item_cat   as material_item_cat,
      article.vendor              as vendor,
      article.grade               as grade,
      article.layout_module       as layout_module,
      article.purch_status        as purch_status,
      article.tax_rate            as tax_rate,
      article.tax_classification  as tax_classification,
      article.tax_code            as tax_code,
      @Semantics.quantity.unitOfMeasure: 'weight_unit'
      article.gross_weight        as gross_weight,
      @Semantics.quantity.unitOfMeasure: 'weight_unit'
      article.net_weight          as net_weight,
      @Semantics.unitOfMeasure: true
      article.weight_unit         as weight_unit,
      article.commodity_code      as commodity_code,
      @Semantics.address.country: true
      article.country_origin      as country_origin,

      article.pos_entry_code      as pos_entry_code,
      article.pos_fam_number      as pos_fam_number,

      case
      when article.pos_staff_disc = 'X' then '1'
      when article.pos_staff_disc = ' ' then '0'
      else article.pos_staff_disc
      end                         as pos_staff_disc,

      article.pos_add_text        as pos_add_text,
      article.pos_repeat_key      as pos_repeat_key,
      article.pos_multibuy        as pos_multibuy,
      article.pos_add_item        as pos_add_item,
      article.pos_decimals        as pos_decimals,
      article.pos_fixedprice      as pos_fixedprice,
      article.freight_group       as freight_group,

      case
       when price.offer_id       is not initial then price.offer_id
       when price.offer_id_dsc   is not initial then price.offer_id_dsc
       else concat('KNUMH-', price.eff_price_fkey)
      end                         as offer_id,

      status.pos_recipient        as pos_recipient,
      status.pos_recipient_status as pos_recipient_status,
      @Semantics.businessDate.at: true
      status.timestamp            as timestamp,
      status.docnum               as docnum

}

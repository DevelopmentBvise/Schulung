@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help for I_PRODUCT'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZI_PRODUCTS_000
  as select from I_Product
{
  key Product                                                 as Product,
      _Text[1: Language=$session.system_language].ProductName as ProductText,
      @Semantics.amount.currencyCode: 'Currency'
      case
        when Product = 'D001' then cast ( 1000.00 as abap.dec(16,2) ) 
        when Product = 'D002' then cast ( 499.00 as abap.dec(16,2) ) 
        when Product = 'D003' then cast ( 799.00 as abap.dec(16,2) ) 
        when Product = 'D004' then cast ( 249.00 as abap.dec(16,2) )
        when Product = 'D005' then cast ( 1500.00 as abap.dec(16,2) ) 
        when Product = 'D006' then cast ( 30.00 as abap.dec(16,2) ) 
        else cast ( 100000.00 as abap.dec(16,2) ) 
      end                                                     as Price,

      @UI.hidden: true
      cast ( 'EUR' as abap.cuky( 5 ) )                        as Currency,

      @UI.hidden: true
      ProductGroup                                            as ProductGroup,

      @UI.hidden: true
      BaseUnit                                                as BaseUnit

}
where
    Product = 'D001'
  or Product = 'D002'
  or Product = 'D003'
  or Product = 'D004'
  or Product = 'D005'
  or Product = 'D006'

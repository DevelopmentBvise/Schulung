# Interface auf bestehende RAP-Objekte

Im Beispiel wurde eine Funktionalität der Entität I_SalesQuotationTP nicht für die Verwendung in der Cloud freigegeben, obwohl sie in der darunterliegenden Entität R_SalesQuotationTP vorliegt. Um diese Funktion wieder herzustellen, wurde die R_SalesQuotation als Basis genommen und ein eigenes RAP-Objekt daraus erstellt. 

Um dies zu ermöglichen, müssen alle benötigten CDS-Views der Vorlage kopiert bzw eine Projection-View auf diese Views erstellt werden. Wichtig ist der Zusatz `transactional_interface`. 
Die Objekte können mit einem Rechtsklick auf das Original -> New Data Definition komfortabel angelegt werden. 
Jedoch müssen alle Assoziationen erneut eingetragen werden. 

## Root-View CDS definition

```abap
@EndUserText.label: 'Projection View R_SalesQuotationTP'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity YCDS_SD_I_SalesQuotationTP
  provider contract transactional_interface
  as projection on R_SalesQuotationTP
{
  key SalesQuotation,
      SalesQuotationType,
      CustomerName,
      SoldToPartyAddressID,
      SalesOrganization,
      DistributionChannel,
      [...]
      _Item: redirected to composition child YCDS_SD_I_SalesQuotationItemTP,
      _Partner: redirected to composition child YCDS_SD_I_SalesQuotationPartTP,
      [...]
```

## Item-View

Für die Sub-Entitäten müssen ebenfalls projection-views erstellt werden, die mit `redirect to parent` auf die Root-View zeigen.

```abap
@EndUserText.label: 'Projection View R_SalesQuotationItemTP'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity YCDS_SD_I_SalesQuotationItemTP
  as projection on R_SalesQuotationItemTP
{
  key SalesQuotation,
  key SalesQuotationItem,
      ItemOrderProbabilityInPercent,
      SalesQuotationItemCategory,
      [...]
      _SalesQuotation: redirected to parent YCDS_SD_I_SalesQuotationTP,
```

## Partner-View

```abap
@EndUserText.label: 'Projection View R_SalesQuotationPartnerTP'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity YCDS_SD_I_SalesQuotationPartTP
  as projection on R_SalesQuotationPartnerTP
{
  key SalesQuotation,
  key PartnerFunction,
  [...]
  _SalesQuotation: redirected to parent YCDS_SD_I_SalesQuotationTP,
  [...]
```

## Behavior definition

Die Behavior-Definition (ebenfalls per Rechtsklick erstellt), enthält als wesentliche Information noch die Eigenschaft `interface` und keine eigene Implementierungsklasse (wir wollen die Original-Klasse verwenden).
Nun können alle Aktionen des BOs für eine Verwendung in der Cloud freigegeben werden

```abap
interface;

define behavior for YCDS_SD_I_SalesQuotationTP alias SalesQuotation
{
  use create;
  use update;

  use action CreateWithReference;
  use action UpdatePrices;
  use action ActivateIncompletenessInfo;
  use action WithdrawFromApproval;
  use action DetermineOutputItems;

  use association _Item { create; }
  use association _Text { create; }
  use association _Partner { create; }
  use association _PricingElement { create; }
  use association _ShipToParty;
}

define behavior for YCDS_SD_I_SalesQuotationItemTP alias SalesQuotationItem
{
  use update;
  use delete;

  use action UpdatePrices;
  use action SetRejectionReason;
  use action RemoveRejectionReason;

  use association _SalesQuotation;
}

define behavior for YCDS_SD_I_SalesQuotationPartTP alias SalesQuotationPartner
{
  use update;
  use delete;

  use action ChangeAddress;

  use association _SalesQuotation;
}

define behavior for YCDS_SD_I_SalesQuotationPETP alias SalesQuotationPricingElement
{
  use update;
  use delete;

  use association _SalesQuotation;
}

define behavior for YCDS_SD_I_SalesQuotationTextTP alias SalesQuotationText
{
  use update;
  use delete;

  use association _SalesQuotation;
}
```

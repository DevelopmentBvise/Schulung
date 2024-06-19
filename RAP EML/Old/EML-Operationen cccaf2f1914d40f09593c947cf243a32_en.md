# EML-Operationen

## EML READ

The READ function is not a SELECT, so it is important to remember that all keys of the business object (BO) must be known for a READ to be possible.

There are different ways to call up a read.

```
DATA:
  lt_selection TYPE TABLE FOR READ IMPORT ZBS_I_RAPPartner.

lt_selection = VALUE #(
  ( PartnerNumber = '1000000001' )
  ( PartnerNumber = '1000000003' )
).

READ ENTITIES OF ZBS_I_RAPPartner ENTITY Partner
  ALL FIELDS
  WITH lt_selection
  RESULT DATA(lt_partner_long)
  FAILED DATA(ls_failed)
  REPORTED DATA(ls_reported).

out->write( lt_partner_long ).

```

In this example, all fields offered by the BO are returned (`ALL FIELDS` ). The key is passed to the entity via a table defined for this purpose (`TYPE TABLE FOR READ IMPORT`).

A shorter version:

```abap
READ ENTITIES OF ZBS_I_RAPPartner ENTITY Partner
  FIELDS ( PartnerName Street City ) WITH VALUE #(
    ( PartnerNumber = '1000000001' )
    ( PartnerNumber = '1000000003' )
  )
  RESULT DATA(lt_partner_short)
  FAILED ls_failed
  REPORTED ls_reported.

out->write( lt_partner_short ).

```

In this example, only `PartnerName`, `Street` and `City` are read out.
The selection parameters are transferred directly.

Broken down in detail, the command consists of the following components:

- READ ENTITIES
    - Specifies the business object to be read from. This is usually the ROOT node
- (IN LOCAL MODE)
    - is only possible if the EML call is implemented from the behaviour pool of the BO itself. IN LOCAL MODE overrides the access control, authorisation check and all prechecks
- ENTITY
    - Specifies the sub-entity (can also be the root node)
- FIELDS
    - Defines which fields are to be read. `ALL FIELDS` is also possible
- WITH
    - Table with the key fields that are to be read. The table is to be defined as `TYPE TABLE FOR READ IMPORT`
- RESULT
    - Results table of the type `TYPE TABLE FOR READ RESULT`
- FAILED
    - Contains keys from incorrect read attempts
- REPORTED
    - Contains error messages for incorrect read attemptsten Leseversuchen

### Another example:

After calling an action in the RAP application, the keys are transferred in a table of the `Type TABLE FOR ACTION IMPORT`. The data does not necessarily have to be remapped. EML access then looks like this:

```abap
DATA: lt_keys TYPE TABLE FOR ACTION IMPORT ycds_sd_agcopy~propagateChanges.

READ ENTITIES OF YCDS_SD_AGCOPY_SalesQuotation IN LOCAL MODE
  ENTITY ycds_sd_agcopy
  ALL FIELDS
  WITH CORRESPONDING #( lt_keys )
  RESULT gt_agcopy.

```

As an implementation within the behavioural implementation of the same BO was carried out in this example, the `IN LOCAL MODE` addition should be used.

## CREATE

A Create is used to write new entries in the BO. The table type for the call is `TYPE TABLE FOR CREATE`.

```
DATA:
  lt_creation  TYPE TABLE FOR CREATE ZBS_I_RAPPartner.

lt_creation = VALUE #(
  (
    %cid = 'DummyKey1'
    PartnerNumber = '1000000007'
    PartnerName = 'Amazon'
    Country = 'US'
    %control-PartnerNumber = if_abap_behv=>mk-on
    %control-PartnerName = if_abap_behv=>mk-on
    %control-Country = if_abap_behv=>mk-on
  )
).

MODIFY ENTITIES OF ZBS_I_RAPPartner
  ENTITY Partner
  CREATE FROM lt_creation
  FAILED ls_failed
  MAPPED DATA(ls_mapped)
  REPORTED ls_reported.

TRY.
    out->write( ls_mapped-partner[ 1 ]-PartnerNumber ).
    COMMIT ENTITIES.

  CATCH cx_sy_itab_line_not_found.
    out->write( ls_failed-partner[ 1 ]-%cid ).
ENDTRY.

```

In addition to the actual data, important fields include the `%CID` field and the `%CONTROL` structure.

- %CID
    - must be filled with a unique value that identifies the data record
- %CONTROL
    - All fields that are to be taken into account by the BO when processing the call must be activated in this structure. If the fields are not activated, the corresponding data is not transferred to the BO
- MAPPED
    - In addition to FAILED and REPORTED, a further structure can be mapped here in which all keys created by the BO are listed. This is particularly important for BOs that create the key themselves.

`COMMIT ENTITIES` corresponds to the `COMMIT WORK` command of conventional ABAP, but is used specifically for RAP-BOs and triggers the database update.

## UPDATE

The UPDATE command can be used to update entities that have already been entered in the database (or those that are in the transaction buffer!).
The `TYPE TABLE FOR UPDATE` table type is used for this operation.
Following on from the CREATE example, an addition to the data would look like this:

```abap
DATA:
  lt_update    TYPE TABLE FOR UPDATE ZBS_I_RAPPartner.

lt_update = VALUE #(
  (
    PartnerNumber = '1000000007'
    PartnerName = 'Amazon Fake'
    City = 'Seattle'
    PaymentCurrency = 'USD'
    %control-PaymentCurrency = if_abap_behv=>mk-on
    %control-City = if_abap_behv=>mk-on
  )
).

MODIFY ENTITIES OF ZBS_I_RAPPartner
  ENTITY Partner
  UPDATE FROM lt_update
  FAILED ls_failed
  MAPPED ls_mapped
  REPORTED ls_reported.

IF ls_failed-partner IS INITIAL.
  out->write( 'Updated' ).
  COMMIT ENTITIES.
ENDIF.

```

In the example, the `PartnerName` is transferred, but the `%control` structure for this field is not activated. The value is therefore not transferred.

## ACTION

Der Aufruf einer ACTION wird  über den `MODIFY ENITITES`-Befehl angestoßen (über welchen auch CREATE, UPDATE und DELETE ausgeführt werden).
Anstelle von `CREATE` oder `UPDATE` wird der Befehl `EXECUTE` verwendet, um die im BO hinterlegten Aktionen zu triggern

The call of an ACTION is triggered via the `MODIFY ENITITES`command (which is also used to execute `CREATE`, `UPDATE` and `DELETE`).
Instead of `CREATE` or `UPDATE`, the `EXECUTE` command is used to trigger the actions stored in the BO

```abap
**********************************************************************
* EML - Create Sales Order
**********************************************************************
    MODIFY ENTITIES OF YCDS_SD_I_SalesOrderTP
    ENTITY SalesOrder
    EXECUTE CreateWithReference
    FROM VALUE #( ( %cid       = 'H001'
                      %param   = VALUE #( ReferenceSDDocument     = '100001'
                                          SalesDocumentType       = 'TA'
                                          %control = VALUE #( referencesddocument = cl_abap_behv=>flag_changed
                                                              salesdocumenttype   = cl_abap_behv=>flag_changed ) ) ) )
    MAPPED ycrcl_sd_cds_agcopy_salesquo=>mapped_sales_orders
    FAILED DATA(lo_create_failed)
    REPORTED DATA(lo_create_reported).

```

In the example, the parameters are passed directly.
The table type for the explicit declaration is `TYPE TABLE FOR ACTION IMPORT [ENTITY_NAME]~[ACTION_NAME]` .
The specifications in the `%cid` as a unique reference and the `%control` structure are important here, otherwise the transferred values with the same name are ignored.

## ACTION with UPDATE and DELETE

It is possible to combine several calls with each other, e.g. to first create a new object and then update it directly. Such a call then looks like this:

```abap
MODIFY ENTITIES OF YCDS_SD_I_SalesQuotationTP

  ENTITY SalesQuotation
  EXECUTE CreateWithReference FROM lt_action_sq_cwr

  ENTITY SalesQuotation
  UPDATE FROM lt_sales_quotations

  ENTITY SalesQuotationItem
  DELETE FROM lt_sales_quotation_items

  ENTITY SalesQuotationPartner
  UPDATE FROM lt_sales_quotation_partners

  MAPPED ls_mapped
  FAILED gs_failed
  REPORTED gs_reported.

```

### More detailed practical example

In the example, an offer is first created with a reference to a predecessor document and then data that is not required is updated/deleted.
To do this, the value that is written to the `%cid` field must be transferred to the `%cid-ref` field of the child entities.

```abap
   DATA:  lt_sales_quotations         TYPE TABLE FOR UPDATE YCDS_SD_i_SalesQuotationtp,
          ls_sales_quotation          LIKE LINE OF lt_sales_quotations,
          lt_sales_quotation_partners TYPE TABLE FOR UPDATE YCDS_SD_I_SalesQuotationPartTP,
          ls_sales_quotation_partner  LIKE LINE OF lt_sales_quotation_partners,
          lt_partner_delete           TYPE TABLE FOR DELETE YCDS_SD_I_SalesQuotationPartTP,
          ls_partner_delete           LIKE LINE OF lt_partner_delete,
          lt_sales_quotation_items_u  TYPE TABLE FOR UPDATE YCDS_SD_I_SalesQuotationItemTP,
          lt_sales_quotation_items    TYPE TABLE FOR DELETE YCDS_SD_I_SalesQuotationItemTP,
          ls_sales_quotation_item     LIKE LINE OF lt_sales_quotation_items,
          lt_action_sq_cwr            TYPE TABLE FOR ACTION IMPORT YCDS_SD_I_SalesQuotationTP~CreateWithReference,
          ls_action_sq_cwr            LIKE LINE OF lt_action_sq_cwr,
          ls_mapped                   TYPE RESPONSE FOR MAPPED ycds_sd_i_salesquotationtp.

    CLEAR lt_action_sq_cwr.
**********************************************************************
* Prepare table for CreateWithReference-Call
**********************************************************************
    lt_action_sq_cwr = VALUE #( ( %cid = gs_agcopy-uuid
                                  %param = VALUE #(
                                    ReferenceSDDocument = gs_agcopy-VbelnRef
                                    SalesDocumentType   = gs_agcopy-Auart
                                    %control = VALUE #(
                                      referencesddocument = cl_abap_behv=>flag_changed
                                      salesdocumenttype   = cl_abap_behv=>flag_changed
                                      )
                                    )
                                ) ).

    " Set the customer and validity end date according to the values in AGCOPY
    ls_sales_quotation = VALUE #(
         %cid_ref                     = gs_agcopy-uuid
         soldtoparty                  = gs_agcopy-kunag
         BindingPeriodValidityEndDate = gs_agcopy-Bnddt
         PricingDate                  = COND #( WHEN gs_agcopy-CB_Price = abap_true
                                                THEN gs_refdoc-PricingDate
                                                ELSE COND #( WHEN gs_agcopy-pricedt IS NOT INITIAL
                                                             THEN gs_agcopy-Pricedt
                                                             ELSE cl_abap_context_info=>get_system_date( ) ) )
         %control                     = VALUE #(
           soldtoparty                    = cl_abap_behv=>flag_changed
           BindingPeriodValidityEndDate   = cl_abap_behv=>flag_changed
           PricingDate                    = cl_abap_behv=>flag_changed
         )
    ).
    APPEND ls_sales_quotation TO lt_sales_quotations.

    " Delete alternative positions (if requested)
    " Table will remain empty, if this was not requested
    LOOP AT gt_alt_pos_for_del INTO DATA(ls_alt_pos).
      APPEND VALUE #(
          %cid_ref           = gs_agcopy-uuid
          SalesQuotationItem = ls_alt_pos-SalesDocumentItem
      ) TO lt_sales_quotation_items.
    ENDLOOP.

    READ TABLE gt_refdoctpartners REFERENCE INTO DATA(ls_partner)
        WITH KEY PartnerFunction = 'YM'.
    IF sy-subrc = 0.
        ls_sales_quotation_partner = VALUE #(
                %cid_ref         = gs_agcopy-uuid
                PartnerFunction  = 'YM'
                Personnel        = ls_partner->Personnel
                %control         = VALUE #(
                    Personnel = cl_abap_behv=>flag_changed ) ).
    APPEND ls_sales_quotation_partner TO lt_sales_quotation_partners.
    ENDIF.
**********************************************************************
* EML - Create Sales Quotation (& apply changes)
**********************************************************************
    MODIFY ENTITIES OF YCDS_SD_I_SalesQuotationTP

    ENTITY SalesQuotation
    EXECUTE CreateWithReference FROM lt_action_sq_cwr

    ENTITY SalesQuotation
    UPDATE FROM lt_sales_quotations

    ENTITY SalesQuotationItem
    DELETE FROM lt_sales_quotation_items

    ENTITY SalesQuotationPartner
    UPDATE FROM lt_sales_quotation_partners

    MAPPED ls_mapped
    FAILED gs_failed
    REPORTED gs_reported.

```
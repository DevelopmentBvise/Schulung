# EML-Operationen

## EML READ

Die READ-Funktion ist kein SELECT, deshalb ist zu Bedenken, dass sämtliche Keys des Business Objects (BO) bekannt sein müssen, damit ein READ möglich ist.

Es gibt unterschiedliche Varianten, einen Read aufzurufen. 

```ABAP
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

In diesem Beispiel werden alle Felder zurückgegeben, die das BO anbietet (`ALL FIELDS` ). Der Schlüssel wird der Entität über eine dafür definierte Tabelle überreicht (`TYPE TABLE FOR READ IMPORT`).

Eine kürzere Version: 

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

In diesem Beispiel werden lediglich `PartnerName`, `Street` und `City` ausgelesen.
Die Selektionsparameter werden direkt übergeben. 

Im Einzelnen aufgeschlüsselt besteht der Befehl aus den folgenden Bestandteilen: 

- READ ENTITIES
  - Gibt das Business Object an, aus dem gelesen werden soll. Ist in der Regel die ROOT-Node
- (IN LOCAL MODE)
  - nur möglich, wenn die Implementierung des EML-Aufrufs aus dem Verhaltenspool des BOs selbst heraus erfolgt. IN LOCAL MODE hebelt die Zugriffskontrolle, Berechtigungsprüfung und sämtliche Prechecks aus
- ENTITY
  - Gibt die Unter-Entität an (kann auch die Root-Node sein)
- FIELDS
  - Legt fest, welche Felder gelesen werden sollen. `ALL FIELDS` ist auch möglich
- WITH
  - Tabelle mit den Schlüsselfeldern, die gelesen werden sollen. Die Tabelle ist zu definieren als `TYPE TABLE FOR READ IMPORT`
- RESULT
  - Ergebnistabelle des Typs `TYPE TABLE FOR READ RESULT`
- FAILED
  - Enthält Schlüssel von fehlerhaften Leseversuchen
- REPORTED
  - Enthält Fehlermeldungen bei fehlerhaften Leseversuchen

### Weiteres Beispiel:

Nach dem Aufruf einer Action in der RAP-Applikationen, werden die Schlüssel in einer Tabelle der Art `Type TABLE FOR ACTION IMPORT` übergeben. Die Daten müssen nun nicht zwangsläufig umgemappt werden. Ein EML-Zugriff sieht dann wie folgt aus:

```abap
DATA: lt_keys TYPE TABLE FOR ACTION IMPORT ycds_sd_agcopy~propagateChanges.

READ ENTITIES OF YCDS_SD_AGCOPY_SalesQuotation IN LOCAL MODE
  ENTITY ycds_sd_agcopy
  ALL FIELDS
  WITH CORRESPONDING #( lt_keys )
  RESULT gt_agcopy.
```

Da in diesem Beispiel eine Implementierung innerhalb der Verhaltensimplementierung des gleichen BOs vorgenommen wurde, sollte der Zusatz `IN LOCAL MODE` genutzt werden. 

## CREATE

Ein Create dient dem schreiben von neuen Einträgen im BO. Der Tabellentyp für den Aufruf lautet `TYPE TABLE FOR CREATE`.

```ABAP
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

Wichtige Felder umfassen - neben den eigentlichen Daten - das Feld `%CID` sowie die `%CONTROL`-Struktur. 

- %CID
  - muss mit einem eindeutigem Wert gefüllt werden, der den Datensatz identifiziert
- %CONTROL
  - alle Felder, die vom BO bei der Verarbeitung des Aufrufs beachtet werden sollen, müssen in dieser Struktur aktiviert werden. Wenn die Felder nicht aktiviert werden, werden die entsprechenden Daten nicht an das BO übergeben
- MAPPED
  - neben FAILED und REPORTED kann hier eine weitere Struktur gemappt werden, in welche alle Schlüssel aufgelistet werden, die vom BO erstellt wurden. Das ist insbesondere wichtig bei BOs, die den Schlüssel selbst anlegen. 

`COMMIT ENTITIES` entspricht dem `COMMIT WORK`-Befehl des herkömmlichen ABAPs, wird aber spezifisch bei RAP-BOs verwendet und löst die Datenbankverbuchung aus. 

## UPDATE

Mit dem UPDATE-Befehl können bereits auf der Datenbank verbuchte Entitäten (oder auch solche, die im Transaktionspuffer befindlich sind!) aktualisiert werden. 
Für diese Operation wird der Tabellentyp `TYPE TABLE FOR UPDATE` genutzt. 
Anknüpfend an das CREATE-Beispiel, sähe eine Ergänzung der Daten wie folgt aus: 

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

Im Beispiel wird der `PartnerName` zwar übergeben, aber die `%control`-Struktur für dieses Feld nicht aktiviert. Somit wird der Wert auch nicht übernommen. 

## ACTION

Der Aufruf einer ACTION wird  über den `MODIFY ENITITES`-Befehl angestoßen (über welchen auch CREATE, UPDATE und DELETE ausgeführt werden).
Anstelle von `CREATE` oder `UPDATE` wird der Befehl `EXECUTE` verwendet, um die im BO hinterlegten Aktionen zu triggern

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

In dem Beispiel werden die Parameter direkt übergeben. 
Der Tabellentyp für die explizite Deklaration lautet `TYPE TABLE FOR ACTION IMPORT [ENTITY_NAME]~[ACTION_NAME]` .
Wichtig hierbei sind die Angaben in der `%cid` als eindeutige Referenz, sowie der `%control`-Struktur, sonst werden die gleichnamigen übergebenen Werte nicht beachtet.

## ACTION mit UPDATE und DELETE

Es ist möglich, mehrere Aufrufe miteinander zu kombinieren um z.B. zunächst ein neues Objekt zu erzeugen und im Anschluss direkt zu aktualisieren. Ein solcher Aufruf sieht dann so aus: 

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

### Ausführlicheres Praxis-Beispiel

Im Beispiel wird zunächst ein Angebot mit Referenz auf einen Vorgänger-Beleg erzeug und im Anschluss nicht benötigte Daten aktualisiert/gelöscht. 
Hierfür muss der Wert, der in das Feld `%cid` geschrieben wird, in das Feld `%cid-ref` der Kind-Entitäten übertragen werden. 

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

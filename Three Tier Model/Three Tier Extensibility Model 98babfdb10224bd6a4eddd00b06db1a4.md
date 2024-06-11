# Three Tier Extensibility Model

![https://learning.sap.com/service/media/topic/cb2ac825-f27d-45db-ade3-914b882c9ca0/S4D426_26_en-US_media/S4D426_26_en-US_images/3_Tier_Extensibility_001.png](https://learning.sap.com/service/media/topic/cb2ac825-f27d-45db-ade3-914b882c9ca0/S4D426_26_en-US_media/S4D426_26_en-US_images/3_Tier_Extensibility_001.png)

To provide a conceptual framework for customers to understand extensibility options for SAP S/4HANA Cloud Private Edition scenarios, SAP has established a three tier extensibility concept, as follows:

- Tier 1: Cloud Extensibility Model
- Tier 2: Cloud API Enablement
- Tier 3: Classic ABAP Extensions

**Tier 1**

Tier 1 is based off of the ABAP Cloud development model. To briefly summarize, the essential elements are as follows:

- Only approved ABAP Cloud object types (ABAP RESTful application programming model artifacts, for example) can be developed
- ABAP Cloud language is enforced via syntax check
- Usage of released APIs is enforced via a syntax check
- ABAP development tools for Eclipse is used to create all development objects

For SAP S/4HANA Cloud Public Edition customers, tier 1 is the only category available for usage. As mentioned previously, SAP S/4HANA Cloud Private Edition customers can also utilize tier 1 and are encouraged to do so as a first option. Development utilizing a tier 1 approach can make full use of key user extensions, on-stack developer extensions, and side-by-side extensions that run on SAP BTP, ABAP environment.

**Tier 2**

This tier is only available for SAP S/4HANA Cloud Private Edition customers. This tier covers use cases where a non-released API (a BAPI or SAPscript texts, for example) is nonetheless still needed for development. To enable usage of the non-released API, a customer created and released API that references it is done instead. In essence, the customer released API "wraps" around the non-released SAP API, thus enabling its usage. This satisfies the usage of released APIs that the ABAP Cloud development model requires. Due to the "wrapping" that forms the basis of tier 2, the artifacts used in tier 2 development are sometimes referred to as "custom wrappers". Multiple possibilities for wrapping exist and can be utilized by developers. An ABAP objects class, for example, can wrap around a non-released BAPI. Another possibility is a CDS view that wraps around an SAP table, or that wraps around a non-released CDS view.

To minimize potential software upgrade disruptions, tier 2 development can only be done using a tier 1 approach and therefore all tier 1 rules apply in tier 2 also (with the exception of the usage of the custom wrapper). Whereas in tier 1, usage of the ABAP Cloud development model is enforced via syntax and runtime checks, in tier 2, ABAP test cockpit checks are used instead to ensure ABAP Cloud compliance, and the usage of a non-released SAP API (in the custom wrapper) is handled via an ABAP test cockpit exemption. This approach allows customers to monitor and govern deviations from the ABAP Cloud development model.

Customers are encouraged to use the Customer Influence Channel to request missing APIs. Over time, SAP intends to move more and more APIs into the public released category so tier 2 usages should be analyzed after software upgrades. If a previously non-released API has been released, then the use case for tier 2 is no longer present and the custom wrapper should be replaced with direct usage of the now released API.

**Tier 3**

The final tier available for customers is reserved for classic extensibility based on custom ABAP code that is not implementable using the tier 1 or tier 2 approach. This tier carries the greatest risk of software upgrade disruptions and SAP suggests avoiding development in this layer in favor of tier 1 or tier 2 to minimize this risk.

In tier 3, there are no restrictions concerning the ABAP language or object types that can be used. Any classical extension techniques are possible. However, to minimize the risk of disruption, the customer should consider the following approaches:

- For DDIC extensions, use customer includes or extension includes.
- For CDS extensions, use CDS extends and CDS metadata extensions.
- Redefine OData services.
- The usage of a non-released BAdI is okay. There is a high probability that the BAdI will either be released or replaced with a successor BAdI.
- User exits and customer exits can be used when necessary. Analyze all usages after a software update and if a released successor BAdI is available, then refactor to use it instead.
- Explicit enhancement spots should only be used when no other approach suffices.
- Implicit enhancement spots and modifications should be completely avoided. If absolutely necessary, make sure to use the Modification Assistant.

The following list covers some examples of possible development activities and the tier that they would fall into. This list is not inclusive but will help to conceptualize the tier concept in a practical way.

Tier 1 use cases:

- Adding a custom field on a database table or a CDS view via released extension include
- Implementing a released SAP BAdI
- Creating a custom ABAP RESTful application programming model-based SAP Fiori app

Tier 2 use cases:

- Creating a wrapper class around any SAP objects that have not been released (for example, BAPI)
- Creating a wrapper CDS view for an SAP table or a CDS view that has not been released
- Creating an ABAP RESTful application programming model interface around non-released SAP objects

Tier 3 use cases (to be avoided if possible):

- Implementing a non-released BAdI
- Extending an SAP Fiori app based on the ABAP Programming Model For SAP Fiori (SEGW, BOPF, UI5)
- Extending an SAP application with legacy UI technology, for example, SAP GUI transaction
- Modifying any SAP object. If necessary, the Modification Assistant should be used

To reiterate after software upgrades all developments regardless of tier should be reevaluated. If any development is no longer necessary (for example, a previously non-released API has been released or a previous extension is now part of standard functionality), then it should be refactored as appropriate or retired.
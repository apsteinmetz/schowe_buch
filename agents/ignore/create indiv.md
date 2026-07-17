---
title: "Create master individual records"
---

### Create a master individual record for each person in the datasets, persons, spouses and children. 

The record should include:

-   unique individual ID

-   surname

-   given name

-   date of birth

-   place of birth

-   death date

-   place of death

-   ID of mother

-   ID of father

-   ID of spouse(s)

-   other facts specific only to that individual

Do not include information about the individual's children since that will be inferred by the references to the parents of individuals.
Do not create new records for witnesses or godparents.  They are facts associated with marriage and birth.
Do not include marriage facts.  Create a separate marriage file containing spousal name pairs, IDs and marriage facts.

## ID number creation scheme

Most individuals are already in the parsed persons file. Children and spouses often have their own record which the forward references reveal.  Use the parsed person record id as the individual ID in these cases.  Where there is no existing reference in spouses or children back to their individual record in parsed_person, create a new unique ID.  Name spouses IDs as the original record_id plus "S" plus the marriage number.  example: the spouse in the second marriage of the primary person in record_id 7 would be individual ID 7S2. Children with no existing individual record would receive an id derived from the record_id they are attached to plus "C" plus the child_num.  Do this only if the child or spouse does not already exist in parsed_persons.  Duplicate IDs are not allowed.

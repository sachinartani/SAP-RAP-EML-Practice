" Local class to hold transactional data
CLASS lcl_transaction_data DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      " Create operations
      materials_to_create TYPE TABLE FOR CREATE zsac_i_mat_header,
      plants_to_create    TYPE TABLE FOR CREATE zsac_i_mat_header\_materialplant,

      " Update operations
      materials_to_update TYPE TABLE FOR UPDATE zsac_i_mat_header,
      plants_to_update    TYPE TABLE FOR UPDATE zsac_i_mat_plant,

      " Delete operations
      materials_to_delete TYPE TABLE FOR DELETE zsac_i_mat_header,
      plants_to_delete    TYPE TABLE FOR DELETE zsac_i_mat_plant.

    CLASS-METHODS:
      clear_all.
ENDCLASS.

CLASS lcl_transaction_data IMPLEMENTATION.
  METHOD clear_all.
    CLEAR: materials_to_create,
           plants_to_create,
           materials_to_update,
           plants_to_update,
           materials_to_delete,
           plants_to_delete.
  ENDMETHOD.
ENDCLASS.

CLASS lhc_materialheader DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING REQUEST requested_authorizations FOR MaterialHeader RESULT result,
      create FOR MODIFY
        IMPORTING entities FOR CREATE MaterialHeader,
      update FOR MODIFY
        IMPORTING entities FOR UPDATE MaterialHeader,
      delete FOR MODIFY
        IMPORTING keys FOR DELETE MaterialHeader,
      read FOR READ
        IMPORTING keys FOR READ MaterialHeader RESULT result,
      lock FOR LOCK
        IMPORTING keys FOR LOCK MaterialHeader,
      rba_MaterialPlant FOR READ
        IMPORTING keys_rba FOR READ MaterialHeader\_MaterialPlant FULL result_requested RESULT result LINK association_links,
      cba_MaterialPlant FOR MODIFY
        IMPORTING entities_cba FOR CREATE MaterialHeader\_MaterialPlant.
ENDCLASS.

CLASS lhc_materialheader IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD create.
    " Just collect entities for later processing in save
    LOOP AT entities INTO DATA(entity).
      " Set creation info
      entity-%data-createdby = cl_abap_context_info=>get_user_technical_name( ).
      entity-%data-createdat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
      entity-%data-lastchangedby = cl_abap_context_info=>get_user_technical_name( ).
      entity-%data-lastchangedat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
      entity-%data-locallastchangedat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).

      APPEND entity TO lcl_transaction_data=>materials_to_create.
    ENDLOOP.
  ENDMETHOD.

  METHOD update.
    " Just collect entities for later processing in save
    LOOP AT entities INTO DATA(entity).
      " Set change info
      entity-%data-lastchangedby = cl_abap_context_info=>get_user_technical_name( ).
      entity-%data-lastchangedat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
      entity-%data-locallastchangedat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).

      APPEND entity TO lcl_transaction_data=>materials_to_update.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
    " Just collect keys for later processing in save
    APPEND LINES OF keys TO lcl_transaction_data=>materials_to_delete.
  ENDMETHOD.

  METHOD read.
    SELECT * FROM zsac_mat_header
      FOR ALL ENTRIES IN @keys
      WHERE material_number = @keys-material_number
      INTO TABLE @DATA(materials_db).

    result = CORRESPONDING #( materials_db MAPPING TO ENTITY ).
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD rba_MaterialPlant.
    DATA: materials_plant_db TYPE TABLE OF zsac_mat_plant.

    " Read associated plant data
    LOOP AT keys_rba INTO DATA(key_rba).
      SELECT * FROM zsac_mat_plant
        WHERE material_number = @key_rba-material_number
        APPENDING TABLE @materials_plant_db.
    ENDLOOP.

    result = CORRESPONDING #( materials_plant_db MAPPING TO ENTITY ).

    " Build association links
    LOOP AT result INTO DATA(plant).
      INSERT VALUE #( source-%tky = VALUE #( material_number = plant-material_number )
                      target-%tky = VALUE #( material_number = plant-material_number
                                             plant = plant-plant ) ) INTO TABLE association_links.
    ENDLOOP.
  ENDMETHOD.

  METHOD cba_MaterialPlant.
    " Just collect entities for later processing in save
    APPEND LINES OF entities_cba TO lcl_transaction_data=>plants_to_create.
  ENDMETHOD.
ENDCLASS.

CLASS lhc_materialplant DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      update FOR MODIFY
        IMPORTING entities FOR UPDATE MaterialPlant,
      delete FOR MODIFY
        IMPORTING keys FOR DELETE MaterialPlant,
      read FOR READ
        IMPORTING keys FOR READ MaterialPlant RESULT result,
      rba_MaterialHeader FOR READ
        IMPORTING keys_rba FOR READ MaterialPlant\_MaterialHeader FULL result_requested RESULT result LINK association_links.
ENDCLASS.

CLASS lhc_materialplant IMPLEMENTATION.
  METHOD update.
    " Just collect entities for later processing in save
    LOOP AT entities INTO DATA(entity).
      " Set change info
      entity-%data-lastchangedby = cl_abap_context_info=>get_user_technical_name( ).
      entity-%data-lastchangedat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
      entity-%data-locallastchangedat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).

      APPEND entity TO lcl_transaction_data=>plants_to_update.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
    " Just collect keys for later processing in save
    APPEND LINES OF keys TO lcl_transaction_data=>plants_to_delete.
  ENDMETHOD.

  METHOD read.
    SELECT * FROM zsac_mat_plant
      FOR ALL ENTRIES IN @keys
      WHERE material_number = @keys-material_number
      AND plant = @keys-plant
      INTO TABLE @DATA(plants_db).

    result = CORRESPONDING #( plants_db MAPPING TO ENTITY ).
  ENDMETHOD.

  METHOD rba_MaterialHeader.
    DATA: materials_db TYPE TABLE OF zsac_mat_header.

    " Get unique material numbers
    DATA material_numbers TYPE SORTED TABLE OF matnr WITH UNIQUE KEY table_line.
    material_numbers = VALUE #( FOR key IN keys_rba ( key-material_number ) ).

    " Read header data
    SELECT * FROM zsac_mat_header
      FOR ALL ENTRIES IN @material_numbers
      WHERE material_number = @material_numbers-table_line
      INTO TABLE @materials_db.

    result = CORRESPONDING #( materials_db MAPPING TO ENTITY ).

    " Build association links
    LOOP AT result INTO DATA(header).
      INSERT VALUE #( source-%tky = VALUE #( material_number = header-material_number plant = keys_rba[ 1 ]-plant )
                      target-%tky = VALUE #( material_number = header-material_number ) ) INTO TABLE association_links.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lhc_saver DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    METHODS save REDEFINITION.
    METHODS cleanup_finalize REDEFINITION.
ENDCLASS.

CLASS lhc_saver IMPLEMENTATION.

  METHOD save.
  DATA: materials_db TYPE TABLE OF zsac_mat_header,
        plants_db    TYPE TABLE OF zsac_mat_plant.

  " Process CREATE operations
  IF lines( lcl_transaction_data=>materials_to_create ) > 0.
    materials_db = CORRESPONDING #( lcl_transaction_data=>materials_to_create MAPPING FROM ENTITY ).
    INSERT zsac_mat_header FROM TABLE @materials_db.
  ENDIF.

  IF lines( lcl_transaction_data=>plants_to_create ) > 0.
    CLEAR: plants_db.
    LOOP AT lcl_transaction_data=>plants_to_create INTO DATA(entity_cba).
      LOOP AT entity_cba-%target INTO DATA(plant_create).
        plant_create-%data-material_number = entity_cba-material_number.
        plant_create-%data-createdby = cl_abap_context_info=>get_user_technical_name( ).
        plant_create-%data-createdat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
        plant_create-%data-lastchangedby = cl_abap_context_info=>get_user_technical_name( ).
        plant_create-%data-lastchangedat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
        plant_create-%data-locallastchangedat = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
        APPEND CORRESPONDING #( plant_create-%data ) TO plants_db.
      ENDLOOP.
    ENDLOOP.
    INSERT zsac_mat_plant FROM TABLE @plants_db.
  ENDIF.

  " Process UPDATE operations - MATERIALS
  IF lines( lcl_transaction_data=>materials_to_update ) > 0.
    CLEAR: materials_db.
    " Read current data
    SELECT * FROM zsac_mat_header
      FOR ALL ENTRIES IN @lcl_transaction_data=>materials_to_update
      WHERE material_number = @lcl_transaction_data=>materials_to_update-material_number
      INTO TABLE @materials_db.

    " Update changed fields
    LOOP AT lcl_transaction_data=>materials_to_update INTO DATA(entity_update).
      LOOP AT materials_db INTO DATA(material_db) WHERE material_number = entity_update-material_number.
        DATA(lv_tabix) = sy-tabix.

        " Update only changed fields
        IF entity_update-%control-material_type = if_abap_behv=>mk-on.
          material_db-material_type = entity_update-material_type.
        ENDIF.
        IF entity_update-%control-material_group = if_abap_behv=>mk-on.
          material_db-material_group = entity_update-material_group.
        ENDIF.
        IF entity_update-%control-base_unit = if_abap_behv=>mk-on.
          material_db-base_unit = entity_update-base_unit.
        ENDIF.
        IF entity_update-%control-material_desc = if_abap_behv=>mk-on.
          material_db-material_desc = entity_update-material_desc.
        ENDIF.
        IF entity_update-%control-industry_sector = if_abap_behv=>mk-on.
          material_db-industry_sector = entity_update-industry_sector.
        ENDIF.

        " Set change info from entity (already set in update method)
        material_db-lastchangedby = entity_update-lastchangedby.
        material_db-lastchangedat = entity_update-lastchangedat.
        material_db-locallastchangedat = entity_update-locallastchangedat.

        " Use index-based modification
        MODIFY materials_db FROM material_db INDEX lv_tabix.
      ENDLOOP.
    ENDLOOP.
    UPDATE zsac_mat_header FROM TABLE @materials_db.
  ENDIF.

  " Process UPDATE operations - PLANTS
  IF lines( lcl_transaction_data=>plants_to_update ) > 0.
    CLEAR: plants_db.
    " Read current data
    SELECT * FROM zsac_mat_plant
      FOR ALL ENTRIES IN @lcl_transaction_data=>plants_to_update
      WHERE material_number = @lcl_transaction_data=>plants_to_update-material_number
      AND plant = @lcl_transaction_data=>plants_to_update-plant
      INTO TABLE @plants_db.

    " Update changed fields
    LOOP AT lcl_transaction_data=>plants_to_update INTO DATA(plant_update).
      LOOP AT plants_db INTO DATA(plant_db) WHERE material_number = plant_update-material_number
                                            AND plant = plant_update-plant.
        DATA(lv_plant_tabix) = sy-tabix.

        " Update only changed fields
        IF plant_update-%control-procurement_type = if_abap_behv=>mk-on.
          plant_db-procurement_type = plant_update-procurement_type.
        ENDIF.
        IF plant_update-%control-mrp_controller = if_abap_behv=>mk-on.
          plant_db-mrp_controller = plant_update-mrp_controller.
        ENDIF.
        IF plant_update-%control-lot_size = if_abap_behv=>mk-on.
          plant_db-lot_size = plant_update-lot_size.
        ENDIF.

        " Set change info from entity (already set in update method)
        plant_db-lastchangedby = plant_update-lastchangedby.
        plant_db-lastchangedat = plant_update-lastchangedat.
        plant_db-locallastchangedat = plant_update-locallastchangedat.

        " Use index-based modification instead of key-based
        MODIFY plants_db FROM plant_db INDEX lv_plant_tabix.

        " Exit inner loop after finding the record
        EXIT.
      ENDLOOP.
    ENDLOOP.
    UPDATE zsac_mat_plant FROM TABLE @plants_db.
  ENDIF.

  " Process DELETE operations - OPTIMIZED VERSION
  " Delete individual plant records first
  IF lines( lcl_transaction_data=>plants_to_delete ) > 0.
    " Build range table for plant deletions
    DATA: lr_plant_keys TYPE TABLE OF zsac_mat_plant.
    lr_plant_keys = CORRESPONDING #( lcl_transaction_data=>plants_to_delete ).

    " Bulk delete plants
    DELETE zsac_mat_plant FROM TABLE @lr_plant_keys.
  ENDIF.

  " Delete material headers with cascade
  IF lines( lcl_transaction_data=>materials_to_delete ) > 0.
    " Build range table for material numbers
    DATA: lr_material_keys TYPE RANGE OF matnr,
          ls_material_range LIKE LINE OF lr_material_keys.

    LOOP AT lcl_transaction_data=>materials_to_delete INTO DATA(material_key).
      ls_material_range-sign = 'I'.
      ls_material_range-option = 'EQ'.
      ls_material_range-low = material_key-material_number.
      APPEND ls_material_range TO lr_material_keys.
    ENDLOOP.

    " First delete all related plant data
    DELETE FROM zsac_mat_plant
      WHERE material_number IN @lr_material_keys.

    " Then delete header data
    DELETE FROM zsac_mat_header
      WHERE material_number IN @lr_material_keys.
  ENDIF.
ENDMETHOD.


  METHOD cleanup_finalize.
    " Clear transaction data after commit/rollback
    lcl_transaction_data=>clear_all( ).
  ENDMETHOD.
ENDCLASS.

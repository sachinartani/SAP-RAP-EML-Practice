CLASS zcl_sac_generate_mat_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PRIVATE SECTION.
    TYPES:
      tt_mat_header TYPE TABLE OF zsac_mat_header,
      tt_mat_plant  TYPE TABLE OF zsac_mat_plant.

    DATA:
      lt_mat_headers TYPE tt_mat_header,
      lt_mat_plants  TYPE tt_mat_plant,
      lv_timestamp   TYPE timestampl.

    METHODS:
      clear_existing_data
        IMPORTING
          io_out TYPE REF TO if_oo_adt_classrun_out,

      generate_mat_headers
        IMPORTING
          iv_count TYPE i,

      generate_mat_plants
        IMPORTING
          iv_plants_per_mat TYPE i,

      insert_data
        IMPORTING
          io_out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.

CLASS zcl_sac_generate_mat_data IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " --- Configuration ---
    DATA(lv_material_count)   = 10. " Number of materials to create
    DATA(lv_plants_per_mat)  = 3.  " Number of plants for each material
    DATA(lv_clear_data)      = abap_true. " Set to abap_true to clear old data
    " --- End Configuration ---

    lv_timestamp = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).

    out->write( |Starting material test data generation...| ).
    out->write( |Material Count: { lv_material_count }, Plants per Material: { lv_plants_per_mat }| ).

    " 1. Clear existing data if requested
    IF lv_clear_data = abap_true.
      clear_existing_data( out ).
    ENDIF.

    " 2. Generate data in memory
    generate_mat_headers( lv_material_count ).
    generate_mat_plants( lv_plants_per_mat ).

    " 3. Insert or update data in database tables
    insert_data( out ).

    " --- Summary ---
    out->write( '==============================' ).
    out->write( '      GENERATION SUMMARY      ' ).
    out->write( '==============================' ).
    out->write( |Material Headers Processed: { lines( lt_mat_headers ) }| ).
    out->write( |Material Plants Processed:  { lines( lt_mat_plants ) }| ).
    out->write( '==============================' ).

  ENDMETHOD.

  METHOD clear_existing_data.
    io_out->write( '-> Clearing existing test data...' ).
    TRY.
        " Delete from child table first, then parent
        DELETE FROM zsac_mat_plant WHERE material_number LIKE 'MAT%'.
        DELETE FROM zsac_mat_header WHERE material_number LIKE 'MAT%'.
        COMMIT WORK.
        io_out->write( |  ✓ Existing test data cleared successfully.| ).
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        io_out->write( |  ✗ Error clearing data: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD generate_mat_headers.

    DATA ls_header TYPE zsac_mat_header.
    CLEAR lt_mat_headers.

    DO iv_count TIMES.
      CLEAR ls_header.

      ls_header-client          = sy-mandt.
      ls_header-material_number = |MAT{ sy-index WIDTH = 4 PAD = '0' }|.
      ls_header-material_type   = SWITCH #( sy-index MOD 3 WHEN 0 THEN 'ROH' WHEN 1 THEN 'FERT' ELSE 'HALB' ).
      ls_header-material_group  = |MG{ sy-index MOD 5 }|.
      ls_header-base_unit       = SWITCH #( ls_header-material_type WHEN 'ROH' THEN 'KG' ELSE 'EA' ).
      ls_header-material_desc   = |Test Material { sy-index } ({ ls_header-material_type })|.
      ls_header-industry_sector = SWITCH #( sy-index MOD 2 WHEN 0 THEN 'M' ELSE 'C' ).
      ls_header-createdby       = sy-uname.
      ls_header-createdat       = lv_timestamp.
      ls_header-lastchangedby   = sy-uname.
      ls_header-lastchangedat   = lv_timestamp.
      ls_header-locallastchangedat = lv_timestamp.

      APPEND ls_header TO lt_mat_headers.
    ENDDO.

  ENDMETHOD.

  METHOD generate_mat_plants.

    DATA ls_plant TYPE zsac_mat_plant.
    CLEAR lt_mat_plants.

    LOOP AT lt_mat_headers INTO DATA(ls_header).
      DO iv_plants_per_mat TIMES.
        CLEAR ls_plant.

        ls_plant-client          = sy-mandt.
        ls_plant-material_number = ls_header-material_number.
        ls_plant-plant           = |{ 1000 + ( ( sy-tabix - 1 ) * 10 ) + ( sy-index - 1 ) }|.
        ls_plant-procurement_type = SWITCH #( ls_header-material_type
                                      WHEN 'ROH' THEN 'F' " External procurement
                                      WHEN 'FERT' THEN 'E' " In-house production
                                      ELSE 'X' ).           " Both
        ls_plant-mrp_controller  = |{ sy-index MOD 5 }{ sy-index MOD 3 }{ sy-index MOD 2 }|.
        ls_plant-lot_size        = SWITCH #( ls_plant-procurement_type WHEN 'F' THEN 'EX' ELSE 'FX' ).
        ls_plant-createdby       = sy-uname.
        ls_plant-createdat       = lv_timestamp.
        ls_plant-lastchangedby   = sy-uname.
        ls_plant-lastchangedat   = lv_timestamp.
        ls_plant-locallastchangedat = lv_timestamp.

        APPEND ls_plant TO lt_mat_plants.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD insert_data.
    io_out->write( '-> Upserting new data into database using MODIFY...' ).
    TRY.
        IF lt_mat_headers IS NOT INITIAL.
          MODIFY zsac_mat_header FROM TABLE @lt_mat_headers.
          io_out->write( |  ✓ { lines( lt_mat_headers ) } Material Headers inserted/updated.| ).
        ENDIF.

        IF lt_mat_plants IS NOT INITIAL.
          MODIFY zsac_mat_plant FROM TABLE @lt_mat_plants.
          io_out->write( |  ✓ { lines( lt_mat_plants ) } Material Plants inserted/updated.| ).
        ENDIF.

        COMMIT WORK.
        io_out->write( |  ✓ All data committed successfully!| ).

      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        ROLLBACK WORK.
        io_out->write( |  ✗ Database modification error: { lx_error->get_text( ) }| ).
      CATCH cx_root INTO DATA(lx_general).
        ROLLBACK WORK.
        io_out->write( |  ✗ General error during modification: { lx_general->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.


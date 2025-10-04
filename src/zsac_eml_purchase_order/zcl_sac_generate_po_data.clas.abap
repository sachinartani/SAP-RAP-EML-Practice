CLASS zcl_sac_generate_po_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PRIVATE SECTION.
    TYPES: tt_po_headers  TYPE TABLE OF zsac_po_header,
           tt_po_items    TYPE TABLE OF zsac_po_item,
           tt_po_accounts TYPE TABLE OF zsac_po_account,
           tt_po_history  TYPE TABLE OF zsac_po_history.

    DATA: lt_po_headers  TYPE tt_po_headers,
          lt_po_items    TYPE tt_po_items,
          lt_po_accounts TYPE tt_po_accounts,
          lt_po_history  TYPE tt_po_history,
          lv_timestamp   TYPE timestampl.

    METHODS: generate_po_headers
      IMPORTING iv_count TYPE i,
      generate_po_items
        IMPORTING iv_items_per_po TYPE i,
      generate_po_accounts,
      generate_po_history,
      insert_data
        IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      clear_existing_data
        IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.

CLASS zcl_sac_generate_po_data IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " Configuration - modify these values as needed
    DATA(lv_po_count) = 5.        " Number of PO headers
    DATA(lv_items_per_po) = 3.    " Items per PO
    DATA(lv_clear_data) = abap_true. " Clear existing data

    lv_timestamp = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ). " ← Convert utclong to timestampl

    out->write( |Starting test data generation...| ).
    out->write( |PO Count: { lv_po_count }, Items per PO: { lv_items_per_po }| ).

    " Clear existing test data
    IF lv_clear_data = abap_true.
      clear_existing_data( out ).
    ENDIF.

    " Generate all data
    generate_po_headers( lv_po_count ).
    generate_po_items( lv_items_per_po ).
    generate_po_accounts( ).
    generate_po_history( ).

    " Insert all data
    insert_data( out ).

    " Summary
    out->write( |=== GENERATION COMPLETED ===| ).
    out->write( |PO Headers: { lines( lt_po_headers ) }| ).
    out->write( |PO Items: { lines( lt_po_items ) }| ).
    out->write( |Account Assignments: { lines( lt_po_accounts ) }| ).
    out->write( |History Records: { lines( lt_po_history ) }| ).

  ENDMETHOD.

  METHOD clear_existing_data.

    TRY.
        DELETE FROM zsac_po_history WHERE po_number LIKE '45%'.
        DELETE FROM zsac_po_account WHERE po_number LIKE '45%'.
        DELETE FROM zsac_po_item WHERE po_number LIKE '45%'.
        DELETE FROM zsac_po_header WHERE po_number LIKE '45%'.
        COMMIT WORK.
        io_out->write( |✓ Existing test data cleared successfully| ).

      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        io_out->write( |✗ Error clearing data: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

 METHOD generate_po_headers.

  DATA: lv_po_number_calc TYPE int8,
        lv_po_number      TYPE ebeln,
        lv_counter        TYPE i,
        ls_header         TYPE zsac_po_header.

  CLEAR lt_po_headers.

  DO iv_count TIMES.
    lv_counter = sy-index.         " 1 → 5 for your example

    " start at 4500000000 and add sy-index
    lv_po_number_calc = 4500000000 + lv_counter.

    " convert to string with leading zeros to length 10
    lv_po_number = |{ lv_po_number_calc WIDTH = 10 PAD = '0' }|.

    CLEAR ls_header.
    ls_header-client             = sy-mandt.
    ls_header-po_number          = lv_po_number.
    ls_header-company_code       = '1000'.
    ls_header-purch_organization = '1000'.
    ls_header-vendor_number      = '0000100001'.
    ls_header-document_date      = cl_abap_context_info=>get_system_date( ).
    ls_header-currency           = 'USD'.
    ls_header-total_value        = 0.
    ls_header-po_status          = 'N'.
    ls_header-createdby          = sy-uname.
    ls_header-createdat          = lv_timestamp.
    ls_header-lastchangedby      = sy-uname.
    ls_header-lastchangedat      = lv_timestamp.
    ls_header-locallastchangedat = lv_timestamp.

    APPEND ls_header TO lt_po_headers.
  ENDDO.

ENDMETHOD.


  METHOD generate_po_items.

    DATA: ls_item     TYPE zsac_po_item,
          lv_item_num TYPE i.

    CLEAR lt_po_items.

    LOOP AT lt_po_headers INTO DATA(ls_header).
      DO iv_items_per_po TIMES.
        lv_item_num = sy-index * 10.

        CLEAR ls_item.
        ls_item-client = sy-mandt.
        ls_item-po_number = ls_header-po_number.
        ls_item-item_number = |{ lv_item_num WIDTH = 4 PAD = '0' }|.   " Safe
        ls_item-material_number = |MAT{ sy-index WIDTH = 3 PAD = '0' }|.
        ls_item-plant = SWITCH #( sy-index MOD 3
                                WHEN 0 THEN '1000'
                                WHEN 1 THEN '2000'
                                ELSE '3000' ).
        ls_item-quantity = ( sy-index + 1 ) * 10.
        ls_item-unit_of_measure = 'EA'.
        ls_item-net_price = ( sy-index + 1 ) * '12.50'.
        ls_item-item_value = ls_item-quantity * ls_item-net_price.
        ls_item-item_text = |Test Material { sy-index }|.
        ls_item-delivery_date = ls_header-document_date + 14.
        ls_item-createdby = sy-uname.
        ls_item-createdat = lv_timestamp.
        ls_item-lastchangedby = sy-uname.
        ls_item-lastchangedat = lv_timestamp.
        ls_item-locallastchangedat = lv_timestamp.

        APPEND ls_item TO lt_po_items.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD generate_po_accounts.

    DATA ls_account TYPE zsac_po_account.

    CLEAR lt_po_accounts.

    LOOP AT lt_po_items INTO DATA(ls_item).
      DATA(lv_num) = 1 + ( sy-tabix MOD 2 ). " 1 or 2 accounts
      DO lv_num TIMES.
        CLEAR ls_account.
        ls_account-client = sy-mandt.
        ls_account-po_number = ls_item-po_number.
        ls_account-item_number = ls_item-item_number.
        ls_account-assignment_number = |{ sy-index WIDTH = 2 PAD = '0' }|.
        ls_account-account_category = SWITCH #( sy-index MOD 3
                                              WHEN 0 THEN 'K'
                                              WHEN 1 THEN 'S'
                                              ELSE 'A' ).
        ls_account-cost_center = |{ 1000 + ( sy-index MOD 4 ) * 1000 }|.
        ls_account-gl_account = |{ 400000 + ( sy-index MOD 3 ) * 10000 WIDTH = 10 PAD = '0' }|.
        ls_account-assignment_value = ls_item-item_value / lv_num.
        ls_account-percentage = ( ls_account-assignment_value / ls_item-item_value ) * 100.
        ls_account-createdby = sy-uname.
        ls_account-createdat = lv_timestamp.
        ls_account-lastchangedby = sy-uname.
        ls_account-lastchangedat = lv_timestamp.
        ls_account-locallastchangedat = lv_timestamp.
        APPEND ls_account TO lt_po_accounts.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.


  METHOD generate_po_history.

    DATA ls_hist TYPE zsac_po_history.

    CLEAR lt_po_history.

    LOOP AT lt_po_items INTO DATA(ls_item).
      DATA(lv_num) = 1 + ( sy-tabix MOD 3 ).
      DO lv_num TIMES.
        CLEAR ls_hist.
        ls_hist-client = sy-mandt.
        ls_hist-po_number = ls_item-po_number.
        ls_hist-item_number = ls_item-item_number.
        ls_hist-history_number = |{ sy-index WIDTH = 4 PAD = '0' }|.
        ls_hist-document_number = |{ 5000000 + sy-index + sy-tabix WIDTH = 10 PAD = '0' }|.
        ls_hist-movement_type = SWITCH #( sy-index MOD 4
                                        WHEN 0 THEN '101'
                                        WHEN 1 THEN '102'
                                        WHEN 2 THEN '543'
                                        ELSE '201' ).
        ls_hist-posting_date = ls_item-delivery_date + sy-index.
        ls_hist-quantity = ls_item-quantity / lv_num.
        ls_hist-amount = ls_item-item_value / lv_num.
        ls_hist-reference_doc = |REF{ sy-index }{ sy-tabix }|.
        ls_hist-createdby = sy-uname.
        ls_hist-createdat = lv_timestamp.
        ls_hist-lastchangedby = sy-uname.
        ls_hist-lastchangedat = lv_timestamp.
        ls_hist-locallastchangedat = lv_timestamp.
        APPEND ls_hist TO lt_po_history.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.


  METHOD insert_data.

    TRY.
        " Insert in correct order (parent -> child)

        IF lt_po_headers IS NOT INITIAL.
          INSERT zsac_po_header FROM TABLE @lt_po_headers.
          io_out->write( |✓ PO Headers inserted| ).
        ENDIF.

        IF lt_po_items IS NOT INITIAL.
          INSERT zsac_po_item FROM TABLE @lt_po_items.
          io_out->write( |✓ PO Items inserted| ).
        ENDIF.

        IF lt_po_accounts IS NOT INITIAL.
          INSERT zsac_po_account FROM TABLE @lt_po_accounts.
          io_out->write( |✓ Account Assignments inserted| ).
        ENDIF.

        IF lt_po_history IS NOT INITIAL.
          INSERT zsac_po_history FROM TABLE @lt_po_history.
          io_out->write( |✓ History Records inserted| ).
        ENDIF.

        COMMIT WORK.
        io_out->write( |✓ All data committed successfully!| ).

      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        ROLLBACK WORK.
        io_out->write( |✗ Database error: { lx_error->get_text( ) }| ).

      CATCH cx_root INTO DATA(lx_general).
        ROLLBACK WORK.
        io_out->write( |✗ General error: { lx_general->get_text( ) }| ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.


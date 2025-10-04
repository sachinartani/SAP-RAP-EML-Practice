CLASS lhc_PurchaseOrderHeader DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR PurchaseOrderHeader RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR PurchaseOrderHeader RESULT result.

    METHODS approve FOR MODIFY
      IMPORTING keys FOR ACTION PurchaseOrderHeader~approve RESULT result.

    METHODS reject FOR MODIFY
      IMPORTING keys FOR ACTION PurchaseOrderHeader~reject RESULT result.

    METHODS reset FOR MODIFY
      IMPORTING keys FOR ACTION PurchaseOrderHeader~reset RESULT result.

    METHODS setInitialStatus FOR DETERMINE ON SAVE
      IMPORTING keys FOR PurchaseOrderHeader~setInitialStatus.

    METHODS setPoNumber FOR DETERMINE ON SAVE
      IMPORTING keys FOR PurchaseOrderHeader~setPoNumber.

ENDCLASS.

CLASS lhc_PurchaseOrderHeader IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderHeader
          FIELDS ( po_status )
          WITH CORRESPONDING #( keys )
        RESULT DATA(purchase_orders).

    LOOP AT purchase_orders ASSIGNING FIELD-SYMBOL(<po>).
      APPEND VALUE #(
        %tky   = <po>-%tky
        %update = COND #( WHEN <po>-po_status = 'N'
                          THEN if_abap_behv=>fc-o-enabled
                          ELSE if_abap_behv=>fc-o-disabled )
        %delete = COND #( WHEN <po>-po_status = 'R'
                          THEN if_abap_behv=>fc-o-disabled
                          ELSE if_abap_behv=>fc-o-enabled )
        %action-approve = COND #( WHEN <po>-po_status = 'N'
                                 THEN if_abap_behv=>fc-o-enabled
                                 ELSE if_abap_behv=>fc-o-disabled )
        %action-reject = COND #( WHEN <po>-po_status = 'N'
                                THEN if_abap_behv=>fc-o-enabled
                                ELSE if_abap_behv=>fc-o-disabled )
        %action-reset = COND #( WHEN <po>-po_status CA 'AR'
                               THEN if_abap_behv=>fc-o-enabled
                               ELSE if_abap_behv=>fc-o-disabled )
      ) TO result.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD approve.

    MODIFY ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderHeader
          UPDATE FIELDS ( po_status )
          WITH VALUE #( FOR key IN keys
                          ( %tky = key-%tky
                            po_status = 'A' ) )
        FAILED failed
        REPORTED reported.

    READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
      ENTITY PurchaseOrderHeader
        FIELDS ( po_number po_status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(updated_pos).

    result = VALUE #( FOR updated IN updated_pos
                       ( %tky      = updated-%tky
                         %param    = updated ) ).

  ENDMETHOD.

  METHOD reject.

    MODIFY ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderHeader
          UPDATE FIELDS ( po_status )
          WITH VALUE #( FOR key IN keys
                          ( %tky = key-%tky
                            po_status = 'R' ) )
        FAILED failed
        REPORTED reported.

    READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
      ENTITY PurchaseOrderHeader
        FIELDS ( po_number po_status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(updated_pos).

    result = VALUE #( FOR updated IN updated_pos
                       ( %tky      = updated-%tky
                         %param    = updated ) ).

  ENDMETHOD.

  METHOD reset.

    MODIFY ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderHeader
          UPDATE FIELDS ( po_status )
          WITH VALUE #( FOR key IN keys
                          ( %tky = key-%tky
                            po_status = 'N' ) )
        FAILED failed
        REPORTED reported.

    READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
      ENTITY PurchaseOrderHeader
        FIELDS ( po_number po_status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(updated_pos).

    result = VALUE #( FOR updated IN updated_pos
                       ( %tky      = updated-%tky
                         %param    = updated ) ).

  ENDMETHOD.

  METHOD setInitialStatus.

    READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderHeader
          FIELDS ( po_status )
          WITH CORRESPONDING #( keys )
        RESULT DATA(purchase_orders).

    LOOP AT purchase_orders ASSIGNING FIELD-SYMBOL(<purchase_order>) WHERE po_status IS INITIAL.
      <purchase_order>-po_status = 'N'.
    ENDLOOP.

    MODIFY ENTITIES OF zsac_i_po_header IN LOCAL MODE
      ENTITY PurchaseOrderHeader
        UPDATE FIELDS ( po_status )
        WITH VALUE #( FOR po IN purchase_orders  (
                           %tky      = po-%tky
                           po_status  = po-po_status ) ).


  ENDMETHOD.

  METHOD setPoNumber.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_PurchaseOrderItem DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculateItemValue FOR DETERMINE ON SAVE
      IMPORTING keys FOR PurchaseOrderItem~calculateItemValue.

    METHODS setDeliveryDate FOR DETERMINE ON SAVE
      IMPORTING keys FOR PurchaseOrderItem~setDeliveryDate.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR PurchaseOrderItem RESULT result.

ENDCLASS.

CLASS lhc_PurchaseOrderItem IMPLEMENTATION.

  METHOD calculateItemValue.

    READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderItem
          FIELDS ( po_number item_number quantity net_price item_value )
          WITH CORRESPONDING #( keys )
        RESULT DATA(po_items).

    DATA lt_updates TYPE TABLE FOR UPDATE zsac_i_po_header\\PurchaseOrderItem.
    LOOP AT po_items INTO DATA(po_item).
      DATA(lv_new_value) = po_item-quantity * po_item-net_price.
      IF po_item-item_value <> lv_new_value.
        APPEND VALUE #( %tky = po_item-%tky
                        item_value = lv_new_value ) TO lt_updates.
      ENDIF.
    ENDLOOP.

    IF lt_updates IS NOT INITIAL.
      MODIFY ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderItem
          UPDATE FIELDS ( item_value )
          WITH lt_updates.
    ENDIF.

  ENDMETHOD.

  METHOD setDeliveryDate.

    READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderItem
          FIELDS ( delivery_date )
          WITH CORRESPONDING #( keys )
        RESULT DATA(po_items).

    LOOP AT po_items ASSIGNING FIELD-SYMBOL(<po_item>) WHERE delivery_date IS INITIAL.
      <po_item>-delivery_date = cl_abap_context_info=>get_system_date( ) + 14.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.

    IF requested_features-%field-currency = if_abap_behv=>mk-on.
      READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderItem FIELDS ( Currency ) WITH CORRESPONDING #( keys )
        RESULT DATA(lt_po_item).

      result = VALUE #( FOR ls_po_item IN lt_po_item
                      (
                         %key = ls_po_item-%key
                         %is_draft = if_abap_behv=>mk-on
                         %features-%field-currency = if_abap_behv=>fc-f-read_only
                      ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lhc_PurchaseOrderAccount DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculatePercentage FOR DETERMINE ON SAVE
      IMPORTING keys FOR PurchaseOrderAccount~calculatePercentage.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR PurchaseOrderAccount RESULT result.

ENDCLASS.

CLASS lhc_PurchaseOrderAccount IMPLEMENTATION.

  METHOD calculatePercentage.

    READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderAccount
          FIELDS ( po_number item_number assignment_value percentage )
          WITH CORRESPONDING #( keys )
        RESULT DATA(po_accounts).

    READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
      ENTITY PurchaseOrderAccount BY \_PurchaseOrderItem
        FIELDS ( item_value )
        WITH CORRESPONDING #( po_accounts )
      RESULT DATA(po_items).

    DATA lt_updates TYPE TABLE FOR UPDATE zsac_i_po_header\\PurchaseOrderAccount.
    LOOP AT po_accounts INTO DATA(po_account).
      READ TABLE po_items INTO DATA(po_item)
        WITH KEY %tky = po_account-%tky.

      IF sy-subrc = 0 AND po_item-item_value > 0.
        DATA(lv_new_percentage) = ( po_account-assignment_value / po_item-item_value ) * 100.
        IF po_account-percentage <> lv_new_percentage.
          APPEND VALUE #( %tky = po_account-%tky
                          percentage = lv_new_percentage ) TO lt_updates.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lt_updates IS NOT INITIAL.
      MODIFY ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderAccount
          UPDATE FIELDS ( percentage )
          WITH lt_updates.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance_features.

    IF requested_features-%field-currency = if_abap_behv=>mk-on.
      READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderAccount FIELDS ( Currency ) WITH CORRESPONDING #( keys )
        RESULT DATA(lt_po_account).

      result = VALUE #( FOR ls_po_account IN lt_po_account
          (
          %key = ls_po_account-%key
          %is_draft = if_abap_behv=>mk-on
           %features-%field-currency = if_abap_behv=>fc-f-read_only
          ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lhc_purchaseorderhistory DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR PurchaseOrderHistory RESULT result.

ENDCLASS.

CLASS lhc_purchaseorderhistory IMPLEMENTATION.

  METHOD get_instance_features.

    IF requested_features-%field-currency = if_abap_behv=>mk-on.
      READ ENTITIES OF zsac_i_po_header IN LOCAL MODE
        ENTITY PurchaseOrderHistory FIELDS ( Currency ) WITH CORRESPONDING #( keys )
        RESULT DATA(lt_po_history).

      result = VALUE #( FOR ls_po_history IN lt_po_history
          (
           %key = ls_po_history-%key
           %is_draft = if_abap_behv=>mk-on
           %features-%field-currency = if_abap_behv=>fc-f-read_only
          ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZSAC_I_PO_HEADER DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZSAC_I_PO_HEADER IMPLEMENTATION.

  METHOD adjust_numbers.

    TYPES tt_if_t100_message TYPE STANDARD TABLE OF REF TO if_t100_message WITH EMPTY KEY.

    DATA: lt_messages    TYPE tt_if_t100_message,
          lv_po_number   TYPE ebeln,
          lv_max_acctnum TYPE n LENGTH 2,
          lv_max_histnum TYPE n LENGTH 2.

* More than one item record with Account and History data are not allowed to create at a time
    IF mapped-purchaseorderitem IS NOT INITIAL AND
        ( mapped-purchaseorderaccount IS NOT INITIAL or mapped-purchaseorderhistory IS NOT INITIAL )
        AND lines( mapped-purchaseorderitem ) GT 1.

        APPEND VALUE #( %pid = mapped-purchaseorderitem[ 1 ]-%pid
                        %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                      text = 'Sorry, this operation cannot be performed.' )
                       ) TO reported-purchaseorderheader.
        EXIT.

    ENDIF.


* Get new Purchase Order key
    IF mapped-purchaseorderheader IS NOT INITIAL.

      TRY.
          cl_numberrange_runtime=>number_get(
            EXPORTING
              nr_range_nr       = '01'
              object            = 'ZSAC_NR_PO'
            IMPORTING
              number            = DATA(lv_key)
              returncode        = DATA(lv_return_code)
              returned_quantity = DATA(lv_returned_quantity)
          ).
        CATCH cx_number_ranges INTO DATA(lx_number_ranges).
          APPEND lx_number_ranges TO lt_messages.
          EXIT.
      ENDTRY.

      LOOP AT mapped-purchaseorderheader REFERENCE INTO DATA(lr_po_header) .
        lr_po_header->po_number = |{ lv_key ALPHA = OUT }|.
      ENDLOOP.

    ENDIF.

    IF mapped-purchaseorderitem IS NOT INITIAL.

      IF mapped-purchaseorderheader IS NOT INITIAL. " When PO header and item both are created together
        lv_po_number = mapped-purchaseorderheader[ 1 ]-po_number.
      ELSE.                                         " When PO item is being created for already exists PO header
        lv_po_number = mapped-purchaseorderitem[ 1 ]-%tmp-po_number.
      ENDIF.

      SELECT SINGLE MAX( item_number ) FROM zsac_po_item
          WHERE po_number = @lv_po_number GROUP BY po_number INTO @DATA(lv_max_itemno).

      LOOP AT mapped-purchaseorderitem REFERENCE INTO DATA(lr_po_item) .
        lv_max_itemno += 1.
        lr_po_item->po_number = lv_po_number.
        lr_po_item->item_number = lv_max_itemno.
*        lr_po_item->%tmp-item_number = lv_max_itemno.
      ENDLOOP.

    ENDIF.

    IF mapped-purchaseorderaccount IS NOT INITIAL.

      CLEAR lv_max_acctnum.
      LOOP AT mapped-purchaseorderaccount REFERENCE INTO DATA(lr_po_account).

        IF lr_po_account->%tmp-item_number IS NOT INITIAL. " PO header and Item already exists

          lr_po_account->po_number = lr_po_account->%tmp-po_number.
          lr_po_account->item_number = lr_po_account->%tmp-item_number.

        ELSEIF lr_po_account->%tmp-po_number IS NOT INITIAL AND lr_po_account->%tmp-item_number IS INITIAL. " PO Header exists, but item is new

          lr_po_account->po_number = lr_po_account->%tmp-po_number.
          lr_po_account->item_number = lr_po_item->item_number.

        ELSE. " PO Header, Item, Account all are fresh records

          lr_po_account->po_number = lv_po_number.
          lr_po_account->item_number = lv_max_itemno.

        ENDIF.

        IF lv_max_acctnum IS INITIAL.
          SELECT SINGLE MAX( assignment_number ) FROM zsac_po_account
            WHERE po_number = @lr_po_account->po_number
              AND item_number = @lr_po_account->item_number GROUP BY po_number INTO @lv_max_acctnum.
          lv_max_acctnum += 1.
        ELSE.
          lv_max_acctnum += 1.
        ENDIF.

        lr_po_account->assignment_number = lv_max_acctnum.

      ENDLOOP.

    ENDIF.

    IF mapped-purchaseorderhistory IS NOT INITIAL.

      CLEAR lv_max_acctnum.
      LOOP AT mapped-purchaseorderhistory REFERENCE INTO DATA(lr_po_history).

        IF lr_po_history->%tmp-item_number IS NOT INITIAL. " PO header and Item already exists

          lr_po_history->po_number = lr_po_history->%tmp-po_number.
          lr_po_history->item_number = lr_po_history->%tmp-item_number.

        ELSEIF lr_po_history->%tmp-po_number IS NOT INITIAL AND lr_po_history->%tmp-item_number IS INITIAL. " PO Header exists, but item is new

          lr_po_history->po_number = lr_po_history->%tmp-po_number.
          lr_po_history->item_number = lr_po_item->item_number.

        ELSE. " PO Header, Item, History all are fresh records

          lr_po_history->po_number = lv_po_number.
          lr_po_history->item_number = lv_max_itemno.

        ENDIF.

        IF lv_max_histnum IS INITIAL.
          SELECT SINGLE MAX( history_number ) FROM zsac_po_history
            WHERE po_number = @lr_po_history->po_number
              AND item_number = @lr_po_history->item_number GROUP BY po_number INTO @lv_max_histnum.
          lv_max_histnum += 1.
        ELSE.
          lv_max_histnum += 1.
        ENDIF.

        lr_po_history->history_number = lv_max_histnum.

      ENDLOOP.

    ENDIF.


  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

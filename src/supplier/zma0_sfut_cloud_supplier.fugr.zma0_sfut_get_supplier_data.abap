FUNCTION ZMA0_SFUT_GET_SUPPLIER_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_OFFSET) TYPE  INT4
*"     VALUE(IM_PAGE_SIZE) TYPE  INT4
*"     VALUE(IM_SUPPLIERS) TYPE  ZMA0_SFUT_LIFNR_IF_T
*"     VALUE(IM_SEARCH_EXPRESSION) TYPE  ZMA0_SFUT_SUPPLIER_SQL_QUERY
*"       OPTIONAL
*"     VALUE(IM_SQL_SORT) TYPE  STRING
*"  EXPORTING
*"     VALUE(EX_SCHEDULE_DATA) TYPE  ZMA0_SFUT_SUPPLIER_IF_T
*"     VALUE(EX_NUMBER_OF_RECORDS) TYPE  INT4
*"----------------------------------------------------------------------
  DATA lr_lifnr TYPE tt_range_lifnr.

  " Check if the RFC can be called by the cloud entity
*  AUTHORITY-CHECK OBJECT 'S_RFC'
*    ID 'RFC_TYPE' FIELD 'FUNC'
*    ID 'RFC_NAME' FIELD 'ZMA0_SFUT_GET_SUPPLIER_DATA'
*    ID 'ACTVT'    FIELD '16'.
*
*  CHECK sy-subrc IS INITIAL. " Leave if the user have no authorization

  PERFORM prepare_suppliers USING    im_suppliers
                            CHANGING lr_lifnr.

  " Get last report generated for that specific suppliers
  SELECT ebeln,
         ebelp,
         etenr,
         pkkey,
         pabnum,
         pabpos,
         matnr,
         maktx,
         werks,
         bsart,
         plant_name,
         eindt,
         berid,
         bertx,
         lifnr,
         vendor_name,
         ship_to,
         ship_from,
         ship_from_name,
         ekgrp,
         dispo,
         calc_ship_date,
         in_transit_qty,
         bal_qty_w_asn,
         ship_status,
         full_name,
         smtp_addr,
         datum
    FROM zma0_cds_sfut_supplier_data
    WHERE lifnr IN @lr_lifnr
      AND (im_search_expression)
    ORDER BY (im_sql_sort)
    INTO CORRESPONDING FIELDS OF TABLE @ex_schedule_data
    OFFSET @im_offset UP TO @im_page_size ROWS.

  CHECK sy-subrc IS INITIAL.

  " Get the total number of records for RAP process
  SELECT COUNT(*)
  FROM zma0_cds_sfut_supplier_data
  WHERE lifnr IN @lr_lifnr
    AND (im_search_expression)
  INTO @ex_number_of_records.

ENDFUNCTION.

FORM prepare_suppliers
  USING p_suppliers TYPE zma0_sfut_lifnr_if_t
  CHANGING ch_supplier_range TYPE tt_range_lifnr.

  " If no supplier has been informed, the call must been made from the
  " Control Tower using an API, thus, the RFC can return each and every
  " supplier registered on the ZMA0_SFUT_DATA table
  CHECK p_suppliers IS NOT INITIAL.

  DATA(lt_suppliers) = p_suppliers.

  SORT lt_suppliers.
  DELETE ADJACENT DUPLICATES FROM lt_suppliers
  COMPARING ALL FIELDS.

  LOOP AT lt_suppliers INTO DATA(lv_supplier).
    APPEND VALUE #(
      sign   = 'I'
      option = 'EQ'
      low    = lv_supplier
    ) TO ch_supplier_range.
  ENDLOOP.

ENDFORM.

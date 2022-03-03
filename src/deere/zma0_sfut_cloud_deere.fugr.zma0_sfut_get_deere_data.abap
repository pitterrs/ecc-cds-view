FUNCTION ZMA0_SFUT_GET_DEERE_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_OFFSET) TYPE  INT4
*"     VALUE(IM_PAGE_SIZE) TYPE  INT4
*"     VALUE(IM_MRP_CONTROLLERS) TYPE  ZMA0_SFUT_DISPO_IF_T
*"     VALUE(IM_SEARCH_EXPRESSION) TYPE  ZMA0_SFUT_DEERE_SQL_QUERY
*"       OPTIONAL
*"     VALUE(IM_SQL_SORT) TYPE  STRING
*"  EXPORTING
*"     VALUE(EX_SCHEDULE_DATA) TYPE  ZMA0_SFUT_DEERE_IF_T
*"     VALUE(EX_NUMBER_OF_RECORDS) TYPE  INT4
*"----------------------------------------------------------------------

  DATA lr_dispo TYPE tt_range_dispo.

  " Check if the RFC can be called by the cloud entity
*  AUTHORITY-CHECK OBJECT 'S_RFC'
*    ID 'RFC_TYPE' FIELD 'FUNC'
*    ID 'RFC_NAME' FIELD 'ZMA0_SFUT_GET_DEERE_DATA'
*    ID 'ACTVT'    FIELD '16'.
*
*  CHECK sy-subrc IS INITIAL. " Leave if the user have no authorization

  PERFORM prepare_mrp_controllers USING    im_mrp_controllers
                                  CHANGING lr_dispo.

  " Get last report generated for that specific supplier
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
         ekgrp,
         dispo,
         calc_ship_date,
         in_transit_qty,
         bal_qty_w_asn,
         ship_status,
         order_type,
         full_name,
         smtp_addr,
         user_id,
         datum
    FROM zma0_cds_sfut_deere_data
    WHERE dispo IN @lr_dispo
    AND (im_search_expression)
    ORDER BY (im_sql_sort)
    INTO CORRESPONDING FIELDS OF TABLE @ex_schedule_data
    OFFSET @im_offset UP TO @im_page_size ROWS.

  CHECK sy-subrc IS INITIAL.

  " Get the total number of records for RAP process
  SELECT COUNT(*)
  FROM zma0_cds_sfut_deere_data
  WHERE dispo IN @lr_dispo
  AND (im_search_expression)
  INTO @ex_number_of_records.

ENDFUNCTION.

FORM prepare_mrp_controllers
    USING    p_mrp_controllers TYPE zma0_sfut_dispo_if_t
    CHANGING ch_dispo_range    TYPE tt_range_dispo.

  " If no MRP controller has been informed, the call must been made
  " from the Control Tower using an API, thus, the RFC can return each
  " and every register on the ZMA0_SFUT_DATA table
  CHECK p_mrp_controllers IS NOT INITIAL.

  DATA(lt_mrp_controllers) = p_mrp_controllers.

  SORT lt_mrp_controllers.
  DELETE ADJACENT DUPLICATES FROM lt_mrp_controllers
  COMPARING ALL FIELDS.

  LOOP AT lt_mrp_controllers INTO DATA(lv_mrp_controllers).
    APPEND VALUE #(
        sign = 'I'
        option = 'EQ'
        low = lv_mrp_controllers
    ) TO ch_dispo_range.
  ENDLOOP.

ENDFORM.

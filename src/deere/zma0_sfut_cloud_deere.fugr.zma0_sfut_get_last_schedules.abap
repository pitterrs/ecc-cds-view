FUNCTION ZMA0_SFUT_GET_LAST_SCHEDULES.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     VALUE(CH_SCHEDULE_LINES) TYPE  ZMA0_SFUT_DEERE_LAST_LINES_IFT
*"----------------------------------------------------------------------

  " Check if the RFC can be called by the cloud entity
*  AUTHORITY-CHECK OBJECT 'S_RFC'
*    ID 'RFC_TYPE' FIELD 'FUNC'
*    ID 'RFC_NAME' FIELD 'ZMA0_SFUT_GET_LAST_SCHEDULES'
*    ID 'ACTVT'    FIELD '16'.
*
*  CHECK sy-subrc IS INITIAL. " Leave if the user have no authorization

  DATA lt_order_items TYPE SORTED TABLE OF zma0_sfut_deere_last_lines_if
  WITH NON-UNIQUE KEY primary_key COMPONENTS ebeln ebelp.

  " Prepare data for the selection operation
  lt_order_items = ch_schedule_lines.
  DELETE ADJACENT DUPLICATES FROM lt_order_items
  USING KEY primary_key.

  CHECK lt_order_items IS NOT INITIAL.

  " Get all schedule lines based on the filtered purchasing orders
  SELECT ebeln,
         ebelp,
         etenr
  FROM eket
  INTO TABLE @DATA(lt_schedule_lines)
  FOR ALL ENTRIES IN @lt_order_items
  WHERE ebeln = @lt_order_items-ebeln
  AND ebelp = @lt_order_items-ebelp.

  CHECK sy-subrc IS INITIAL.

  " Remove duplicates considering only the last schedule lines
  SORT lt_schedule_lines
  BY ebeln ebelp ASCENDING etenr DESCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_schedule_lines
  COMPARING ebeln ebelp.

  ch_schedule_lines = lt_schedule_lines.

ENDFUNCTION.

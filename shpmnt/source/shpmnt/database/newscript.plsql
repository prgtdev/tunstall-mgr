DECLARE
   attr_       VARCHAR2(32000);
   sql_msg_    VARCHAR2(32000);
   stmt_       VARCHAR2(32000);
   job_id_     NUMBER;
   
   new_qty_assigned_ NUMBER;
   old_qty_assigned_ NUMBER;
   part_no_ VARCHAR2(20);
   contract_ VARCHAR2(20);
   configuration_id_ VARCHAR2(20);
   location_no_      VARCHAR2(20);
   lot_batch_no_     VARCHAR2(20);
   serial_no_        VARCHAR2(20);
   eng_chg_level_    VARCHAR2(10);
   waiv_dev_rej_no_  VARCHAR2(10);
   activity_seq_     VARCHAR2(10);
   handling_unit_id_ VARCHAR2(10);
   
   qty_onhand_ NUMBER;
   qty_reserved_ NUMBER;
   
   FUNCTION Valid_Reservation___ RETURN BOOLEAN
   IS
      lot_tracking_code_ VARCHAR2(40);
      part_type_code_ VARCHAR2(20);

   BEGIN
      lot_tracking_code_ := Part_Catalog_API.Get_Lot_Tracking_Code_Db(part_no_);
      part_type_code_ := Inventory_Part_API.Get_Type_Code_Db(contract_,part_no_ );
      IF lot_tracking_code_ = 'LOT TRACKING' AND part_type_code_ = '1' THEN
         IF qty_onhand_ > qty_reserved_ THEN
            RETURN TRUE;
         END IF;
      END IF;
      RETURN FALSE;
   END Valid_Reservation___;
BEGIN
   stmt_ := '  
      DECLARE
         order_no_         VARCHAR2(20);
         line_no_          NUMBER;
         rel_no_           NUMBER;
         line_item_no_     NUMBER;
         contract_         VARCHAR2(20);
         part_no_          VARCHAR2(20);
         configuration_id_ VARCHAR2(20);
         location_no_      VARCHAR2(20);
         lot_batch_no_     VARCHAR2(20);
         serial_no_        VARCHAR2(20);
         eng_chg_level_    VARCHAR2(10);
         waiv_dev_rej_no_  VARCHAR2(10);
         activity_seq_     VARCHAR2(10);
         handling_unit_id_ VARCHAR2(10);
         qty_assigned_     NUMBER;
         shipment_id_      NUMBER;
         
         err_code_ VARCHAR2(20);
         err_msg_ VARCHAR2(200);
         valid_reservation_ BOOLEAN;
         
         PROCEDURE Validate_Reservation___(
            valid_reservation_ OUT BOOLEAN) 
         IS
            FUNCTION Get_Lot_Batch_No_Reserv_Count___ RETURN NUMBER
            IS
               lot_batch_no_count_ NUMBER;
            BEGIN
               SELECT COUNT(*)
                 INTO lot_batch_no_count_
                 FROM inv_part_stock_reservation_uiv
                WHERE lot_batch_no = lot_batch_no_
                  AND part_no = part_no_
                  AND contract = contract_
                  AND location_no = location_no_;
               RETURN lot_batch_no_count_;
            EXCEPTION
               WHEN OTHERS THEN
                  RETURN 0;            
            END Get_Lot_Batch_No_Reserv_Count___;
         BEGIN
            IF Get_Lot_Batch_No_Reserv_Count___ > 0 THEN
               valid_reservation_ := FALSE;
               Transaction_SYS.Log_Status_Info(''Additional reservation was not made as the lot is reserved to multiple orders'',''WARNING'');
            END IF;
         END Validate_Reservation___;
      BEGIN
         order_no_ := ''&NEW:ORDER_NO'';
         line_no_ := ''&NEW:LINE_NO'';
         rel_no_ := ''&NEW:REL_NO'';
         line_item_no_ := ''&NEW:LINE_ITEM_NO'';
         contract_ := ''&NEW:CONTRACT'';
         part_no_ := ''&NEW:PART_NO'';
         configuration_id_ := ''&NEW:CONFIGURATION_ID'';
         location_no_ := ''&NEW:LOCATION_NO'';
         lot_batch_no_ := ''&NEW:LOT_BATCH_NO'';
         serial_no_ := ''&NEW:SERIAL_NO'';
         eng_chg_level_ := ''&NEW:ENG_CHG_LEVEL'';
         waiv_dev_rej_no_ := ''&NEW:WAIV_DEV_REJ_NO'';
         activity_seq_ := ''&NEW:ACTIVITY_SEQ'';
         handling_unit_id_ := ''&NEW:HANDLING_UNIT_ID'';   
         qty_assigned_ := ''&NEW:QTY_ASSIGNED'';
         shipment_id_ := ''&NEW:SHIPMENT_ID'';
         
         Validate_Reservation___(valid_reservation_);
         IF valid_reservation_ THEN
            Transaction_SYS.Log_Progress_Info(''Reservation Started'');
            BARR_Customization_Util_API.Reserve_Remaining_Lot_Quantity__(order_no_,
                                                                         line_no_,
                                                                         rel_no_,
                                                                         line_item_no_,
                                                                         contract_,
                                                                         part_no_,
                                                                         configuration_id_,
                                                                         location_no_,
                                                                         lot_batch_no_,
                                                                         serial_no_,
                                                                         eng_chg_level_,
                                                                         waiv_dev_rej_no_,
                                                                         activity_seq_,
                                                                         handling_unit_id_,
                                                                         qty_assigned_,
                                                                         shipment_id_);
            Transaction_SYS.Log_Progress_Info(''Reservation Completed'');          
            Transaction_SYS.Log_Status_Info(''Reservation for remaining quantity in the lot is completed -  order_no- ''
                                     || order_no_|| '' line no- '' ||line_no_ ||'' rel no- '' ||rel_no_, ''INFO'');
         END IF;
      EXCEPTION
         WHEN OTHERS THEN
            err_code_ := SQLCODE;
            err_msg_ := SUBSTR(SQLERRM, 1, 200);
            Transaction_SYS.Log_Status_Info(''Unexpected error when reserving extra quantity -  (order_no- ''
                                  || order_no_|| '' line no- '' ||line_no_ ||'' rel no- '' ||rel_no_||'')'' || SQLCODE ||'' -ERROR- ''|| SQLERRM , ''WARNING'');
            RAISE;                      
      END;';
      
   new_qty_assigned_ := NVL('&NEW:QTY_ASSIGNED',0);
   old_qty_assigned_ := NVL('&OLD:QTY_ASSIGNED',0);
   part_no_ := '&NEW:PART_NO';
   contract_ := '&NEW:CONTRACT';
   configuration_id_ := '&NEW:CONFIGURATION_ID';
   location_no_ := '&NEW:LOCATION_NO';
   lot_batch_no_ := '&NEW:LOT_BATCH_NO';
   serial_no_ := '&NEW:SERIAL_NO';
   eng_chg_level_ := '&NEW:ENG_CHG_LEVEL';
   waiv_dev_rej_no_ := '&NEW:WAIV_DEV_REJ_NO';
   activity_seq_ := '&NEW:ACTIVITY_SEQ';
   handling_unit_id_ := '&NEW:HANDLING_UNIT_ID'; 
   
   qty_onhand_ := Inventory_Part_In_Stock_API.Get_Qty_Onhand(contract_,
                                                             part_no_,
                                                             configuration_id_,
                                                             location_no_,
                                                             lot_batch_no_,
                                                             serial_no_,
                                                             eng_chg_level_,
                                                             waiv_dev_rej_no_,
                                                             activity_seq_,
                                                             handling_unit_id_);
   qty_reserved_ :=  Inventory_Part_In_Stock_API.Get_Qty_Reserved(contract_,
                                                             part_no_,
                                                             configuration_id_,
                                                             location_no_,
                                                             lot_batch_no_,
                                                             serial_no_,
                                                             eng_chg_level_,
                                                             waiv_dev_rej_no_,
                                                             activity_seq_,
                                                             handling_unit_id_);
                                                             
   IF (new_qty_assigned_ > 0) AND (new_qty_assigned_ > old_qty_assigned_) THEN
      
      IF Valid_Reservation___ THEN
         sql_msg_ := Message_SYS.Construct('UPD');
         Message_SYS.Add_Attribute(sql_msg_, 'SQL', stmt_);

         Client_SYS.Clear_Attr(attr_);
         Client_SYS.Add_To_Attr('SQL_DATA_', sql_msg_, attr_);
         Client_SYS.Add_To_Attr('MSG_', '', attr_);
        
         Transaction_SYS.Deferred_Call(job_id_,
                                       'Fnd_Event_Action_API.Action_Executeonlinesql',
                                       'PARAMETER',
                                        attr_,
                                       'Make additional reservations for the Customer Order Line');
      END IF;
   END IF;       
--Error_SYS.Appl_General('Error','END');
/*EXCEPTION
   WHEN OTHERS THEN
      NULL;*/           
END; 
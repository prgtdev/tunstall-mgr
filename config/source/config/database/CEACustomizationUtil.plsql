-----------------------------------------------------------------------------
--
--  Logical unit: CEACustomizationUtil
--  Component:    CONFIG
--
--  IFS Developer Studio Template Version 3.0
--
--  Date    Sign    History
--  ------  ------  ---------------------------------------------------------
-----------------------------------------------------------------------------

layer Core;

-------------------- PUBLIC DECLARATIONS ------------------------------------


-------------------- PRIVATE DECLARATIONS -----------------------------------


-------------------- LU SPECIFIC IMPLEMENTATION METHODS ---------------------


-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------


-------------------- LU SPECIFIC PROTECTED METHODS --------------------------


-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------
 
 --C0392 EntPrageG (START)

PROCEDURE Bulk_Update_Invoice_Header_Notes__(
   company_  IN VARCHAR2,
   identity_ IN VARCHAR2) 
IS
   stmt_ VARCHAR2(32000);
BEGIN
   stmt_ := '
      DECLARE
         invoice_header_notes_rec_ invoice_header_notes_tab%ROWTYPE;
         
         CURSOR bulk_update_notes_(company_ VARCHAR2, identity_ VARCHAR2) IS
            SELECT *
              FROM invoice_header_notes_cfv
             WHERE company = company_
               AND identity = identity_
               AND cf$_bulk_update_db = ''TRUE''
               AND cf$_bulk_update_user = Fnd_Session_API.get_fnd_user;
                 
         CURSOR bulk_update_ledger_items_ (company_ VARCHAR2, identity_ VARCHAR2, invoice_id_ NUMBER) IS
            SELECT *
              FROM ledger_item_cu_det_qry_cfv
             WHERE company = company_
               AND identity = identity_        
               AND invoice_id != invoice_id_ -- if the invoice id similar no need to insert again
               AND cf$_bulk_update IS NOT NULL
               AND cf$_bulk_update_user = Fnd_Session_API.Get_Fnd_User;

         FUNCTION Get_Party_Type___ (
            objkey_ VARCHAR2) RETURN VARCHAR2
         IS
            party_type_ VARCHAR2(20);
         BEGIN
            SELECT party_Type
              INTO party_type_
              FROM invoice_header_notes
             WHERE objkey = objkey_;
            RETURN party_type_;
         EXCEPTION
            WHEN OTHERS THEN
               RETURN NULL;
         END Get_Party_Type___;

         FUNCTION Get_Invoice_Header_Note___ (
            company_    IN VARCHAR2,
            invoice_id_ IN VARCHAR2,
            note_id_    IN VARCHAR2) RETURN invoice_header_notes_tab%ROWTYPE
         IS
          rec_ invoice_header_notes_tab%ROWTYPE;
         BEGIN
            SELECT company,
                 invoice_id,
                 identity,
                 party_type,
                 note_date,
                 note_text,
                 credit_analyst,
                 user_id,
                 cust_contact_name,
                 cust_telephone_number,
                 follow_up_date,note_status_id
            INTO rec_.company,
                 rec_.invoice_id,
                 rec_.identity,
                 rec_.party_type,
                 rec_.note_date,
                 rec_.note_text,
                 rec_.credit_analyst,
                 rec_.user_id,
                 rec_.cust_contact_name,
                 rec_.cust_telephone_number,
                 rec_.follow_up_date,
                 rec_.note_status_id
            FROM invoice_header_notes_tab
            WHERE company = company_
             AND invoice_id = invoice_id_
             AND note_id = note_id_;
            RETURN rec_;
         EXCEPTION
            WHEN no_data_found THEN
               RETURN rec_;
         END Get_Invoice_Header_Note___;

         FUNCTION Get_Note_Id___(
            company_    IN VARCHAR2,
            invoice_id_ IN NUMBER) RETURN NUMBER
         IS
            note_id_ NUMBER;
         BEGIN
            SELECT NVL(MAX(note_id), 0) + 1
             INTO note_id_
             FROM invoice_header_notes
            WHERE company = company_
              AND invoice_id = invoice_id_;
            RETURN note_id_;
         EXCEPTION
            WHEN no_data_found THEN
               RETURN 1;
         END Get_Note_Id___;

         FUNCTION Get_Attr___(
            invoice_id_ IN NUMBER,
            rec_        IN invoice_header_notes_tab%ROWTYPE) RETURN VARCHAR2
         IS
            attr_ VARCHAR2(32000);
         BEGIN
            DBMS_OUTPUT.put_line(rec_.rowkey);
            Client_SYS.Clear_Attr(attr_);
            Client_SYS.Add_To_Attr(''COMPANY'', rec_.company, attr_);
            Client_SYS.Add_To_Attr(''IDENTITY'', rec_.identity, attr_);
            Client_SYS.Add_To_Attr(''PARTY_TYPE'', rec_.party_type, attr_);
            Client_SYS.Add_To_Attr(''INVOICE_ID'', invoice_id_, attr_);
            Client_SYS.Add_To_Attr(''NOTE_ID'', get_note_id___(rec_.company,invoice_id_), attr_);
            Client_SYS.Add_To_Attr(''NOTE_DATE'', rec_.note_date, attr_);
            Client_SYS.Add_To_Attr(''NOTE_TEXT'', rec_.note_text, attr_);
            Client_SYS.Add_To_Attr(''CREDIT_ANALYST'', rec_.credit_analyst, attr_);
            Client_SYS.Add_To_Attr(''USER_ID'', rec_.user_id, attr_);
            Client_SYS.Add_To_Attr(''CUST_CONTACT_NAME'', rec_.cust_contact_name, attr_);
            Client_SYS.Add_To_Attr(''CUST_TELEPHONE_NUMBER'', rec_.cust_telephone_number, attr_);
            Client_SYS.Add_To_Attr(''FOLLOW_UP_DATE'', rec_.follow_up_date, attr_);
            Client_SYS.Add_To_Attr(''NOTE_STATUS_ID'', rec_.note_status_id, attr_);
            Client_SYS.Add_To_Attr(''ROWVERSION'', SYSDATE, attr_);
         RETURN attr_;
         END Get_Attr___;

         FUNCTION Get_Attr___ (
            query_reason_ IN VARCHAR2) RETURN VARCHAR2
         IS
            attr_ VARCHAR2(32000);
         BEGIN
            Client_SYS.Clear_Attr(attr_);
            Client_SYS.Add_To_Attr(''CF$_QUERY_ID_DB'', query_reason_, attr_);
            RETURN attr_;
         END Get_Attr___;

         PROCEDURE Add_Header_Note___(
            invoice_id_   IN NUMBER,
            query_reason_ IN VARCHAR2,
            rec_          IN invoice_header_notes_tab%ROWTYPE)
         IS
            attr_  VARCHAR2(32000);
            attr_cf_  VARCHAR2(32000);
            info_  VARCHAR2(32000);
            objid_ VARCHAR2(50);
            objversion_ VARCHAR2(50);
         BEGIN
            attr_:= get_attr___(invoice_id_,rec_);
            attr_cf_ := get_attr___(query_reason_);

            Invoice_Header_Notes_API.new__(info_,objid_,objversion_,attr_,''DO'');
            IF query_reason_ IS NOT NULL THEN
              Invoice_Header_Notes_CFP.cf_new__ (info_,objid_,attr_cf_,'''',''DO'');
            END IF;
         END Add_Header_Note___;

         PROCEDURE Clear_Bulk_Update_Flag___
         IS
         BEGIN
            DELETE
             FROM ledger_item_ext_clt
            WHERE cf$_bulk_update_user = Fnd_Session_API.Get_Fnd_User;
                  
            DELETE
             FROM invoice_header_notes_ext_clt
            WHERE cf$_bulk_update_user = Fnd_Session_API.Get_Fnd_User;
         END Clear_Bulk_Update_Flag___;  
      BEGIN
         FOR rec_ IN bulk_update_notes_(:company_,:identity_) LOOP
          DBMS_OUTPUT.put_line(rec_.note_id);
          invoice_header_notes_rec_ := Get_Invoice_Header_Note___(rec_.company,rec_.invoice_id,rec_.note_id);
          FOR ledger_item_rec_ IN bulk_update_ledger_items_(rec_.company,rec_.identity,rec_.invoice_id) LOOP
             --clear_ledger_item_bulk_update_flag___(rec_.company,rec_.identity,rec_.invoice_id);
             DBMS_OUTPUT.put_line(rec_.invoice_id);      
             Add_Header_Note___(ledger_item_rec_.invoice_id,rec_.cf$_query_id_db,invoice_header_notes_rec_);
          END LOOP;
         END LOOP;
         Clear_Bulk_Update_Flag___;
      END;';
   IF (Database_SYS.View_Exist('INVOICE_HEADER_NOTES_CFV') AND
      Database_SYS.View_Exist('LEDGER_ITEM_CU_DET_QRY_CFV') AND
      Database_SYS.Method_Exist('INVOICE_HEADER_NOTES_API', 'New__')) THEN
      --@ApproveDynamicStatement('2021-02-16',EntPrageG);
      EXECUTE IMMEDIATE stmt_
         USING IN company_, IN identity_;
   ELSE
      Error_Sys.Appl_General(lu_name_, 'Custom Objects are not published!');
   END IF;
END Bulk_Update_Invoice_Header_Notes__;

FUNCTION Get_Update_Follow_Up_Date_Sql__(
   calender_id_    IN VARCHAR2,
   company_        IN VARCHAR2,
   invoice_id_     IN VARCHAR2,
   note_id_        IN NUMBER,
   note_status_id_ IN NUMBER) RETURN VARCHAR2 IS
   stmt_ VARCHAR2(32000);
BEGIN
   stmt_ := '
   DECLARE
      follow_up_date_ DATE;
      objkey_ VARCHAR2(50);
      company_ VARCHAR2(20);
      invoice_id_ VARCHAR2(20);
      note_id_ NUMBER;
      calender_id_       VARCHAR2(20);
      note_status_id_    NUMBER;         
      note_status_days_  VARCHAR2(20);
      query_reason_days_ VARCHAR2(20);

      FUNCTION Extract_Number___(text_ VARCHAR2) RETURN VARCHAR2
      IS
      BEGIN     
         RETURN regexp_replace(text_, ''[^[:digit:]]'', '''');
      END Extract_Number___;

      FUNCTION Get_Follow_Up_Time_Days___ (
         note_status_id_    IN NUMBER,
         note_status_days_  IN VARCHAR2) RETURN NUMBER
      IS
         working_days_ NUMBER;
      BEGIN
         working_days_ := NVL(Extract_Number___ (note_status_days_),0); 
         RETURN working_days_;
      END Get_Follow_Up_Time_Days___;

      FUNCTION Get_Follow_Up_Date___(
         calendar_id_       IN VARCHAR2,
         note_status_id_    IN NUMBER,      
         note_status_days_  IN VARCHAR2) RETURN DATE
      IS
         follow_up_days_ NUMBER;
         next_work_day_ date;
      BEGIN
         follow_up_days_ := Get_Follow_Up_Time_Days___(note_status_id_,note_status_days_);            
         SELECT work_day
           INTO next_work_day_
           FROM (SELECT work_day, rownum as counter
                   FROM work_time_counter
                  WHERE work_day > SYSDATE
               ORDER BY counter) t
           WHERE t.counter = follow_up_days_;
      RETURN next_work_day_;
      EXCEPTION
        WHEN OTHERS THEN
          RETURN SYSDATE;
      END Get_Follow_Up_Date___;

      PROCEDURE Update_Follow_Up_Date___ (
        objkey_         VARCHAR2,
        follow_up_date_ DATE)
      IS
         invoice_header_notes_rec_ Invoice_Header_Notes_API.Public_Rec;
         attr_ VARCHAR2(32000);
         info_ VARCHAR2(4000);
         objid_      VARCHAR2(200);
         objversion_ VARCHAR2(200);
      BEGIN
         invoice_header_notes_rec_ := Invoice_Header_Notes_API.Get_By_Rowkey(objkey_);
         Client_SYS.Clear_Attr(attr_);
         Client_SYS.Add_To_Attr(''FOLLOW_UP_DATE'', follow_up_date_ , attr_);
         IF invoice_header_notes_rec_.note_id IS NOT NULL AND follow_up_date_ <> SYSDATE THEN
            objversion_ := to_char(invoice_header_notes_rec_.rowversion,''YYYYMMDDHH24MISS'');
            Invoice_Header_Notes_API.Modify__(info_, invoice_header_notes_rec_."rowid",objversion_ , attr_, ''DO'');
         END IF;
      END ;

      FUNCTION Get_Note_Status_Follow_Up_Days___ (
         company_        IN VARCHAR2,
         note_status_id_ IN NUMBER) RETURN VARCHAR2
      IS
         follow_up_days_ VARCHAR2(20);
      BEGIN
         SELECT cf$_follow_up_time
           INTO follow_up_days_
           FROM credit_note_status_cfv
          WHERE company = company_
            AND note_status_id = note_status_id_;
         RETURN follow_up_days_;
      EXCEPTION
         WHEN no_data_found THEN
            RETURN NULL;    
      END Get_Note_Status_Follow_Up_Days___;
   BEGIN
      calender_id_ := ''' || calender_id_ || ''';
      note_status_id_ := ' || note_status_id_ || ';
      company_ := ''' || company_ || ''';
      invoice_id_ := ''' || invoice_id_ || ''';
      note_id_:= ' || note_id_ || ';

      objkey_:= Invoice_Header_Notes_API.Get_Objkey(company_, invoice_id_, note_id_);
      note_status_days_ := Get_Note_Status_Follow_Up_Days___ (company_ ,note_status_id_ );

      follow_up_date_ := Get_Follow_Up_Date___(calender_id_,note_status_id_,note_status_days_);  
      Update_Follow_Up_Date___(objkey_,follow_up_date_);         
   END;';
   RETURN stmt_;
END Get_Update_Follow_Up_Date_Sql__;

PROCEDURE Update_Follow_Up_Date__(
   calender_id_    IN VARCHAR2,
   company_        IN VARCHAR2,
   invoice_id_     IN VARCHAR2,
   note_id_        IN NUMBER,
   note_status_id_ IN NUMBER) 
IS
   stmt_ VARCHAR2(32000);
BEGIN
   stmt_ := Get_Update_Follow_Up_Date_Sql__(calender_id_,
                                            company_,
                                            invoice_id_,
                                            note_id_,
                                            note_status_id_);
   IF (Database_SYS.View_Exist('CREDIT_NOTE_STATUS_CFV') AND
          Database_SYS.View_Exist('QUERY_REASON_CLV') AND
             Database_SYS.View_Exist('INVOICE_HEADER_NOTES_CFV')) THEN
      --@ApproveDynamicStatement('2021-02-24',EntPrageG);
      EXECUTE IMMEDIATE stmt_;
   ELSE
      Error_Sys.Appl_General(lu_name_, 'Custom Objects are not published!');
   END IF;
END Update_Follow_Up_Date__;
  
--C0392 EntPrageG (END)
  
--C0446 EntChamuA (START)
PROCEDURE Create_Equipment_Object__(
   order_no_ IN VARCHAR2) 
IS
   temps_      NUMBER;
   seriliazed_ NUMBER;
   object_id_  VARCHAR2(3200);
   
   -- To get customer order lines 
   CURSOR get_co_line IS
      SELECT line_no,
             rel_no,
             line_item_no,
             catalog_no,
             contract,
             cust_warranty_id,
             real_ship_date,
             catalog_desc,
             cf$_object_id,
             buy_qty_due,
             objid
      FROM customer_order_line_cfv a
      WHERE order_no = order_no_
      AND state IN ('Delivered', 'Invoiced/Closed');
   
   CURSOR check_sales_part(catalog_ IN VARCHAR2) IS
      SELECT 1
      FROM sales_part_cfv
      WHERE catalog_no = catalog_
      AND contract = '2012'
      AND cf$_serviceable_db = 'TRUE';
   
   --Check serialised
   CURSOR check_serialized(catalog_ IN VARCHAR2) IS
      SELECT 1 
      FROM part_serial_catalog 
      WHERE part_no = catalog_;
BEGIN
   
   FOR rec_ IN get_co_line LOOP
      -- To check sales part 
      OPEN check_sales_part(rec_.catalog_no);
      FETCH check_sales_part
         INTO temps_;
      CLOSE check_sales_part;
      
      object_id_ := SUBSTR(rec_.CF$_OBJECT_ID, INSTR(rec_.CF$_OBJECT_ID, '^') + 1, length(rec_.CF$_OBJECT_ID));
      
      IF (object_id_ IS NOT NULL) THEN
         --Create objects only if site 2012 and serviceable
         IF (temps_ = 1) THEN
            OPEN check_serialized(rec_.catalog_no);
            FETCH check_serialized INTO seriliazed_;
            IF (check_serialized%FOUND) THEN
               
               Create_Serial_Object__(rec_.catalog_no,
                  order_no_,
                  rec_.line_no,
                  rec_.line_item_no,
                  rec_.rel_no,
                  rec_.cust_warranty_id,
                  object_id_,
                  rec_.objid,
                  rec_.real_ship_date);
               temps_ := 0;
            ELSE
               Create_Functional_Object_(rec_.catalog_no,
                  order_no_,
                  rec_.line_no,
                  rec_.rel_no,
                  TRUNC(rec_.real_ship_date),
                  rec_.cust_warranty_id,
                  object_id_,
                  rec_.catalog_desc,
                  rec_.contract,
                  rec_.buy_qty_due,
                  rec_.objid);
               temps_ := 0;
            END IF;
            CLOSE check_serialized;
         END IF;
      END IF;
   END LOOP;
END Create_Equipment_Object__;

PROCEDURE Create_Functional_Object_(
   catalog_no_     IN VARCHAR2,
   order_no_       IN VARCHAR2,
   line_no_        IN VARCHAR2,
   rel_no_         IN VARCHAR2,
   real_ship_date_ IN DATE,
   warranty_id_    IN VARCHAR2,
   object_id_      IN VARCHAR2,
   catalog_desc_   IN VARCHAR2,
   contract_       IN VARCHAR2,
   buy_qty_due_    IN NUMBER,
   col_objid_      IN VARCHAR2) 
IS
   status_                VARCHAR2(100);
   type_                  VARCHAR2(100);
   unit_                  VARCHAR2(100);
   values_                VARCHAR2(10);
   value_                 NUMBER;
   lot_no_                VARCHAR2(20);
   func_obj_              VARCHAR2(3200);
   info_                  VARCHAR2(3200);
   objid_                 VARCHAR2(3200);
   objversion_            VARCHAR2(3200);
   attr_                  VARCHAR2(3200);
   warr_objkey_           VARCHAR2(3200);
   contract_2_            VARCHAR2(3200) := '2013';
   current_sup_mch_code_  VARCHAR2(3200);
   attr_2_                VARCHAR2(3200);
   attr_cf_               VARCHAR2(3200);
   temps_3_               NUMBER;
   parent_part_no_        VARCHAR2(3200);
   parent_object_         VARCHAR2(3200);
   
   --to get lot number and qty 
   CURSOR get_lot_batch_dets(order_no_   IN VARCHAR2,
                             line_no_    IN VARCHAR2,
                             rel_no_     IN VARCHAR2,
                             catalog_no_ IN VARCHAR2,
                             contract_   IN VARCHAR2) IS
      SELECT lot_batch_no
        FROM lot_batch_history a
       WHERE order_ref1 = order_no_
         AND order_ref2 = line_no_
         AND order_ref3 = rel_no_
         AND contract = contract_
         AND part_no = catalog_no_
         AND order_type_db = 'CUST ORDER'
         AND transaction_desc LIKE 'Delivered on customer order %';
   
   --get warranty type 
   CURSOR get_warranty_type(warranty_id_ IN VARCHAR2) IS
      SELECT warranty_type_id
        FROM cust_warranty_type
       WHERE warranty_id = warranty_id_;
   
   -- get warranty details 
   CURSOR get_warranty_info(warranty_id_ IN VARCHAR2,
      type_id_     IN VARCHAR2) IS
         SELECT Warranty_Condition_API.Get_Time_Unit(condition_id), MAX_VALUE
           FROM cust_warranty_condition
          WHERE warranty_id = warranty_id_
            AND warranty_type_id = type_id_;
   
   --get objkey of warranty 
   CURSOR get_objkey(values_ IN VARCHAR2) IS
      SELECT OBJKEY FROM warranty_clv WHERE CF$_NAME = values_;
   
   --get objid and objversion 
   CURSOR get_equipment_object_dets(contract_ IN VARCHAR2, mch_code_ IN VARCHAR2) IS
      SELECT objid, objversion
        FROM equipment_object
       WHERE contract = contract_
         AND mch_code = mch_code_;
   
BEGIN
   IF (object_id_ IS NOT NULL) THEN
      
      status_ := Cust_Warranty_API.Get_Objstate(warranty_id_);
      OPEN get_lot_batch_dets(order_no_, line_no_, rel_no_, catalog_no_, contract_);
      FETCH get_lot_batch_dets INTO lot_no_;
      CLOSE get_lot_batch_dets;
      
      OPEN get_warranty_type(warranty_id_);
      FETCH get_warranty_type INTO type_;
      CLOSE get_warranty_type;
      
      OPEN get_warranty_info(warranty_id_, type_);
      FETCH get_warranty_info INTO unit_, value_;
      CLOSE get_warranty_info;
      
      IF (unit_ = 'Year') THEN
         value_ := value_ * 12;
      END IF;
      
      values_   := value_ || 'M';
      func_obj_ := catalog_no_ || '_' || order_no_ || '_' || rel_no_;
      
      Client_SYS.Add_To_Attr('MCH_CODE', func_obj_, attr_);
      Client_SYS.Add_To_Attr('MCH_NAME', catalog_desc_, attr_);
      Client_SYS.Add_To_Attr('CONTRACT', '2013', attr_);
      Client_SYS.Add_To_Attr('OBJ_LEVEL', '900_FUNCTIONAL_EQUIPMENT', attr_);
      Client_SYS.Add_To_Attr('PART_NO', catalog_no_, attr_);
      Client_SYS.Add_To_Attr('PRODUCTION_DATE', real_ship_date_, attr_);
      Client_SYS.Add_To_Attr('SUP_MCH_CODE', object_id_, attr_);
      Client_SYS.Add_To_Attr('SUP_CONTRACT', '2013', attr_);
      
      IF NOT Equipment_Functional_API.Exists(contract_2_, func_obj_) THEN            
         
         Equipment_Functional_API.New__(info_, objid_, objversion_, attr_, 'DO');
         
         Client_Sys.Clear_Attr(attr_);
         
         IF (values_ IS NOT NULL) THEN
            OPEN get_objkey(values_);
            FETCH get_objkey INTO warr_objkey_;
            CLOSE get_objkey;
         END IF;
         
         Client_SYS.Add_To_Attr('CF$_LOT_BATCH_NO', lot_no_, attr_);
         Client_SYS.Add_To_Attr('CF$_EQUIPMENT_QUANTITY', buy_qty_due_, attr_);
         Client_SYS.Add_To_Attr('CF$_SERVICE_WARRANTY', warr_objkey_, attr_);
         Equipment_Functional_CFP.Cf_Modify__(info_, objid_, attr_, ' ', 'DO');
         
         Client_SYS.Add_To_Attr('CF$_OBJECT_CREATED', func_obj_, attr_cf_);
         Customer_Order_Line_CFP.Cf_Modify__(info_, col_objid_, attr_cf_, ' ', 'DO');
      ELSE
         current_sup_mch_code_:= Equipment_Functional_API.Get_Sup_Mch_Code(contract_2_, func_obj_);
         
         IF object_id_ != current_sup_mch_code_ THEN
            OPEN get_equipment_object_dets (contract_2_, func_obj_);
            FETCH get_equipment_object_dets INTO objid_, objversion_;
            CLOSE get_equipment_object_dets;
            
            Client_SYS.Add_To_Attr('SUP_MCH_CODE', object_id_, attr_2_);
            Equipment_Functional_API.Modify__(info_, objid_, objversion_, attr_2_, 'DO');
         END IF;
      END IF;
   END IF;
END Create_Functional_Object_;

PROCEDURE Create_Serial_Object__(
   catalog_no_     IN VARCHAR2,
   order_no_       IN VARCHAR2,
   line_no_        IN VARCHAR2,
   line_item_no_   IN VARCHAR2,
   rel_no_         IN VARCHAR2,
   warranty_id_    IN VARCHAR2,
   object_id_      IN VARCHAR2,
   col_objid_      IN VARCHAR2,
   real_ship_date_ IN DATE) 
IS  
   max_value_    NUMBER;
   unit_         VARCHAR2(10);
   sstep1_       VARCHAR2(100);
   sstep2_       VARCHAR2(100);
   sstep3_       VARCHAR2(100);
   sstep4_       VARCHAR2(50);
   mch_code_     VARCHAR2(2000):= NULL;
   site_         VARCHAR2(2000);
   info_         VARCHAR2(3200);
   objid_        VARCHAR2(3200);
   objversion_   VARCHAR2(3200);
   attr_         VARCHAR2(3200);
   contract_     VARCHAR2(50) := '2013';
   warr_objkey_  VARCHAR2(3200);
   values_       VARCHAR2(10);
   attr_cf_      VARCHAR2(3200);
   sup_mch_code_ VARCHAR2(2000);
   location_     VARCHAR2(10) := NULL;
   obj_created_  VARCHAR2(2000);
   concat_obj_   VARCHAR2(3200):= NULL;
   code_         VARCHAR2(3200);
   
   --To get serial objects qty per CO line
   CURSOR get_serial_objects_col(catalog_   IN VARCHAR2,
                                 order_no_ IN VARCHAR2,
                                 line_no_  IN VARCHAR2,
                                 item_no_  IN NUMBER,
                                 rel_no_   IN NUMBER) IS
      SELECT transaction_date, serial_no, sequence_no
        FROM part_serial_history_tab
       WHERE part_no = catalog_
         AND order_no = order_no_
         AND line_no = line_no_
         AND line_item_no = item_no_
         AND release_no = rel_no_
         AND order_type = 'CUST ORDER'
         AND transaction_description LIKE '%Delivered on customer order%';
   
   --To get warranty details for serial object
   CURSOR get_serial_warranty_dets(catalog_   IN VARCHAR2,
                                   warr_id_   IN NUMBER,
                                   serial_no_ IN VARCHAR2) IS
      SELECT b.max_value,
             Warranty_Condition_API.Get_Time_Unit(a.condition_id) AS Time_Unit
        FROM serial_warranty_dates a, cust_warranty_temp_cond b
       WHERE a.warranty_type_id = b.template_id
         AND a.condition_id = b.condition_id
         AND a.warranty_id = warr_id_
         AND a.part_no = catalog_
         AND a.serial_no = serial_no_;
   
   --to modify production date of serial objects
   CURSOR modify_serial_obj_date(mch_code_  IN VARCHAR,
                                 serial_no_ IN VARCHAR) IS
         SELECT objid, objversion
           FROM equipment_serial a
          WHERE a.serial_no = serial_no_
            AND a.mch_code = mch_code_;
   
   --get objkey of warranty 
   CURSOR get_objkey(values_ IN VARCHAR2) IS
      SELECT objkey 
      FROM warranty_clv 
      WHERE cf$_name = values_;
   
   CURSOR get_serial_object(order_no_ IN VARCHAR2, catalog_no_ IN VARCHAR2) IS
      SELECT mch_code, objid, objversion
        FROM equipment_serial_cfv 
       WHERE cf$_customer_order_no = order_no_
         AND part_no = catalog_no_;

   CURSOR get_co_line_obj(order_no_ IN VARCHAR2, catalog_no_ IN VARCHAR2) IS
      SELECT cf$_object_created
        FROM customer_order_line_cfv
       WHERE order_no = order_no_
         AND catalog_no = catalog_no_;
   
BEGIN
   IF (object_id_ IS NOT NULL) THEN
      --create serial object
      FOR obj IN get_serial_objects_col(catalog_no_, order_no_, line_no_,  line_item_no_, rel_no_) LOOP
         
         --For each object get warranty details
         OPEN get_serial_warranty_dets(catalog_no_, warranty_id_, obj.serial_no);
         FETCH get_serial_warranty_dets INTO max_value_, unit_;
         CLOSE get_serial_warranty_dets;
         
         IF (unit_ = 'Year') THEN
            max_value_ := max_value_ * 12;
         END IF;
         
         values_ := max_value_ || 'M';
         
         IF (values_ IS NOT NULL) THEN
            OPEN get_objkey(values_);
            FETCH get_objkey INTO warr_objkey_;
            CLOSE get_objkey;
         END IF;
         
         sstep1_ := Equipment_Serial_API.Check_Serial_Exist(catalog_no_,obj.serial_no);
         
         sstep2_ := Maintenance_Site_Utility_API.Is_User_Allowed_Site(contract_);
         
         sstep3_ := Part_Serial_Catalog_API.Get_Objstate(catalog_no_,obj.serial_no);
         
         sstep4_ := Part_Serial_Catalog_API.Delivered_To_Internal_Customer(catalog_no_,obj.serial_no);

         OPEN get_co_line_obj(order_no_, catalog_no_);
         FETCH get_co_line_obj INTO obj_created_;
         IF(obj_created_ IS null)THEN
            --CREATE NEW SERIAL OBJECTS
            Equipment_Serial_API.Create_Maintenance_Aware(catalog_no_, obj.serial_no, contract_, NULL, 'FALSE');
            Equipment_Serial_API.Get_Obj_Info_By_Part(site_, mch_code_, catalog_no_, obj.serial_no);
            Equipment_Object_API.Move_From_Invent_To_Facility(contract_, object_id_, catalog_no_, obj.serial_no, mch_code_);
            
            OPEN modify_serial_obj_date(mch_code_, obj.serial_no);
            FETCH modify_serial_obj_date INTO objid_, objversion_;
            CLOSE modify_serial_obj_date;
            
            Client_Sys.Add_To_Attr('PRODUCTION_DATE', real_ship_date_, attr_);
            Equipment_Serial_API.Modify__(info_,objid_,objversion_, attr_,'DO');
            
            Client_Sys.Clear_Attr(attr_);
            
            Client_Sys.Add_To_Attr('CF$_SERVICE_WARRANTY', warr_objkey_, attr_);
            Client_Sys.Add_To_Attr('CF$_CUSTOMER_ORDER_NO', order_no_, attr_);
            Equipment_Serial_CFP.Cf_Modify__(info_, objid_, attr_, ' ', 'DO');
            
            concat_obj_ := mch_code_||';'||concat_obj_;
         ELSE
            FOR rec_modify_ IN get_serial_object(order_no_, catalog_no_)LOOP
               sup_mch_code_:= Equipment_Serial_API.Get_Sup_Mch_Code(contract_, rec_modify_.mch_code);
               IF(sup_mch_code_ != object_id_ )THEN  
                  Client_SYS.Clear_Attr(attr_);
                  Client_Sys.Add_To_Attr('SUP_MCH_CODE', object_id_, attr_);
                  Equipment_Serial_API.Modify__(info_, rec_modify_.objid, rec_modify_.objversion, attr_, 'DO');
               END IF;
            END LOOP;
         END IF;
         CLOSE get_co_line_obj;
      END LOOP;
      IF(obj_created_ IS null) THEN
         Client_Sys.Clear_Attr(attr_);
         Client_Sys.Add_To_Attr('CF$_OBJECT_CREATED', concat_obj_, attr_);
         Customer_Order_Line_CFP.Cf_Modify__(info_, col_objid_, attr_, ' ', 'DO');
      END IF;
   END IF; 
END Create_Serial_Object__;
--C0446 EntChamuA (END)
  
--C0317 EntChamuA (START)
FUNCTION Transf_Part_To_Inv_Part(
   target_key_ref_ IN VARCHAR2,
   service_name_   IN VARCHAR2) RETURN VARCHAR2 
IS
   part_no_             part_manu_part_no_tab.part_no%TYPE;
   source_key_ref_      VARCHAR2(32000);
   source_key_ref_list_ VARCHAR2(32000);

   CURSOR get_part_nos IS
      SELECT manufacturer_no, manu_part_no
        FROM part_manu_part_no_tab
       WHERE part_no = part_no_;
BEGIN
   part_no_ := Client_SYS.Get_Key_Reference_Value(target_key_ref_,
                                                'PART_NO');
   FOR rec_ IN get_part_nos LOOP
      source_key_ref_ := 'MANU_PART_NO=' || rec_.MANU_PART_NO ||
                         '^MANUFACTURER_NO=' || rec_.MANUFACTURER_NO ||
                         '^PART_NO=' || part_no_ || '^';
      Obj_Connect_Lu_Transform_API.Add_To_Source_Key_Ref_List(source_key_ref_list_,
                                                              source_key_ref_);
   END LOOP;
   RETURN source_key_ref_list_;
END Transf_Part_To_Inv_Part;
--C0317 EntChamuA (END)

FUNCTION Get_First_Time_Fix_Percentage__(
   start_date_      IN VARCHAR2,
   end_Date_        IN VARCHAR2,
   customer_id_     IN VARCHAR2 DEFAULT NULL,
   organization_id_ IN VARCHAR2 DEFAULT NULL,
   work_type_id_    IN VARCHAR2 DEFAULT NULL,
   work_task_       IN VARCHAR2 DEFAULT NULL,
   resource_id_     IN VARCHAR2 DEFAULT NULL) RETURN NUMBER 
IS
    task_closed_percentage_ NUMBER;
BEGIN
   SELECT count(*)
     INTO task_closed_percentage_
     FROM (SELECT wo_no
             FROM active_separate_uiv wo
            WHERE (SELECT count(*)
                     FROM jt_task_uiv wt, jt_execution_instance_uiv wa
                    WHERE wt.wo_no = wa.wo_no
                      AND wt.task_seq = wa.task_seq
                      AND wa.state = 'Completed'
                      AND wo.wo_no = wt.wo_no
                      AND wt.customer_no LIKE NVL(customer_id_, '%')
                      AND wt.organization_id LIKE NVL(organization_id_, '%')
                      AND wt.work_type_id LIKE NVL(work_type_id_, '%')
                       --AND wt.actual_start BETWEEN start_date_ AND  end_date_
                      AND TO_DATE(TO_CHAR(wt.actual_start, 'dd/mm/yyyy'),
                                'dd/mm/yyyy') BETWEEN
                        TO_DATE(start_date_, 'dd/mm/yyyy') AND
                        TO_DATE(end_date_, 'dd/mm/yyyy')
                      AND wt.task_seq LIKE NVL(work_task_, '%')) = 1
            UNION
            SELECT wo_no
              FROM active_separate_uiv wo
             WHERE (SELECT COUNT(*)
                      FROM jt_task_uiv wt
                     WHERE wt.wo_no = wo.wo_no
                       AND wt.customer_no LIKE NVL(customer_id_, '%')
                       AND wt.organization_id LIKE NVL(organization_id_, '%')
                       AND wt.work_type_id LIKE NVL(work_type_id_, '%')
                          --AND wt.actual_start BETWEEN start_date_ AND  end_date_
                       AND TO_DATE(TO_CHAR(wt.actual_start, 'dd/mm/yyyy'),
                                   'dd/mm/yyyy') BETWEEN
                           TO_DATE(start_date_, 'dd/mm/yyyy') AND
                           TO_DATE(end_date_, 'dd/mm/yyyy')
                       AND wt.task_seq LIKE NVL(work_task_, '%')
                       AND (SELECT COUNT(*)
                              FROM jt_execution_instance_uiv ta
                             WHERE ta.wo_no = wt.wo_no
                               AND state = 'Completed'
                               AND sent_date < SYSDATE
                               AND resource_id LIKE NVL(resource_id_, '%')) = 1) > 1);
   RETURN task_closed_percentage_;
END Get_First_Time_Fix_Percentage__;

--C0438 EntPrageG (START)

FUNCTION Get_Begin_Date__(
   year_    IN VARCHAR2,
   quarter_ IN VARCHAR2,
   month_   IN VARCHAR2,
   week_    IN VARCHAR2) RETURN DATE DETERMINISTIC
IS
   FUNCTION Get_Week_Start___(
      year_    IN VARCHAR2, 
      week_no_ IN VARCHAR2) RETURN DATE DETERMINISTIC
   IS
      week_start_date_ DATE;
   BEGIN
      SELECT MIN(dt)
      INTO week_start_date_
      FROM (SELECT dt, to_char(dt + 1, 'iw') week_no
         FROM (SELECT add_months(to_date('31/12/'||year_, 'dd/mm/yyyy'),-12) + ROWNUM dt
                 FROM all_objects
                WHERE ROWNUM < 366))
                WHERE week_no = TO_NUMBER(week_no_);
      RETURN week_start_date_;
   END Get_Week_Start___;   
BEGIN
   IF year_ IS NOT NULL OR year_ != '' THEN
      IF REGEXP_LIKE(year_, '^[[:digit:]]+$') THEN
         IF quarter_ IS NOT NULL OR quarter_ != '' THEN
            CASE quarter_
               WHEN '1' THEN
                  RETURN TO_DATE('01/01/2020', 'dd/mm/yyyy');
               WHEN '2' THEN
                  RETURN TO_DATE('01/04/2020', 'dd/mm/yyyy');
               WHEN '3' THEN
                  RETURN TO_DATE('01/07/2020', 'dd/mm/yyyy');
               WHEN '4' THEN
                  RETURN TO_DATE('01/10/2020', 'dd/mm/yyyy');
         ELSE
            Error_SYS.Appl_General('Error','INVALID_FORMAT: Invalid Quater Value!');
            END CASE;
         ELSE
            IF month_ IS NOT NULL OR month_ != '' THEN
               IF REGEXP_LIKE(month_, '^[[:digit:]]+$') THEN
                  RETURN TO_DATE('01/' || month_ || '/' || year_, 'dd/mm/yyyy');
               ELSE
                  Error_SYS.Appl_General('Error','INVALID_FORMAT: Invalid Month Value!');
              END IF;
            ELSE
               IF week_ IS NOT NULL OR week_ != '' THEN
                  IF REGEXP_LIKE(week_, '^[[:digit:]]+$') THEN
                     RETURN Get_Week_Start___(year_,week_);
                  ELSE
                     Error_SYS.Appl_General('Error','INVALID_FORMAT: Invalid Week Value!');
                  END IF;
               END IF;
            END IF;
         END IF;
         RETURN TO_DATE('01/01/' || year_, 'dd/mm/yyyy');
      ELSE
         Error_SYS.Appl_General('Error','INVALID_FORMAT: Invalid Year Value!');
      END IF;
   ELSE
      RETURN TO_DATE('01/01/1900', 'dd/mm/yyyy');
   END IF;
END Get_Begin_Date__;

FUNCTION Get_End_Date__(
   year_    IN VARCHAR2,
   quarter_ IN VARCHAR2,
   month_   IN VARCHAR2,
   week_    IN VARCHAR2) RETURN DATE DETERMINISTIC 
IS
   FUNCTION Get_Week_End___(
      year_    IN VARCHAR2, 
      week_no_ IN VARCHAR2) RETURN DATE DETERMINISTIC
   IS
      week_end_date_ DATE;
   BEGIN
      SELECT MAX(dt)
      INTO week_end_date_
      FROM (SELECT dt, to_char(dt + 1, 'iw') week_no
         FROM (SELECT add_months(to_date('31/12/'||year_, 'dd/mm/yyyy'),-12) + ROWNUM dt
            FROM all_objects
            WHERE ROWNUM < 366))
            WHERE week_no = TO_NUMBER(week_no_);
      RETURN week_end_date_;
   END Get_Week_End___;

   FUNCTION End_Of_Date_Time___(
      date_ DATE) RETURN DATE
   IS
   BEGIN
      RETURN date_ + 86399/86400;
   END End_Of_Date_Time___; 
BEGIN
   IF year_ IS NOT NULL OR year_ != '' THEN
      IF REGEXP_LIKE(year_, '^[[:digit:]]+$') THEN
         IF quarter_ IS NOT NULL OR quarter_ != '' THEN
            CASE quarter_
               WHEN '1' THEN
                  RETURN End_Of_Date_Time___(add_months(last_day(trunc(to_date('01/01/2020',
                                                                               'dd/mm/yyyy'),
                                                                               'Q')),
                                                                               2));
               WHEN '2' THEN
                  RETURN End_Of_Date_Time___(add_months(last_day(trunc(to_date('01/04/2020',
                                                                               'dd/mm/yyyy'),
                                                                               'Q')),
                                                                               2));
               WHEN '3' THEN
                  RETURN End_Of_Date_Time___(add_months(last_day(trunc(to_date('01/07/2020',
                                                                               'dd/mm/yyyy'),
                                                                               'Q')),
                                                                               2));
               WHEN '4' THEN
                  RETURN End_Of_Date_Time___(add_months(last_day(trunc(to_date('01/10/2020',
                                                                               'dd/mm/yyyy'),
                                                                               'Q')),
                                                                               2));
         ELSE
            Error_SYS.Appl_General('Error','INVALID_FORMAT: Invalid Quater Value!');
            END CASE;
         ELSE
            IF month_ IS NOT NULL OR month_ != '' THEN
               IF REGEXP_LIKE(month_, '^[[:digit:]]+$') THEN
                  RETURN End_Of_Date_Time___(LAST_DAY(TO_DATE('01/' || month_ || '/' || year_,'dd/mm/yyyy')));
               ELSE
                  Error_SYS.Appl_General('Error','INVALID_FORMAT: Invalid Month Value!');
               END IF;
            ELSE
               IF week_ IS NOT NULL OR month_ != '' THEN
                  IF REGEXP_LIKE(week_, '^[[:digit:]]+$') THEN
                     RETURN End_Of_Date_Time___(Get_Week_End___(year_,week_));
                  ELSE
                     Error_SYS.Appl_General('Error','INVALID_FORMAT: Invalid Week Value!');
                  END IF;
               END IF;
            END IF;
         END IF;
         RETURN End_Of_Date_Time___(TO_DATE('31/12/' || year_, 'dd/mm/yyyy'));
      ELSE
         Error_SYS.Appl_General('Error','INVALID_FORMAT: Invalid Year Value!');
      END IF;
   ELSE
      RETURN End_Of_Date_Time___(TO_DATE('31/12/9999', 'dd/mm/yyyy'));
   END IF;
END Get_End_Date__;

--C0438 EntPrageG (END)

--C0628 EntPrageG (START)

PROCEDURE Copy_Circuit_Reference__(
   part_no_  IN VARCHAR2,
   part_rev_ IN VARCHAR2) 
IS
   sub_part_no_  VARCHAR2(20);
   sub_part_rev_ VARCHAR2(20);
   structure_id_ VARCHAR2(20);
   pos_          VARCHAR2(20);

   objkey_        VARCHAR2(50);
   source_objkey_ VARCHAR2(50);

   PROCEDURE Remove_Copy_Circuit_Ref_Flag___(
      objkey_ IN VARCHAR2) 
   IS
   BEGIN
      UPDATE eng_part_structure_cft
         SET cf$_copy_circuit_reference = NULL
       WHERE rowkey = objkey_;
   END Remove_Copy_Circuit_Ref_Flag___;

   PROCEDURE Copy_Circuit_Reference___(
      objkey_      IN VARCHAR2,
      circuit_ref_ IN VARCHAR2) 
   IS
      info_    VARCHAR2(4000);
      objid_   VARCHAR2(200);
      attr_cf_ VARCHAR2(4000);
      rec_     Eng_Part_Structure_API.Public_Rec;
   BEGIN
      rec_   := Eng_Part_Structure_API.Get_By_Rowkey(objkey_);
      objid_ := rec_."rowid";

      Client_SYS.Clear_Attr(attr_cf_);
      Client_SYS.Add_To_Attr('CF$_CIRCUIT_REF', circuit_ref_, attr_cf_);

      Eng_Part_Structure_CFP.Cf_New__(info_, objid_, attr_cf_, '', 'DO');
   END Copy_Circuit_Reference___;
  
   FUNCTION Get_Source_Eng_Part_Struc_Objkey___(
      part_no_      IN VARCHAR2,
      sub_part_no_  IN VARCHAR2,
      sub_part_rev_ IN VARCHAR2,
      structure_id_ IN VARCHAR2,
      pos_          IN VARCHAR2) RETURN VARCHAR2 
   IS
      objkey_ VARCHAR2(50);
   BEGIN
      SELECT objkey
        INTO objkey_
        FROM eng_part_structure_ext_cfv
       WHERE part_no = part_no_
         AND sub_part_no = sub_part_no_
         AND sub_part_rev = sub_part_rev_
         AND structure_id = structure_id_
         AND pos = pos_
         AND cf$_copy_circuit_reference_db = 'TRUE';
   RETURN objkey_;
   EXCEPTION
      WHEN OTHERS THEN
        RETURN NULL;
   END Get_Source_Eng_Part_Struc_Objkey___;
BEGIN
   FOR rec_ IN (SELECT *
                  FROM eng_part_structure_ext
                 WHERE part_no = part_no_
                   AND part_rev = part_rev_) LOOP
      sub_part_no_  := rec_.sub_part_no;
      sub_part_rev_ := rec_.sub_part_rev;
      structure_id_ := rec_.structure_id;
      pos_          := rec_.pos;

      objkey_ := Eng_Part_Structure_API.Get_Objkey(part_no_,
                                                   part_rev_,
                                                   sub_part_no_,
                                                   sub_part_rev_,
                                                   structure_id_,
                                                   pos_);

      source_objkey_ := Get_Source_Eng_Part_Struc_objkey___(part_no_,
                                                            sub_part_no_,
                                                            sub_part_rev_,
                                                            structure_id_,
                                                            pos_);

      IF source_objkey_ IS NOT NULL THEN
         IF Eng_Part_Structure_CFP.Get_Cf$_Copy_Circuit_Reference(source_objkey_) = 'TRUE' THEN
            Copy_Circuit_Reference___(objkey_,
                                      Eng_Part_Structure_CFP.Get_Cf$_Circuit_Ref(source_objkey_));
            Remove_Copy_Circuit_Ref_Flag___(source_objkey_);
          END IF;
      END IF;
   END LOOP;
END Copy_Circuit_Reference__;

--C0628 EntPrageG (END)

PROCEDURE Close_Non_Finance_User_Group 
IS
   non_finance_user_group_ VARCHAR2(30) := 'NF';

   CURSOR check_valid_until_date IS
   SELECT *
     FROM ACC_PERIOD_LEDGER t
    WHERE date_until < SYSDATE
      AND period_status_db = 'O';
BEGIN
   FOR rec_ IN check_valid_until_date LOOP
      IF (User_Group_Period_API.Check_Exist(rec_.company,
                                            rec_.accounting_year,
                                            rec_.accounting_period,
                                            non_finance_user_group_,
                                            rec_.ledger_id) = TRUE) THEN
        User_Group_Period_API.Close_Period(rec_.company,
                                           non_finance_user_group_,
                                           rec_.accounting_year,
                                           rec_.accounting_period,
                                           rec_.ledger_id);

      END IF;
   END LOOP;
END Close_Non_Finance_User_Group;

--C0613 EntChathI (START)
FUNCTION Check_Cause_Discount(
   wo_no_   IN NUMBER,
   type_    IN VARCHAR2) RETURN VARCHAR2 
IS

   temp_                  VARCHAR2(5) := 'FALSE';
      cause_discount_        NUMBER;
      material_discount_     NUMBER;
      personnel_discount_    NUMBER;
      pm_material_discount_  NUMBER;
      pm_personnel_discount_ NUMBER;
      cause_objkey_          VARCHAR2(2000);
   
      CURSOR get_wo_tasks is
         SELECT task_seq,
                work_type_id,
                t.error_cause,
                t.cf$_service_package
           FROM JT_TASK_UIV_CFV t
          WHERE wo_no = wo_no_
            AND Active_Separate_API.Get_State(wo_no) = 'Reported'
            AND NVL((SELECT t.cf$_ready_to_invoice
                      FROM ACTIVE_SEPARATE_CFT t
                     WHERE rowkey IN (SELECT objkey
                                        FROM ACTIVE_SEPARATE_UIV
                                       WHERE wo_no = wo_no_)), 'FALSE') = 'FALSE';
   
      CURSOR get_cause_details(in_wo_no_ NUMBER, task_seq_ NUMBER) IS
         SELECT cf$_material_discount,
                cf$_personnel_discount,
                t.cf$_pm_material_discount,
                t.cf$_pm_personnel_discount
           FROM service_package_detail_clt t
          WHERE cf$_cause IN
                (SELECT objkey
                   FROM maintenance_cause_code
                  WHERE err_cause IN
                        (SELECT error_cause
                           FROM jt_task_uiv_cfv
                          WHERE wo_no = in_wo_no_
                            AND task_seq = task_seq_))
            AND cf$_service_package in
                (SELECT objkey
                   FROM service_package_clv
                  WHERE CF$_NAME IN
                        (SELECT cf$_service_package
                           FROM jt_task_uiv_cfv
                          WHERE wo_no = in_wo_no_
                            AND task_seq = task_seq_));
   
   BEGIN
   
      FOR tasks_ IN get_wo_tasks LOOP
         IF (get_wo_tasks%notfound) THEN
            temp_ := 'FALSE';
            EXIT;
         ELSE
            OPEN get_cause_details(wo_no_, tasks_.task_seq);
            FETCH get_cause_details
               INTO material_discount_,
                    personnel_discount_,
                    pm_material_discount_,
                    pm_personnel_discount_;
            CLOSE get_cause_details;
         
            IF (type_ = 'Chargeable') THEN
               IF (tasks_.cf$_service_package IS NULL OR  tasks_.error_cause IS NULL) THEN
                  temp_ := 'TRUE';
                  EXIT;
               END IF;
               IF (Work_Type_API.Get_Work_Type_Category(tasks_.work_type_id) = 'Preventive') THEN
                  IF (pm_material_discount_ = 0 AND pm_personnel_discount_ = 0) THEN
                     temp_ := 'TRUE';
                     EXIT;
                  END IF;
               ELSE
                  IF (material_discount_ = 0 AND personnel_discount_ = 0) THEN
                     temp_ := 'TRUE';
                     EXIT;
                  END IF;
               
               END IF;
            
            ELSIF (type_ = 'Non-Chargeable') THEN
               IF (Work_Type_API.Get_Work_Type_Category(tasks_.work_type_id) = 'Preventive') THEN
                  IF (pm_material_discount_ = 1 AND pm_personnel_discount_ = 1) THEN
                     temp_ := 'TRUE';
                     EXIT;
                  END IF;
               ELSE
                  IF (material_discount_ = 1 AND personnel_discount_ = 1) THEN
                     temp_ := 'TRUE';
                     EXIT;
                  END IF;               
               END IF;
            END IF;
         END IF;
      
      END LOOP;
   
      RETURN temp_;
   END Check_Cause_Discount;

 FUNCTION Check_Inv_Preview(wo_no_ IN NUMBER) RETURN VARCHAR2 IS
      temp_               VARCHAR2(5) := 'FALSE';
      task_               NUMBER;
      sales_lines_        NUMBER;
      journal_            NUMBER;
      service_invoice_id_ NUMBER;
   
      CURSOR get_task(wo_no_ NUMBER) IS
         SELECT 1
           FROM ACTIVE_SEPARATE_UIV t
          WHERE t.wo_no = wo_no_
            AND t.STATE = 'Reported'
            AND (SELECT t.cf$_ready_to_invoice
                   FROM ACTIVE_SEPARATE_CFT t
                  WHERE rowkey IN (SELECT objkey
                                     FROM ACTIVE_SEPARATE_UIV
                                    WHERE wo_no = wo_no_)) = 'TRUE';
   
      CURSOR get_sales_lines(wo_no_ NUMBER) IS
         SELECT 1
           FROM JT_TASK_SALES_LINE_UIV
          WHERE wo_no = wo_no_
            AND state = 'Invoiceable';
   
      CURSOR get_journal_info(wo_no_ NUMBER) IS
         SELECT 1
           FROM (SELECT DT_CRE
                   FROM WORK_ORDER_JOURNAL
                  WHERE wo_no = wo_no_
                    AND SOURCE_DB = 'WORK_ORDER'
                    AND EVENT_TYPE_DB = 'STATUS_CHANGE'
                    AND NEW_VALUE = 'REPORTED'
                  ORDER BY DT_CRE DESC)
          WHERE ROWNUM = 1
            AND TRUNC(DT_CRE) <= TRUNC(SYSDATE) - 3;
   
      CURSOR get_inv_prev(wo_no_ NUMBER) IS
         SELECT service_invoice_id
         FROM SERVICE_INVOICE_UIV
 where (service_invoice_id IN
       (SELECT service_invoice_id
           FROM JT_TASK_SALES_LINE_UIV
          WHERE ((task_seq IN (SELECT task_seq
                                 FROM IFSAPP.JT_TASK_UIV
                                WHERE wo_no = wo_no_)) OR
                ((task_seq IS NULL AND wo_no = wo_no_) AND
                (COST_TYPE_DB = 'FIXED_QUOTATION')))));
   
   BEGIN
      OPEN get_task(wo_no_);
      FETCH get_task
         INTO task_;
      CLOSE get_task;
   
      IF (task_ = 1) THEN
      
         OPEN get_sales_lines(wo_no_);
         FETCH get_sales_lines
            INTO sales_lines_;
         CLOSE get_sales_lines;
      
         IF (sales_lines_ = 1) THEN
            OPEN get_journal_info(wo_no_);
            FETCH get_journal_info
               INTO journal_;
            CLOSE get_journal_info;
         
            OPEN get_inv_prev(wo_no_);
            FETCH get_inv_prev
               INTO service_invoice_id_;
            CLOSE get_inv_prev;
         END IF;
      END IF;
   
      IF (task_ = 1 AND journal_ = 1 AND service_invoice_id_ IS NULL) THEN
         temp_ := 'TRUE';
      END IF;
   
      RETURN temp_;
   
   END Check_Inv_Preview;
--C0613 EntChathI (END)

--C0380 EntPrageG (START)

FUNCTION Get_Allowed_Tax_Codes__(
   company_    IN VARCHAR2,
   customer_   IN VARCHAR2,
   catalog_no_ IN VARCHAR2) RETURN VARCHAR2
IS
   allowed_tax_codes_ VARCHAR2(2000);
   stmt_ VARCHAR2(2000);
BEGIN
   stmt_ := 
    'SELECT LISTAGG(fee_code, '', '') WITHIN GROUP(ORDER BY fee_code) fee_code
     FROM (SELECT fee_code
             FROM tax_code_restricted
            WHERE fee_code IN
                  (SELECT a.cf$_Tax_Code
                     FROM sales_part_tax_code_clv a
                    WHERE a.cf$_sales_part_no = :1)
              AND fee_code IN
                  (SELECT b.cf$_tax_code
                     FROM cust_applicable_tax_code_clv b
                    WHERE b.cf$_company = :2
                      AND b.cf$_customer = :3)
              AND COMPANY = :4)';
   IF Database_SYS.View_Exist('SALES_PART_TAX_CODE_CLV') AND Database_SYS.View_Exist('CUST_APPLICABLE_TAX_CODE_CLV') THEN
      EXECUTE IMMEDIATE stmt_    
         INTO allowed_tax_codes_
        USING catalog_no_,company_,customer_,company_;
   END IF;          
   RETURN allowed_tax_codes_;
EXCEPTION
   WHEN OTHERS THEN
      RETURN NULL;
END Get_Allowed_Tax_Codes__;

--C0380 EntPrageG (END)

--C0566 EntChamuA (START)
FUNCTION Transf_Mansup_Attach_To_PO(
   target_key_ref_ IN VARCHAR2,
   service_name_   IN VARCHAR2) RETURN VARCHAR2 
IS
   order_no_            invoice_tab.PO_REF_NUMBER%TYPE;
   source_key_ref_      VARCHAR2(32000);
   source_key_ref_list_ VARCHAR2(32000);

   CURSOR get_part_nos IS
      SELECT COMPANY, INVOICE_ID
        FROM invoice_tab
       WHERE rowtype LIKE '%ManSuppInvoice'
         AND PO_REF_NUMBER = order_no_;
BEGIN
   order_no_ := Client_SYS.Get_Key_Reference_Value(target_key_ref_,
                                                 'ORDER_NO');
   FOR rec_ IN get_part_nos LOOP
      source_key_ref_ := 'COMPANY=' || rec_.company || '^INVOICE_ID=' ||
                         rec_.invoice_id || '^';
      Obj_Connect_Lu_Transform_API.Add_To_Source_Key_Ref_List(source_key_ref_list_,
                                                              source_key_ref_);
   END LOOP;

   RETURN source_key_ref_list_;
END Transf_Mansup_Attach_To_PO;
--C0566 EntChamuA (END)

PROCEDURE Create_Functional_Object__ (
   project_id_     IN VARCHAR2,
   sub_project_id_ IN VARCHAR2)
IS
   company_ VARCHAR2(20);
   object_id_              VARCHAR2(20);
   sub_proj_inst_site_     VARCHAR2(20);
   sales_contract_no_      VARCHAR2(20);
   project_default_site_   VARCHAR2(20);
   installation_site_      VARCHAR2(10);
   warranty_period_        VARCHAR2(10);
   warranty_period_objkey_ VARCHAR2(50);

   hand_over_date_          DATE;
   sales_contract_site_rec_ Sales_Contract_Site_CLP.Public_Rec;

   CURSOR get_activity_misc_parts IS
      SELECT part_no, part_desc, SUM(require_qty) quantity
        FROM (SELECT t.part_no,
                     Project_Misc_Procurement_API.
                        Get_Selected_Description(supply_option,site,t.part_no,matr_seq_no) part_desc,
                     require_qty
                FROM project_misc_procurement t,
                     (SELECT part_no, contract, cf$_serviceable_Db
                        FROM sales_part_cfv t
                       WHERE t.cf$_serviceable_db = 'TRUE') a
               WHERE  t.site = a.contract
                 AND t.part_no = a.part_no
                 AND company = company_
                 AND t.activity_seq IN
                     (SELECT activity_seq
                        FROM activity1 t
                       WHERE t.sub_project_id = sub_project_id_
                         AND t.project_id = project_id_)
                 AND t.part_no IS NOT NULL)
    GROUP BY part_no, part_desc;

   FUNCTION Get_Object_Id___(
     project_id_     IN VARCHAR2,
     sub_project_id_ IN VARCHAR2) RETURN VARCHAR2 
   IS
     object_id_ VARCHAR2(50);
     rec_       Equipment_Object_api.Public_Rec;
   BEGIN
      SELECT cf$_object_id_db
        INTO object_id_
        FROM sub_project_cfv
       WHERE project_id = project_id_
         AND sub_project_id = sub_project_id_;
      rec_ := Equipment_Object_API.Get_By_Rowkey(object_id_);
      RETURN rec_.mch_code;
   EXCEPTION
      WHEN OTHERS THEN
         RETURN NULL;
   END Get_Object_Id___;

   FUNCTION Get_Sales_Contract_No___(
      project_id_ IN VARCHAR2,
      company_    IN VARCHAR2) RETURN VARCHAR2 
   IS
      sales_contract_no_ VARCHAR2(50);
   BEGIN
      SELECT cf$_sales_contract_no
        INTO sales_contract_no_
        FROM project_cfv
       WHERE project_id = project_id_
         AND company = company_;
      RETURN sales_contract_no_;
   EXCEPTION
      WHEN OTHERS THEN
        RETURN NULL;
   END Get_Sales_Contract_No___;

   FUNCTION Get_Installation_Site___(
      project_id_     IN VARCHAR2,
      sub_project_id_ IN VARCHAR2) RETURN VARCHAR2 
   IS
      installation_site_ VARCHAR2(50);
   BEGIN
      SELECT cf$_installation_site
        INTO installation_site_
        FROM sub_project_cfv
       WHERE project_id = project_id_
         AND sub_project_id = sub_project_id_;
      RETURN installation_site_;
   EXCEPTION
      WHEN OTHERS THEN
         RETURN NULL;
   END Get_Installation_Site___;

   FUNCTION Get_Sales_Contract_Site_Rec___(
      contract_no_ IN VARCHAR2,
      site_no_     IN VARCHAR2) RETURN Sales_Contract_Site_CLP.Public_Rec 
   IS
      objkey_ VARCHAR2(50);
      rec_    Sales_Contract_Site_CLP.Public_Rec;
   BEGIN
      SELECT a.objkey
        INTO objkey_
        FROM sales_contract_site_clv a, contract_revision b
       WHERE a.cf$_contract_no = b.contract_no
         AND a.cf$_rev_seq = b.rev_seq
         AND b.objstate = 'Active'
         AND a.cf$_contract_no = contract_no_
         AND a.cf$_site_no = site_no_;
      IF objkey_ IS NOT NULL THEN
        rec_ := Sales_Contract_Site_CLP.Get(objkey_);
      END IF;
      RETURN rec_;
   EXCEPTION
      WHEN OTHERS THEN
         RETURN NULL;
   END Get_Sales_Contract_Site_Rec___;

   FUNCTION Get_Service_warranty_Objkey___(
      warranty_period_ IN VARCHAR2) RETURN VARCHAR2 
   IS
      objkey_ VARCHAR2(50);
   BEGIN
      SELECT objkey
        INTO objkey_
        FROM warranty_clv
       WHERE cf$_warranty_period = warranty_period_;
      RETURN objkey_;
   EXCEPTION
      WHEN OTHERS THEN
         RETURN NULL;
   END Get_Service_warranty_Objkey___;

   FUNCTION Get_Default_Project_Site___(
      company_    IN VARCHAR2,
      project_id_ IN VARCHAR2) RETURN VARCHAR2 
   IS
      site_ VARCHAR2(20);
   BEGIN
      SELECT site
        INTO site_
        FROM project_site_ext
       WHERE company = company_
         AND project_id = project_id_
         AND project_site_type_db = 'DEFAULTSITE';
      RETURN site_;
   EXCEPTION
      WHEN OTHERS THEN
         RETURN NULL;
   END Get_Default_Project_Site___;

   FUNCTION Get_Attr___(
      part_no_         IN VARCHAR2,
      part_desc_       IN VARCHAR2,
      project_id_      IN VARCHAR2,
      sub_project_id_  IN VARCHAR2,
      sup_mch_code_    IN VARCHAR2,
      production_date_ IN DATE) RETURN VARCHAR2 
   IS
      attr_               VARCHAR2(32000);
      site_               VARCHAR2(20) := '2013';
      object_level_       VARCHAR2(100) := '900_FUNCTIONAL_EQUIPMENT';
      operational_status_ VARCHAR2(20) := 'IN_OPERATION';
   BEGIN
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('PART_NO', part_no_, attr_);
      Client_SYS.Add_To_Attr('MCH_CODE',part_no_ || project_id_ || sub_project_id_,attr_);
      Client_SYS.Add_To_Attr('MCH_NAME', part_desc_, attr_);
      Client_SYS.Add_To_Attr('CONTRACT', site_, attr_);
      Client_SYS.Add_To_Attr('PRODUCTION_DATE', production_date_, attr_);
      Client_SYS.Add_To_Attr('SUP_CONTRACT', '2012', attr_);
      Client_SYS.Add_To_Attr('OBJ_LEVEL', object_level_, attr_);
      Client_SYS.Add_To_Attr('OPERATIONAL_STATUS_DB',operational_status_,attr_);
      Client_SYS.Add_To_Attr('SUP_MCH_CODE', sup_mch_code_, attr_);
      RETURN attr_;
   END Get_Attr___;

   FUNCTION Get_Cf_Attr___(
      project_id_       IN VARCHAR2,
      sub_project_id_   IN VARCHAR2,
      equipment_qty_    IN NUMBER,
      service_warranty_ IN VARCHAR2) RETURN VARCHAR2 
   IS
      attr_ VARCHAR2(32000);
   BEGIN
      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('CF$_EQUIPMENT_QUANTITY', equipment_qty_, attr_);
      Client_SYS.Add_To_Attr('CF$_SUB_PROJECT_ID', sub_project_id_, attr_);
      Client_SYS.Add_To_Attr('CF$_PROJ_ID', project_id_, attr_);
      Client_SYS.Add_To_Attr('CF$_SERVICE_WARRANTY_DB', service_warranty_, attr_);
      RETURN attr_;
   END Get_Cf_Attr___;

   PROCEDURE Create_Funct_Equip_Obj___(
      part_no_          IN VARCHAR2,
      part_desc_        IN VARCHAR2,
      project_id_       IN VARCHAR2,
      sub_project_id_   IN VARCHAR2,
      sup_mch_code_     IN VARCHAR2,
      production_date_  IN DATE,
      equipment_qty_    IN NUMBER,
      service_warranty_ IN VARCHAR2) 
   IS
      info_       VARCHAR2(32000);
      objid_      VARCHAR2(50);
      objversion_ VARCHAR2(50);
      attr_       VARCHAR2(32000);
      attr_cf_    VARCHAR2(32000);
      site_       VARCHAR2(20) := '2013';
   BEGIN
      IF NOT Equipment_Functional_API.Exists(site_,part_no_ || project_id_ || sub_project_id_) THEN
         attr_ := Get_Attr___(part_no_,
                              part_desc_,
                              project_id_,
                              sub_project_id_,
                              sup_mch_code_,
                              production_date_);
         Equipment_Functional_API.New__(info_,objid_,objversion_,attr_,'DO');
         IF objid_ IS NOT NULL THEN             
            attr_cf_ := Get_Cf_Attr___(project_id_,sub_project_id_,equipment_qty_,service_warranty_);
            Equipment_Functional_CFP.Cf_New__(info_,objid_,attr_cf_,'','DO');
         END IF;
      END IF;
      NULL;
   END Create_Funct_Equip_Obj___;
BEGIN
   company_ := Project_API.Get_Company(project_id_);
   object_id_ := Get_Object_Id___(project_id_, sub_project_id_);

   IF object_id_ IS NULL THEN
      Error_SYS.Appl_General('custom',
                             'ERROR_OBJ_ID: This operation is only possible for sub projects with Object ID');
   END IF;
   sub_proj_inst_site_ := Get_Installation_Site___(project_id_, sub_project_id_);    
   sales_contract_no_ := Get_Sales_Contract_No___(project_id_, company_);    
   project_default_site_ := Get_Default_Project_Site___(company_, project_id_);   
   installation_site_ := Get_Installation_Site___(project_id_,sub_project_id_);   
   sales_contract_site_rec_ := Get_Sales_Contract_Site_Rec___(sales_contract_no_, installation_site_);

   hand_over_date_  := sales_contract_site_rec_.cf$_delivered_on;
   warranty_period_ := sales_contract_site_rec_.cf$_warranty_period;


   IF sales_contract_site_rec_.cf$_contract_no IS NULL THEN
      Error_SYS.Appl_General('Custom',
                             'ERROR_NO_INSTALLATION_SITE: Installation site :P1 is not found in installation sites of the connected sales contract',
                             installation_site_);
   END IF;

   IF hand_over_date_ IS NULL THEN
      Error_SYS.Appl_General('Custom',
                             'ERROR_COMPLETE_DATE: Completion/Handover date of the installation site of connected sales contract is empty');
   END IF;

   IF warranty_period_ IS NULL THEN
      Error_SYS.Appl_General('Custom',
                             'ERROR_WARRANTY_PERIOD: Warranty Period of the installation site of connected sales contract is empty');
   END IF;
   warranty_period_objkey_ := Get_Service_warranty_Objkey___(warranty_period_);

   FOR rec_ IN get_activity_misc_parts LOOP
      Create_Funct_Equip_Obj___(rec_.part_no,
                                rec_.part_desc,
                                project_id_,
                                sub_project_id_,
                                object_id_,
                                hand_over_date_,
                                rec_.quantity,
                                warranty_period_objkey_);
   END LOOP;
END Create_Functional_Object__;
  
--C0335 EntChathI (START)
   PROCEDURE Sales_Contract_Auto_Closure IS
   
      remaining_amt_        NUMBER := 0;
      open_afp_exist_       VARCHAR2(5);
      total_paid_           NUMBER := 0;
      contract_sales_value_ NUMBER := 0;
   
      valid_to_close_ BOOLEAN := true;
      state_          VARCHAR2(10);
      attr_           VARCHAR2(32000);
      afp_no_         NUMBER;
   
      $IF (Component_Apppay_SYS.INSTALLED) $THEN
      temp_afp_no_      NUMBER;
      payment_per_curr_ NUMBER;
      info_             VARCHAR2(32000) := null;
   
      CURSOR cur_codes(contract_no VARCHAR2) IS
         SELECT DISTINCT (currency_code)
           FROM app_for_payment
          WHERE contract_no = contract_no;
   
      CURSOR get_remaining_amount(contract_no_ VARCHAR2, afp_no_ NUMBER) IS
         SELECT SUM(App_For_Payment_API.Get_Tot_Ret_Remaining(contract_no_,
                                                              afp_no_))
           FROM app_for_payment
          WHERE contract_no = contract_no
            AND objstate <> 'Cancelled'
          GROUP BY currency_code, supply_country, project_id, billing_seq;
      $END
   
      CURSOR get_eligible_sales_contrcts IS
         SELECT *
           FROM (SELECT t.*
                   FROM SALES_CONTRACT t
                  WHERE APP_FOR_PAYMENT_API.Get_Tot_Remaining_Ret_Per_Con(CONTRACT_NO) = 0
                    AND state NOT IN ('Cancelled', 'Closed')
                    AND EXISTS
                  (SELECT 1
                           FROM APP_FOR_PAYMENT_MAIN p
                          WHERE FIN_RELEASED_ALL = 'TRUE'
                            AND p.CONTRACT_NO = t.CONTRACT_NO)
                 UNION
                 SELECT t.*
                   FROM SALES_CONTRACT t
                  WHERE state NOT IN ('Cancelled', 'Closed')
                    AND INT_RETENTION = 0
                       -- to check if any APF exist and then check fully invoiced
                    AND EXISTS (SELECT 1
                           FROM app_for_payment_tab
                          WHERE contract_no = t.contract_no)
                    AND NOT EXISTS
                  (SELECT 1
                           FROM APP_FOR_PAYMENT_MAIN p
                          WHERE STATE NOT IN ('Fully Paid')
                            AND p.CONTRACT_NO = t.CONTRACT_NO));
   BEGIN
      state_ := 'Closed';
   
      FOR rec_ IN get_eligible_sales_contrcts LOOP
      
         Client_SYS.Clear_Attr(attr_);
      
         -- The contract must be in the Completed status to be eligible to close
         IF (rec_.state not in ('Completed')) THEN
            valid_to_close_ := false;
            Client_SYS.Add_To_Attr('CF$_ERROR_REPORTED',
                                   'Sales Contract must be in Completed status.',
                                   attr_);
            SALES_CONTRACT_CFP.Cf_Modify__(info_,rec_.objid,attr_,'','DO');
            CONTINUE;
         END IF;
         --check for core closure validations    
         -- START validate
         $IF (Component_Apppay_SYS.INSTALLED) $THEN
      
         SELECT max(afp_no)
           INTO afp_no_
           FROM app_for_payment
          WHERE contract_no = rec_.contract_no
            AND objstate <> 'Cancelled';
      
         OPEN get_remaining_amount(rec_.contract_no, afp_no_);
         FETCH get_remaining_amount
            INTO remaining_amt_;
         CLOSE get_remaining_amount;
      
         open_afp_exist_ := App_For_Payment_API.Check_Open_Afp_Exist(rec_.contract_no);
      
         FOR item_ IN cur_codes(rec_.contract_no) LOOP
            temp_afp_no_      := App_For_Payment_API.Get_Latest_Afp_No_Curr(rec_.contract_no,
                                                                            item_.currency_code);
            payment_per_curr_ := Afp_Payment_API.Get_App_Paid_To_Date(rec_.company,
                                                                      rec_.contract_no,
                                                                      temp_afp_no_);
            total_paid_       := total_paid_ + payment_per_curr_;
         END LOOP;
         $END
      
         IF (open_afp_exist_ = 'TRUE') THEN
            valid_to_close_ := false;
            Client_SYS.Add_To_Attr('CF$_ERROR_REPORTED',
                                   'There are application for payments not in Fully Paid or Cancelled statuses.',
                                   attr_);
         
            SALES_CONTRACT_CFP.Cf_Modify__(info_,rec_.objid, attr_,'','DO');
            CONTINUE;
         END IF;
      
         IF (remaining_amt_ != 0) THEN
            valid_to_close_ := false;
            Client_SYS.Add_To_Attr('CF$_ERROR_REPORTED',
                                   'The retention is not released fully in application for payments.',
                                   attr_);
         
            SALES_CONTRACT_CFP.Cf_Modify__(info_,rec_.objid,attr_,'','DO');
            CONTINUE;
         
         END IF;
      
         contract_sales_value_ := Contract_Revision_Util_API.Get_Contract_Sales_Value_Co(rec_.contract_no,
                                                                                         Contract_Revision_Util_API.Get_Active_Revision(rec_.contract_no));
      
         IF (NVL(contract_sales_value_, 0) > NVL(total_paid_, 0)) THEN
            valid_to_close_ := false;
            Client_SYS.Add_To_Attr('CF$_ERROR_REPORTED',
                                   'The total paid amount on all applications must be equal to (or higher) than the total contract sales value.',
                                   attr_);
         
            SALES_CONTRACT_CFP.Cf_Modify__(info_,rec_.objid,attr_,'','DO');
            CONTINUE;
         END IF;
         -- END validate
         -- START finaite state set
         IF (valid_to_close_) THEN
            -- Finite_State_Set
            UPDATE sales_contract_tab
               SET rowstate = state_, rowversion = sysdate
             WHERE contract_no = rec_.contract_no;
            rec_.objstate := state_;
         END IF;
         -- END Finite state set
         -- START Calculate_Revenue
         IF (Contract_Project_API.Has_Connected_Activity(rec_.contract_no) =
            'TRUE') THEN
            Contract_Project_API.Calculate_Revenue(rec_.contract_no);
         END IF;
         -- END Calculate_Revenue
      END LOOP;
   END Sales_Contract_Auto_Closure;
--C0335 EntChathI (END)
  
--C0367 EntChathI (START)  
FUNCTION Check_Inv_Header_CA(
   identity_       IN VARCHAR2, 
   credit_analyst_ IN VARCHAR2, 
   company_ IN VARCHAR2) RETURN VARCHAR2
IS
  
   follow_up_date_ DATE;
   status_ VARCHAR2(100);
   inv_state_ VARCHAR2(100);
   inv_due_ DATE;
   current_bucket_ NUMBER;
   credit_note_ NUMBER;
   amount_due_ NUMBER;
   temp_ VARCHAR2(5):='FALSE';

   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_analyst_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         AND Customer_Credit_Info_API.Get_Credit_Analyst_Code(company_,identity_) LIKE  NVL(credit_analyst_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes (company_ VARCHAR2, identity_ VARCHAR2, invoice_id_ NUMBER )IS
      SELECT * 
        FROM (SELECT 1 AS exist, follow_up_date, Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company =company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_ 
            ORDER BY follow_up_date DESC, note_id DESC 
            )
        WHERE rownum  =1;

   CURSOR get_inv_info(company_ VARCHAR2, identity_ VARCHAR2, invoice_id_ NUMBER )IS
      SELECT inv_state, due_date
        FROM invoice_ledger_item_cu_qry
       WHERE company =company_
         AND (identity = identity_ AND invoice_id = invoice_id_ );


   CURSOR get_acount_due(company_ VARCHAR2, identity_ VARCHAR2)IS
      SELECT amount_due
        FROM identity_pay_info_cu_qry
       WHERE company = company_
       AND identity = identity_;
   
   CURSOR get_aging_bucket (company_ VARCHAR2, invoice_id_ VARCHAR2)IS
      SELECT 1
      FROM bucket_invoice_cu_query 
      WHERE bucket =1
      AND company = company_
      AND invoice_id = invoice_id_ ;
  
BEGIN
   FOR rec_ IN get_inv_headers(identity_,credit_analyst_ , company_ ) LOOP
       OPEN get_inv_header_notes(rec_.company, rec_.identity,rec_.invoice_id);
      FETCH get_inv_header_notes 
       INTO credit_note_,follow_up_date_, status_;
      CLOSE get_inv_header_notes;

       OPEN  get_inv_info(rec_.company, rec_.identity,rec_.invoice_id);
      FETCH  get_inv_info 
       INTO inv_state_,inv_due_;
      CLOSE get_inv_info;

      IF(follow_up_date_ IS NOT NULL AND follow_up_date_<= SYSDATE AND 
         (inv_state_ NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')AND inv_due_< SYSDATE) )THEN 
         temp_ := 'TRUE';
         EXIT;
      END IF;      

      OPEN  get_acount_due(rec_.company, rec_.identity);
      FETCH  get_acount_due INTO amount_due_;
      CLOSE get_acount_due;

      IF(credit_note_ IS NULL AND amount_due_>0)THEN 
         temp_ := 'TRUE';
         EXIT;
      END IF; 
      
      OPEN  get_aging_bucket(rec_.company, rec_.invoice_id);
      FETCH  get_aging_bucket INTO  current_bucket_;
      CLOSE get_aging_bucket;
      
      IF(status_ NOT IN ('Complete','Escalated to Credit Manager','Escalated to Finance Controller') AND current_bucket_ IS NULL)THEN
         temp_ := 'TRUE';
         EXIT;
      END IF;
   END LOOP;
   RETURN temp_;
END Check_Inv_Header_CA;
   
FUNCTION Get_Largest_Overdue_Debt(
   identity_ IN VARCHAR2, 
   company_ IN VARCHAR2) RETURN VARCHAR2
IS 
   temp_ NUMBER;
 
   CURSOR get_inv_info(company_ VARCHAR2, identity_ VARCHAR2) IS
      SELECT MAX(open_amount)
        FROM invoice_ledger_item_cu_qry
       WHERE company = company_
         AND identity = identity_
         AND inv_state NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')
         AND due_date < SYSDATE;
  
BEGIN
   OPEN get_inv_info (company_,identity_);
   FETCH get_inv_info INTO temp_;
   CLOSE get_inv_info;
   
   RETURN temp_;
 END  Get_Largest_Overdue_Debt;
      
FUNCTION Get_Oldest_Overdue_Debt(
   identity_ IN VARCHAR2, 
   company_  IN VARCHAR2) RETURN VARCHAR2
IS 
   temp_ NUMBER; 
   
   CURSOR get_inv_info(company_ VARCHAR2, identity_ VARCHAR2) IS
         SELECT *
           FROM (SELECT open_amount
                   FROM invoice_ledger_item_cu_qry
                  WHERE company = company_
                    AND identity = identity_
                    AND inv_state NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')
                    AND due_date in
                        (SELECT min(due_date)
                           FROM invoice_ledger_item_cu_qry
                          WHERE company = company_
                            AND identity = identity_
                            AND inv_state NOT IN('Preliminary', 'Cancelled', 'PaidPosted')
                            AND due_date < SYSDATE
                            AND open_amount > 0)
                  ORDER BY open_amount DESC)
          WHERE ROWNUM = 1;
BEGIN
   OPEN get_inv_info (company_,identity_);
   FETCH get_inv_info INTO temp_;
   CLOSE get_inv_info;
   
   RETURN temp_;
END  Get_Oldest_Overdue_Debt;
 
FUNCTION Get_Oldest_Followup_Date(
   identity_ IN VARCHAR2, 
   company_  IN VARCHAR2) RETURN VARCHAR2
IS
 
   temp_ DATE;

      CURSOR get_inv_info(company_ VARCHAR2, identity_ VARCHAR2) IS
         SELECT MIN(follow_up_date)
           FROM invoice_header_notes
          WHERE invoice_id IN
                (SELECT *
                   FROM (SELECT invoice_id
                           FROM invoice_tab t
                          WHERE party_type = 'CUSTOMER'
                            AND EXISTS (SELECT 1
                                          FROM user_finance_auth_pub
                                         WHERE t.company = company_)
                            AND identity = identity_
                            AND company = company_
                            AND EXISTS
                                (SELECT 1
                                   FROM invoice_ledger_item_cu_qry
                                  WHERE company = company_
                                    AND identity = identity_
                                    AND invoice_id = t.invoice_id
                                    AND inv_state NOT IN('Preliminary','Cancelled','PaidPosted')
                                    and due_date < sysdate)
                          ORDER BY creation_date ASC)
                  WHERE rownum = 1);     
BEGIN
   OPEN get_inv_info (company_,identity_);
   FETCH get_inv_info INTO temp_;
   CLOSE get_inv_info;
   
   RETURN temp_;
END  Get_Oldest_Followup_Date;
 
FUNCTION Check_Credit_Escalated(
   company_        IN VARCHAR2,
   credit_analyst_ IN VARCHAR2,
   identity_       IN VARCHAR2) RETURN VARCHAR2 
IS
   status_ varchar2(100);
   temp_   varchar2(5) := 'FALSE';
   
   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_analyst_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         AND Customer_Credit_Info_API.Get_Credit_Analyst_Code(company_,identity_) LIKE  NVL(credit_analyst_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes(company_  VARCHAR2,
                             identity_   VARCHAR2,
                             invoice_id_ NUMBER) IS
      SELECT *
        FROM (SELECT Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;

BEGIN
   FOR rec_ in get_inv_headers(identity_, credit_analyst_, company_) LOOP
      OPEN get_inv_header_notes(rec_.company,
                                rec_.identity,
                                rec_.invoice_id);
      FETCH get_inv_header_notes
        into status_;
      CLOSE get_inv_header_notes;

      IF (status_ IN ('Escalated to Credit Manager')) THEN
        temp_ := 'TRUE';
        EXIT;
      END IF;
   END LOOP;
   RETURN temp_;
END Check_Credit_Escalated;
  
FUNCTION Check_Credit_Note_Queries(
   company_        IN VARCHAR2,
   credit_analyst_ IN VARCHAR2,
   identity_       IN VARCHAR2,
   type_           IN VARCHAR2) RETURN VARCHAR2 IS
   
    follow_up_date_ DATE;
    status_         VARCHAR2(100);
    inv_state_      VARCHAR2(100);
    temp_           VARCHAR2(5) := 'FALSE';

   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_analyst_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         AND Customer_Credit_Info_API.Get_Credit_Analyst_Code(company_,identity_) LIKE  NVL(credit_analyst_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes(company_  VARCHAR2,
                             identity_   VARCHAR2,
                             invoice_id_ NUMBER) IS
      SELECT *
        FROM (SELECT follow_up_date, Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;

   CURSOR get_inv_info(company_  VARCHAR2,
                     identity_   VARCHAR2,
                     invoice_id_ NUMBER) IS
      SELECT inv_state
        FROM invoice_ledger_item_cu_qry
       WHERE company = company_
         AND (identity = identity_ AND invoice_id = invoice_id_);

   CURSOR get_credit_note(company_ VARCHAR2, identity_ VARCHAR2) IS
      SELECT 1
        FROM customer_credit_note
       WHERE company = company_
         AND customer_id = identity_;

BEGIN
   FOR rec_ in get_inv_headers(identity_, credit_analyst_, company_) LOOP
       OPEN get_inv_header_notes(rec_.company,rec_.identity,rec_.invoice_id);
      FETCH get_inv_header_notes
       INTO follow_up_date_, status_;
      CLOSE get_inv_header_notes;

      OPEN get_inv_info(rec_.company, rec_.identity, rec_.invoice_id);
     FETCH get_inv_info
      INTO inv_state_;
     CLOSE get_inv_info;

      IF (type_ = 'Open') THEN
         IF (follow_up_date_ IS NOT NULL AND follow_up_date_ > SYSDATE AND
            status_ IN ('In Query') ) THEN
           temp_ := 'TRUE';
           EXIT;
         END IF;
      ELSIF (type_ = 'Overdue') THEN
         IF (follow_up_date_ IS NOT NULL AND follow_up_date_ < SYSDATE AND
            status_ IN ('In Query') AND
            inv_state_ NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')) THEN
           temp_ := 'TRUE';
           EXIT;
         END IF;
      END IF;
   END LOOP;
   RETURN temp_;
END Check_Credit_Note_Queries;
  
FUNCTION Check_Credit_Legal(
   company_        IN VARCHAR2,
   credit_analyst_ IN VARCHAR2,
   identity_       IN VARCHAR2) RETURN VARCHAR2 
IS  
   status_ varchar2(100);
   temp_   varchar2(5) := 'FALSE';
   
   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_analyst_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         AND Customer_Credit_Info_API.Get_Credit_Analyst_Code(company_,identity_) LIKE  NVL(credit_analyst_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes(company_  VARCHAR2,
                             identity_   VARCHAR2,
                             invoice_id_ NUMBER) IS
      SELECT *
        FROM (SELECT Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;
  
BEGIN
  
   FOR rec_ in get_inv_headers(identity_, credit_analyst_, company_) LOOP
      OPEN get_inv_header_notes(rec_.company,rec_.identity,rec_.invoice_id);
      FETCH get_inv_header_notes
        into status_;
      CLOSE get_inv_header_notes;

      IF (status_ IN ('Legal')) THEN
        temp_ := 'TRUE';
        EXIT;
      END IF;
   END LOOP;
   RETURN temp_;  
    
END Check_Credit_Legal;
  
FUNCTION Check_Credit_Escalated_FC(
   company_        IN VARCHAR2,
   credit_analyst_ IN VARCHAR2,
   identity_       IN VARCHAR2) RETURN VARCHAR2 
IS
  
   status_ VARCHAR2(100);
   temp_   VARCHAR2(5) := 'FALSE';
   
   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_analyst_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         AND Customer_Credit_Info_API.Get_Credit_Analyst_Code(company_,identity_) LIKE  NVL(credit_analyst_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes(company_  VARCHAR2,
                             identity_   VARCHAR2,
                             invoice_id_ NUMBER) IS
      SELECT *
        FROM (SELECT Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;
  
BEGIN
  
   FOR rec_ IN get_inv_headers(identity_, credit_analyst_, company_) LOOP
      OPEN get_inv_header_notes(rec_.company,rec_.identity,rec_.invoice_id);
      FETCH get_inv_header_notes
       INTO status_;
      CLOSE get_inv_header_notes;

      IF (status_ IN ('Escalated to Finance Controller')) THEN
         temp_ := 'TRUE';
         EXIT;
      END IF;
   END LOOP;
   RETURN temp_;
END Check_Credit_Escalated_FC;

FUNCTION Get_Period_Target(credit_analyst_ IN VARCHAR2,
                           company_        IN VARCHAR2,
                           target_period_  IN VARCHAR2) RETURN NUMBER
IS
      month_  VARCHAR2(5);
      year_   VARCHAR2(10);
      target_ NUMBER;
   
      CURSOR get_target_info(year_ NUMBER) IS
         SELECT t.*
           FROM credit_targets_clv t
           WHERE t.cf$_company = company_
             AND t.cf$_year =year_
            AND cf$_credit_analyst_code_db in
                (select objkey
                   from credit_analyst
                  where credit_analyst_code = credit_analyst_);
   
   BEGIN
      IF (target_period_ IS NOT NULL) then
         SELECT SUBSTR(target_period_, 1, Instr(target_period_, '-') - 1)
           INTO month_
           FROM dual;
      
         SELECT SUBSTR(target_period_, Instr(target_period_, '-') + 1)
           INTO year_
           FROM dual;
      
      END IF;
   
      FOR rec_ IN get_target_info(to_number(year_)) LOOP
      
         IF (month_ = '01') THEN
            target_ := rec_.CF$_TARGET_JAN;
         ELSIF (month_ = '02') THEN
            target_ := rec_.CF$_TARGET_FEB;
         ELSIF (month_ = '03') THEN
            target_ := rec_.CF$_TARGET_MARCH;
         ELSIF (month_ = '04') THEN
            target_ := rec_.CF$_TARGET_APRIL;
         ELSIF (month_ = '05') THEN
            target_ := rec_.CF$_TARGET_MAY;
         ELSIF (month_ = '06') THEN
            target_ := rec_.CF$_TARGET_JUNE;
         ELSIF (month_ = '07') THEN
            target_ := rec_.CF$_TARGET_JULLY;
         ELSIF (month_ = '08') THEN
            target_ := rec_.CF$_TARGET_AUG;
         ELSIF (month_ = '09') THEN
            target_ := rec_.CF$_TARGET_SEPT;
         ELSIF (month_ = '10') THEN
            target_ := rec_.CF$_TARGET_OCT;
         ELSIF (month_ = '11') THEN
            target_ := rec_.CF$_TARGET_NOV;
         ELSIF (month_ = '12') THEN
            target_ := rec_.CF$_TARGET_DEC;
         END IF;
      
      END LOOP;
   
      RETURN target_;
   END Get_Period_Target;


FUNCTION Get_Cash_Collected(credit_analyst_ IN VARCHAR2,
                            company_        IN VARCHAR2,
                            target_period_  IN VARCHAR2) RETURN NUMBER 
IS
                            
      month_          VARCHAR2(05);
      year_           VARCHAR2(10);
      cash_collected_ NUMBER;
   
      CURSOR get_cash_collected(in_company_     VARCHAR2,
                                year_           VARCHAR2,
                                month_          VARCHAR2,
                                credit_analyst_ VARCHAR2) IS
      SELECT SUM(a.CURR_AMOUNT)
        FROM payment_per_currency_cu_qry a
       WHERE company =in_company_
         AND SERIES_ID = 'CUPAY'
         AND (TO_CHAR(a.pay_date, 'YYYY') = year_ AND TO_CHAR(a.pay_date, 'MM') = month_)
  AND EXISTS 
      (SELECT 1 FROM LEDGER_TRANSACTION_CU_QRY
        WHERE company = a.company
          AND series_id = a.SERIES_ID
          AND payment_id = a.PAYMENT_ID
          AND LEDGER_COMPANY =a.company 
          AND Customer_Credit_Info_API.Get_Credit_Analyst_Code(company, identity) = credit_analyst_
          AND UPPER(Invoice_Party_Type_Group_API.Get_Description(company,'CUSTOMER',Identity_Invoice_Info_API.Get_Group_Id(company,identity,'CUSTOMER'))) NOT LIKE UPPER('%Intercompany%')
          );
   
BEGIN
      IF (target_period_ IS NOT NULL) THEN
         SELECT SUBSTR(target_period_, 1, Instr(target_period_, '-') - 1)
           INTO month_
           FROM dual;
      
         SELECT SUBSTR(target_period_, Instr(target_period_, '-') + 1)
           INTO year_
           FROM dual;
      END IF;
   
      OPEN get_cash_collected(company_, year_, month_, credit_analyst_);
      FETCH get_cash_collected
       INTO cash_collected_;
      CLOSE get_cash_collected;
   
      RETURN NVL(cash_collected_, 0);
   END Get_Cash_Collected;
   
   FUNCTION Check_Inv_Courtesy_Call(company_        IN VARCHAR2,
                                   identity_       IN VARCHAR2)  RETURN VARCHAR2 IS
   
      status_ VARCHAR2(100);
      inv_state_ VARCHAR2(100);
      temp_   VARCHAR2(5) := 'FALSE';
      
      CURSOR get_inv_headers(identity_       VARCHAR2,
                             company_        VARCHAR2) IS
         SELECT identity,invoice_id
           FROM OUTGOING_INVOICE_QRY
          WHERE identity = identity_
            AND company LIKE NVL(company_,'%');
   
      CURSOR get_inv_header_notes(company_    VARCHAR2, identity_ VARCHAR2, invoice_id_ NUMBER) is
         SELECT *
           FROM (SELECT Credit_Note_Status_API.Get_Note_Status_Description(COMPANY, NOTE_STATUS_ID)
                   FROM invoice_header_notes 
                  WHERE company = company_
                    AND identity = identity_
                    AND party_type = 'Customer'
                    AND invoice_id = invoice_id_
                  ORDER BY note_date DESC, note_id DESC)
          WHERE ROWNUM = 1;
          
          CURSOR get_inv_info(company_ VARCHAR2,identity_ VARCHAR2, invoice_id_ NUMBER) IS
         SELECT inv_state
           FROM INVOICE_LEDGER_ITEM_CU_QRY
          WHERE company = company_
            AND identity = identity_
            AND invoice_id = invoice_id_
            AND open_amount>=10000;
   
   BEGIN
   
      FOR rec_ in get_inv_headers(identity_, company_) LOOP
         OPEN get_inv_header_notes(company_,rec_.identity,rec_.invoice_id);
         FETCH get_inv_header_notes
          INTO status_;
         CLOSE get_inv_header_notes;
         
         OPEN get_inv_info(company_, rec_.identity, rec_.invoice_id);
         FETCH get_inv_info INTO inv_state_;
         CLOSE get_inv_info;      
     
         IF (status_ IN ('Large Invoice Call') AND inv_state_ NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')  ) THEN
            temp_ := 'TRUE';
            EXIT;
         END IF;
      END LOOP;
      RETURN temp_;
   END Check_Inv_Courtesy_Call;
--C0367 EntChathI (END)
--C0368 EntChathI (START)
FUNCTION Check_Credit_Note_Queries_CM(
   company_        IN VARCHAR2,
   credit_manager_ IN VARCHAR2,
   identity_       IN VARCHAR2,
   type_           IN VARCHAR2) RETURN VARCHAR2 
IS
   follow_up_date_ DATE;
   status_         VARCHAR2(100);
   inv_state_      VARCHAR2(100);
   temp_           VARCHAR2(5) := 'FALSE';


   CURSOR get_inv_headers(identity_ VARCHAR2, credit_manager_ VARCHAR2,company_ VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
       AND company = company_
       AND Customer_Credit_Info_API.Get_Manager(company_,identity_) LIKE  NVL(credit_manager_, '%') ;
   
   CURSOR get_inv_header_notes(company_  VARCHAR2, identity_ VARCHAR2, invoice_id_ NUMBER) IS
      SELECT *
      FROM (SELECT follow_up_date,
                  Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;

   CURSOR get_inv_info(company_  VARCHAR2,
                     identity_   VARCHAR2,
                     invoice_id_ NUMBER) IS
      SELECT INV_STATE
        FROM INVOICE_LEDGER_ITEM_CU_QRY
       WHERE COMPANY = company_
         AND (IDENTITY = identity_ AND INVOICE_ID = invoice_id_);

   CURSOR get_credit_note(company_ VARCHAR2, identity_ VARCHAR2) IS
      SELECT 1
        FROM CUSTOMER_CREDIT_NOTE
       WHERE company = company_
         AND customer_id = identity_;
  
BEGIN
   FOR rec_ in get_inv_headers(identity_, credit_manager_, company_) LOOP
      OPEN get_inv_header_notes(rec_.company,rec_.identity,rec_.invoice_id);
      FETCH get_inv_header_notes
      into follow_up_date_, status_;
      CLOSE get_inv_header_notes;

      OPEN get_inv_info(rec_.company, rec_.identity, rec_.invoice_id);
      FETCH get_inv_info
      INTO inv_state_;
      CLOSE get_inv_info;

      IF (type_ = 'Open') THEN
         IF (follow_up_date_ IS NOT NULL AND follow_up_date_ > SYSDATE AND
            status_ in ('In Query') AND
            inv_state_ NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')) THEN
            temp_ := 'TRUE';
            EXIT;
         END IF;
      ELSIF (type_ = 'Overdue') THEN
         IF (follow_up_date_ IS NOT NULL AND follow_up_date_ < SYSDATE AND
            status_ in ('In Query') AND
            inv_state_ NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')) THEN
            temp_ := 'TRUE';
            EXIT;
         END IF;
      END IF;
   END LOOP;
RETURN temp_;
  
END Check_Credit_Note_Queries_CM;
  
FUNCTION Check_Cr_Note_Queries_All_CM(
   company_    IN VARCHAR2,
   identity_   IN VARCHAR2) RETURN VARCHAR2 
IS
   follow_up_date_ DATE;
   status_         VARCHAR2(100);
   inv_state_      VARCHAR2(100);
   temp_           VARCHAR2(5) := 'FALSE';

   CURSOR get_inv_headers(identity_ VARCHAR2, company_ VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         AND company = company_;

   CURSOR get_inv_header_notes(company_  VARCHAR2, identity_ VARCHAR2, invoice_id_ NUMBER) IS
      SELECT *
      FROM (SELECT follow_up_date,
                  Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;

   CURSOR get_inv_info(company_  VARCHAR2,identity_  VARCHAR2, invoice_id_ NUMBER) IS
   SELECT INV_STATE
      FROM invoice_ledger_item_cu_qry
      WHERE company = company_
      AND (identity = identity_ AND invoice_id = invoice_id_);

   CURSOR get_credit_note(company_ VARCHAR2, identity_ VARCHAR2) IS
      SELECT 1
      FROM customer_credit_note
      WHERE company = company_
      AND customer_id = identity_;
  
BEGIN
   FOR rec_ IN get_inv_headers(identity_, company_) LOOP
      OPEN get_inv_header_notes(rec_.company,rec_.identity,rec_.invoice_id);
      FETCH get_inv_header_notes
       INTO follow_up_date_, status_;
      CLOSE get_inv_header_notes;

      OPEN get_inv_info(rec_.company, rec_.identity, rec_.invoice_id);
      FETCH get_inv_info
       INTO inv_state_;
      CLOSE get_inv_info;

         IF (status_ IN ('In Query') AND inv_state_ NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')) THEN
            temp_ := 'TRUE';
            EXIT;
         END IF;   
   END LOOP;
   RETURN temp_;

END Check_Cr_Note_Queries_All_CM;
  
FUNCTION Check_Inv_Header_CM(
   identity_ IN VARCHAR2, 
   credit_manager_ IN VARCHAR2, 
   company_ IN VARCHAR2) RETURN VARCHAR2
IS
  
   follow_up_date_ DATE;
   status_ VARCHAR2(100);
   inv_state_ VARCHAR2(100);
   inv_due_ DATE;
   balance_ NUMBER;
   credit_note_ NUMBER;
   current_bucket_ NUMBER;
   
   temp_ VARCHAR2(5):='FALSE';

   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_manager_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         AND Customer_Credit_Info_API.Get_Manager(company_,identity_) LIKE  NVL(credit_manager_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes (company_ VARCHAR2, identity_ VARCHAR2, invoice_id_ NUMBER )
   IS
      SELECT * FROM (
      SELECT 1 AS exist, follow_up_date, Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
      from invoice_header_notes
      WHERE company = company_
        AND identity = identity_
        AND party_type = 'Customer'
        AND invoice_id = invoice_id_
      ORDER BY follow_up_date DESC, note_id DESC)
      WHERE rownum = 1;

   CURSOR get_inv_info(company_ VARCHAR2, identity_ VARCHAR2, invoice_id_ NUMBER )IS
      SELECT inv_state, due_date
      FROM invoice_ledger_item_cu_qry
      WHERE COMPANY =company_
      AND (identity = identity_ AND invoice_id = invoice_id_ );


   CURSOR get_acount_due(company_ VARCHAR2, identity_ VARCHAR2)IS
      SELECT  balance
      FROM identity_pay_info_cu_qry
      where company = company_
      AND identity = identity_;
      
      CURSOR get_aging_bucket (company_ VARCHAR2, invoice_id_ VARCHAR2)IS
      SELECT 1
      FROM bucket_invoice_cu_query 
      WHERE bucket =1
      AND company = company_
      AND invoice_id = invoice_id_ ;
  

BEGIN
   FOR rec_ IN get_inv_headers(identity_,credit_manager_ , company_ ) LOOP
      OPEN get_inv_header_notes(rec_.company, rec_.identity,rec_.invoice_id);
      FETCH get_inv_header_notes 
      INTO credit_note_,follow_up_date_, status_;
      CLOSE get_inv_header_notes;

      OPEN  get_inv_info(rec_.company, rec_.identity,rec_.invoice_id);
      FETCH  get_inv_info INTO inv_state_,inv_due_;
      CLOSE get_inv_info;

      IF(follow_up_date_ IS NOT NULL AND follow_up_date_<= SYSDATE AND
         (inv_state_ NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')AND inv_due_< SYSDATE)
         AND status_ NOT IN ('Complete','Escalated to Credit Manager','Escalated to Finance Controller') )THEN 
         temp_ := 'TRUE';
         EXIT;
      END IF;      

      OPEN  get_acount_due(rec_.company, rec_.identity);
      FETCH  get_acount_due INTO  balance_;
      CLOSE get_acount_due;
      
      OPEN  get_aging_bucket(rec_.company, rec_.invoice_id);
      FETCH  get_aging_bucket INTO  current_bucket_;
      CLOSE get_aging_bucket;
      

      IF(credit_note_ IS NULL AND current_bucket_ IS NULL)THEN 
         temp_ := 'TRUE';
         EXIT;
      END IF; 

   END LOOP;
   
      RETURN temp_;
END Check_Inv_Header_CM;
  
FUNCTION Get_CA_Open_Items_Count(
   company_        IN VARCHAR2,
   credit_analyst_ IN VARCHAR2) RETURN NUMBER   
IS
   temp_ NUMBER;

   CURSOR get_open_item_count(company_ VARCHAR2 ,credit_analyst_ VARCHAR2 )IS
      SELECT count(INVOICE_ID) Open_Items 
      FROM invoice_ledger_item_cu_qry
      WHERE company =company_
      AND Customer_Credit_Info_API.Get_Credit_Analyst_Code(company, identity) = credit_analyst_
      AND inv_state NOT IN ('Preliminary', 'Cancelled', 'PaidPosted') ;
      
BEGIN
   OPEN get_open_item_count(company_ ,credit_analyst_);
   FETCH get_open_item_count INTO temp_;
   CLOSE get_open_item_count;

   RETURN temp_;
END Get_CA_Open_Items_Count;
   
FUNCTION Check_Credit_Legal_CM(
   company_        IN VARCHAR2,
   credit_manager_ IN VARCHAR2,
   identity_       IN VARCHAR2) RETURN VARCHAR2 
IS

   status_ varchar2(100);
   temp_   varchar2(5) := 'FALSE';
   
   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_manager_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         and Customer_Credit_Info_API.Get_Manager(company_,identity_) LIKE  NVL(credit_manager_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes(company_  VARCHAR2,
                             identity_   VARCHAR2,
                             invoice_id_ NUMBER) IS
      SELECT *
        FROM (SELECT Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;
BEGIN

   FOR rec_ IN get_inv_headers(identity_, credit_manager_, company_) LOOP
      OPEN get_inv_header_notes(rec_.company,
                                rec_.identity,
                                rec_.invoice_id);
      FETCH get_inv_header_notes
       INTO status_;
      CLOSE get_inv_header_notes;

      IF (status_ IN ('Legal')) THEN
         temp_ := 'TRUE';
         EXIT;
      END IF;
   END LOOP;
   RETURN temp_;
END Check_Credit_Legal_CM;
  
FUNCTION Check_Cr_Escalated_FC_CM(
   company_        IN VARCHAR2,
   credit_manager_ IN VARCHAR2,
   identity_       IN VARCHAR2) RETURN VARCHAR2 
IS
  
   status_ VARCHAR2(100);
   temp_   VARCHAR2(5) := 'FALSE';
   
   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_manager_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         and Customer_Credit_Info_API.Get_Manager(company_,identity_) LIKE  NVL(credit_manager_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes(company_  VARCHAR2,
                             identity_   VARCHAR2,
                             invoice_id_ NUMBER) IS
      SELECT *
        FROM (SELECT Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;
  
BEGIN  
   FOR rec_ IN get_inv_headers(identity_, credit_manager_, company_) LOOP
      OPEN get_inv_header_notes(rec_.company,
                                rec_.identity,
                                rec_.invoice_id);
      FETCH get_inv_header_notes
        into status_;
      CLOSE get_inv_header_notes;

      IF (status_ IN ('Escalated to Finance Controller')) THEN
         temp_ := 'TRUE';
         EXIT;
      END IF;
   END LOOP;
   RETURN temp_;
END Check_Cr_Escalated_FC_CM;
  
FUNCTION Check_Escalated_CM(
   company_        IN VARCHAR2,
   credit_manager_ IN VARCHAR2,
   identity_       IN VARCHAR2) RETURN VARCHAR2 
IS
   status_ VARCHAR2(100);
   temp_   VARCHAR2(5) := 'FALSE';
   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_manager_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         and Customer_Credit_Info_API.Get_Manager(company_,identity_) LIKE  NVL(credit_manager_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes(company_  VARCHAR2,
                             identity_   VARCHAR2,
                             invoice_id_ NUMBER) IS
      SELECT *
        FROM (SELECT Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id)
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;

BEGIN

   FOR rec_ IN get_inv_headers(identity_, credit_manager_, company_) LOOP
      OPEN get_inv_header_notes(rec_.company,
                                rec_.identity,
                                rec_.invoice_id);
      FETCH get_inv_header_notes
        INTO status_;
      CLOSE get_inv_header_notes;

      IF (status_ IN ('Escalated to Credit Manager','Request Customer Block')) THEN
         temp_ := 'TRUE';
         EXIT;
      END IF;
   END LOOP;
RETURN temp_;  

END Check_Escalated_CM;

FUNCTION Get_Team_Period_Target(company_        IN VARCHAR2,
                                target_period_  IN VARCHAR2) RETURN NUMBER
IS
      month_  VARCHAR2(5);
      year_   VARCHAR2(10);
      target_ NUMBER:=0;  
   
      CURSOR get_target_info(year_ NUMBER) IS
         SELECT t.*
           FROM credit_targets_clv t
          WHERE t.cf$_company = company_
            AND t.cf$_year =year_;
   
   BEGIN
      IF (target_period_ IS NOT NULL) then
         SELECT SUBSTR(target_period_, 1, Instr(target_period_, '-') - 1)
           INTO month_
           FROM dual;
      
         SELECT SUBSTR(target_period_, Instr(target_period_, '-') + 1)
           INTO year_
           FROM dual;
      
      END IF;
   
      FOR rec_ IN get_target_info(to_number(year_)) LOOP
      
         IF (month_ = '01') THEN
            target_ := target_+ rec_.CF$_TARGET_JAN;
         ELSIF (month_ = '02') THEN
            target_ := target_+ rec_.CF$_TARGET_FEB;
         ELSIF (month_ = '03') THEN
            target_ := target_+ rec_.CF$_TARGET_MARCH;
         ELSIF (month_ = '04') THEN
            target_ := target_+ rec_.CF$_TARGET_APRIL;
         ELSIF (month_ = '05') THEN
            target_ := target_+ rec_.CF$_TARGET_MAY;
         ELSIF (month_ = '06') THEN
            target_ := target_+ rec_.CF$_TARGET_JUNE;
         ELSIF (month_ = '07') THEN
            target_ := target_+ rec_.CF$_TARGET_JULLY;
         ELSIF (month_ = '08') THEN
            target_ := target_+ rec_.CF$_TARGET_AUG;
         ELSIF (month_ = '09') THEN
            target_ := target_+ rec_.CF$_TARGET_SEPT;
         ELSIF (month_ = '10') THEN
            target_ := target_+ rec_.CF$_TARGET_OCT;
         ELSIF (month_ = '11') THEN
            target_ := target_+ rec_.CF$_TARGET_NOV;
         ELSIF (month_ = '12') THEN
            target_ := target_+ rec_.CF$_TARGET_DEC;
         END IF;
      
      END LOOP;
      IF(target_=0)THEN
          target_:=NULL;
      END IF;  
      RETURN target_;
END Get_Team_Period_Target;
   
FUNCTION Get_Team_Cash_Collected(company_        IN VARCHAR2,
                                 target_period_  IN VARCHAR2) RETURN NUMBER 
IS
                            
      month_          VARCHAR2(05);
      year_           VARCHAR2(10);
      cash_collected_ NUMBER:=0;
   
      CURSOR get_cash_collected(in_company_     VARCHAR2,
                                year_           VARCHAR2,
                                month_          VARCHAR2) IS
      SELECT SUM(a.CURR_AMOUNT)
        FROM payment_per_currency_cu_qry a
       WHERE company =in_company_
         AND SERIES_ID = 'CUPAY'
         AND (TO_CHAR(a.pay_date, 'YYYY') = year_ AND TO_CHAR(a.pay_date, 'MM') = month_)
  AND EXISTS 
      (SELECT 1 FROM LEDGER_TRANSACTION_CU_QRY
        WHERE company = a.company
          AND series_id = a.SERIES_ID
          AND payment_id = a.PAYMENT_ID
          AND LEDGER_COMPANY =a.company
          AND UPPER(Invoice_Party_Type_Group_API.Get_Description(company,'CUSTOMER',Identity_Invoice_Info_API.Get_Group_Id(company,identity,'CUSTOMER'))) NOT LIKE UPPER('%Intercompany%')
          );
   
BEGIN
      IF (target_period_ IS NOT NULL) THEN
         SELECT SUBSTR(target_period_, 1, Instr(target_period_, '-') - 1)
           INTO month_
           FROM dual;
      
         SELECT SUBSTR(target_period_, Instr(target_period_, '-') + 1)
           INTO year_
           FROM dual;
      END IF;
   
      OPEN get_cash_collected(company_, year_, month_);
      FETCH get_cash_collected
       INTO cash_collected_;
      CLOSE get_cash_collected;
   
      RETURN NVL(cash_collected_, 0);
END Get_Team_Cash_Collected;

FUNCTION Check_Cr_Note_Concecutive(company_        IN VARCHAR2,
                                      credit_manager_ IN VARCHAR2,
                                      identity_       IN VARCHAR2) RETURN VARCHAR2
   
IS
      note_state1_ VARCHAR2(100);
      note_state2_ VARCHAR2(100);
      temp_        VARCHAR2(5) := 'FALSE';
      itr_         NUMBER := 0;
   
      CURSOR get_inv_headers(identity_       VARCHAR2,
                             credit_manager_ VARCHAR2,
                             company_        VARCHAR2) IS
         SELECT company, identity, invoice_id
           FROM outgoing_invoice_qry
          WHERE identity = identity_
            and Customer_Credit_Info_API.Get_Manager(company_, identity_) LIKE  NVL(credit_manager_, '%')
            AND company = company_;
   
      CURSOR get_inv_header_notes(company_    VARCHAR2,
                                  identity_   VARCHAR2,
                                  invoice_id_ NUMBER) IS
         SELECT *
           FROM (SELECT Credit_Note_Status_API.Get_Note_Status_Description(company,note_status_id) status
                   FROM invoice_header_notes
                  WHERE company = company_
                    AND identity = identity_
                    AND party_type = 'Customer'
                    AND invoice_id = invoice_id_
                  ORDER BY follow_up_date DESC, note_id DESC)
          WHERE rownum <= 2;
   
BEGIN
   
      FOR rec_ IN get_inv_headers(identity_, credit_manager_, company_) LOOP
         
         FOR note_rec_ IN get_inv_header_notes(rec_.company,
                                               rec_.identity,
                                               rec_.invoice_id) LOOP
            itr_ := itr_ + 1;
            IF (itr_ = 1) THEN
               note_state1_ := note_rec_.status;
            ELSIF (itr_ = 2) THEN
               note_state2_ := note_rec_.status;
            END IF;
         END LOOP;
      
         IF (note_state1_ IS NOT NULL AND (note_state1_ = note_state2_)) THEN
            temp_ := 'TRUE';
            EXIT;
         END IF;      
      END LOOP;
      RETURN temp_;
END Check_Cr_Note_Concecutive;
   
FUNCTION Check_Followup_Date_Mised(
   company_        IN VARCHAR2,
   credit_manager_  IN VARCHAR2,
   identity_       IN VARCHAR2) RETURN VARCHAR2 
IS
   
   inv_state_ VARCHAR2(100);
   inv_due_ DATE;
   follow_up_date_ DATE;
   days_missed_ NUMBER;  
   temp_   VARCHAR2(5) := 'FALSE';
   CURSOR get_inv_headers(identity_     VARCHAR2,
                        credit_manager_ VARCHAR2,
                        company_        VARCHAR2) IS
      SELECT company, identity, invoice_id
        FROM outgoing_invoice_qry
       WHERE identity = identity_
         and Customer_Credit_Info_API.Get_Manager(company_,identity_) LIKE  NVL(credit_manager_, '%')
         AND company = company_;

   CURSOR get_inv_header_notes(company_  VARCHAR2,
                             identity_   VARCHAR2,
                             invoice_id_ NUMBER) IS
      SELECT *
        FROM (SELECT follow_up_date
                FROM invoice_header_notes
               WHERE company = company_
                 AND identity = identity_
                 AND party_type = 'Customer'
                 AND invoice_id = invoice_id_
               ORDER BY follow_up_date DESC, note_id DESC)
       WHERE rownum = 1;
   
   CURSOR get_inv_info(company_ VARCHAR2, identity_ VARCHAR2, invoice_id_ NUMBER )IS
   SELECT inv_state, due_date
     FROM invoice_ledger_item_cu_qry
    WHERE company =company_
      AND (identity = identity_ AND invoice_id = invoice_id_ );
BEGIN

      FOR rec_ IN get_inv_headers(identity_, credit_manager_, company_) LOOP
         OPEN get_inv_header_notes(rec_.company,
                                   rec_.identity,
                                   rec_.invoice_id);
         FETCH get_inv_header_notes
           INTO follow_up_date_;
         CLOSE get_inv_header_notes;
         
         SELECT days_missed
           INTO days_missed_
           FROM (SELECT count(work_day)days_missed
                   FROM WORK_TIME_COUNTER_TAB t
                  WHERE t.calendar_id ='*'
                  AND t.work_day BETWEEN follow_up_date_ AND sysdate 
                  );
         
      OPEN  get_inv_info(rec_.company, rec_.identity,rec_.invoice_id);
     FETCH  get_inv_info 
      INTO inv_state_,inv_due_;
      CLOSE get_inv_info;
                
      IF(inv_state_ NOT IN ('Preliminary', 'Cancelled', 'PaidPosted')AND inv_due_< SYSDATE AND days_missed_ >=4 )THEN 
         temp_ := 'TRUE';
         EXIT;
      END IF;       
      END LOOP;
      
      IF(Check_Cr_Note_Concecutive(company_,credit_manager_, identity_ )='TRUE')THEN
       temp_ := 'TRUE';
      END IF;
      
  RETURN temp_;  
END  Check_Followup_Date_Mised;
--C0368 EntChathI (END)

--C0401 EntPrageG (START)
  
FUNCTION Get_Part_Hierachy__(
   part_no_       IN VARCHAR2,
   contract_      IN VARCHAR2,
   eng_chg_level_ IN VARCHAR2) RETURN VARCHAR2 DETERMINISTIC
IS
   hierachy_ VARCHAR2(32000);      
   phase_in_date_ DATE;
   phase_out_date_ DATE;

   PROCEDURE Build_Part_Hierachy___(
      hierachy_       IN OUT VARCHAR2,
      part_no_            IN VARCHAR2,
      contract_           IN VARCHAR,
      level_              IN NUMBER,
      phase_in_date_      IN DATE,
      phase_out_date_     IN DATE)
   IS
      next_level_ NUMBER;
      CURSOR get_component_part_ IS
         SELECT part_no,
                bom_Type,
                eng_Chg_level,
                alternative_no,
                contract,
                qty_per_assembly,
                eff_phase_in_date,
                eff_phase_out_date
           FROM manuf_structure t
          WHERE component_part = part_no_
            AND contract = contract_
            AND bom_type_db IN ('M', 'T')
            AND eng_Chg_level IN
                              (SELECT eng_chg_level
                                 FROM manuf_structure
                                WHERE component_contract = t.component_contract
                                  AND component_part = t.component_part
                                  AND part_no = t.part_No
                                  AND bom_type_db = t.bom_type_db
                                  AND eff_phase_out_date IS NULL)
            AND (
                 ((eff_phase_out_date IS NULL) AND (phase_out_date_ IS NULL)) OR 
                 ((phase_out_date_ IS NULL) AND (eff_phase_out_date >= phase_in_date_ )) OR
                 ((eff_phase_out_date IS NULL) AND ( phase_in_date_ >= eff_phase_in_date)) OR
                 ((eff_phase_out_date IS NULL) AND (phase_out_date_ >= eff_phase_in_date)) OR
                 (eff_phase_in_date BETWEEN phase_in_date_ AND phase_out_date_ ) OR
                 (eff_phase_out_date BETWEEN phase_in_date_ AND phase_out_date_ ) OR
                 (phase_in_date_ BETWEEN eff_phase_in_date AND eff_phase_out_date) OR
                 (phase_out_date_ BETWEEN eff_phase_in_date AND eff_phase_out_date)
                );
   BEGIN                       
      FOR rec_ IN get_component_part_  LOOP          
        hierachy_ := hierachy_  ||','|| LPAD(level_,(level_),'.') || '#<>#' ||rec_.part_no || '#<>#' ||rec_.qty_per_assembly;
        next_level_ := level_+1;
        Build_Part_Hierachy___(hierachy_,rec_.part_no, contract_,next_level_,rec_.eff_phase_in_date,rec_.eff_phase_out_date);                             
      END LOOP; 
   END Build_Part_Hierachy___;

   FUNCTION Get_Phase_In_Date___(
      part_no_       IN VARCHAR,
      eng_chg_level_ IN VARCHAR2) RETURN DATE DETERMINISTIC
   IS
      phase_in_date_ DATE;
   BEGIN
      SELECT eff_phase_in_date
        INTO phase_in_date_
        FROM part_revision
       WHERE part_no = part_no_
         AND eng_chg_level = eng_chg_level_;
      RETURN phase_in_date_;
   EXCEPTION
      WHEN OTHERS THEN
         RETURN NULL;
   END Get_Phase_In_Date___;

   FUNCTION Get_Phase_Out_Date___(
      part_no_       IN VARCHAR,
      eng_chg_level_ IN VARCHAR2) RETURN DATE DETERMINISTIC
   IS
      phase_in_date_ DATE;
   BEGIN
      SELECT eff_phase_out_date
        INTO phase_in_date_
        FROM part_revision
       WHERE part_no = part_no_
         AND eng_chg_level = eng_chg_level_;
      RETURN phase_in_date_;
   EXCEPTION
      WHEN OTHERS THEN
         RETURN NULL;
   END Get_Phase_Out_Date___;
BEGIN
   phase_in_date_:= Get_Phase_In_Date___('S1180000','1');
   phase_out_date_ := Get_Phase_Out_Date___('S1180000','1');
   hierachy_ := '1' || '#<>#' || part_no_;
   Build_Part_Hierachy___(hierachy_,part_no_,contract_,2,phase_in_date_,phase_out_date_);
   RETURN hierachy_;
END Get_Part_Hierachy__; 

   FUNCTION Get_Phase_In_Date__(
      part_no_       IN VARCHAR,
      eng_chg_level_ IN VARCHAR2) RETURN DATE DETERMINISTIC
   IS
      phase_in_date_ DATE;
   BEGIN
      SELECT eff_phase_in_date
        INTO phase_in_date_
        FROM part_revision
       WHERE part_no = part_no_
         AND eng_chg_level = eng_chg_level_;
      RETURN phase_in_date_;
   EXCEPTION
      WHEN OTHERS THEN
         RETURN NULL;
   END Get_Phase_In_Date__;

FUNCTION Get_Phase_Out_Date__(
   part_no_       IN VARCHAR,
   eng_chg_level_ IN VARCHAR2) RETURN DATE DETERMINISTIC
IS
   phase_in_date_ DATE;
BEGIN
   SELECT eff_phase_out_date
     INTO phase_in_date_
     FROM part_revision
    WHERE part_no = part_no_
      AND eng_chg_level = eng_chg_level_;
   RETURN phase_in_date_;
EXCEPTION
   WHEN OTHERS THEN
      RETURN NULL;
END Get_Phase_Out_Date__;  

--C0401 EntPrageG (END)

-- C0654 EntChamuA (START)
FUNCTION Check_Kitted_Reservation_Date(
   rev_start_date_  IN DATE,
   contract_        IN VARCHAR2) RETURN VARCHAR2
   
IS
   return_         VARCHAR2(6);
   i               NUMBER := 2;
   calender_id_    VARCHAR2(100);
   
   CURSOR get_info(count_row IN NUMBER, rev_start_date_ IN DATE, calender_id_ IN VARCHAR2) IS
      SELECT 'TRUE'
      FROM (SELECT work_day
         FROM (SELECT TRUNC(work_day) AS work_day, counter
            FROM (SELECT work_day, ROWNUM AS counter
               FROM work_time_counter
               WHERE calendar_id = calender_id_
               AND TRUNC(work_day) >= TRUNC(sysdate)
               AND ROWNUM <= 5
            ORDER BY work_day)
               WHERE counter = count_row)
               WHERE work_day = rev_start_date_);
   
BEGIN
   
   FOR l_counter IN 2 .. 5 LOOP
      
      calender_id_ := Site_API.Get_Manuf_Calendar_Id(contract_);
      
      OPEN get_info(i, rev_start_date_, calender_id_);
      FETCH get_info
         INTO return_;
      CLOSE get_info;
      
      IF (return_ IS NULL) THEN
         i := i + 1;
      END IF;
      
      IF (return_ = 'TRUE') THEN
         EXIT;
      END IF;
      
   END LOOP;
   
   RETURN return_;
END Check_Kitted_Reservation_Date;

PROCEDURE Reserve_Kitted_Kanban_Parts

IS
   info_ VARCHAR2(3200);
   attr_ VARCHAR2(3200);
   
   CURSOR get_kitted_parts IS 
      SELECT so.order_no AS order_no,
             so.release_no AS release_no,
             so.sequence_no AS sequence_no,
             soa.line_item_no AS line_item_no
      FROM shop_ord so, shop_material_alloc_uiv soa
      WHERE so.order_no = soa.order_no
      AND so.release_no = soa.release_no
      AND so.sequence_no = soa.sequence_no
      AND so.state IN ('Released', 'Reserved')
      AND soa.issue_type = 'Reserve'
      AND soa.consumption_item = 'Consumed'
      AND soa.qty_required - soa.qty_assigned <> '0'
      AND Check_Kitted_Reservation_Date(TRUNC(so.revised_start_date), so.contract) = 'TRUE';
   
   CURSOR get_kanban_parts IS
      SELECT so.order_no      AS order_no,
             so.release_no    AS release_no,
             so.sequence_no   AS sequence_no,
             soa.line_item_no AS line_item_no
      FROM shop_ord so, shop_material_alloc_uiv soa
      WHERE so.order_no = soa.order_no
      AND so.release_no = soa.release_no
      AND so.sequence_no = soa.sequence_no
      AND so.state IN ('Released', 'Reserved')
      AND soa.issue_type = 'Reserve And Backflush'
      AND soa.consumption_item = 'Consumed'
      AND soa.qty_required - soa.qty_assigned <> '0'
      AND Check_Kanban_Reservation_Date(TRUNC(so.revised_start_date), so.contract) = 'TRUE';
   
BEGIN
   
   FOR rec IN get_kitted_parts LOOP
      Shop_Material_Alloc_API.Reserve(info_, attr_, rec.order_no, rec.release_no, rec.sequence_no, rec.line_item_no);
      Client_SYS.Clear_ATTR(attr_);
   END LOOP;
   
   FOR rec_ IN get_kanban_parts LOOP
      Shop_Material_Alloc_API.Reserve(info_, attr_, rec_.order_no, rec_.release_no, rec_.sequence_no, rec_.line_item_no);
      Client_SYS.Clear_ATTR(attr_);
   END LOOP;
   
END Reserve_Kitted_Kanban_Parts;

FUNCTION Check_Kanban_Reservation_Date(
   rev_start_date_  IN DATE,
   contract_        IN VARCHAR2) RETURN VARCHAR2
IS
   return_         VARCHAR2(6);
   i               NUMBER := 2;
   calender_id_    VARCHAR2(100);
   
   CURSOR get_kanban_info(rev_start_date_ IN DATE, calender_id_ IN VARCHAR2) IS
      SELECT 'TRUE'
      FROM (SELECT work_day
         FROM (SELECT TRUNC(work_day) AS work_day, counter
            FROM (SELECT work_day, ROWNUM AS counter
               FROM work_time_counter
               WHERE calendar_id = calender_id_
               AND TRUNC(work_day) >=
               TRUNC(sysdate)
               AND ROWNUM <= 2
            ORDER BY work_day)
               WHERE counter = 2)
               WHERE work_day = rev_start_date_);
BEGIN   
   calender_id_ := Site_API.Get_Manuf_Calendar_Id(contract_);
   
   OPEN get_kanban_info(rev_start_date_, calender_id_);
   FETCH get_kanban_info
      INTO return_;
   CLOSE get_kanban_info;
   RETURN return_;
END Check_Kanban_Reservation_Date; 
-- C0654 EntChamuA (END)

-- C0449 EntChamuA (START)
FUNCTION Get_Contract_Id(
   mch_code_ VARCHAR2,
   contract_ VARCHAR2) RETURN VARCHAR2
IS
   return_value_ VARCHAR2(100);
   
   CURSOR get_contract_id (mch_code_ IN VARCHAR2, contract_ IN VARCHAR2) IS
      SELECT DISTINCT cp.contract_id
      FROM psc_contr_product_uiv cp
      WHERE connection_type_db IN ('EQUIPMENT', 'CATEGORY', 'PART')
      AND (EXISTS (SELECT 1
         FROM psc_srv_line_objects t
         WHERE t.contract_id = cp.contract_id
         AND t.line_no = cp.line_no
         AND (t.mch_code, t.mch_contract) IN
         (SELECT mch_code, contract
            FROM Equipment_All_Object_Uiv
            START WITH mch_code = mch_code_
            AND contract = contract_
            CONNECT BY PRIOR mch_code = sup_mch_code
            AND PRIOR contract = sup_contract)));

BEGIN
   
   OPEN get_contract_id(mch_code_, contract_);
   FETCH get_contract_id INTO return_value_;
   CLOSE get_contract_id;
   RETURN return_value_;
END Get_Contract_Id;
--  C0049 EntChamuA (END)

--240521 ISURUG Calculate Shift Admin Time (START)
FUNCTION Calculate_Shift_Admin_Time (emp_name_ IN VARCHAR2) RETURN NUMBER
IS
   employee_name_            VARCHAR2(2000) := '%' || emp_name_ || '%';
   survey_daily_vec_check_   NUMBER;
   survey_monthly_vec_check_ NUMBER;
   survey_non_wo_mileage_    NUMBER;
   survey_non_wo_time_       NUMBER;
   shift_admin_time_         NUMBER;
   
   CURSOR daily_vec_check IS
      SELECT NVL(SUM(v.ANSWER) / 60, 0)
      FROM JT_TASK_SURVEY_ANSWERS v 
      WHERE v.survey_id = 'DAILY_VEC_CHECK' 
        AND SURVEY_QUESTION_API.Get_Question_No(v.survey_id, v.question_id) = 5
        AND COMPANY_EMP_API.Get_Name(v.company_id, v.emp_no) LIKE NVL(employee_name_,'%');
        
   CURSOR monthly_vec_check IS
      SELECT NVL(SUM(v.ANSWER ) / 60, 0)
      FROM JT_TASK_SURVEY_ANSWERS v 
      WHERE v.SURVEY_ID = 'MON_VEC_CHECK' 
        AND SURVEY_QUESTION_API.Get_Question_No(v.survey_id, v.question_id) = 14
        AND COMPANY_EMP_API.Get_Name(v.company_id, v.emp_no) LIKE NVL(employee_name_,'%');
        
   CURSOR non_wo_mileage IS
      SELECT NVL(SUM((t2.Answer2 - t1.Answer1) * 24), 0)
      FROM (
         SELECT TO_DATE(v.ANSWER, 'YYYY-MM-DD-HH24.MI.SS') Answer1
         FROM JT_TASK_SURVEY_ANSWERS v 
         WHERE v.SURVEY_ID = 'NON WO MILEAGE' 
           AND SURVEY_QUESTION_API.Get_Question_No(v.survey_id, v.question_id) = 2
           AND COMPANY_EMP_API.Get_Name(v.company_id, v.emp_no) LIKE NVL(employee_name_,'%')
      ) t1,
      (
         SELECT TO_DATE(v.ANSWER, 'YYYY-MM-DD-HH24.MI.SS') Answer2
         FROM JT_TASK_SURVEY_ANSWERS v 
         WHERE v.SURVEY_ID = 'NON WO MILEAGE' 
           AND SURVEY_QUESTION_API.Get_Question_No(v.survey_id, v.question_id) = 4
           AND COMPANY_EMP_API.Get_Name(v.company_id, v.emp_no) LIKE NVL(employee_name_,'%')
      ) t2;
      
   CURSOR non_wo_time IS
      SELECT NVL(SUM((t2.Answer2 - t1.Answer1) * 24), 0)
      FROM (
         SELECT TO_DATE(v.ANSWER, 'YYYY-MM-DD-HH24.MI.SS') Answer1 
         FROM JT_TASK_SURVEY_ANSWERS v 
         WHERE v.SURVEY_ID = 'NON WO TIME' 
           AND SURVEY_QUESTION_API.Get_Question_No(v.survey_id, v.question_id) = 2
           AND COMPANY_EMP_API.Get_Name(v.company_id, v.emp_no) LIKE NVL(employee_name_,'%')
      ) t1,
      (
         SELECT TO_DATE(v.ANSWER, 'YYYY-MM-DD-HH24.MI.SS') Answer2 
         FROM JT_TASK_SURVEY_ANSWERS v 
         WHERE v.SURVEY_ID = 'NON WO TIME' 
           AND SURVEY_QUESTION_API.Get_Question_No(v.survey_id, v.question_id) = 3
           AND COMPANY_EMP_API.Get_Name(v.company_id, v.emp_no) LIKE NVL(employee_name_,'%')
      ) t2;
BEGIN
   OPEN daily_vec_check;
   FETCH daily_vec_check INTO survey_daily_vec_check_;
   CLOSE daily_vec_check; 
   
   OPEN monthly_vec_check;
   FETCH monthly_vec_check INTO survey_monthly_vec_check_;
   CLOSE monthly_vec_check; 
   
   OPEN non_wo_mileage;
   FETCH non_wo_mileage INTO survey_non_wo_mileage_;
   CLOSE non_wo_mileage; 
   
   OPEN non_wo_time;
   FETCH non_wo_time INTO survey_non_wo_time_;
   CLOSE non_wo_time; 

   shift_admin_time_ := survey_daily_vec_check_ + survey_monthly_vec_check_ + survey_non_wo_mileage_ + survey_non_wo_time_;

   RETURN shift_admin_time_;
END Calculate_Shift_Admin_Time;
--240521 ISURUG Calculate Shift Admin Time (END)

--240521 ISURUG Calculate Idle Time (START)
FUNCTION Calculate_Idle_Time (emp_name_ IN VARCHAR2, sdate_ IN VARCHAR2, edate_ IN VARCHAR2) RETURN NUMBER
IS
   employee_name_         VARCHAR2(2000) := '%' || emp_name_ || '%';
   start_date_            VARCHAR2(2000) := '%' || sdate_    || '%';
   stop_date_             VARCHAR2(2000) := '%' || edate_    || '%';
   answer_work_hours_     NUMBER;
   answer_travel_hours_   NUMBER;
   shift_admin_time_      NUMBER;
   answer_shift_end_time_ NUMBER;
   idle_time_             NUMBER;
   
   CURSOR work_hours IS
      SELECT NVL(SUM(v.work_hours), 0) work_hours
      FROM JT_TASK_CLOCKING_UIV v
      WHERE v.clocking_category = 'Work'
        AND v.employee_name LIKE NVL(employee_name_,'%')
        AND TO_CHAR(v.start_time, 'DD/MM/YYYY HH:MI:SS') LIKE NVL(start_date_,'%')
        AND TO_CHAR(v.stop_time, 'DD/MM/YYYY HH:MI:SS') LIKE NVL(stop_date_,'%');
        
   CURSOR travel_hours IS
      SELECT NVL(SUM(v.work_hours), 0) travel_hours
      FROM JT_TASK_CLOCKING_UIV v
      WHERE v.clocking_category = 'Travel'
        AND v.employee_name LIKE NVL(employee_name_,'%')
        AND TO_CHAR(v.start_time, 'DD/MM/YYYY HH:MI:SS') LIKE NVL(start_date_,'%')
        AND TO_CHAR(v.stop_time, 'DD/MM/YYYY HH:MI:SS') LIKE NVL(stop_date_,'%');
        
    CURSOR shift_end_time_ IS
       SELECT NVL((et.end_time - st1.start_time) * 24, 0) + NVL((et.end_time - st2.start_time) * 24, 0)
       FROM (
          SELECT v.date_created end_time
          FROM (
             SELECT v.*
             FROM JT_TASK_SURVEY_ANSWERS v
             ORDER BY v.answer_set DESC, v.emp_no ASC, v.date_created ASC, v.answer_id ASC
          ) v 
          WHERE v.SURVEY_ID = 'END_MILEAGE'
            AND COMPANY_EMP_API.Get_Name(v.company_id, v.emp_no) LIKE NVL(employee_name_,'%') 
            AND rownum = 1
       ) et,
       (
          SELECT v1.date_created start_time
          FROM (
             SELECT v.*
             FROM JT_TASK_SURVEY_ANSWERS v
             ORDER BY v.answer_set DESC, v.emp_no ASC, v.date_created ASC, v.answer_id ASC
          ) v1
          WHERE v1.SURVEY_ID = 'DAILY_VEC_CHECK'
            AND COMPANY_EMP_API.Get_Name(v1.company_id, v1.emp_no) LIKE NVL(employee_name_,'%') 
            AND rownum = 1
       ) st1,
       (
          SELECT v2.date_created start_time
          FROM (
             SELECT v.*
             FROM JT_TASK_SURVEY_ANSWERS v
             ORDER BY v.answer_set DESC, v.emp_no ASC, v.date_created ASC, v.answer_id ASC
          ) v2 
          WHERE v2.SURVEY_ID = 'MON_VEC_CHECK'
            AND COMPANY_EMP_API.Get_Name(v2.company_id, v2.emp_no) LIKE NVL(employee_name_,'%')
            AND rownum = 1
       ) st2; 
BEGIN
   OPEN work_hours;
   FETCH work_hours INTO answer_work_hours_;
   CLOSE work_hours;
   
   OPEN travel_hours;
   FETCH travel_hours INTO answer_travel_hours_;
   CLOSE travel_hours;
   
   OPEN shift_end_time_;
   FETCH shift_end_time_ INTO answer_shift_end_time_;
   CLOSE shift_end_time_;

   shift_admin_time_ := Calculate_Shift_Admin_Time(employee_name_);
   
   IF answer_shift_end_time_ IS NULL THEN
      answer_shift_end_time_ := 0;
   END IF;

   idle_time_ := answer_shift_end_time_ - (answer_work_hours_ + answer_travel_hours_ + shift_admin_time_);

   RETURN idle_time_;
END Calculate_Idle_Time;
--240521 ISURUG Calculate Idle Time (END)

-- C209 EntMahesR (START)
-- Note that here contract 2011 has hardcoded because it is the only manufacturing site
-- and this will be mentioned in delivery notes as well
FUNCTION Get_Occurrence_Count(
   component_part_no_   IN VARCHAR2,
   prod_structure_list_  IN VARCHAR2) RETURN NUMBER
IS
   product_structures_   Utility_SYS.STRING_TABLE;
   no_of_structures_     NUMBER;
   count_                NUMBER := 0;
   no_of_occurences_     NUMBER := 0;
   found_                NUMBER := 0;
   
   CURSOR check_exist(part_no_ VARCHAR2) IS
      SELECT 1 FROM(SELECT component_part
                    FROM prod_structure 
                    START WITH part_no = part_no_
                    CONNECT BY PRIOR component_part = part_no
                    AND contract = '2011')
      WHERE component_part = component_part_no_;
   
BEGIN
   Utility_SYS.Tokenize(prod_structure_list_, ';', product_structures_, no_of_structures_);
   WHILE (count_ < no_of_structures_) LOOP       
      count_ := count_+ 1;
      OPEN check_exist(product_structures_(count_));
      FETCH check_exist INTO found_;
      IF check_exist%FOUND THEN
         no_of_occurences_ := no_of_occurences_ + found_;
      END  IF;
      CLOSE check_exist;
   END LOOP; 
   RETURN no_of_occurences_;
END Get_Occurrence_Count; 

-- Note that here contract '2011' has hardcoded because it is the only manufacturing site.
-- Also considering Structure Type 'Manufacturing'('M') only in the product structure and 
-- this will be mentioned in delivery notes as well.
FUNCTION Check_Phrase_In_Out_Dates(
   part_no_             IN VARCHAR2,   
   revision_no_         IN VARCHAR2,
   required_date_       IN DATE) RETURN NUMBER
IS   
   eff_phase_in_date_    DATE;
   eff_phase_out_date_   DATE;
   
   CURSOR get_dates IS
      SELECT eff_phase_in_date, eff_phase_out_date
      FROM prod_structure_head
      WHERE contract = '2011' 
      AND part_no = part_no_
      AND eng_chg_level =  revision_no_
      AND bom_type_db = 'M';      
BEGIN
   OPEN get_dates;
   FETCH get_dates INTO eff_phase_in_date_, eff_phase_out_date_;
   CLOSE get_dates;  
   IF (TRUNC(eff_phase_in_date_) > TRUNC(SYSDATE)) THEN
      RETURN 0;
   ELSIF (TRUNC(required_date_) > TRUNC(eff_phase_in_date_) AND TRUNC(required_date_) > TRUNC(eff_phase_out_date_)) THEN 
      RETURN 0;
   ELSIF (Prod_Struct_Alternate_API.Get_State_Db('2011', part_no_, revision_no_, 'M', '*' ) != 'Buildable') THEN  
      RETURN 0; 
   END IF;   
   RETURN 1;   
END Check_Phrase_In_Out_Dates;

FUNCTION Get_Stock_Available_Per_Site (
   contract_   IN VARCHAR2,
   part_no_    IN VARCHAR2 ) RETURN NUMBER
IS 
   total_qty_available_  NUMBER;
   CURSOR get_available_total IS  
      SELECT SUM(qty_onhand - qty_reserved)
      FROM inventory_part_in_stock_uiv
      WHERE contract = contract_
      AND part_no = part_no_
      GROUP BY contract, part_no; 
BEGIN   
   OPEN get_available_total;
   FETCH get_available_total INTO total_qty_available_;
   CLOSE get_available_total;    
   RETURN total_qty_available_;    
END Get_Stock_Available_Per_Site;

-- Note that here contract 2011 has hardcoded because it is the only manufacturing site
-- and this will be mentioned in delivery notes as well
FUNCTION Get_Total_Mrp_Demand (
   part_no_         IN VARCHAR2,
   required_date_   IN DATE ) RETURN NUMBER
IS 
   mrp_total_demand_  NUMBER;
   CURSOR get_mrp_total_demand IS  
      SELECT SUM(projected_onhand)
      FROM mrp_part_supply_demand_all     
      WHERE contract = '2011'
      AND part_no = part_no_
      AND required_date BETWEEN TRUNC(SYSDATE) AND required_date_;      
BEGIN   
   OPEN get_mrp_total_demand;
   FETCH get_mrp_total_demand INTO mrp_total_demand_;
   CLOSE get_mrp_total_demand; 
   RETURN mrp_total_demand_;     
END Get_Total_Mrp_Demand;

-- Note that here contract 2011 has hardcoded because it is the only manufacturing site
-- and this will be mentioned in delivery notes as well
FUNCTION Get_Total_Mrp_Specific_Demand(
   part_no_              IN VARCHAR2,
   prod_structure_list_  IN VARCHAR2,
   required_date_        IN DATE ) RETURN NUMBER
IS
   mrp_specific_total_demand_  NUMBER := 0;
   mrp_specific_demand_        NUMBER := 0;   
   product_structures_         Utility_SYS.STRING_TABLE;
   no_of_structures_           NUMBER;
   count_                      NUMBER := 0;
   CURSOR get_mrp_specific_total_demand(part_no_ VARCHAR2, parent_part_no_ VARCHAR2) IS  
      SELECT SUM(projected_onhand)
      FROM mrp_part_supply_demand_all     
      WHERE contract = '2011'
      AND part_no = part_no_
      AND parent_part_no = parent_part_no_
      AND required_date BETWEEN TRUNC(SYSDATE) AND required_date_;      
BEGIN  
   Utility_SYS.Tokenize(prod_structure_list_, ';', product_structures_, no_of_structures_);
   WHILE (count_ < no_of_structures_) LOOP       
      count_ := count_+ 1;
      OPEN get_mrp_specific_total_demand(part_no_, product_structures_(count_));
      FETCH get_mrp_specific_total_demand INTO mrp_specific_demand_;
      CLOSE get_mrp_specific_total_demand;
      mrp_specific_total_demand_ := mrp_specific_total_demand_ + NVL(mrp_specific_demand_, 0);
   END LOOP;  
   RETURN mrp_specific_total_demand_;     
END Get_Total_Mrp_Specific_Demand;

-- Note that here contract 2011 has hardcoded because it is the only manufacturing site
-- and this will be mentioned in delivery notes as well
FUNCTION Get_Total_Sales_Mrp_Demand (
   part_no_         IN VARCHAR2,
   required_date_   IN DATE ) RETURN NUMBER
IS 
   mrp_total_sales_demand_  NUMBER;
   -- Note that here mrp_source_db '1' represents Cust Order entries and 
   -- mrp_source_db '27' represents Distribution Order entries 
   CURSOR get_mrp_total_sales_demand IS  
      SELECT SUM(projected_onhand)
      FROM mrp_part_supply_demand_all     
      WHERE contract = '2011'
      AND part_no = part_no_
      AND (mrp_source_db = '1' OR mrp_source_db ='27')
      AND required_date BETWEEN TRUNC(SYSDATE) AND required_date_;        
BEGIN   
   OPEN get_mrp_total_sales_demand;
   FETCH get_mrp_total_sales_demand INTO mrp_total_sales_demand_;
   CLOSE get_mrp_total_sales_demand; 
   RETURN mrp_total_sales_demand_;     
END Get_Total_Sales_Mrp_Demand;

-- C209 EntMahesR (END)

-- C0321 EntChamuA (START)
FUNCTION Attach_Docs_In_PO (xml_ CLOB) RETURN CLOB

IS
   
BEGIN
   RETURN xml_;

END Attach_Docs_In_PO;
-- C0321 EntChamuA (END)

--C449 EntChamuA (START)
FUNCTION Get_Identity(
   mch_code_ VARCHAR2,
   contract_ VARCHAR2) RETURN VARCHAR2
IS
   return_value_ VARCHAR2(100);
  
   CURSOR get_identity(mch_code_ IN VARCHAR2, contract_ IN VARCHAR2) IS
      SELECT identity
        FROM equipment_object_party_uiv
       WHERE mch_code = mch_code_
         AND contract = contract_;
BEGIN
   OPEN get_identity(mch_code_, contract_);
   FETCH get_identity INTO return_value_;
   IF(get_identity%NOTFOUND)THEN
      return_value_ := '';
   END IF;
   CLOSE get_identity;
   
   RETURN return_value_;
END Get_Identity;
--C449 EntChamuA (END)
-------------------- LU  NEW METHODS -------------------------------------

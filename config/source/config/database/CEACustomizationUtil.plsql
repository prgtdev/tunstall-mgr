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

  PROCEDURE Bulk_Update_Invoice_Header_Notes__(company_  IN VARCHAR2,
                                               identity_ IN VARCHAR2) IS
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
      --@ApproveDynamicStatement('2021-02-16',entpragg);
      EXECUTE IMMEDIATE stmt_
        USING IN company_, IN identity_;
    ELSE
      Error_Sys.Appl_General(lu_name_, 'Custom Objects are not published!');
    END IF;
  END Bulk_Update_Invoice_Header_Notes__;

  FUNCTION Get_Update_Follow_Up_Date_Sql__(calender_id_    IN VARCHAR2,
                                           company_        IN VARCHAR2,
                                           invoice_id_     IN VARCHAR2,
                                           note_id_        IN NUMBER,
                                           note_status_id_ IN NUMBER)
    RETURN VARCHAR2 IS
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

  PROCEDURE Update_Follow_Up_Date__(calender_id_    IN VARCHAR2,
                                    company_        IN VARCHAR2,
                                    invoice_id_     IN VARCHAR2,
                                    note_id_        IN NUMBER,
                                    note_status_id_ IN NUMBER) IS
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
      --@ApproveDynamicStatement('2021-02-24',entpragg);
      EXECUTE IMMEDIATE stmt_;
    ELSE
      Error_Sys.Appl_General(lu_name_, 'Custom Objects are not published!');
    END IF;
  END Update_Follow_Up_Date__;

  --C0446
  PROCEDURE Create_Equipment_Object__(order_no_ IN VARCHAR2) IS
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
             CF$_OBJECT_ID,
             BUY_QTY_DUE,
             CF$_OBJECT_CREATED,
             objid
        FROM customer_order_line_cfv a
       WHERE order_no = order_no_
         AND state IN ('Delivered', 'Invoiced/Closed');
  
    CURSOR check_sales_part(catalog_ IN VARCHAR2) IS
      SELECT 1
        FROM sales_part_cfv
       WHERE catalog_no = catalog_
         AND contract = '2013'
         AND CF$_SERVICEABLE_DB = 'TRUE';
  
    --Check serialised
    CURSOR check_serialized(catalog_ IN VARCHAR2) IS
      SELECT 1 FROM part_serial_catalog WHERE part_no = catalog_;
  
  BEGIN
  
    FOR rec_ IN get_co_line LOOP
      -- To check sales part 
      OPEN check_sales_part(rec_.catalog_no);
      FETCH check_sales_part
        INTO temps_;
      CLOSE check_sales_part;
    
      object_id_ := SUBSTR(rec_.CF$_OBJECT_ID,
                           INSTR(rec_.CF$_OBJECT_ID, '^') + 1,
                           length(rec_.CF$_OBJECT_ID));
    
      IF (object_id_ is not null) THEN
        --Create objects only if site 2013 and serviceable
        IF (temps_ = 1) THEN
          OPEN check_serialized(rec_.catalog_no);
          FETCH check_serialized
            INTO seriliazed_;
        
          IF (check_serialized%FOUND) THEN
          
            Create_Serial_Object__(rec_.catalog_no,
                                   order_no_,
                                   rec_.line_no,
                                   rec_.line_item_no,
                                   rec_.rel_no,
                                   rec_.cust_warranty_id,
                                   object_id_,
                                   rec_.CF$_OBJECT_CREATED,
                                   rec_.objid,
                                   rec_.real_ship_date);
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
                                      rec_.CF$_OBJECT_CREATED,
                                      rec_.objid);
          END IF;
          CLOSE check_serialized;
        END IF;
      END IF;
    END LOOP;
  END Create_Equipment_Object__;

  PROCEDURE Create_Functional_Object_(catalog_no_     IN VARCHAR2,
                                      order_no_       IN VARCHAR2,
                                      line_no_        IN VARCHAR2,
                                      rel_no_         IN VARCHAR2,
                                      real_ship_date_ IN DATE,
                                      warranty_id_    IN VARCHAR2,
                                      object_id_      IN VARCHAR2,
                                      catalog_desc_   IN VARCHAR2,
                                      contract_       IN VARCHAR2,
                                      buy_qty_due_    IN NUMBER,
                                      object_created_ IN VARCHAR2,
                                      col_objid_      IN VARCHAR2) IS
  
    status_      VARCHAR2(100);
    type_        VARCHAR2(100);
    unit_        VARCHAR2(100);
    values_      VARCHAR2(10);
    value_       NUMBER;
    lot_no_      VARCHAR2(20);
    func_obj_    VARCHAR2(3200);
    info_        VARCHAR2(3200);
    objid_       VARCHAR2(3200);
    objversion_  VARCHAR2(3200);
    attr_        VARCHAR2(3200);
    warr_objkey_ VARCHAR2(3200);
  
    --to get lot number and qty 
    CURSOR get_lot_batch_dets(order_no_   IN VARCHAR2,
                              line_no_    IN VARCHAR2,
                              rel_no_     IN VARCHAR2,
                              catalog_no_ IN VARCHAR2,
                              contract_   IN VARCHAR2) IS
    
      SELECT lot_batch_no
        FROM lot_batch_history a
       WHERE ORDER_REF1 = order_no_
         AND ORDER_REF2 = line_no_
         AND ORDER_REF3 = rel_no_
         AND contract = contract_
         AND part_no = catalog_no_
         AND order_type_db = 'CUST ORDER';
  
    --get warranty type 
    CURSOR get_warranty_type(warranty_id_ IN VARCHAR2) IS
      SELECT warranty_type_id
        FROM cust_warranty_type
       WHERE warranty_id = warranty_id_;
  
    -- get warranty details 
    CURSOR get_warranty_info(warranty_id_ IN VARCHAR2,
                             type_id_     IN VARCHAR2) IS
      SELECT Warranty_Condition_API.Get_Time_Unit(condition_id), MAX_VALUE
        FROM CUST_WARRANTY_CONDITION
       WHERE WARRANTY_ID = warranty_id_
         AND WARRANTY_TYPE_ID = type_id_;
  
    --get objkey of warranty 
    CURSOR get_objkey(values_ IN VARCHAR2) IS
      select OBJKEY from WARRANTY_CLV where CF$_NAME = values_;
  
  BEGIN
    IF (object_id_ is not null AND object_created_ = 'False') THEN
    
      status_ := Cust_Warranty_API.Get_Objstate(warranty_id_);
      OPEN get_lot_batch_dets(order_no_,
                              line_no_,
                              rel_no_,
                              catalog_no_,
                              contract_);
      FETCH get_lot_batch_dets
        INTO lot_no_;
      CLOSE get_lot_batch_dets;
    
      OPEN get_warranty_type(warranty_id_);
      FETCH get_warranty_type
        INTO type_;
      CLOSE get_warranty_type;
    
      OPEN get_warranty_info(warranty_id_, type_);
      FETCH get_warranty_info
        INTO unit_, value_;
      CLOSE get_warranty_info;
    
      IF (unit_ = 'Year') THEN
        value_ := value_ * 12;
      END IF;
    
      values_   := value_ || 'M';
      func_obj_ := catalog_no_ || '_' || order_no_ || '_' || rel_no_;
    
      Client_SYS.Add_To_Attr('MCH_CODE', func_obj_, attr_);
      Client_SYS.Add_To_Attr('MCH_NAME', catalog_desc_, attr_);
      Client_SYS.Add_To_Attr('CONTRACT', '2013', attr_);
      Client_SYS.Add_To_Attr('OBJ_LEVEL',
                             '900_FUNCTIONAL_EQUIPMENT',
                             attr_);
      Client_SYS.Add_To_Attr('PART_NO', catalog_no_, attr_);
      Client_SYS.Add_To_Attr('PRODUCTION_DATE', real_ship_date_, attr_);
      Client_SYS.Add_To_Attr('SUP_MCH_CODE', object_id_, attr_);
      Client_SYS.Add_To_Attr('SUP_CONTRACT', '2013', attr_);
    
      EQUIPMENT_FUNCTIONAL_API.New__(info_,
                                     objid_,
                                     objversion_,
                                     attr_,
                                     'DO');
    
      Client_Sys.Clear_Attr(attr_);
    
      IF (values_ is not null) THEN
        OPEN get_objkey(values_);
        FETCH get_objkey
          INTO warr_objkey_;
        CLOSE get_objkey;
      END IF;
    
      Client_SYS.Add_To_Attr('CF$_LOT_BATCH_NO', lot_no_, attr_);
      Client_SYS.Add_To_Attr('CF$_EQUIPMENT_QUANTITY', buy_qty_due_, attr_);
      Client_SYS.Add_To_Attr('CF$_SERVICE_WARRANTY', warr_objkey_, attr_);
      EQUIPMENT_FUNCTIONAL_CFP.Cf_Modify__(info_, objid_, attr_, ' ', 'DO');
    
      info_ := null;
      Client_Sys.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('CF$_OBJECT_CREATED_DB', 'TRUE', attr_);
    
      Customer_Order_Line_CFP.Cf_Modify__(info_,
                                          col_objid_,
                                          attr_,
                                          ' ',
                                          'DO');
    END IF;
  END Create_Functional_Object_;

  PROCEDURE Create_Serial_Object__(catalog_no_     IN VARCHAR2,
                                   order_no_       IN VARCHAR2,
                                   line_no_        IN VARCHAR2,
                                   line_item_no_   IN VARCHAR2,
                                   rel_no_         IN VARCHAR2,
                                   warranty_id_    IN VARCHAR2,
                                   object_id_      IN VARCHAR2,
                                   object_created_ IN VARCHAR2,
                                   col_objid_      IN VARCHAR2,
                                   real_ship_date_ IN DATE) IS
  
    max_value_   NUMBER;
    unit_        VARCHAR2(10);
    sstep1_      VARCHAR2(100);
    sstep2_      VARCHAR2(100);
    sstep3_      VARCHAR2(100);
    sstep4_      VARCHAR2(50);
    mch_code_    VARCHAR2(2000);
    site_        VARCHAR2(2000);
    info_        VARCHAR2(3200);
    objid_       VARCHAR2(3200);
    objversion_  VARCHAR2(3200);
    attr_        VARCHAR2(3200);
    contract_    VARCHAR2(50) := '2013';
    warr_objkey_ VARCHAR2(3200);
    values_      VARCHAR2(10);
  
    --To get serial objects qty per CO line
    CURSOR get_serial_objects_col(catalog_  IN VARCHAR2,
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
         AND transaction_description like '%Delivered on customer order%';
  
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
      select OBJKEY from WARRANTY_CLV where CF$_NAME = values_;
  
  BEGIN
    IF (object_id_ is not null AND object_created_ = 'False') THEN
      --create serial object
    
      FOR obj IN get_serial_objects_col(catalog_no_,
                                        order_no_,
                                        line_no_,
                                        line_item_no_,
                                        rel_no_) LOOP
      
        --For each object get warranty details
        OPEN get_serial_warranty_dets(catalog_no_,
                                      warranty_id_,
                                      obj.serial_no);
        FETCH get_serial_warranty_dets
          INTO max_value_, unit_;
        CLOSE get_serial_warranty_dets;
      
        IF (unit_ = 'Year') THEN
          max_value_ := max_value_ * 12;
        END IF;
      
        values_ := max_value_ || 'M';
      
        IF (values_ is not null) THEN
          OPEN get_objkey(values_);
          FETCH get_objkey
            INTO warr_objkey_;
          CLOSE get_objkey;
        END IF;
      
        sstep1_ := Equipment_Serial_API.Check_Serial_Exist(catalog_no_,
                                                           obj.serial_no);
      
        sstep2_ := MAINTENANCE_SITE_UTILITY_API.Is_User_Allowed_Site(contract_);
      
        sstep3_ := Part_Serial_Catalog_API.Get_Objstate(catalog_no_,
                                                        obj.serial_no);
      
        sstep4_ := Part_Serial_Catalog_API.Delivered_To_Internal_Customer(catalog_no_,
                                                                          obj.serial_no);
      
        Equipment_Serial_API.Create_Maintenance_Aware(catalog_no_,
                                                      obj.serial_no,
                                                      contract_,
                                                      NULL,
                                                      'FALSE');
      
        Equipment_Serial_API.Get_Obj_Info_By_Part(site_,
                                                  mch_code_,
                                                  catalog_no_,
                                                  obj.serial_no);
      
        Equipment_Object_API.Move_From_Invent_To_Facility(contract_,
                                                          object_id_,
                                                          catalog_no_,
                                                          obj.serial_no,
                                                          mch_code_);
      
        OPEN modify_serial_obj_date(mch_code_, obj.serial_no);
        FETCH modify_serial_obj_date
          INTO objid_, objversion_;
        CLOSE modify_serial_obj_date;
      
        Client_Sys.Add_To_Attr('PRODUCTION_DATE', real_ship_date_, attr_);
      
        EQUIPMENT_SERIAL_API.Modify__(info_,
                                      objid_,
                                      objversion_,
                                      attr_,
                                      'DO');
      
        Client_Sys.Clear_Attr(attr_);
      
        Client_Sys.Add_To_Attr('CF$_SERVICE_WARRANTY', warr_objkey_, attr_);
      
        EQUIPMENT_SERIAL_CFP.Cf_Modify__(info_, objid_, attr_, ' ', 'DO');
      
        info_ := null;
        Client_Sys.Clear_Attr(attr_);
        Client_SYS.Add_To_Attr('CF$_OBJECT_CREATED_DB', 'TRUE', attr_);
      
        Customer_Order_Line_CFP.Cf_Modify__(info_,
                                            col_objid_,
                                            attr_,
                                            ' ',
                                            'DO');
      END LOOP;
    END IF;
  END Create_Serial_Object__;

  PROCEDURE Create_Ext_Load_Forecast(info_         IN OUT NOCOPY VARCHAR2,
                                     load_file_id_ IN NUMBER) IS
  
    objversion_ VARCHAR2(3200);
    objid_      VARCHAR2(3200);
    infos_      VARCHAR2(3200);
    attr_       VARCHAR2(3200);
    flag_       VARCHAR2(3200);
    row_state_  VARCHAR2(2);
    error_text_ VARCHAR2(2000);
  
    CURSOR get_info_ IS
      SELECT *
        FROM ext_file_trans_tab b
       WHERE load_file_id = load_file_id_
         AND row_state = '2'
         AND EXISTS (SELECT *
                FROM COMPANY_SITE_TAB a
               WHERE a.CONTRACT = b.C1
                 and a.COMPANY = b.C4)
       ORDER BY row_no;
  
    CURSOR get_forecast_details(contract_     VARCHAR2,
                                part_no_      VARCHAR2,
                                png_          VARCHAR2,
                                ms_set_       NUMBER,
                                activity_seq_ NUMBER,
                                ms_date_      DATE) IS
      SELECT objversion, objid, SYSGEN_FLAG
        FROM LEVEL_1_FORECAST a
       WHERE contract = contract_
         AND part_no = part_no_
         AND png = png_
         AND ms_set = ms_set_
         AND activity_seq = activity_seq_
         AND ms_date = ms_date_;
  BEGIN
  
    FOR rec IN get_info_ LOOP
      BEGIN
        TRACE_SYS.MESSAGE(rec.file_line);
      
        OPEN get_forecast_details(rec.c1,
                                  rec.c2,
                                  rec.c3,
                                  rec.n1,
                                  rec.n2,
                                  rec.d1);
        FETCH get_forecast_details
          INTO objversion_, objid_, flag_;
        CLOSE get_forecast_details;
        Dbms_Output.put_line('AAA objversion_-->' || objversion_);
        Dbms_Output.put_line('AAA objid_-->' || objid_);
      
        IF (objversion_ is null and objid_ is null) THEN
          Client_Sys.Clear_Attr(attr_);
        
          TRACE_SYS.MESSAGE('NEW' || rec.d1);
        
          Client_Sys.Add_To_Attr('CONTRACT', rec.c1, attr_);
          Client_Sys.Add_To_Attr('MS_DATE', rec.d1, attr_);
          Client_Sys.Add_To_Attr('PART_NO', rec.c2, attr_);
          Client_Sys.Add_To_Attr('PNG', rec.c3, attr_);
          Client_Sys.Add_To_Attr('ACTIVITY_SEQ', rec.n2, attr_);
          Client_Sys.Add_To_Attr('FORECAST_LEV0', rec.n3, attr_);
          Client_Sys.Add_To_Attr('SYSGEN_FLAG', flag_, attr_);
          Dbms_Output.put_line('attr_-->' || attr_);
        
          LEVEL_1_FORECAST_API.New__(infos_,
                                     objid_,
                                     objversion_,
                                     attr_,
                                     'DO');
        ELSE
          Client_Sys.Clear_Attr(attr_);
          Client_Sys.Add_To_Attr('SYSGEN_FLAG', 'Proposed', attr_);
        
          TRACE_SYS.MESSAGE('MODIFY  rec.d1-->' || rec.d1);
        
          Client_Sys.Add_To_Attr('FORECAST_LEV0', rec.n3, attr_);
          LEVEL_1_FORECAST_API.Modify__(infos_,
                                        objid_,
                                        objversion_,
                                        attr_,
                                        'DO');
        END IF;
        Ext_File_Trans_API.Update_Row_State(load_file_id_, rec.row_no, '3');
      EXCEPTION
        WHEN OTHERS THEN
          TRACE_SYS.MESSAGE('EXCEPTION');
          error_text_ := SUBSTR(SQLERRM, 1, 2000);
          Ext_File_Trans_API.Update_Row_State(load_file_id_,
                                              rec.row_no,
                                              '5',
                                              error_text_);
      END;
    END LOOP;
  
  END Create_Ext_Load_Forecast;

  FUNCTION Transf_Part_To_Inv_Part(target_key_ref_ IN VARCHAR2,
                                   service_name_   IN VARCHAR2)
    RETURN VARCHAR2 IS
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

  FUNCTION Get_First_Time_Fix_Percentage__(start_date_      IN VARCHAR2,
                                           end_Date_        IN VARCHAR2,
                                           customer_id_     IN VARCHAR2 DEFAULT NULL,
                                           organization_id_ IN VARCHAR2 DEFAULT NULL,
                                           work_type_id_    IN VARCHAR2 DEFAULT NULL,
                                           work_task_       IN VARCHAR2 DEFAULT NULL,
                                           resource_id_     IN VARCHAR2 DEFAULT NULL)
    RETURN NUMBER IS
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

  FUNCTION Get_Begin_Date__(year_    IN VARCHAR2,
                            quarter_ IN VARCHAR2,
                            month_   IN VARCHAR2,
                            week_    IN VARCHAR2) RETURN DATE DETERMINISTIC IS
  BEGIN
    IF year_ IS NOT NULL THEN
      IF quarter_ IS NOT NULL THEN
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
            Error_SYS.Appl_General('Error',
                                   'INVALID_FORMAT: Invalid Quater Value!');
        END CASE;
      ELSE
        IF month_ IS NOT NULL THEN
          RETURN TO_DATE('01/' || month_ || '/' || year_, 'dd/mm/yyyy');
        ELSE
          IF week_ IS NOT NULL THEN
            RETURN trunc(SYSDATE, 'iw') - 7 *(to_number(to_char(trunc(SYSDATE),
                                                                'iw')) -
                                              week_);
          END IF;
        END IF;
      END IF;
      RETURN TO_DATE('01/01/' || year_, 'dd/mm/yyyy');
    ELSE
      RETURN TO_DATE('01/01/1900', 'dd/mm/yyyy');
    END IF;
  END Get_Begin_Date__;

  FUNCTION Get_End_Date__(year_    IN VARCHAR2,
                          quarter_ IN VARCHAR2,
                          month_   IN VARCHAR2,
                          week_    IN VARCHAR2) RETURN DATE DETERMINISTIC IS
  BEGIN
    IF year_ IS NOT NULL THEN
      IF quarter_ IS NOT NULL THEN
        CASE quarter_
          WHEN '1' THEN
            RETURN add_months(last_day(trunc(to_date('01/01/2020',
                                                     'dd/mm/yyyy'),
                                             'Q')),
                              2);
          WHEN '2' THEN
            RETURN add_months(last_day(trunc(to_date('01/04/2020',
                                                     'dd/mm/yyyy'),
                                             'Q')),
                              2);
          WHEN '3' THEN
            RETURN add_months(last_day(trunc(to_date('01/07/2020',
                                                     'dd/mm/yyyy'),
                                             'Q')),
                              2);
          WHEN '4' THEN
            RETURN add_months(last_day(trunc(to_date('01/10/2020',
                                                     'dd/mm/yyyy'),
                                             'Q')),
                              2);
          ELSE
            Error_SYS.Appl_General('Error',
                                   'INVALID_FORMAT: Invalid Quater Value!');
        END CASE;
      ELSE
        IF month_ IS NOT NULL THEN
          RETURN LAST_DAY(TO_DATE('01/' || month_ || '/' || year_,
                                  'dd/mm/yyyy'));
        ELSE
          IF week_ IS NOT NULL THEN
            RETURN(trunc(SYSDATE, 'iw') -
                   7 * (to_number(to_char(trunc(SYSDATE), 'iw')) - week_)) + 6;
          END IF;
        END IF;
      END IF;
      RETURN TO_DATE('31/12/' || year_, 'dd/mm/yyyy');
    ELSE
      RETURN SYSDATE;
    END IF;
  END Get_End_Date__;

  PROCEDURE Copy_Circuit_Reference__(part_no_  IN VARCHAR2,
                                     part_rev_ IN VARCHAR2) IS
    sub_part_no_  VARCHAR2(20);
    sub_part_rev_ VARCHAR2(20);
    structure_id_ VARCHAR2(20);
    pos_          VARCHAR2(20);
  
    objkey_        VARCHAR2(50);
    source_objkey_ VARCHAR2(50);
  
    PROCEDURE Remove_Copy_Circuit_Ref_Flag___(objkey_ IN VARCHAR2) IS
    BEGIN
      UPDATE eng_part_structure_cft
         SET cf$_copy_circuit_reference = NULL
       WHERE rowkey = objkey_;
    END Remove_Copy_Circuit_Ref_Flag___;
  
    PROCEDURE Copy_Circuit_Reference___(objkey_      IN VARCHAR2,
                                        circuit_ref_ IN VARCHAR2) IS
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
  
    FUNCTION Get_Source_Eng_Part_Struc_Objkey___(part_no_      IN VARCHAR2,
                                                 sub_part_no_  IN VARCHAR2,
                                                 sub_part_rev_ IN VARCHAR2,
                                                 structure_id_ IN VARCHAR2,
                                                 pos_          IN VARCHAR2)
      RETURN VARCHAR2 IS
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
                  WHERE PART_NO = part_no_
                    AND PART_REV = part_rev_) LOOP
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
      --dbms_output.put_line(objkey_);   
      source_objkey_ := Get_Source_Eng_Part_Struc_objkey___(part_no_,
                                                            sub_part_no_,
                                                            sub_part_rev_,
                                                            structure_id_,
                                                            pos_);
      dbms_output.put_line(sub_part_no_);
      IF source_objkey_ IS NOT NULL THEN
        IF Eng_Part_Structure_CFP.Get_Cf$_Copy_Circuit_Reference(source_objkey_) =
           'TRUE' THEN
          Copy_Circuit_Reference___(objkey_,
                                    Eng_Part_Structure_CFP.Get_Cf$_Circuit_Ref(source_objkey_));
          Remove_Copy_Circuit_Ref_Flag___(source_objkey_);
        END IF;
      END IF;
    END LOOP;
  END Copy_Circuit_Reference__;

  PROCEDURE Close_Non_Finance_User_Group IS
    non_finance_user_group_ VARCHAR2(30) := 'NF';
  
    CURSOR check_valid_until_date IS
      SELECT *
        from ACC_PERIOD_LEDGER t
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

FUNCTION Check_Cause_Discount(wo_no_ IN VARCHAR2, type_ IN varchar2) return VARCHAR2 IS
  
    temp_           VARCHAR2(5);
    cause_discount_ number;
    material_discount_ number;
    personnel_discount_ number;
    cause_objkey_   varchar2(2000);
  
    cursor get_wo_task_cause(in_wo_no_ varchar2) is
      select error_cause, REPORTED_OBJ_CONN_ROWKEY
        from JT_TASK_UIV
       where wo_no = in_wo_no_
         and error_cause is not null;
  
    cursor get_cause(error_cause_              varchar2,
                     reported_obj_conn_rowkey_ varchar2) is
      select objkey
        from SERVICE_PACKAGE_DETAIL_CLV t
       where cf$_cause = error_cause_
         and cf$_Service_package =
             Jt_Task_Cfp.Get_Cf$_Service_Package(reported_obj_conn_rowkey_);
  
  BEGIN
  
    for rec_ in get_wo_task_cause(wo_no_) loop
      if (get_wo_task_cause%notfound) then
        temp_ := 'false';
        exit;
      else
        open get_cause(rec_.error_cause, rec_.reported_obj_conn_rowkey);
        fetch get_cause
          into cause_objkey_;
        close get_cause;
      
        if (cause_objkey_ is not null) then
          material_discount_ := SERVICE_PACKAGE_DETAIL_CLP.Get_Cf$_Material_Discount(maintenance_cause_code_api.get_objkey(cause_objkey_));
          personnel_discount_ := SERVICE_PACKAGE_DETAIL_CLP.Get_Cf$_Personnel_Discount(maintenance_cause_code_api.get_objkey(cause_objkey_));
          
          if(material_discount_ =0 and personnel_discount_= 0)then
           cause_discount_ := 0;
          elsif(material_discount_ =100 and personnel_discount_= 100)then
           cause_discount_ := 100;
          end if;
          
          if (type_ = 'Chargeable') then
            if (cause_discount_ is not null and cause_discount_ = 0) then
              temp_ := 'true';
              exit;
            end if;
          elsif (type_ = 'Non-Chargeable') then
            if (cause_discount_ is not null and cause_discount_ = 100) then
              temp_ := 'true';
              exit;
            end if;
          end if;         
          
        end if;
      end if;
    end loop;
  
    return temp_;
  END Check_Cause_Discount;

  FUNCTION Get_Allowed_Tax_Codes__(company_    IN VARCHAR2,
                                   customer_   IN VARCHAR2,
                                   catalog_no_ IN VARCHAR2) RETURN VARCHAR2 IS
    allowed_tax_codes_ VARCHAR2(2000);
  BEGIN
    SELECT LISTAGG(fee_code, ', ') within GROUP(ORDER BY fee_code) fee_code
      INTO allowed_tax_codes_
      FROM (SELECT FEE_CODE
              FROM TAX_CODE_RESTRICTED
             WHERE fee_code IN
                   (SELECT a.cf$_Tax_Code
                      FROM SALES_PART_TAX_CODE_CLV a
                     WHERE a.CF$_SALES_PART_NO = catalog_no_)
               AND fee_code IN
                   (SELECT b.cf$_tax_code
                      FROM CUST_APPLICABLE_TAX_CODE_CLV b
                     WHERE b.CF$_COMPANY = company_
                       AND b.CF$_CUSTOMER = customer_)
               AND COMPANY = company_);
    RETURN allowed_tax_codes_;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END;

  FUNCTION Transf_Mansup_Attach_To_PO(target_key_ref_ IN VARCHAR2,
                                      service_name_   IN VARCHAR2)
    RETURN VARCHAR2 IS
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

  -- C641
  FUNCTION Get_Inv_Part_Barcode_Rep_Head RETURN VARCHAR2 IS
  BEGIN 
    
     RETURN Language_SYS.Translate_Constant(lu_name_, 'BARCODEREPHEADER: Tunstall - Approved '|| TO_CHAR(SYSDATE, 'DD/MM/YYYY'));
  END  Get_Inv_Part_Barcode_Rep_Head;
  
  -- C641
  FUNCTION Get_Location_Inv_Part_Barcode(part_no_ IN VARCHAR2,
                                         contract_ IN VARCHAR2)
  RETURN VARCHAR2 IS
     CURSOR get_location IS
        SELECT location_no
        FROM receipt_inv_location ril, inventory_part_barcode ipb
        WHERE ril.contract = ipb.contract
        AND ril.part_no = ipb.part_no
        AND ril.configuration_id = ipb.configuration_id
        AND ril.lot_batch_no = ipb.lot_batch_no
        AND ril.serial_no = ipb.serial_no
        AND ril.eng_chg_level = ipb.eng_chg_level
        AND ril.waiv_dev_rej_no = ipb.waiv_dev_rej_no
        AND ril.activity_seq = ipb.activity_seq
        AND ril.part_no = part_no_
        AND ril.contract = contract_;  
        
  location_no_ VARCHAR2(35);        
  BEGIN
    FOR rec_ IN get_location LOOP
      location_no_ := rec_.location_no;
      exit;
    END LOOP;
    /*OPEN get_location;
    FECTH get_location INTO location_no_;
    CLOSE get_location;*/

    RETURN location_no_;
  END Get_Location_Inv_Part_Barcode; 
  
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
    sales_contract_site_rec_ SALES_CONTRACT_SITE_CLP.Public_Rec;

    CURSOR get_activity_misc_parts IS
      SELECT part_no, part_desc, SUM(require_qty) quantity
        FROM (SELECT t.part_no,
                     Project_Misc_Procurement_API.Get_Selected_Description(SUPPLY_OPTION,
                                                                           SITE,
                                                                           t.PART_NO,
                                                                           MATR_SEQ_NO) part_desc,
                     require_qty
                FROM PROJECT_MISC_PROCUREMENT t,
                     (SELECT part_no, contract, cf$_serviceable_Db
                        FROM SALES_PART_CFV t
                       WHERE t.CF$_SERVICEABLE_DB = 'TRUE') a
               where t.site = a.contract
                 and t.part_no = a.part_no
                 and company = company_
                 and t.ACTIVITY_SEQ IN
                     (SELECT ACTIVITY_SEQ
                        FROM ACTIVITY1 t
                       where t.sub_project_id = sub_project_id_
                         and t.project_id = project_id_)
                 and t.part_no is not null)
       group by part_no, part_desc;

    FUNCTION Get_Object_Id___(
      project_id_     IN VARCHAR2,
      sub_project_id_ IN VARCHAR2) RETURN VARCHAR2 
    IS
      object_id_ VARCHAR2(50);
      rec_       Equipment_Object_api.Public_Rec;
    BEGIN
      SELECT cf$_object_id_db
        INTO object_id_
        FROM SUB_PROJECT_CFV
       WHERE project_id = project_id_
         AND sub_project_id = sub_project_id_;
      rec_ := Equipment_Object_api.Get_By_Rowkey(object_id_);
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
        FROM PROJECT_CFV
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
        FROM SUB_PROJECT_CFV
       WHERE project_id = project_id_
         AND sub_project_id = sub_project_id_;
      RETURN installation_site_;
    EXCEPTION
      WHEN OTHERS THEN
        RETURN NULL;
    END Get_Installation_Site___;

    FUNCTION Get_Sales_Contract_Site_Rec___(
       contract_no_ IN VARCHAR2,
       site_no_     IN VARCHAR2) RETURN SALES_CONTRACT_SITE_CLP.Public_Rec 
    IS
      objkey_ VARCHAR2(50);
      rec_    SALES_CONTRACT_SITE_CLP.Public_Rec;
    BEGIN
      dbms_output.put_line(contract_no_ || '-' || site_no_);
      SELECT a.objkey
        INTO objkey_
        FROM SALES_CONTRACT_SITE_CLV a, CONTRACT_REVISION b
       WHERE a.CF$_CONTRACT_NO = b.contract_no
         AND a.CF$_REV_SEQ = b.rev_seq
         AND b.objstate = 'Active'
         and a.cf$_contract_no = contract_no_
         and a.CF$_SITE_NO = site_no_;
      dbms_output.put_line(objkey_);
      IF objkey_ IS NOT NULL THEN
        rec_ := SALES_CONTRACT_SITE_CLP.Get(objkey_);
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
        FROM WARRANTY_CLV
       WHERE CF$_WARRANTY_PERIOD = warranty_period_;
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
        FROM PROJECT_SITE_EXT
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
      Client_SYS.Add_To_Attr('MCH_CODE',
                             part_no_ || project_id_ || sub_project_id_,
                             attr_);
      Client_SYS.Add_To_Attr('MCH_NAME', part_desc_, attr_);
      Client_SYS.Add_To_Attr('CONTRACT', site_, attr_);
      Client_SYS.Add_To_Attr('PRODUCTION_DATE', production_date_, attr_);
      Client_SYS.Add_To_Attr('SUP_CONTRACT', '2012', attr_);
      Client_SYS.Add_To_Attr('OBJ_LEVEL', object_level_, attr_);
      Client_SYS.Add_To_Attr('OPERATIONAL_STATUS_DB',
                             operational_status_,
                             attr_);
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
      Client_SYS.Add_To_Attr('CF$_SERVICE_WARRANTY_DB',
                             service_warranty_,
                             attr_);
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
      IF NOT EQUIPMENT_FUNCTIONAL_API.Exists(site_,
                                             part_no_ || project_id_ ||
                                             sub_project_id_) THEN
        attr_ := Get_Attr___(part_no_,
                             part_desc_,
                             project_id_,
                             sub_project_id_,
                             sup_mch_code_,
                             production_date_);
        EQUIPMENT_FUNCTIONAL_API.New__(info_,
                                       objid_,
                                       objversion_,
                                       attr_,
                                       'DO');
        IF objid_ IS NOT NULL THEN
          dbms_output.put_line(service_warranty_);
          attr_cf_ := Get_Cf_Attr___(project_id_,
                                     sub_project_id_,
                                     equipment_qty_,
                                     service_warranty_);
          EQUIPMENT_FUNCTIONAL_CFP.Cf_New__(info_,
                                            objid_,
                                            attr_cf_,
                                            '',
                                            'DO');
        END IF;
      END IF;
      NULL;
    END Create_Funct_Equip_Obj___;

  BEGIN
    company_ := Project_API.Get_Company(project_id_);
    object_id_      := Get_Object_Id___(project_id_, sub_project_id_);

    IF object_id_ IS NULL THEN
      Error_SYS.Appl_General('custom',
                             'ERROR_OBJ_ID: This operation is only possible for sub projects with Object ID');
    END IF;
    sub_proj_inst_site_ := Get_Installation_Site___(project_id_,
                                                    sub_project_id_);
    
    sales_contract_no_ := Get_Sales_Contract_No___(project_id_, company_);
    
    project_default_site_ := Get_Default_Project_Site___(company_,
                                                         project_id_);
   
    installation_site_ := Get_Installation_Site___(project_id_,
                                                   sub_project_id_);
   
    sales_contract_site_rec_ := Get_Sales_Contract_Site_Rec___(sales_contract_no_,
                                                               installation_site_);

    hand_over_date_  := sales_contract_site_rec_.CF$_DELIVERED_ON;
    warranty_period_ := sales_contract_site_rec_.CF$_WARRANTY_PERIOD;


    IF sales_contract_site_rec_.CF$_CONTRACT_NO IS NULL THEN
      Error_SYS.Appl_General('Custom',
                             'ERROR_NO_INSTALLATION_SITE: Installation site :P1 is not found in installation sites of the connected Sales Contract items',
                             installation_site_);
    END IF;

    IF hand_over_date_ IS NULL THEN
      Error_SYS.Appl_General('Custom',
                             'ERROR_COMPLETE_DATE: Completion/Handover date of the installation site of connected Sales Contract items is empty');
    END IF;

    IF warranty_period_ IS NULL THEN
      Error_SYS.Appl_General('Custom',
                             'ERROR_WARRANTY_PERIOD: Warranty Period of the installation site of connected Sales Contract items is empty');
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
  
   PROCEDURE Sales_Contract_Auto_Closure IS
  
    remaining_amt_        NUMBER := 0;
    open_afp_exist_       VARCHAR2(5);
    total_paid_           NUMBER := 0;
    contract_sales_value_ NUMBER := 0;
  
    valid_to_close_ boolean := true;
    state_          varchar2(10);
    attr_    VARCHAR2(32000);
    afp_no_   NUMBER;
    
    $IF (Component_Apppay_SYS.INSTALLED) $THEN
    temp_afp_no_      NUMBER;
    payment_per_curr_ NUMBER;
    info_ varchar2(32000):=null;
  
    CURSOR cur_codes(contract_no varchar2) IS
      SELECT DISTINCT (currency_code)
        FROM app_for_payment
       WHERE contract_no = contract_no;
  
  
    CURSOR get_remaining_amount(contract_no_ varchar2, afp_no_ number) IS
      SELECT SUM(App_For_Payment_API.Get_Tot_Ret_Remaining(contract_no_,
                                                         afp_no_))
        FROM app_for_payment
       WHERE contract_no = contract_no
         AND objstate <> 'Cancelled'
       GROUP BY currency_code, supply_country, project_id, billing_seq;
    $END
  
    CURSOR get_eligible_sales_contrcts IS
      SELECT *
        FROM (select t.*
                from SALES_CONTRACT t
               where APP_FOR_PAYMENT_API.Get_Tot_Remaining_Ret_Per_Con(CONTRACT_NO) = 0
                 and state not in ('Cancelled', 'Closed')
                 and exists
               (select 1
                        from APP_FOR_PAYMENT_MAIN p
                       where FIN_RELEASED_ALL = 'TRUE'
                         and p.CONTRACT_NO = t.CONTRACT_NO)
              UNION
              select t.*
                from SALES_CONTRACT t
               where state not in ('Cancelled', 'Closed')
                 and INT_RETENTION = 0
                    -- to check if any APF exist and then check fully invoiced
                 and exists (select 1
                        from app_for_payment_tab
                       where contract_no = t.contract_no)
                 and not exists
               (select 1
                        from APP_FOR_PAYMENT_MAIN p
                       where STATE not in ('Fully Paid')
                         and p.CONTRACT_NO = t.CONTRACT_NO))
                         ;
  BEGIN
    state_ := 'Closed';
  
    FOR rec_ IN get_eligible_sales_contrcts LOOP
      
      Client_SYS.Clear_Attr(attr_);
             
      -- The contract must be in the Completed status to be eligible to close
      IF (rec_.state not in ('Completed')) THEN
        valid_to_close_ := false;
        Client_SYS.Add_To_Attr('CF$_ERROR_REPORTED','Sales Order must be in Completed status.', attr_);
         IFSAPP.SALES_CONTRACT_CFP.Cf_Modify__(info_, rec_.objid,attr_,'','DO');
        CONTINUE;
      END IF;
      --check for core closure validations    
      --**** START validate
      $IF (Component_Apppay_SYS.INSTALLED) $THEN
      
       select max(afp_no) into afp_no_
        FROM app_for_payment WHERE contract_no = rec_.contract_no   AND objstate <> 'Cancelled';
        
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
        Client_SYS.Add_To_Attr('CF$_ERROR_REPORTED','There are application for payments not in Fully Paid or Cancelled statuses.', attr_);
        
        IFSAPP.SALES_CONTRACT_CFP.Cf_Modify__(info_, rec_.objid,attr_,'','DO');
        CONTINUE;
        
      END IF;
    
      IF (remaining_amt_ != 0) THEN
        valid_to_close_ := false;
        Client_SYS.Add_To_Attr('CF$_ERROR_REPORTED','The retention is not released fully in application for payments.', attr_);
        
        IFSAPP.SALES_CONTRACT_CFP.Cf_Modify__(info_, rec_.objid,attr_,'','DO');
        CONTINUE;
        
      END IF;
    
      contract_sales_value_ := Contract_Revision_Util_API.Get_Contract_Sales_Value_Co(rec_.contract_no,
                                                                                      Contract_Revision_Util_API.Get_Active_Revision(rec_.contract_no));
    
      IF (NVL(contract_sales_value_,0) > NVL(total_paid_,0)) THEN
        valid_to_close_ := false;
        Client_SYS.Add_To_Attr('CF$_ERROR_REPORTED','The total paid amount on all applications must be equal to (or higher) than the total contract sales value.', attr_);
      
        IFSAPP.SALES_CONTRACT_CFP.Cf_Modify__(info_, rec_.objid,attr_,'','DO'); 
       CONTINUE;
       
      END IF;
      --**** END validate
      --*** START finaite state set
      IF (valid_to_close_) THEN
        -- Finite_State_Set
        UPDATE sales_contract_tab
           SET rowstate = state_, rowversion = sysdate
         WHERE contract_no = rec_.contract_no;
        rec_.objstate := state_;
      END IF;
      --*** END Finite state set
      --*** START Calculate_Revenue
      IF (Contract_Project_API.Has_Connected_Activity(rec_.contract_no) = 'TRUE') THEN
         Contract_Project_API.Calculate_Revenue(rec_.contract_no);
      END IF;
      --*** END Calculate_Revenue
    END LOOP;
  
  END Sales_Contract_Auto_Closure;
  
  FUNCTION Check_Inv_Header_CA(identity_ IN VARCHAR2, credit_analyst_ IN VARCHAR2, company_ IN VARCHAR2) return VARCHAR2
  IS
  
  follow_up_date_ date;
  status_ varchar2(100);
  inv_state_ varchar2(100);
  inv_due_ DATE;
  aging_ number;
  credit_note_ number;
  amount_due_ number;
  temp_ varchar2(5):='FALSE';
  
  CURSOR get_inv_headers (identity_ varchar2,  credit_analyst_  VARCHAR2, company_  VARCHAR2)IS
  SELECT * FROM OUTGOING_INVOICE_QRY
  WHERE identity=identity_
 and CUSTOMER_CREDIT_INFO_API.Get_Credit_Analyst_Code(company_, identity_)=credit_analyst_
 AND company = company_;
  
  CURSOR get_inv_header_notes (company_ varchar2, identity_ varchar2, invoice_id_ number )is
  select * from (
  select 1 AS EXIST, FOLLOW_UP_DATE, IFSAPP.CREDIT_NOTE_STATUS_API.Get_Note_Status_Description(COMPANY,NOTE_STATUS_ID) 
  from IFSAPP.INVOICE_HEADER_NOTES
  where COMPANY =company_
  and IDENTITY = identity_
  and PARTY_TYPE = 'Customer'
  and INVOICE_ID = invoice_id_ 
  order by note_date desc, note_id desc 
  )where ROWNUM  =1;
  
  CURSOR get_inv_info(company_ varchar2, identity_ varchar2, invoice_id_ number )IS
  SELECT INV_STATE, DUE_DATE
  FROM INVOICE_LEDGER_ITEM_CU_QRY
  WHERE COMPANY =company_
  AND (IDENTITY = identity_ AND INVOICE_ID = invoice_id_ );
  
  CURSOR get_bucket(company_ varchar2, identity_ varchar2)IS
  SELECT 1 
  FROM BUCKETS_QRY
  WHERE company =company_
  AND identity =identity_
  AND bucket_value!=0;
  
  CURSOR get_credit_note(company_ varchar2, identity_ varchar2)IS
  SELECT 1
  FROM CUSTOMER_CREDIT_NOTE
  where company = company_
  AND customer_id = identity_;
  
  
  CURSOR get_acount_due(company_ varchar2, identity_ varchar2)IS
  SELECT AMOUNT_DUE
  FROM IDENTITY_PAY_INFO_CU_QRY
  where company = company_
  AND identity = identity_;
  

  BEGIN
    FOR rec_ in get_inv_headers(identity_,credit_analyst_ , company_ ) LOOP
       OPEN get_inv_header_notes(rec_.company, rec_.identity,rec_.invoice_id);
       FETCH get_inv_header_notes into credit_note_,follow_up_date_, status_;
       CLOSE get_inv_header_notes;
       
       OPEN  get_inv_info(rec_.company, rec_.identity,rec_.invoice_id);
       FETCH  get_inv_info INTO inv_state_,inv_due_;
       CLOSE get_inv_info;
       
       IF(follow_up_date_ IS NOT NULL AND follow_up_date_<= SYSDATE AND (inv_state_ NOT IN ('Preliminary', 'Canceled', 'PaidPosted')AND inv_due_< SYSDATE) )THEN 
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
          
       OPEN get_bucket(rec_.company, rec_.identity);
       FETCH get_bucket INTO aging_;
       CLOSE get_bucket;
       IF(status_ NOT IN ('Complete','Escalated to Credit Manager','Escalated to Finance Controller') AND aging_ IS NULL)THEN
         temp_ := 'TRUE';
         EXIT;
       END IF;
    END LOOP;
       RETURN temp_;
   END Check_Inv_Header_CA;

-------------------- LU  NEW METHODS -------------------------------------

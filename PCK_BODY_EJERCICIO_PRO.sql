create or replace PACKAGE BODY pck_ejercicio_pro
AS

 FUNCTION fa_1erEl (
  consec_des IN VARCHAR2
)
  RETURN NUMBER
 as
 x NUMBER;
 elem NUMBER;
 M NUMBER;
 
  begin
   select length(consec_des) - length(REPLACE(consec_des,'.')) into M FROM dual;

  if m >=1 then
  
      elem := substr(consec_des,1,instr(consec_des,'.'));
   /*   select instr(consec_des , '.') into x FROM dual;
      x := x-1;
      SELECT SUBSTR(consec_des, 1 ,x) into elem FROM dual;
      elem := to_number (elem); */
      
  elsif m=0 then
      elem := to_number (consec_des);
    end if;   
 
 return elem;
 end fa_1erEl;

--------------------------------------------------------------------------------
 FUNCTION fa_consec(
  consec_ord IN VARCHAR2
  )
    RETURN NUMBER
  IS
   consec NUMBER ;
   BEGIN    
    RETURN SUBSTR ( consec_ord , LENGTH(consec_ord), 1);   
   
  END fa_consec;
  
 ---------------------------------------------------------
  FUNCTION fa_consec_ord(
  consec_des VARCHAR2
  )
  return VARCHAR2
  AS
 aux VARCHAR2(2) := '0';
 consec_ord varchar2 (400);
 elem1 NUMBER; 
 long_nivel_1 number;
 long_consec_des NUMBER;
 consec_des1 VARCHAR (400);
 
 BEGIN
    consec_ord := '';
    consec_des1 := consec_des;
    
    FOR i IN  1..LENGTH(consec_des) 
    LOOP
     long_consec_des := LENGTH(consec_des1) ;
     elem1 := FA_1EREL (consec_des1);
     long_nivel_1 := length (elem1);
     
  IF long_nivel_1 = 1 then
    consec_ord := consec_ord || aux || elem1;
  ELSE
    consec_ord := consec_ord || elem1;
  END IF;
    
     consec_des1 := substr (consec_des1, long_nivel_1 +2  ,long_consec_des);
    
    END LOOP;

    
    RETURN consec_ord; 
  END fa_consec_ord;
  
  ----------------------
  
  FUNCTION fa_niv_ar (
    consec_des IN VARCHAR2
)
 return number
 is
 niv_ar NUMBER;
 
 BEGIN
    RETURN length(consec_des) - length(REPLACE(consec_des,'.' ,'')) + 1 ;  
end fa_niv_ar;


 --------------------------
 FUNCTION fa_recorrido (
  consec_des IN VARCHAR2
,  tipo IN NUMBER
)
 RETURN VARCHAR2
  as
  resul VARCHAR2(240);
  elem NUMBER;
  elem1 VARCHAR2(240);
  cad VARCHAR2(240);
  niv_ar NUMBER;
  cad_rec VARCHAR2 (240);
  u_el NUMBER;
  
  BEGIN
  elem := FA_1EREL (consec_des);
  cad := ltrim (consec_des, elem);
    IF tipo =1 THEN  ----- recorrido derecha
          elem := elem +1;
          resul := elem || cad;
          
    ELSIF tipo=2 THEN
          elem := elem - 1;
          resul := elem || cad;
          
 /*   ELSIF tipo = 3 THEN 
          niv_ar := length(consec_des) - length(REPLACE(consec_des,'.', '')) ; 
          u_el := FA_ULTIMOEL (consec_des) + 1;
          cad_rec := SUBSTR (consec_des , 1 ,instr (consec_des , '.', 1, niv_ar)) ;
          resul := cad_rec || u_el;
    
    ELSIF tipo = 4 then    
          niv_ar := length(consec_des) - length(REPLACE(consec_des,'.', '')) ; 
          u_el := FA_ULTIMOEL (consec_des) - 1;
          cad_rec := SUBSTR (consec_des , 1 ,instr (consec_des , '.', 1, niv_ar)) ;
          resul := cad_rec || u_el; */
    
    END IF;

    RETURN resul;
  END fa_recorrido;

----------------------------

 FUNCTION fa_ultimoEl (
 consec_des in VARCHAR2
)
 return number
 as

 uel1 NUMBER;
 long_nh NUMBER;
 nivel_arbol NUMBER;
 begin
     
     long_nh := length (consec_des) ;
     nivel_arbol := FA_NIV_AR (consec_des) - 1;
   --  nivel_arbol := nivel_arbol - 1 ;
   /*  elm := instr (consec_des , '.', 1, nivel_arbol);     
     elm := elm + 1;
     uel := SUBSTR (consec_des , elm ,long_nh);
     uel1 := to_number (uel); */
    uel1 := to_number (SUBSTR(consec_des, instr(consec_des,'.',1, nivel_arbol) +1 ,long_nh ));
    
  return uel1;  
 end fa_ultimoEl;
  
  ---------------------------------------------------------------------------- PROCEDIMIENTOS
  
  procedure sp_insertar_nodo1(
  consec_des_destino IN VARCHAR2
, consec_des in VARCHAR2
  )
  AS
   llav_padre NUMBER;
   l_mensaje VARCHAR2(100) := '¡Hola, insercción exitosa!';
   consec_ord VARCHAR2 (400);
   consec NUMBER;
   llav NUMBER;
   llave1 NUMBER;
   niv_ar NUMBER;
   cad1 VARCHAR2(240);
   cad2 VARCHAR2(240);
   consec_des1 varchar2(240);
   elem1 NUMBER;
   consec_ord1 VARCHAR2 (240);
   consec1 NUMBER;
   maxi NUMBER := 0;
   maxi2 NUMBER := 0;
   long_padre NUMBER;
   cad_rec VARCHAR2 (240);
   num_hijos NUMBER;
   long_nh NUMBER;
   num_h VARCHAR2(240);
   uel_par NUMBER;
   uel_cad1 NUMBER;
   uel_cad NUMBER;
   llave_nodo NUMBER;
   des_consec VARCHAR2 (250);
   v NUMBER;
   r NUMBER;
   niv_arb_padre NUMBER;
   consec_des_padre VARCHAR2(240);
   CURSOR c_lv_t IS SELECT llave FROM t_ejercicio_pro WHERE (length(CONSECUTIVO_DES) - length(REPLACE(CONSECUTIVO_DES,'.'))+1) = niv_ar ; ------ cursor nivel arbol
   CURSOR c_sucesores IS  select  consecutivo_des, llave FROM t_ejercicio_pro  start with llave = llave1 connect by llave_padre = prior llave ;--order by level
   -- FOR UPDATE ;
   CURSOR c_hijos IS select llave FROM t_ejercicio_pro where llave_padre = llav_padre ; 

  BEGIN  
  
   niv_ar := FA_NIV_AR(consec_des);
   elem1 := FA_1EREL(consec_des);      
      ---------- llave primaria---
   SELECT MAX (llave) + 1  INTO llav  FROM t_ejercicio_pro;  
   
                  ------- INSERTA AL NODO RAIZ ------
            IF consec_des_destino IS NULL AND niv_ar = 1 THEN
    ---------------- identifica al nodo más grande del nivel del arbol --------------   
                  FOR lista_nodos in c_lv_t
                  LOOP
                    llave_nodo := lista_nodos.llave;
                    SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                    v := FA_1EREL(des_consec);      
                        IF v > maxi  THEN
                        maxi := v;
                        END IF;
                  END LOOP;   
                  maxi := maxi +1;
            
           ------------- corrimiento derecha de los nodos ----------------
                  IF  maxi >= elem1 THEN
                        FOR r1 IN c_lv_t
                        LOOP   
                          llave_nodo := r1.llave;
                          SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                          r := FA_1EREL(des_consec);
                          llave1 := llave_nodo;
                                IF r >= elem1 THEN
                                    FOR r3 IN c_sucesores
                                    LOOP
                                      consec_des1 := FA_RECORRIDO(r3.CONSECUTIVO_DES , 1);
                                      consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                      consec1 := FA_CONSEC (consec_ord1);
                                      UPDATE t_ejercicio_pro SET 
                                      consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE LLAVE = R3.LLAVE ; 
                                    END LOOP;
                                END IF;
                          END LOOP;
                       END IF; 
            ------------ inserta el nodo ------------
                      consec_ord := FA_CONSEC_ORD (consec_des);
                      consec := FA_CONSEC (consec_ord);
                      INSERT INTO T_EJERCICIO_PRO (LLAVE,LLAVE_PADRE,CONSECUTIVO,CONSECUTIVO_DES,CONSECUTIVO_ORD,INDICADOR_TIPO) 
                      VALUES (llav , null ,consec ,consec_des ,consec_ord , 0);
                      dbms_output.put_line( l_mensaje);           
                
            ELSIF llav_padre is NULL and niv_ar= 1 and maxi < elem1
                      then dbms_output.put_line('NO SE PUEDE INSERTAR '||consec_des||' AL NODO PADRE');
                
    
    ---------- INSERTAR A UN NODO HIJO -------- 
            ELSIF consec_des_destino IS NOT NULL THEN   
             SELECT llave INTO llav_padre FROM t_ejercicio_pro WHERE consecutivo_des = consec_des_destino;
              niv_arb_padre := FA_NIV_AR(consec_des_destino) + 1;
              long_padre := LENGTH (consec_des_destino);
              cad_rec := SUBSTR (consec_des, 1 ,long_padre);
              SELECT  COUNT (consecutivo_des) INTO  num_hijos FROM t_ejercicio_pro  START WITH llave = llav_padre CONNECT BY llave_padre = PRIOR llave;  
              uel_par := FA_ULTIMOEL (consec_des);
   
                IF niv_arb_padre = niv_ar AND cad_rec = consec_des_destino AND num_hijos > 1 THEN 
            -------- identifica al ultimo nodo hijo     
                    FOR lista_hijos IN c_hijos
                    LOOP
                      llave_nodo := lista_hijos.llave;
                      SELECT consecutivo_des INTO cad1 FROM t_ejercicio_pro WHERE llave = llave_nodo ;
                      uel_cad := FA_ULTIMOEL ( cad1);
                            IF uel_cad > maxi2  THEN
                              maxi2 := uel_cad;
                            END IF;     
                   END LOOP; 
                   maxi2 := maxi2 +1 ;     
          ------------ corrimiento derecha de los nodos hijos
                    IF  maxi2  >= uel_par  THEN
                      FOR lista_hijos IN c_hijos
                      LOOP
                        llave_nodo := lista_hijos.llave;
                        SELECT consecutivo_des INTO cad1 FROM t_ejercicio_pro WHERE llave = llave_nodo ;
                        uel_cad1 := FA_ULTIMOEL ( cad1);
                        llave1 := llave_nodo;
                            IF uel_par <= uel_cad1 THEN
                                FOR lista_nodos IN c_sucesores
                                LOOP
                                  long_padre := 0 ;
                                  long_padre := LENGTH (consec_des_destino);
                                  cad2 := lista_nodos.consecutivo_des;
                                  long_nh := LENGTH (cad2) ;
                                  num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2 );
                                  num_h := Fa_Recorrido (num_h , 1);
                                  consec_des1 := consec_des_destino || '.' || num_h;
                                  consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                  consec1 := FA_CONSEC (consec_ord1);
                                  
                                  UPDATE t_ejercicio_pro SET 
                                  consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  where llave = lista_nodos.LLAVE ;
                                END LOOP;      
                            END IF;
                      END LOOP;
            ---------- inserta nodo -----------------
                      consec_ord := FA_CONSEC_ORD (consec_des);
                      consec := FA_CONSEC (consec_ord);
                      INSERT INTO T_EJERCICIO_PRO (LLAVE,LLAVE_PADRE,CONSECUTIVO,CONSECUTIVO_DES,CONSECUTIVO_ORD,INDICADOR_TIPO) 
                      VALUES (llav , llav_padre ,consec ,consec_des ,consec_ord ,0);
                      dbms_output.put_line( l_mensaje); 
            
              ELSIF  maxi2 < uel_par then
                  dbms_output.put_line('NO SE PUEDE INSERTAR '||consec_des||' AL NODO '|| consec_des_destino);    
                    
                    END IF; --- if de la cadena ultimo elementos
                         
            ELSIF niv_arb_padre = niv_ar AND cad_rec = consec_des_destino AND num_hijos = 1 AND uel_par = 1 THEN
                  consec_ord := FA_CONSEC_ORD (consec_des);
                  consec := FA_CONSEC (consec_ord);
                  INSERT INTO T_EJERCICIO_PRO (LLAVE,LLAVE_PADRE,CONSECUTIVO,CONSECUTIVO_DES,CONSECUTIVO_ORD,INDICADOR_TIPO) 
                  VALUES (llav , llav_padre ,consec ,consec_des , consec_ord ,0);
                  dbms_output.put_line( l_mensaje); 
                  UPDATE t_ejercicio_pro SET 
                  INDICADOR_TIPO = 1  WHERE llave = llav_padre;                        
            ELSE    
            dbms_output.put_line('NO SE PUEDE INSERTAR '||consec_des||' AL NODO ' || consec_des_destino );
            END IF;    
            
            END IF; -- not null llav padre 
  
    
    END sp_insertar_nodo1;
    
    
    
    
    
    PROCEDURE sp_borrar_nodo1(
  consec_des IN VARCHAR2
  )
  AS
  l_mensaje VARCHAR2(100) := '¡Hola, El nodo se eliminó exitosamente!';
  r varchar2(242);
  ver VARCHAR2 (242);
  niv_ar NUMBER;
  consec_des1 VARCHAR2 (242);
  consec1 VARCHAR2 (242);
  consec_ord1 VARCHAR2 (242);
  lista_hijos VARCHAR2 (242);
  elem1 NUMBER;
  llave1 NUMBER;
  llav_padre NUMBER;
  cad1 VARCHAR2 (242);
  long_padre NUMBER;
  cad_rec VARCHAR2 (240);
  long_nh NUMBER;
   num_h VARCHAR2(240);
   uel_par NUMBER;
   uel_cad1 NUMBER;
   uel_cad NUMBER;
   num_hijos NUMBER;
   consec_des_padre VARCHAR2(240);
   cad2 VARCHAR2(240);
   LLAV NUMBER;
   LLAVE_NODO NUMBER;
   DES_CONSEC VARCHAR2(240);
  CURSOR c_lv_t IS SELECT llave FROM t_ejercicio_pro WHERE (length(CONSECUTIVO_DES) - length(REPLACE(CONSECUTIVO_DES,'.'))+1) = niv_ar   ;
  CURSOR c_sucesores IS  select  consecutivo_des, llave FROM t_ejercicio_pro  start with llave = llave1 connect by llave_padre = prior llave ; --order by level;
  CURSOR c_hijos IS select llave FROM t_ejercicio_pro where llave_padre = llav_padre ;
  
  begin
  
  niv_ar := FA_NIV_AR(consec_des); 
  
  SELECT llave INTO llav FROM t_ejercicio_pro WHERE consecutivo_des = consec_des;
  SELECT llave_padre INTO llav_padre FROM t_ejercicio_pro WHERE llave = llav ;
  
  
    ------------ borrar un nodo de la raiz
    IF  niv_ar = 1 THEN  
       elem1 := FA_1EREL(consec_des);
       DELETE FROM t_ejercicio_pro WHERE CONSECUTIVO_DES like(consec_des||'.%');
       DELETE FROM t_ejercicio_pro WHERE llave = llav ;
       
            FOR r1 IN c_lv_t
            LOOP   
              llave_nodo := r1.llave;
              SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
              r := FA_1EREL(des_consec);
              llave1 := llave_nodo;
        --------------- corrimiento izquierda de los nodos
                  IF r > elem1 THEN
                      FOR r3 IN c_sucesores
                      LOOP
                        consec_des1 := FA_RECORRIDO(r3.CONSECUTIVO_DES , 2);
                        consec_ord1 := FA_CONSEC_ORD (consec_des1);
                        consec1 := FA_CONSEC (consec_ord1);
                        UPDATE t_ejercicio_pro SET 
                        consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = r3.llave ;  
                      END LOOP;
                  END IF;
             END LOOP;   
        dbms_output.put_line(l_mensaje );
    
          ------------ BORRADO A NODOS HIJOS

   ELSIF niv_ar > 1  THEN
      SELECT consecutivo_des INTO consec_des_padre FROM t_ejercicio_pro WHERE llave = llav_padre;
      DELETE FROM t_ejercicio_pro WHERE CONSECUTIVO_DES like(consec_des||'.%');
      DELETE FROM t_ejercicio_pro WHERE llave = llav ;
      SELECT  COUNT (consecutivo_des) INTO  num_hijos FROM t_ejercicio_pro  START WITH llave = llav_padre CONNECT BY llave_padre = PRIOR llave;

         IF num_hijos > 1 THEN
            FOR lista_hijos IN c_hijos
            LOOP
            uel_par := FA_ULTIMOEL ( consec_des);
            llave_nodo := lista_hijos.llave;
            SELECT consecutivo_des INTO cad1 FROM t_ejercicio_pro WHERE llave = llave_nodo ;
            uel_cad1 := FA_ULTIMOEL ( cad1);
            llave1 := llave_nodo;
      --------------- recorrido izquierda nodos hijos ----------
                IF uel_par < uel_cad1 THEN
                    FOR lista_nodos IN c_sucesores
                    LOOP
                     
                      long_padre := 0 ;
                      long_padre := LENGTH (consec_des_padre);
                      cad2 := lista_nodos.consecutivo_des;
                      long_nh := LENGTH (cad2) ;
                      num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                      num_h := Fa_Recorrido (num_h , 2);
                      consec_des1 := consec_des_padre || '.' || num_h;
                      consec_ord1 := FA_CONSEC_ORD (consec_des1);
                      consec1 := FA_CONSEC (consec_ord1);
                      UPDATE t_ejercicio_pro SET 
                      consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  where llave = lista_nodos.llave ;
                    END LOOP;        
                 END IF;                
            END LOOP;
        ELSIF  num_hijos = 1 THEN
                    UPDATE t_ejercicio_pro SET
                    indicador_tipo = 0  WHERE llave = llav_padre;
          END IF;
                            dbms_output.put_line(l_mensaje );

   END IF;
  END ;




PROCEDURE sp_mover_nodo1 (
  consec_des_destino    IN VARCHAR2
, consec_des IN VARCHAR2
, consec_des_posicion IN VARCHAR2
) 
 AS
 l_mensaje VARCHAR2(100) := '¡Hola, el movimiento del nodo se hizo correctamente!';
 l_mensaje2 VARCHAR2(100) := '¡Los datos ingresados, son incorrectos!';
 niv_ar NUMBER;
 niv_ar2 NUMBER;
 nodo_destino VARCHAR2 (242);
 r VARCHAR2 (242);
 nodo NUMBER;
 lista_sucesores1 VARCHAR2 (242);
 llave1 NUMBER;
 llav_padre NUMBER;
 llave_pa_origen NUMBER;
 consec_des1 VARCHAR2(242);
 consec_ord1 VARCHAR2 (242);
 consec1 VARCHAR2 (242) ;
 elem_rep NUMBER;
 cad2 VARCHAR2 (242) ;
 nodo_padre VARCHAR2 (242);
 nodo_origen NUMBER;
 elem1_pos NUMBER;
 num_hijos NUMBER;
 consec_des_padre VARCHAR2 (242);
 long_padre NUMBER;
 uel_par NUMBER;
 uel_cad1 NUMBER;
 cad1 VARCHAR2 (242);
 long_nh NUMBER;
 num_h VARCHAR2 (242);
 llave_padre_Nodorigen NUMBER;
 llave_padre_Nodestino NUMBER;
 maxi2 NUMBER := 0;
 LLAV NUMBER;
 LLAVE_NODO NUMBER;
 num_hijos_aux NUMBER;
 llav_posi  NUMBER;
 llave_destino NUMBER;
 v NUMBER;
 consec_posi VARCHAR2 (240);
 llave_origen NUMBER;
 DES_CONSEC VARCHAR2(240);
 nodo_pos NUMBER;
  CURSOR c_lv_t IS SELECT llave FROM t_ejercicio_pro WHERE (length(CONSECUTIVO_DES) - length(REPLACE(CONSECUTIVO_DES,'.'))+1) = niv_ar ; ------ cursor nivel arbol
  CURSOR c_sucesores IS  select  consecutivo_des, llave FROM t_ejercicio_pro  start with llave = llave1 connect by llave_padre = prior llave ; --order by level
  CURSOR c_hijos IS select llave FROM t_ejercicio_pro where llave_padre = llav_padre ;
 
 BEGIN
  
  niv_ar2 := FA_NIV_AR(consec_des);
  
            SELECT count(consecutivo_des) into nodo_pos from t_ejercicio_pro where consecutivo_des = consec_des_posicion;
                IF nodo_pos = 0 THEN
                  llav_posi := NULL;
                ELSE 
                  SELECT llave into llav_posi from t_ejercicio_pro where consecutivo_des = consec_des_posicion;
                END IF;

    ---------------------- MOVER DE UN NODO RAIZ A OTRA POSICION SOBRE EL NODO RAIZ
        IF consec_des_destino IS NULL AND niv_ar2 = 1 AND llav_posi IS NOT NULL THEN
            SELECT llave INTO llave_origen FROM t_ejercicio_pro WHERE consecutivo_des = consec_des;
            niv_ar := FA_NIV_AR (consec_des_posicion);
            nodo_destino := FA_1EREL (consec_des_posicion);
            nodo_origen := FA_1EREL (consec_des);
            ---- recorrido de los nodos raiz
                  FOR r1 IN c_lv_t
                  LOOP
                      llave_nodo := r1.llave;
                      SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                      nodo := FA_1EREL(des_consec);
                      llave1 := llave_nodo;
                          --- si se desea mover el nodo a la izquierda
                          IF nodo >= nodo_destino AND nodo < nodo_origen  THEN
                              FOR lista_sucesores IN c_sucesores 
                              LOOP
                                  lista_sucesores1 := lista_sucesores.consecutivo_des;
                                  consec_des1 := FA_RECORRIDO(lista_sucesores1 , 1);
                                  consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                  consec1 := FA_CONSEC (consec_ord1);
                                  UPDATE t_ejercicio_pro SET 
                                  consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = lista_sucesores.llave ;               
                              END LOOP;
                          END IF;
                        --- si se desea mover el nodo a la derecha
                        IF nodo >= nodo_origen AND nodo <= nodo_destino THEN
                            FOR lista_sucesores IN c_sucesores 
                            LOOP
                                lista_sucesores1 := lista_sucesores.consecutivo_des;
                                consec_des1 := FA_RECORRIDO(lista_sucesores1 , 2);
                                consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                consec1 := FA_CONSEC (consec_ord1);
                                UPDATE t_ejercicio_pro SET 
                                consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = lista_sucesores.llave ;               
                            END LOOP;
                       END IF;            
                  END LOOP;   
                
                  llave1 := llave_origen;
           --------- se remplaza el nodo a la posición deseada       
            FOR r1 in c_sucesores
            LOOP
               r := r1.consecutivo_des;
               elem_rep := FA_1EREL(r);
               cad2 := ltrim (r , elem_rep);
               elem_rep := REPLACE (elem_rep , elem_rep , nodo_destino );
               consec_des1 := elem_rep || cad2;
               consec_ord1 := FA_CONSEC_ORD (consec_des1);
               consec1 := FA_CONSEC (consec_ord1);
              UPDATE t_ejercicio_pro SET 
              consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = r1.llave ;
           END LOOP;
          dbms_output.put_line( l_mensaje);
        ELSIF consec_des_destino  IS NULL AND niv_ar2 = 1 AND llav_posi is NULL THEN 
            dbms_output.put_line( l_mensaje2);
        END IF;
  
  
  --------------------------------------- MOVER DE UN NODO HIJO AL NODO RAIZ, MENOS LA ULTIMA POSICIÓN
       IF  consec_des_destino IS NULL AND niv_ar2 > 1 AND llav_posi IS NOT NULL THEN 
               SELECT llave INTO llave_origen FROM t_ejercicio_pro WHERE consecutivo_des = consec_des ;
              nodo_destino := FA_1EREL (consec_des_posicion);
              niv_ar := FA_NIV_AR (consec_des_posicion);
            --------- recorrido nodos raiz
                  FOR r1 IN c_lv_t
                  LOOP
                      llave_nodo := r1.llave;
                      SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                      nodo := FA_1EREL(des_consec);
                      llave1 := llave_nodo;
                      
                          IF nodo >= nodo_destino THEN
                                FOR lista_sucesores IN c_sucesores 
                                LOOP
                                    lista_sucesores1 := lista_sucesores.consecutivo_des;
                                    consec_des1 := FA_RECORRIDO(lista_sucesores1 , 1);
                                    consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                    consec1 := FA_CONSEC (consec_ord1);
                                    UPDATE t_ejercicio_pro SET 
                                    consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = lista_sucesores.llave ;               
                                END LOOP;
                          END IF;         
                  END LOOP; 
             ----------- inserta nodo en el nodo raiz 
                 consec_des1 := REPLACE (consec_des , consec_des  ,consec_des_posicion );
                 consec_ord1 := FA_CONSEC_ORD (consec_des1);
                 consec1 := FA_CONSEC (consec_ord1);
                 UPDATE t_ejercicio_pro SET 
                 consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;
                 llav_padre := llave_origen;
                 
                      FOR lista_hijos IN c_hijos
                      LOOP
                          llave_nodo := lista_hijos.llave;
                          llave1 := llave_nodo;      
           
                            FOR r1 IN c_sucesores
                            LOOP
                                r := r1.consecutivo_des;
                                SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE CONSECUTIVO_DES = r ;
                                SELECT consecutivo_des INTO consec_des_padre FROM T_EJERCICIO_PRO WHERE llave = llave_pa_origen ;
                                uel_cad1 := FA_ULTIMOEL(r);
                                consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                consec1 := FA_CONSEC (consec_ord1);
                                UPDATE t_ejercicio_pro SET 
                                consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = r1.llave ;
                            END LOOP; 
                       END LOOP; ----- C_HIJOS       
      ------------------------------------ FIN INSERCION            
            
                      SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE llave = llave_origen  ;
                      select COUNT (consecutivo_des) -1 INTO num_hijos FROM t_ejercicio_pro WHERE llave_padre = llave_pa_origen;
                      UPDATE t_ejercicio_pro SET
                      llave_padre = NULL WHERE llave= llave_origen ; 
               
            IF num_hijos = 0 THEN
                UPDATE t_ejercicio_pro SET
                indicador_tipo = 0 WHERE llave = llave_pa_origen;
     ------------------ Recorrido de los nodos hermanos que del nodo que se movio
            ELSE
                SELECT consecutivo_des INTO consec_des_padre FROM t_ejercicio_pro WHERE llave= llave_pa_origen;
                long_padre := LENGTH (consec_des_padre);
                llav_padre := llave_pa_origen;
                          
                      FOR lista_hijos IN c_hijos
                      LOOP
                          uel_par := FA_ULTIMOEL ( consec_des);
                          llave_nodo := lista_hijos.llave;
                          select consecutivo_des into cad1 FROM t_ejercicio_pro where llave = llave_nodo ;
                          uel_cad1 := FA_ULTIMOEL ( cad1);
                          llave1 := llave_nodo;  
                          
                              IF uel_par < uel_cad1 THEN
                                  FOR lista_nodos IN c_sucesores
                                  LOOP
                                      long_padre := 0 ;
                                      long_padre := LENGTH (consec_des_padre);
                                      cad2 := lista_nodos.consecutivo_des;
                                      long_nh := LENGTH (cad2) ;
                                      num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                                      num_h := Fa_Recorrido (num_h , 2);
                                      consec_des1 := consec_des_padre || '.' || num_h;
                                      consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                      consec1 := FA_CONSEC (consec_ord1);
                                      update t_ejercicio_pro set 
                                      consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  where llave= lista_nodos.llave ;
                                 END LOOP;   
                            END IF;                
                    END LOOP;  
          END IF; 
                    dbms_output.put_line( l_mensaje);    
    END IF;      
  
    
------------------------------ MOVER DE UN NODO HIJO AL NODO RAIZ, EN LA ULTIMA POSICION    
      IF  consec_des_destino IS NULL AND niv_ar2 > 1 AND llav_posi IS NULL THEN 
               SELECT llave INTO llave_origen FROM t_ejercicio_pro WHERE consecutivo_des = consec_des ;
               niv_ar := 1;
               --obtiene el nodo maximo del nodo raíz
               FOR lista_nodos in c_lv_t
               LOOP
               llave_nodo := lista_nodos.llave;
               SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
               v := FA_1EREL(des_consec);    
                   IF v > maxi2  THEN
                      maxi2 := v;
                   END IF;
               END LOOP;   
               maxi2 := maxi2 + 1;
               -- remplaza el nodo por el nodo que se desea mover
                 consec_des1 := REPLACE (consec_des , consec_des  ,maxi2 );
                 consec_ord1 := FA_CONSEC_ORD (consec_des1);
                 consec1 := FA_CONSEC (consec_ord1);
                 UPDATE t_ejercicio_pro SET 
                 consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;
                 llav_padre := llave_origen;
                 
                      FOR lista_hijos IN c_hijos
                      LOOP
                          llave_nodo := lista_hijos.llave;
                          llave1 := llave_nodo;      
           
                            FOR r1 IN c_sucesores
                            LOOP
                                r := r1.consecutivo_des;
                                SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE CONSECUTIVO_DES = r ;
                                SELECT consecutivo_des INTO consec_des_padre FROM T_EJERCICIO_PRO WHERE llave = llave_pa_origen ;
                                uel_cad1 := FA_ULTIMOEL(r);
                                consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                consec1 := FA_CONSEC (consec_ord1);
                                UPDATE t_ejercicio_pro SET 
                                consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = r1.llave ;
                            END LOOP; 
                       END LOOP; ----- C_HIJOS       
                 
                      SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE llave = llave_origen  ;
                      select COUNT (consecutivo_des) -1 INTO num_hijos FROM t_ejercicio_pro WHERE llave_padre = llave_pa_origen;
                      UPDATE t_ejercicio_pro SET
                      llave_padre = NULL WHERE llave= llave_origen ; 
               
            IF num_hijos = 0 THEN
                UPDATE t_ejercicio_pro SET
                indicador_tipo = 0 WHERE llave = llave_pa_origen;
     ------------------ Recorrido de los nodos hermanos que se movio de nodo      
            ELSE
                SELECT consecutivo_des INTO consec_des_padre FROM t_ejercicio_pro WHERE llave= llave_pa_origen;
                long_padre := LENGTH (consec_des_padre);
                llav_padre := llave_pa_origen;
                          
                      FOR lista_hijos IN c_hijos
                      LOOP
                          uel_par := FA_ULTIMOEL ( consec_des);
                          llave_nodo := lista_hijos.llave;
                          select consecutivo_des into cad1 FROM t_ejercicio_pro where llave = llave_nodo ;
                          uel_cad1 := FA_ULTIMOEL ( cad1);
                          llave1 := llave_nodo;  
                          
                              IF uel_par < uel_cad1 THEN
                                  FOR lista_nodos IN c_sucesores
                                  LOOP
                                      long_padre := 0 ;
                                      long_padre := LENGTH (consec_des_padre);
                                      cad2 := lista_nodos.consecutivo_des;
                                      long_nh := LENGTH (cad2) ;
                                      num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                                      num_h := Fa_Recorrido (num_h , 2);
                                      consec_des1 := consec_des_padre || '.' || num_h;
                                      consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                      consec1 := FA_CONSEC (consec_ord1);
                                      update t_ejercicio_pro set 
                                      consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  where llave= lista_nodos.llave ;
                                 END LOOP;   
                            END IF;                
                    END LOOP;  
          END IF; 
                    dbms_output.put_line( l_mensaje);    
      
      END IF ;


    
       -------------------------------------------------------------------------------------------------------------------------------------------------------------------
       IF consec_des_destino IS NOT NULL AND LLAV_POSI IS NULL THEN
            SELECT llave INTO llave_destino FROM t_ejercicio_pro  WHERE CONSECUTIVO_DES = consec_des_destino  ;
            SELECT COUNT(consecutivo_des) INTO num_hijos FROM t_ejercicio_pro  START WITH llave = llave_destino CONNECT BY llave_padre = PRIOR llave;
            num_hijos_aux := num_hijos;
               
         --------------------------------- MOVER DEL NODO RAIZ A OTRO NODO COMO HIJO. EL NODO DESTINO NO TIENE HIJOS
                    IF NUM_HIJOS = 1 and LLAV_POSI is NULL and niv_ar2 = 1 then
                    
                    --- remplaza nodos a la posición deseada
                            SELECT llave INTO llave_origen FROM t_ejercicio_pro where consecutivo_des = consec_des ;
                            llav_padre  := llave_origen ;
                            consec_posi := consec_des_destino    ;
                            consec_posi := consec_posi || '.1' ;
                            consec_des1 := replace (consec_des , consec_des  ,consec_posi );
                            consec_ord1 := FA_CONSEC_ORD (consec_des1);
                            consec1 := FA_CONSEC (consec_ord1);
                            update t_ejercicio_pro set 
                            consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = llave_origen ;                
                
                                  for lista_hijos IN c_hijos
                                  LOOP
                                      llave_nodo := lista_hijos.llave;
                                      llave1 := llave_nodo;
                                          FOR r1 IN c_sucesores
                                          LOOP
                                             r := r1.consecutivo_des;
                                             SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE CONSECUTIVO_DES = r ;
                                             SELECT consecutivo_des INTO consec_des_padre FROM T_EJERCICIO_PRO WHERE llave = llave_pa_origen ;
                                             uel_cad1 := FA_ULTIMOEL(r);
                                             consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                             consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                             consec1 := FA_CONSEC (consec_ord1);
                                             UPDATE t_ejercicio_pro SET 
                                             consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = r1.llave ;
                                          END LOOP; 
                                    END LOOP; ----- C_HIJOS      
                                          UPDATE t_ejercicio_pro SET
                                          llave_padre = llave_destino WHERE llave = llave_origen ;
                                          UPDATE t_ejercicio_pro SET
                                          indicador_tipo = 1 WHERE llave =  llave_destino;
                                          dbms_output.put_line( l_mensaje);        
       
           ----- recorrido izquierda de los nodos raiz donde se movio el nodo.
                              niv_ar := niv_ar2;
                              elem1_pos := FA_1EREL(consec_des);
                                  FOR r1 IN c_lv_t
                                  LOOP   
                                      llave_nodo := r1.llave;
                                      SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                                      r := FA_1EREL(des_consec);
                                      llave1 := llave_nodo;
        
                                          IF r > elem1_pos THEN
                                               FOR r3 IN c_sucesores
                                               LOOP
                                                  consec_des1 := FA_RECORRIDO(r3.CONSECUTIVO_DES , 2);
                                                  consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                  consec1 := FA_CONSEC (consec_ord1);
                                                  UPDATE t_ejercicio_pro SET 
                                                  consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = r3.llave ;  
                                                END LOOP;
                                            END IF;
                                      END LOOP;   
                                          ------------------------------     
                       ELSIF NUM_HIJOS = 1 and llav_posi IS NOT NULL then
                            dbms_output.put_line( l_mensaje2);            
            END IF;
          
     ---------------- MOVER DE UN NODO RAIZ A UN NODO CON HERMANOS SE INSERTA EN LA ULTIMA POSICION
                    IF NUM_HIJOS > 1 AND llav_posi IS NULL AND niv_ar2 = 1 THEN
                    
                        SELECT llave INTO llave_destino FROM t_ejercicio_pro where consecutivo_des = consec_des_destino ;
                        llav_padre := llave_destino ;
                        -- ontiene el nodo más grande del nodo donde se desea 
                            FOR lista_hijos in c_hijos
                            LOOP
                                llave_nodo := lista_hijos.llave;
                                SELECT consecutivo_des INTO cad1 FROM t_ejercicio_pro WHERE llave = llave_nodo ;
                                uel_cad1 := FA_ULTIMOEL ( cad1);
                                
                                    IF uel_cad1 > maxi2  THEN
                                      maxi2 := uel_cad1;
                                    END IF;     
                            END LOOP; 
                                maxi2 := maxi2 +1 ;
                            ----- remplaza el nodo que se dea mover a la posición deseada    
                                SELECT llave INTO llave_origen from T_EJERCICIO_PRO where consecutivo_des = consec_des ;
                                consec_posi := consec_des_destino || '.' || maxi2;
                                consec_des1 := REPLACE (consec_des , consec_des  ,consec_posi );
                                consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                consec1 := FA_CONSEC (consec_ord1);
                                UPDATE t_ejercicio_pro SET 
                                consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;
                                
                                llav_padre := llave_origen ;
                  
                                      FOR lista_hijos in c_hijos
                                      LOOP
                                          llave_nodo := lista_hijos.llave;
                                          llave1 := llave_nodo;
                                              FOR r1 IN c_sucesores
                                                  LOOP
                                                  r := r1.consecutivo_des;
                                                  SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE CONSECUTIVO_DES = r ;
                                                  SELECT consecutivo_des INTO consec_des_padre FROM T_EJERCICIO_PRO WHERE llave = llave_pa_origen ;
                                                  uel_cad1 := FA_ULTIMOEL(r);
                                                  consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                                  consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                  consec1 := FA_CONSEC (consec_ord1);
                                                  UPDATE t_ejercicio_pro SET 
                                                 consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = lista_hijos.llave ;
                                              END LOOP;  
                                      END LOOP;
                                              UPDATE t_ejercicio_pro SET
                                              llave_padre = llave_destino WHERE llave = llave_origen;
                                              
                                   ---------------- recorrido izquierda de los nodos raiz 
                                   niv_ar := 1;
                                   elem1_pos := FA_1EREL(consec_des);
                                             FOR r1 IN c_lv_t
                                             LOOP   
                                                  llave_nodo := r1.llave;
                                                  SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                                                  r := FA_1EREL(des_consec);
                                                  llave1 := llave_nodo;
                                                  
                                                    IF r > elem1_pos THEN
                                                        FOR r3 IN c_sucesores
                                                        LOOP
                                                            consec_des1 := FA_RECORRIDO(r3.CONSECUTIVO_DES , 2);
                                                            consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                            consec1 := FA_CONSEC (consec_ord1);
                                                         UPDATE t_ejercicio_pro SET 
                                                         consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = r3.llave ;  
                                                       END LOOP;
                                                  END IF;
                                            END LOOP;   
                                                 ----- fin recorrido izquierda2    
                                dbms_output.put_line( l_mensaje); 
           END IF;  ----------- if de nodo raiz a un nodo con hermanos 
           
           
        ---------------------------   MOVER NODOS HIJOS EN OTRO NODO HIJO, Y ESTE NODO DESTINO NO TIENE HIJOS. M8
           
                    IF NUM_HIJOS = 1 AND llav_posi IS NULL AND niv_ar2 > 1 THEN
                            --- se remplaza el nodo a la posición deseada
                             SELECT llave INTO llave_origen FROM t_ejercicio_pro WHERE consecutivo_des = consec_des  ;
                             llav_padre  := llave_origen ;
                             consec_posi := consec_des_destino || '.1' ;
                             consec_des1  := REPLACE (consec_des_destino , consec_des_destino  ,consec_posi );
                             consec_ord1 := FA_CONSEC_ORD (consec_des1);
                             consec1 := FA_CONSEC (consec_ord1);
                             UPDATE t_ejercicio_pro SET 
                             consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;                
                
                                  FOR lista_hijos IN c_hijos
                                  LOOP
                                      llave_nodo := lista_hijos.llave;
                                      llave1 := llave_nodo;
           
                                          FOR r1 IN c_sucesores
                                          LOOP
                                              r := r1.consecutivo_des;
                                              SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE CONSECUTIVO_DES = r ;
                                              SELECT consecutivo_des INTO consec_des_padre FROM T_EJERCICIO_PRO WHERE llave = llave_pa_origen ;
                                              uel_cad1 := FA_ULTIMOEL(r);
                                              consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                              consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                              consec1 := FA_CONSEC (consec_ord1);
                                              UPDATE t_ejercicio_pro SET 
                                              consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = r1.llave ;
                                          END LOOP; 
                                    END LOOP; ----- C_HIJOS      

                                    UPDATE t_ejercicio_pro SET
                                    indicador_tipo = 1 WHERE llave =  llave_destino;
             
                         ------------ Inicia recorrido izquierda de los hermanos abandonados
                       SELECT llave_padre into llave_pa_origen FROM T_EJERCICIO_PRO where llave = llave_origen  ;
                       SELECT count (consecutivo_des) - 1 into num_hijos FROM t_ejercicio_pro where llave_padre = llave_pa_origen;
                                                     
                        UPDATE t_ejercicio_pro SET
                        llave_padre = llave_destino WHERE llave= llave_origen ; 
               
                        IF num_hijos = 0 THEN
                          UPDATE t_ejercicio_pro SET
                          indicador_tipo = 0 WHERE llave = llave_pa_origen;
                 ------------------  
                        ELSIF num_hijos > 0 THEN
                          SELECT consecutivo_des INTO consec_des_padre FROM t_ejercicio_pro where llave= llave_pa_origen;
                          long_padre := LENGTH (consec_des_padre);
                          llav_padre := llave_pa_origen;
                                      
                                  for lista_hijos in c_hijos
                                  LOOP
                                      uel_par := FA_ULTIMOEL ( consec_des);
                                      llave_nodo := lista_hijos.llave;
                                      select consecutivo_des into cad1 FROM t_ejercicio_pro where llave = llave_nodo ;
                                      uel_cad1 := FA_ULTIMOEL ( cad1);
                                      llave1 := llave_nodo;               
                                          IF uel_par < uel_cad1 then
                                              for lista_nodos in c_sucesores
                                              LOOP
                                                long_padre := 0 ;
                                                long_padre := length (consec_des_padre);
                                                cad2 := lista_nodos.consecutivo_des;
                                                long_nh := length (cad2) ;
                                                num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                                                num_h := Fa_Recorrido (num_h , 2);
                                                consec_des1 := consec_des_padre || '.' || num_h;
                                                consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                consec1 := FA_CONSEC (consec_ord1);
                                                update t_ejercicio_pro set 
                                                consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  where llave = lista_nodos.llave ;
                                               END LOOP;   
                                          END IF;                
                                  END LOOP;                              
                       END IF; 
           dbms_output.put_line( l_mensaje );       
           END IF; ------- M8
         
          --------------------------- MOVER DE NODO HIJO a OTRO NODO  EN LA ULTIMA POSICION M6
                   IF num_hijos_aux > 1 AND llav_posi IS NULL AND niv_ar2 > 1 THEN
                   
                      SELECT llave into llave_destino FROM t_ejercicio_pro WHERE consecutivo_des = consec_des_destino ;
                      llav_padre := llave_destino ;
                      
                      ----- obtiene el nodo más grande del nodo destino.
                          FOR lista_hijos IN c_hijos
                          LOOP
                             llave_nodo := lista_hijos.llave;
                             SELECT consecutivo_des into des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                             UEL_CAD1 := FA_ULTIMOEL(des_consec);
                          
                                  IF uel_cad1 > maxi2  THEN
                                    maxi2 := uel_cad1;
                                  END IF;     
                           END LOOP; 
                              maxi2 := maxi2 +1 ;
                              
                         --- remplaza el nodo de la posición deseada     
                            SELECT llave into llave_origen from t_ejercicio_pro where consecutivo_des = consec_des ;  
                            consec_posi := consec_des_destino || '.' || maxi2;
                            consec_des1 := REPLACE (consec_des, consec_des ,consec_posi );
                            consec_ord1 := FA_CONSEC_ORD (consec_des1);
                            consec1 := FA_CONSEC (consec_ord1);
                            UPDATE t_ejercicio_pro SET 
                            consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;
                            llav_padre := llave_origen ;
                                    
                                    FOR lista_hijos IN c_hijos
                                    LOOP
                                       llave_nodo := lista_hijos.llave;
                                       llave1 := llave_nodo;
                                        FOR r1 IN c_sucesores
                                            LOOP
                                            r := r1.consecutivo_des;
                                            SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE CONSECUTIVO_DES = r ;
                                            SELECT consecutivo_des into consec_des_padre FROM T_EJERCICIO_PRO WHERE llave = llave_pa_origen ;
                                            uel_cad1 := FA_ULTIMOEL(r);
                                            consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                            consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                            consec1 := FA_CONSEC (consec_ord1);
                                            UPDATE t_ejercicio_pro SET 
                                           consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = r1.llave ;
                                        END LOOP;  
                                    END LOOP;
             
                   --------
                         SELECT llave_padre into llave_pa_origen FROM T_EJERCICIO_PRO WHERE llave = llave_origen  ;
                         SELECT COUNT (consecutivo_des) -1 into num_hijos FROM t_ejercicio_pro WHERE llave_padre = llave_pa_origen;
                                                           
                          UPDATE t_ejercicio_pro SET
                          llave_padre = llave_destino WHERE llave= llave_origen ; 
                             
                          IF num_hijos = 0 THEN
                              UPDATE t_ejercicio_pro SET
                              indicador_tipo = 0 WHERE llave = llave_pa_origen;
     ------------------ Recorrido de los nodos hermanos de donde se movio el nodo      
                          ELSIF num_hijos > 0 THEN
                              SELECT consecutivo_des INTO consec_des_padre FROM t_ejercicio_pro WHERE llave= llave_pa_origen;
                              long_padre := LENGTH (consec_des_padre);
                              llav_padre := llave_pa_origen;
                          
                                      FOR lista_hijos IN c_hijos
                                      LOOP
                                          uel_par := FA_ULTIMOEL ( consec_des);
                                          llave_nodo := lista_hijos.llave;
                                          SELECT consecutivo_des INTO cad1 FROM t_ejercicio_pro WHERE llave = llave_nodo ;
                                          uel_cad1 := FA_ULTIMOEL ( cad1);
                                          llave1 := llave_nodo;              
                                        IF uel_par < uel_cad1 THEN
                                            FOR lista_nodos IN c_sucesores
                                            LOOP
                                              long_padre := 0 ;
                                              long_padre := length (consec_des_padre);
                                              cad2 := lista_nodos.consecutivo_des;
                                              long_nh := length (cad2) ;
                                              num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                                              num_h := Fa_Recorrido (num_h , 2);
                                              consec_des1 := consec_des_padre || '.' || num_h;
                                              consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                              consec1 := FA_CONSEC (consec_ord1);
                                              UPDATE t_ejercicio_pro SET 
                                              consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  WHERE llave = lista_hijos.llave ;
                                             END LOOP;   
                                        END IF;                
                                    END LOOP;                              
                              END IF;  ----------- if de nodo raiz a un nodo con hermanos    
                         dbms_output.put_line( l_mensaje ); 
           END IF ;  ----- M6       
           
          
       END IF; ---- IF 
       
        -------------------------------------------------------------------------------------------------------------
       IF consec_des_destino IS NOT NULL AND llav_posi IS NOT NULL THEN
                  SELECT llave into llave_destino from t_ejercicio_pro where consecutivo_des = consec_des_destino ;
                  SELECT COUNT(consecutivo_des) INTO num_hijos FROM t_ejercicio_pro  START WITH llave = llave_destino CONNECT BY llave_padre = PRIOR llave;
                  SELECT llave into llave_origen from t_ejercicio_pro where consecutivo_des = consec_des;
                  SELECT llave_padre INTO llave_padre_Nodorigen FROM t_ejercicio_pro WHERE llave = llave_origen;
                  SELECT llave_padre INTO llave_padre_Nodestino FROM t_ejercicio_pro WHERE llave = llav_posi;
                  
        
          ---------------------------- MUEVE DE NODO RAIZ A OTRO NODO COMO HIJO CON  HERMANOS (SE MUEVE EN CUALQUIER POSICION MENOS LA ULTIMA) M5
                            IF NUM_HIJOS > 1 AND llav_posi >= 1 AND niv_ar2 = 1 THEN
                                llav_padre := llave_destino ;    
                                SELECT consecutivo_des INTO consec_posi FROM t_ejercicio_pro WHERE LLAVE = llav_posi ;    
                                SELECT consecutivo_des INTO consec_des_padre FROM t_ejercicio_pro WHERE llave = llave_destino;
                          ---------------- recorrido derecha de los nodo hijos destino
                                            FOR lista_hijos IN c_hijos
                                            LOOP
                                                uel_par := FA_ULTIMOEL ( consec_des_posicion );
                                                llave_nodo := lista_hijos.llave;
                                                SELECT consecutivo_des INTO cad1 FROM t_ejercicio_pro WHERE llave = llave_nodo ;
                                                uel_cad1 := FA_ULTIMOEL ( cad1);
                                                llave1 := llave_nodo;
                                                    IF uel_par <= uel_cad1 THEN                    
                                                        FOR lista_nodos IN c_sucesores
                                                        LOOP
                                                              long_padre := 0 ;
                                                              long_padre := length (consec_des_padre);
                                                              cad2 := lista_nodos.consecutivo_des;
                                                              long_nh := length (cad2) ;
                                                              num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                                                              num_h := Fa_Recorrido (num_h , 1);
                                                              consec_des1 := consec_des_padre || '.' || num_h;
                                                              consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                              consec1 := FA_CONSEC (consec_ord1);
                                                              UPDATE t_ejercicio_pro SET 
                                                              consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  WHERE llave = lista_nodos.llave ;
                                                          END LOOP;
                                                      END IF;
                                              END LOOP;               
                --------------- fin recorrido hijos destino
                --------------- insercion del nodo origen al nodo destino
                                       llav_padre  := llave_origen ;
                                       consec_des1 := REPLACE (consec_des , consec_des  ,consec_des_posicion  );
                                       consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                       consec1 := FA_CONSEC (consec_ord1);
                                       UPDATE t_ejercicio_pro SET 
                                       consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;
                                      
                                                FOR lista_hijos IN c_hijos
                                                LOOP
                                                    llave_nodo := lista_hijos.llave;
                                                    llave1 := llave_nodo;
                                                              FOR r1 in c_sucesores
                                                              LOOP
                                                                  r := r1.consecutivo_des;
                                                                 
                                                                  SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE CONSECUTIVO_DES = r ;
                                                                  SELECT consecutivo_des INTO consec_des_padre FROM T_EJERCICIO_PRO WHERE llave = llave_pa_origen ;
                                                                  uel_cad1 := FA_ULTIMOEL(   r);
                                                                  consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                                                  consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                                  consec1 := FA_CONSEC (consec_ord1);
                                                                  UPDATE t_ejercicio_pro SET 
                                                                  consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = r1.llave ;
                                                              END LOOP; 
                                                END LOOP; ----- C_HIJOS      

                                UPDATE t_ejercicio_pro SET
                                llave_padre = llave_destino WHERE llave = llave_origen ;
                                UPDATE t_ejercicio_pro SET
                                indicador_tipo = 1 WHERE llave =  llave_destino;
                                dbms_output.put_line( l_mensaje); 
               
                      ----- recorrido izquierda de los nodo raiz
                    niv_ar := 1;
                    elem1_pos := FA_1EREL(consec_des);
                        FOR r1 IN c_lv_t
                        LOOP   
                                llave_nodo := r1.llave;
                                SELECT consecutivo_des into des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                                r := FA_1EREL(des_consec);
                                llave1 := llave_nodo;
                          IF r > elem1_pos THEN
                            FOR r3 IN c_sucesores
                            LOOP
                                consec_des1 := FA_RECORRIDO(r3.CONSECUTIVO_DES , 2);
                                consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                consec1 := FA_CONSEC (consec_ord1);
                                UPDATE t_ejercicio_pro SET 
                                consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = r3.llave ;  
                            END LOOP;
                          END IF;
                        END LOOP;  
              
           END IF; ------ FIN M5
           
           
               ------------------- MOVER NODO HIJO A OTRO NODO COMO HIJO EN CUALQUIER POSICION MENOS LA ULTIMA M7
           
                            IF NUM_HIJOS > 1 AND llav_posi >= 1 AND niv_ar2 > 1 AND llave_padre_Nodorigen <> llave_padre_Nodestino THEN
                                  SELECT llave into llave_destino from t_ejercicio_pro where consecutivo_des = consec_des_destino ;
                                  llav_padre := llave_destino ;    
                  ---------------- recorrido derecha de los nodo hijos destino
                                           FOR lista_hijos IN c_hijos
                                           LOOP
                                              uel_par := FA_ULTIMOEL ( consec_des_posicion );
                                              llave_nodo := lista_hijos.llave;
                                              SELECT consecutivo_des INTO cad1 FROM t_ejercicio_pro WHERE llave = llave_nodo ;
                                              uel_cad1 := FA_ULTIMOEL ( cad1);
                                              llave1 := llave_nodo;
                                                    IF uel_par <= uel_cad1 THEN                    
                                                        FOR lista_nodos IN c_sucesores
                                                        LOOP
                                                            long_padre := 0 ;
                                                            long_padre := length (consec_des_destino );
                                                            cad2 := lista_nodos.consecutivo_des;
                                                            long_nh := length (cad2) ;
                                                            num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                                                            num_h := Fa_Recorrido (num_h , 1);
                                                            consec_des1 := consec_des_destino  || '.' || num_h;
                                                            consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                            consec1 := FA_CONSEC (consec_ord1);
                                                            UPDATE t_ejercicio_pro set 
                                                            consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  where llave = lista_nodos.llave ;
                                                          END LOOP;
                                                      END IF;
                                            END LOOP;               
                --------------- fin recorrido hijos destino
                --------------- insercion del nodo origen al nodo destino
                                       SELECT llave into llave_origen from t_ejercicio_pro where consecutivo_des = consec_des ;
                                       llav_padre  := llave_origen ;
                                       consec_des1 := REPLACE (consec_des, consec_des ,consec_des_posicion  );
                                       consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                       consec1 := FA_CONSEC (consec_ord1);
                                       UPDATE t_ejercicio_pro SET 
                                       consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = llave_origen ;
                                      
                                            for lista_hijos in c_hijos
                                            LOOP
                                                      llave_nodo := lista_hijos.llave;
                                                      llave1 := llave_nodo;   
           
                                                        FOR r1 in c_sucesores
                                                        LOOP
                                                            r := r1.consecutivo_des;
                                                            SELECT llave_padre into llave_pa_origen FROM T_EJERCICIO_PRO WHERE CONSECUTIVO_DES = r ;
                                                            SELECT consecutivo_des into consec_des_padre FROM T_EJERCICIO_PRO WHERE llave = llave_pa_origen ;
                                                            uel_cad1 := FA_ULTIMOEL(   r);
                                                            consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                                            consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                            consec1 := FA_CONSEC (consec_ord1);
                                                            UPDATE t_ejercicio_pro set 
                                                            consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = r1.llave ;
                                                        END LOOP; 
                                            END LOOP; ----- C_HIJOS      
                        
          
                            SELECT llave_padre into llave_pa_origen FROM T_EJERCICIO_PRO where llave = llave_origen  ;
                            SELECT COUNT (consecutivo_des) into num_hijos  FROM t_ejercicio_pro where llave_padre = llave_pa_origen;
                
                              UPDATE t_ejercicio_pro SET
                              llave_padre = llave_destino where llave= llave_origen ; 
               
                                  IF num_hijos = 0 THEN
                                    UPDATE t_ejercicio_pro SET
                                    indicador_tipo = 0 WHERE llave = llave_pa_origen;
          ------------ Inicia recorrido izquierda de los hermanos abandonados    
                                  ELSIF num_hijos > 0 then
                                      select consecutivo_des into consec_des_padre FROM t_ejercicio_pro where llave= llave_pa_origen;
                                      long_padre := length (consec_des_padre);
                                      llav_padre := llave_pa_origen;
                                        
                                            FOR lista_hijos in c_hijos
                                            LOOP
                                                uel_par := FA_ULTIMOEL ( consec_des);
                                                llave_nodo := lista_hijos.llave;
                                                Select consecutivo_des into cad1 FROM t_ejercicio_pro where llave = llave_nodo ;
                                                uel_cad1 := FA_ULTIMOEL ( cad1);
                                                llave1 := llave_nodo;          
                                                    IF uel_par < uel_cad1 then
                                                        FOR lista_nodos in c_sucesores
                                                        LOOP
                                                            long_padre := 0 ;
                                                            long_padre := length (consec_des_padre);
                                                            cad2 := lista_nodos.consecutivo_des;
                                                            long_nh := length (cad2) ;
                                                            num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2 );
                                                            num_h := Fa_Recorrido (num_h , 2);
                                                            consec_des1 := consec_des_padre || '.' || num_h;
                                                            consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                            consec1 := FA_CONSEC (consec_ord1);
                                                            UPDATE t_ejercicio_pro set 
                                                            consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  where llave = lista_nodos.llave ;
                                                   END LOOP;   
                                              END IF;                
                                          END LOOP;                              
                                  END IF; 
           dbms_output.put_line( l_mensaje);
        END IF ; ----M7   
        
        
        --------------------------- MOVER NODOS HIJOS DENTRO DEL MISMO NODO PADRE M9
         
                          IF NUM_HIJOS > 1 AND llav_posi >=1 and niv_ar2 > 1  AND llave_padre_Nodorigen = llave_padre_Nodestino THEN
                                SELECT llave into llave_origen from t_ejercicio_pro where consecutivo_des = consec_des ;
                                SELECT llave into llave_destino from t_ejercicio_pro where consecutivo_des = consec_des_destino;
                                nodo_origen := FA_ULTIMOEL ( consec_des );
                                nodo_destino := FA_ULTIMOEL (consec_des_posicion );
                                llav_padre := llave_destino;
                            --- inicia recorrido nodos 
                                  FOR lista_hijos IN c_hijos
                                  LOOP
                                      llave_nodo := lista_hijos.llave;
                                      select consecutivo_des into cad1 FROM t_ejercicio_pro where llave = llave_nodo ;
                                      NODO  := FA_ULTIMOEL ( cad1);
                                      llave1 := llave_nodo;
                                  ---- si el nodo se mueve a la izquierda
                                          IF nodo >= nodo_destino AND nodo < nodo_origen  THEN
                                                      FOR lista_nodos IN c_sucesores
                                                      LOOP
                                                          long_padre := 0 ;
                                                          long_padre := length (consec_des_destino);
                                                          cad2 := lista_nodos.consecutivo_des;
                                                          long_nh := length (cad2) ;
                                                          num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                                                          num_h := Fa_Recorrido (num_h , 1);
                                                          consec_des1 := consec_des_destino || '.' || num_h;
                                                          consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                          consec1 := FA_CONSEC (consec_ord1);
                                                          UPDATE t_ejercicio_pro set 
                                                          consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  where llave = lista_nodos.llave ;
                                                      END LOOP;
                                         END IF;
                                     ---- si el nodo se mueve a la derecha
                                        IF nodo >= nodo_origen and nodo <= nodo_destino then
                                            for lista_nodos in c_sucesores
                                                  LOOP
                                                    long_padre := 0 ;
                                                    long_padre := length (consec_des_destino);
                                                    cad2 := lista_nodos.consecutivo_des;
                                                    long_nh := length (cad2) ;
                                                    num_h := SUBSTR (cad2, long_padre + 2  ,long_nh + 2);
                                                    num_h := Fa_Recorrido (num_h , 2);
                                                    consec_des1 := consec_des_destino || '.' || num_h;
                                                    consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                    consec1 := FA_CONSEC (consec_ord1);
                                                    UPDATE t_ejercicio_pro set 
                                                    consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1  WHERE llave = lista_nodos.llave ;
                                                   END LOOP;
                                               END IF;         
                                           END LOOP;                   
                                           
             ---------- remplaza el nodo origen a la posicion deseada.
                        
                          consec_des1 := REPLACE (consec_des, consec_des ,consec_des_posicion  );
                          consec_ord1 := FA_CONSEC_ORD (consec_des1);
                          consec1 := FA_CONSEC (consec_ord1);
                          UPDATE t_ejercicio_pro SET 
                          consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;
                          llav_padre := llave_origen;
                 
                            FOR lista_hijos IN c_hijos
                            LOOP
                              llave_nodo := lista_hijos.llave;
                              llave1 := llave_nodo;
                                  FOR r1 IN c_sucesores
                                  LOOP
                                      r := r1.consecutivo_des;
                                      SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO where CONSECUTIVO_DES = r ;
                                      SELECT consecutivo_des INTO consec_des_padre FROM T_EJERCICIO_PRO where llave = llave_pa_origen ;
                                      uel_cad1 := FA_ULTIMOEL(r);
                                      consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                      consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                      consec1 := FA_CONSEC (consec_ord1);
                                      UPDATE t_ejercicio_pro SET 
                                      consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = r1.llave ;
                                  END LOOP; 
                             END LOOP;     
                              dbms_output.put_line( l_mensaje);
          END IF;  ---- M9
       
       END IF; --------- IF llave_destino >= 1   and llav_posi is not null   */
        
 END sp_mover_nodo1;
 
 PROCEDURE sp_mover_nodo (
  consec_des_destino    IN VARCHAR2
, consec_des IN VARCHAR2
, consec_des_posicion IN VARCHAR2
) 
 AS
 l_mensaje VARCHAR2(100) := '¡Hola, el movimiento del nodo se hizo correctamente!';
 niv_ar NUMBER;
 nodo_destino VARCHAR2 (242);
 r VARCHAR2 (242);
 nodo NUMBER;
 lista_sucesores1 VARCHAR2 (242);
 llave1 NUMBER;
 llav_padre NUMBER;
 llave_pa_origen NUMBER;
 consec_des1 VARCHAR2(242);
 consec_ord1 VARCHAR2 (242);
 consec1 VARCHAR2 (242) ;
 elem_rep NUMBER;
 cad2 VARCHAR2 (242) ;
 nodo_origen NUMBER;
 num_hijos NUMBER;
 consec_des_padre VARCHAR2 (242);
 long_padre NUMBER;
 uel_cad1 NUMBER;
 cad1 VARCHAR2 (242);
 long_nh NUMBER;
 num_h VARCHAR2 (242);
 llave_padre_Nodorigen NUMBER;
 llave_padre_NodoPos NUMBER;
 maxi2 NUMBER := 0;
 LLAV NUMBER;
 LLAVE_NODO NUMBER;
 llave_destino NUMBER;
 llave_suc NUMBER;
 num_hijos_nodo_destino NUMBER;
 llave_origen NUMBER;
 DES_CONSEC VARCHAR2(240);
 num_hijos_padre_origen NUMBER;
 nodo_pos NUMBER;
 niv_ar_origen NUMBER;
  CURSOR c_sucesores IS  select  consecutivo_des, llave FROM t_ejercicio_pro  start with llave = llave1 connect by llave_padre = prior llave ; --order by level
  CURSOR c_hijos IS SELECT llave FROM t_ejercicio_pro WHERE  nvl(llave_padre , 0) = llav_padre ;
 
 BEGIN
      SELECT COUNT(consecutivo_des) INTO nodo_pos FROM t_ejercicio_pro WHERE consecutivo_des = consec_des_posicion;
        IF nodo_pos = 0  THEN
            llave_padre_NodoPos := -1 ;
        ELSE
            SELECT NVL (llave_padre, 0) INTO llave_padre_NodoPos FROM t_ejercicio_pro WHERE consecutivo_des = consec_des_posicion;
        END IF;
        SELECT NVL (llave_padre, 0) INTO llave_padre_Nodorigen FROM t_ejercicio_pro WHERE consecutivo_des = consec_des;
        SELECT llave INTO llave_origen FROM t_ejercicio_pro WHERE consecutivo_des = consec_des;

              ------- MOVIMIENTO DE NODOS CON EL MISMO NODO PADRE. (MOVIMIENTO ENTRE NODOS HERMANOS)
                  --- si nodo origen y nodo posicion tienen el mismo nodo padre.
            IF llave_padre_Nodorigen = llave_padre_NodoPos THEN
                niv_ar := FA_NIV_AR (consec_des_posicion);
                SELECT llave into llave_origen from t_ejercicio_pro where consecutivo_des = consec_des ;
                
                      IF niv_ar = 1 then
                          llav_padre := 0;
                          nodo_destino := FA_1EREL (consec_des_posicion);
                          nodo_origen := FA_1EREL (consec_des);
                      ELSE
                          SELECT llave into llave_destino from t_ejercicio_pro where consecutivo_des = consec_des_destino;
                          llav_padre := llave_destino;
                          nodo_origen := FA_ULTIMOEL (consec_des );
                          nodo_destino := FA_ULTIMOEL (consec_des_posicion );
                      END IF;
                          ------ hace corrimientos de los nodos hermanos.
                                FOR r1 IN c_hijos
                                LOOP
                                    llave_nodo := r1.llave;
                                    SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                                        IF niv_ar = 1 then
                                            nodo := to_number(des_consec);
                                        ELSE
                                            NODO  := FA_ULTIMOEL ( des_consec);
                                        END IF; 
                                        llave1 := llave_nodo;
                                        --- si se desea mover el nodo a la izquierda
                                          IF nodo >= nodo_destino AND nodo < nodo_origen  THEN
                                              FOR lista_sucesores IN c_sucesores 
                                              LOOP
                                                  lista_sucesores1 := lista_sucesores.consecutivo_des;
                                                          IF niv_ar = 1 then
                                                              consec_des1 := FA_RECORRIDO(lista_sucesores1 , 1);
                                                          ELSE
                                                              long_padre := 0 ;
                                                              long_padre := length (consec_des_destino);
                                                              long_nh := length (lista_sucesores1) ;
                                                              num_h := SUBSTR (lista_sucesores1, long_padre + 2  ,long_nh + 2);
                                                              num_h := Fa_Recorrido (num_h , 1);
                                                              consec_des1 := consec_des_destino || '.' || num_h;
                                                          END IF;
                                                          consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                          consec1 := FA_CONSEC (consec_ord1);
                                                          UPDATE t_ejercicio_pro SET 
                                                          consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = lista_sucesores.llave ;               
                                              END LOOP;
                                          END IF;
                                      --- si se desea mover el nodo a la derecha
                                          IF nodo >= nodo_origen AND nodo <= nodo_destino THEN
                                              FOR lista_sucesores IN c_sucesores 
                                              LOOP
                                                  lista_sucesores1 := lista_sucesores.consecutivo_des;
                                                          IF niv_ar = 1 then
                                                          consec_des1 := FA_RECORRIDO(lista_sucesores1 , 2);
                                                          ELSE
                                                            --  long_padre := 0 ;
                                                              long_padre := length (consec_des_destino);
                                                              long_nh := length (lista_sucesores1) ;
                                                              num_h := SUBSTR (lista_sucesores1, long_padre + 2  ,long_nh + 2);
                                                              num_h := Fa_Recorrido (num_h , 2 );
                                                              consec_des1 := consec_des_destino || '.' || num_h;
                                                          END IF;
                                                          consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                          consec1 := FA_CONSEC (consec_ord1);
                                                          UPDATE t_ejercicio_pro SET 
                                                          consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = lista_sucesores.llave ;               
                                              END LOOP;
                                          END IF;            
                                END LOOP;                      
                              -- remplaza el nodo origen al nodo deseado
                                consec_des1 := consec_des_posicion;
                                consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                consec1 := FA_CONSEC (consec_ord1);
                                UPDATE t_ejercicio_pro SET 
                                consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;
                                llav_padre := llave_origen;
         
                                        FOR lista_hijos IN c_hijos
                                        LOOP
                                          llave_nodo := lista_hijos.llave;
                                          llave1 := llave_nodo;
                                              FOR r1 IN c_sucesores
                                              LOOP
                                                  r := r1.consecutivo_des;
                                                  llave_suc := r1.llave;
                                                      IF niv_ar = 1 THEN
                                                           elem_rep := FA_1EREL(r);
                                                           cad2 := ltrim (r , elem_rep);
                                                           elem_rep := consec_des_posicion;
                                                           consec_des1 := elem_rep || cad2;   
                                                      ELSE
                                                          SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO where llave = llave_suc ;
                                                          SELECT consecutivo_des INTO consec_des_padre FROM T_EJERCICIO_PRO where llave = llave_pa_origen ;
                                                          uel_cad1 := FA_ULTIMOEL(r);
                                                          consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                                      END IF;
                                                      consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                      consec1 := FA_CONSEC (consec_ord1);
                                                      UPDATE t_ejercicio_pro SET 
                                                      consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = r1.llave ;
                                              END LOOP; 
                                        END LOOP;
                                        dbms_output.put_line(l_mensaje);
            END IF;
 
 
            ------- APLICA PARA LOS 8 DIFERENTES MOVIMIENTOS RESTANTES
                --- si el padre de los nodo origen y nodo posicion son distintos
           IF  llave_padre_Nodorigen <> llave_padre_NodoPos THEN

                    IF consec_des_destino IS NULL THEN
                        llave_destino := 0 ;
                    ELSE 
                        SELECT llave into llave_destino from t_ejercicio_pro where consecutivo_des = consec_des_destino;
                    END IF;
                    SELECT COUNT (consecutivo_des) INTO num_hijos_nodo_destino FROM t_ejercicio_pro  START WITH nvl(llave_padre , 0) = llave_destino CONNECT BY llave_padre = PRIOR llave ;
                    SELECT COUNT (consecutivo_des) INTO num_hijos_padre_origen FROM t_ejercicio_pro WHERE  nvl(llave_padre , 0) = llave_padre_Nodorigen ;
                    

                              -- si el nodo origen tiene hermanos hace corriemiento izquierda de los nodos hermanos abandonados
                              IF num_hijos_padre_origen > 1 THEN
                                  niv_ar_origen := FA_NIV_AR (consec_des);
                                        IF niv_ar_origen = 1 THEN
                                            llav_padre := 0;
                                            nodo_origen := to_number (consec_des);
                                        ELSE
                                            llav_padre := llave_padre_Nodorigen;
                                            nodo_origen := FA_ULTIMOEL (consec_des );
                                        END IF;
                                        
                                                FOR lista_hijos IN c_hijos
                                                LOOP
                                                    llave_nodo := lista_hijos.llave;
                                                    llave1 := llave_nodo;
                                                    SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo ;
                                                          IF niv_ar_origen = 1 THEN
                                                              r := to_number (des_consec);  
                                                          ELSE
                                                              r := FA_ULTIMOEL ( des_consec);
                                                              SELECT consecutivo_des INTO consec_des_padre FROM t_ejercicio_pro where llave = llave_padre_Nodorigen;
                                                          END IF;
                                                      
                                                               IF r > nodo_origen THEN
                                                                      FOR lista_nodos IN c_sucesores
                                                                      LOOP
                                                                            IF niv_ar_origen = 1 THEN
                                                                                consec_des1 := FA_RECORRIDO(lista_nodos.CONSECUTIVO_DES , 2);
                                                                            ELSE
                                                                                long_padre := 0 ;
                                                                                long_padre := LENGTH (consec_des_padre);
                                                                                cad2 := lista_nodos.consecutivo_des;
                                                                                long_nh := LENGTH (cad2) ;
                                                                                num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                                                                                num_h := Fa_Recorrido (num_h , 2);
                                                                                consec_des1 := consec_des_padre || '.' || num_h;
                                                                            END IF;
                                                                            consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                                            consec1 := FA_CONSEC (consec_ord1);
                                                                            UPDATE t_ejercicio_pro SET 
                                                                            consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = lista_nodos.llave ;
                                                                      END LOOP;
                                                               END IF;
                                                END LOOP;
                              ELSIF num_hijos_padre_origen = 1 THEN
                                    UPDATE t_ejercicio_pro SET 
                                    INDICADOR_TIPO = 0  WHERE llave = llave_padre_Nodorigen; 
                              END IF;
           
                        
                              -- si el nodo destino no tiene hijos. El nodo origen es remplazado por nodo_origen.1
                              IF num_hijos_nodo_destino = 0 THEN
                                  SELECT llave INTO llave_destino FROM t_ejercicio_pro WHERE consecutivo_des = consec_des_destino ;
                                  consec_des1 := consec_des_destino || '.1' ;
                                  consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                  consec1 := FA_CONSEC (consec_ord1);
                                  UPDATE t_ejercicio_pro SET 
                                  consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ; 
                                  
                                  UPDATE t_ejercicio_pro SET
                                  INDICADOR_TIPO = 1  WHERE llave = llave_destino ;
                                  
                              -- si el nodo destino tiene hijos y NO se desea mover a la ultima posición
                              ELSIF num_hijos_nodo_destino > 0 AND nodo_pos > 0 THEN
                                    --- hace corrimiento derecha de los nuevos nodos hermanos
                                    niv_ar := FA_NIV_AR (consec_des_posicion);
                                        IF niv_ar = 1 THEN
                                            llav_padre := 0;
                                            nodo_destino := to_number (consec_des_posicion);
                                        ELSE
                                            llav_padre := llave_padre_NodoPos;
                                            nodo_destino := FA_ULTIMOEL (consec_des_posicion );
                                        END IF;  
                                            
                                            FOR lista_hijos IN c_hijos
                                            LOOP
                                                llave_nodo := lista_hijos.llave;
                                                llave1 := llave_nodo;
                                                SELECT consecutivo_des INTO des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo ;
                                                      IF niv_ar = 1 THEN
                                                          r := to_number (des_consec);  
                                                      ELSE
                                                          r := FA_ULTIMOEL ( des_consec);
                                                          SELECT consecutivo_des INTO consec_des_padre FROM t_ejercicio_pro where llave = llave_padre_NodoPos;
                                                      END IF;
                                                          
                                                            IF r >= nodo_destino THEN
                                                                FOR lista_nodos IN c_sucesores
                                                                LOOP
                                                                    IF niv_ar = 1 THEN
                                                                        consec_des1 := FA_RECORRIDO(lista_nodos.CONSECUTIVO_DES , 1);
                                                                    ELSE
                                                                        long_padre := 0 ;
                                                                        long_padre := LENGTH (consec_des_padre);
                                                                        cad2 := lista_nodos.consecutivo_des;
                                                                        long_nh := LENGTH (cad2) ;
                                                                        num_h := SUBSTR (cad2, long_padre + 2 ,long_nh + 2);
                                                                        num_h := Fa_Recorrido (num_h , 1);
                                                                        consec_des1 := consec_des_padre || '.' || num_h;
                                                                    END IF;
                                                                    consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                                                    consec1 := FA_CONSEC (consec_ord1);
                                                                    UPDATE t_ejercicio_pro SET 
                                                                    consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = lista_nodos.llave ;
                                                                END LOOP;
                                                            END IF;
                                            END LOOP;
                                            ----- se remplaza el nodo origen
                                           consec_des1 := consec_des_posicion;
                                           consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                           consec1 := FA_CONSEC (consec_ord1);
                                           UPDATE t_ejercicio_pro SET 
                                           consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;
                              --- si el  nodo destino tiene hijos y se desea mover a la ultima posicion se obtiene el nodo maximo
                              ELSIF nodo_pos = 0 and num_hijos_nodo_destino > 0 THEN
                                  niv_ar := FA_NIV_AR (consec_des_posicion);
                                      IF niv_ar = 1 THEN
                                          llav_padre := 0;
                                      ELSE
                                          SELECT llave into llave_destino from t_ejercicio_pro where consecutivo_des = consec_des_destino;
                                          llav_padre := llave_destino;
                                      END IF;
                                      
                                          FOR lista_hijos IN c_hijos
                                          LOOP
                                              llave_nodo := lista_hijos.llave;
                                              SELECT consecutivo_des into des_consec FROM t_ejercicio_pro WHERE llave = llave_nodo;
                                                  IF niv_ar = 1 THEN
                                                      nodo := to_number(des_consec) ;
                                                  ELSE
                                                      nodo := FA_ULTIMOEL(des_consec);
                                                  END IF;          
                                                        IF nodo > maxi2  THEN
                                                            maxi2 := nodo;
                                                        END IF;
                                          END LOOP;
                                          maxi2 := maxi2 + 1;
                                          dbms_output.put_line( 'El elemento maximo es: ' || maxi2);
                                          -- remplaza el nodo destino por el ultimo nodo hermano.
                                          IF niv_ar = 1 THEN
                                              consec_des1 := REPLACE (consec_des , consec_des  ,maxi2 );
                                          ELSE
                                              consec_des1 := consec_des_destino || '.' || maxi2;
                                          END IF;
                                          consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                          consec1 := FA_CONSEC (consec_ord1);
                                          UPDATE t_ejercicio_pro SET 
                                          consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 WHERE llave = llave_origen ;
                              END IF;
                              ----- MUEVE A TODA LA RAMA FALTANTE
                              llav_padre  := llave_origen ;
                              FOR lista_hijos IN c_hijos
                              LOOP
                                  llave_nodo := lista_hijos.llave;
                                  llave1 := llave_nodo;   
                                    FOR r1 in c_sucesores
                                    LOOP
                                        llave_nodo := r1.llave;
                                        r := r1.consecutivo_des;
                                        SELECT llave_padre INTO llave_pa_origen FROM T_EJERCICIO_PRO WHERE llave = llave_nodo ;
                                        SELECT consecutivo_des INTO consec_des_padre FROM T_EJERCICIO_PRO WHERE llave = llave_pa_origen ;
                                        uel_cad1 := FA_ULTIMOEL(r);
                                        consec_des1 := consec_des_padre || '.' || uel_cad1; 
                                        consec_ord1 := FA_CONSEC_ORD (consec_des1);
                                        consec1 := FA_CONSEC (consec_ord1);
                                        UPDATE t_ejercicio_pro set 
                                        consecutivo_des = consec_des1 , consecutivo_ord = consec_ord1, CONSECUTIVO = consec1 where llave = r1.llave ;
                                    END LOOP; 
                              END LOOP;
                              --- se actualiza el nuevo nodo padre, del nodo origen
                             IF consec_des_destino IS NOT NULL THEN 
                                 SELECT llave INTO llave_destino FROM t_ejercicio_pro WHERE consecutivo_des = consec_des_destino;
                                 UPDATE t_ejercicio_pro SET
                                 llave_padre = llave_destino WHERE llave = llave_origen ;
                             ELSE
                                UPDATE t_ejercicio_pro SET
                                llave_padre = NULL WHERE llave = llave_origen ;
                             END IF;
                             dbms_output.put_line(l_mensaje);
           END IF;
       
 END sp_mover_nodo;
 
END pck_ejercicio_pro;
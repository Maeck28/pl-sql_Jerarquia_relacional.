---------- https://www.tutorialesprogramacionya.com/oracleya/index.php?inicio=0
---------- https://www.tutorialesprogramacionya.com/oracleya/temarios/descripcion.php?inicio=0&cod=179&punto=21
---------- https://sites.google.com/site/josepando/home/consultas-y-subconsultas-sql/consultas-jerrquicas
-- https://www.tutorialesprogramacionya.com/visualbasicya/index.php?inicio=0
delete from t_ejercicio_pro where llave = 32;
Insert into T_EJERCICIO_PRO (LLAVE,LLAVE_PADRE,CONSECUTIVO,CONSECUTIVO_DES,CONSECUTIVO_ORD,INDICADOR_TIPO) values (31,null,3,'3','03',0);
update t_ejercicio_pro set consecutivo_des = '7' , consecutivo=7 , CONSECUTIVO_ORD = '07' where llave=29;
update t_ejercicio_pro set consecutivo_des = '7.1' , consecutivo=1 , CONSECUTIVO_ORD = '0701' where llave=30;
Delete from T_ejercicio_pro;
---------- funciones ------------
SELECT fa_consec (03010203) from dual;
select fa_consec_ord ('30.1.1.1.1.1.1.1.2.1.1') from dual;
select fa_indicadorT ('1.1') from dual;
select fa_niv_ar ('5') from dual;
select length (fa_1erEl ('123.2.5.3')) from dual;
select fa_recorrido ('8.2.1', 2) from dual;
SELECT fa_ultimoEl ('5') FROM DUAL;
--------------------------------------------

 select  llave, llave_padre , consecutivo_des, level 
 from t_ejercicio_pro 
 start with llave = 15
 connect by llave_padre = prior llave
 order by level;
 ------------------------------------
select llave from t_ejercicio_pro where Consecutivo_Des = '1'; 
select llave_padre from t_ejercicio_pro where llave = 1;
 
 ------------------------------------
select length(1) - length(REPLACE(1,'.'))  from dual;
select llave from t_ejercicio_pro where (length(CONSECUTIVO_DES) - length(REPLACE(CONSECUTIVO_DES,'.'))+1) = 1;  ------------ saber todos los nodos a nivel de arbol
select count (consecutivo_des) from t_ejercicio_pro where consecutivo_des like('5.1.1.1.2%') ;  ------ cuenta cuantos nodos hay por debajo de el + 1.

SELECT llave from t_ejercicio_pro where consecutivo_des = '10';
SELECT llave FROM t_ejercicio_pro WHERE (length(CONSECUTIVO_DES) - length(REPLACE(CONSECUTIVO_DES,'.'))+1) = 1;
select  llave_padre from t_ejercicio_pro  where llave = 7;

select llave from t_ejercicio_pro where llave = 25;
describe t_ejercicio_pro;
select length ('120.10.1.4200') from dual;
select instr ('5.1.2.3.4' , '.', 1 , 4) from dual;

SELECT SUBSTR ('120.10.1.4200', 5 ,13) from dual;
SELECT SUBSTR ('11.110.1100.41200', 0 ,2) from dual;
select ltrim ('5.1.1.1', '5') from dual;
select replace ('5.3.2' , '5.3.2', '2') from dual;
select  ('5.1.1.1' || '.' || '2') from dual;
select CONSECUTIVO_DES  from t_ejercicio_pro where indicador_tipo = 1 ;
select * from t_ejercicio_pro order by  (CONSECUTIVO_ORD) ;  ------------ ver la tabla en orden  DESC ( forma inversa)
select  * from t_ejercicio_pro  start with llave =6 connect by llave_padre = prior llave;
select  llave from t_ejercicio_pro  start with llave = 25 connect by llave_padre = prior llave;
select llave_padre from t_ejercicio_pro where llave= 20;
select llave_padre from t_ejercicio_pro where llave = 19;
select count(consecutivo_des) from t_ejercicio_pro where llave = 18;
select llave_padre from t_ejercicio_pro where llave=25;
select consecutivo_des from t_ejercicio_pro where llave_padre = 19;
SELECT * FROM t_ejercicio_pro  where ROWNUM > 1  ;
select  FA_ULTIMOEL ('3') from dual;
select replace('3.0.0.0.0','.','0') from dual;
select consecutivo_des from t_ejercicio_pro where llave = null;
select llave_padre from t_ejercicio_pro where llave = 15;
select llave from t_ejercicio_pro where llave_padre = 25;

select max( rownum) from t_ejercicio_pro where consecutivo_des = '3';

SELECT CONSECUTIVO_DES FROM t_ejercicio_pro WHERE (length(CONSECUTIVO_DES) - length(REPLACE(CONSECUTIVO_DES,'.'))+1) = 1;

select llave from t_ejercicio_pro where consecutivo_des = '10';
select * from t_ejercicio_pro order by  (llave) ;
select * from t_ejercicio_pro order by  (consecutivo_ord) ;
-------------------------------------------------------
--- PARAMETROS DE ENTRADA (LLAVE_PADRE DONDE SE VA INSERTAR EL NODO, CONSECUTIVO_DES NODO QUE SE VA INSERTAR


BEGIN
PCK_EJERCICIO_PRO.SP_MOVER_NODO1 ( null , 8 , null); 
END;
/


BEGIN
PCK_EJERCICIO_PRO.SP_INSERTAR_NODO1( NULL ,' 1');
END;
/

-------------------------------------------------------

--- PARMETROS DE ENTRADA (CONSECUTIVO_DES DEL NODO QUE SE DESEA BORRAR)
BEGIN
PCK_EJERCICIO_PRO.SP_BORRAR_NODO1 ('1');
END;
/

-------------------------------------------------------- 
----- PAREMETROS DE ENTRADA (LLAVE PADRE DESTINO DONDE SE VA COLOCAR , LLAVE DEL CONSECUTIVO_DES QUE SE DEESEA MOVER , LLAVE DE LA POSICION DESTINO)

BEGIN
--PCK_EJERCICIO_PRO.SP_MOVER_NODO1 ( null , '3.1' , '4'); 
END;
/
select * from t_ejercicio_pro  order by  (consecutivo_ord) ;

BEGIN
PCK_EJERCICIO_PRO.SP_MOVER_NODO ( '7' , '6.1' , '7.2'); 
END;
/

SELECT PCK_EJERCICIO_PRO.fa_niv_ar('20.2.2') FROM DUAL;

select llave FROM t_ejercicio_pro where  nvl(llave_padre , 0) = 0 ;

SELECT COUNT (consecutivo_des


) FROM t_ejercicio_pro  START WITH nvl(llave_padre , 0) = 0 CONNECT BY llave_padre = PRIOR llave ;
SELECT COUNT (consecutivo_des) FROM t_ejercicio_pro WHERE  nvl(llave_padre , 0) = 4 ;

SELECT SUBSTR ('3.2.1', 5 , 7) FROM DUAL;

SELECT * FROM T_EJERCICIO_PRO WHERE ROWNUM < 6 ;
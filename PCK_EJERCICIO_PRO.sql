create or replace PACKAGE pck_ejercicio_pro
AS

 FUNCTION  fa_consec(
  consec_ord IN VARCHAR2
  )
    RETURN NUMBER;
    
 FUNCTION fa_consec_ord(
  consec_des IN VARCHAR2
  )
    RETURN VARCHAR2;
 
 FUNCTION fa_niv_ar (
    consec_des IN VARCHAR2
 )
 RETURN NUMBER ;   
 
 FUNCTION fa_recorrido (
  consec_des IN VARCHAR2
,  tipo IN NUMBER
)
 RETURN VARCHAR2 ;
 
 FUNCTION fa_ultimoEl (
 consec_des IN VARCHAR2
)
 RETURN NUMBER;

 PROCEDURE sp_borrar_nodo1(
  consec_des IN VARCHAR2
  );
  
 PROCEDURE sp_insertar_nodo1(
  consec_des_destino IN VARCHAR2
, consec_des IN VARCHAR2
  );

PROCEDURE sp_mover_nodo1 (
  consec_des_destino    IN VARCHAR2
, consec_des IN VARCHAR2
, consec_des_posicion IN VARCHAR2
) ;


PROCEDURE sp_mover_nodo (
  consec_des_destino    IN VARCHAR2
, consec_des IN VARCHAR2
, consec_des_posicion IN VARCHAR2
) ;

END pck_ejercicio_pro;
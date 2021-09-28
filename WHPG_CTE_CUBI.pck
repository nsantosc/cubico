create or replace package WHPG_CTE_CUBI is
  /***************************************************************************
     Creado  : NSC - 16-07-2021
     Descrip.: paquete de constantes para Integración CUBICO
     Modif.  : 
     Descrip.:
  ****************************************************************************/
  -- Constante de errores
  ERRO CONSTANT NUMBER(1) := 1;
  OK   CONSTANT NUMBER(1) := 0;
  NOEL CONSTANT NUMBER(1) := -1;
  -- Constante respuesta 
  MENS_OK        CONSTANT VARCHAR2(60) := 'PROCESO GENERADO SATISFACTORIAMENTE';
  MENS_NOEL      CONSTANT VARCHAR2(60) := 'DOCUMENTO NO ES ELECTRONICO';
  MENS_ERRO      CONSTANT VARCHAR2(60) := 'ERROR AL PROCESAR TRANSACCI¿N!!!';
  MENS_DATA_NULL CONSTANT VARCHAR2(60) := 'NO SE ENCONTRO DATA';
  MENS_TOO_ROWS  CONSTANT VARCHAR2(60) := 'SE ENCONTRO MAS REGISTROS DE LO ESPERADO';
  -- Compa¿ia
  COD_COMP VARCHAR(2) := '01';
  COD_CIA  VARCHAR(2) := '00';
  -- Tipos de Movimiento
  MOVI_IC    VARCHAR(2) := 'IC';
  MOVI_IT    VARCHAR(2) := 'IT';
  MOVI_IU    VARCHAR(2) := 'IU';
  DOCU_PI    VARCHAR(2) := 'PI';
  DOC_REF_OC VARCHAR(2) := 'OC';
  MOVI_SM    VARCHAR(2) := 'SM';
  DOCU_GU    VARCHAR(2) := 'GU';
  MOTI_CV    VARCHAR(2) := 'CV';
  MOTI_SV    VARCHAR(2) := 'SV';
  MOVI_SU    VARCHAR(2) := 'SU';
  MOVI_ID    VARCHAR(2) := 'ID';
  MOVI_ST    VARCHAR(2) := 'ST';
  -- NSC 07-10-20
  MOVI_SC    VARCHAR(2) := 'SC';
  MOTI_POP   VARCHAR(2) := '11';
  MOTI_DEG   VARCHAR(2) := '03';
  -- Prefijos
  ORCO       VARCHAR(2) := 'OC';
  PEDI       VARCHAR(2) := 'PA';
  TRAN       VARCHAR(2) := 'TR';
  ACON_PTER  VARCHAR(3) := 'APT';
  ACON_INSU  VARCHAR(3) := 'AIN';
  LOGI_INVE  VARCHAR(2) := 'LI';
  ARTI       VARCHAR(2) := 'AR';
  CLIE       VARCHAR(2) := 'CL';
  -- Estado BE
  EST_BE     VARCHAR(2) := 'BE';
  EST_PR     VARCHAR(2) := 'PR';
  -- Ubicación
  UBI_RECEP    VARCHAR(5) := 'RECEP';
  UBI_PRODU    VARCHAR(5) := 'PRODU';
  -- Almacen CUBICO
  COD_ALMA_OLIS       VARCHAR(2) := '67';
  COD_ALMA_OLIS_MAQU  VARCHAR(2) := '76';
  
  
  
  
end;
/
create or replace package body WHPG_CTE_CUBI is

end;
/

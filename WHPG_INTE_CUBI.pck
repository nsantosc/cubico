create or replace package WHPG_INTE_CUBI is

  -- Author  : Nilton Santos
  -- Created : 5/07/2021 15:33:15
  -- Purpose : Procedimientos y funciones usados en la integracion de CUBICO con XRAY

  TYPE LIST_CURS IS REF CURSOR;

  PROCEDURE WHPR_CONS_DOCU_ELEC(pv_Empresa         IN VARCHAR2,
                                pv_TipoDocumento   IN VARCHAR2,
                                pv_SerieDocumento  IN VARCHAR2,
                                pv_NumeroDocumento IN VARCHAR2,
                                pc_refc            OUT SYS_REFCURSOR,
                                S_ERRO             OUT NUMBER,
                                S_MENS             OUT VARCHAR2);

  PROCEDURE WHPR_INTE_ENVI_ACON_PTER(pc_refc OUT SYS_REFCURSOR);

  PROCEDURE WHPR_UPDT_ORDE_PROD(pv_NroSecuencia IN VARCHAR2,
                                pv_Estado       IN VARCHAR2,
                                S_ERRO          OUT NUMBER,
                                S_MENS          OUT VARCHAR2);

  PROCEDURE WHPR_INTE_ENVI_RECE(pc_refc OUT SYS_REFCURSOR);

  PROCEDURE WHPR_UPDT_ORDE_RECE(pv_TipoMov      IN VARCHAR2,
                                pv_CodProveedor IN VARCHAR2,
                                pv_NroOrden     IN VARCHAR2,
                                pv_FecOrden     IN VARCHAR2,
                                pv_Estado       IN VARCHAR2,
                                S_ERRO          OUT NUMBER,
                                S_MENS          OUT VARCHAR2);

  PROCEDURE WHPR_VALI_ORDE_RECE(pv_Tipo               IN VARCHAR2,
                                pv_CodProveedor       IN VARCHAR2,
                                pv_CodCliente         IN VARCHAR2,
                                pv_FecOrden           IN VARCHAR2,
                                pv_NroOrden           IN VARCHAR2,
                                ln_ValidacionExistosa OUT NUMBER,
                                S_ERRO                OUT NUMBER,
                                S_MENS                OUT VARCHAR2);

  PROCEDURE WHPR_PROC_CONFI_ORCO(pv_TipMovim      IN VARCHAR2,
                                 pv_NumOrden      IN VARCHAR2,
                                 pv_FecOrden      IN VARCHAR2,
                                 pv_CodProveedor  IN VARCHAR2,
                                 pv_IdTransaccion IN VARCHAR2,
                                 S_ERRO           OUT NUMBER,
                                 S_MENS           OUT VARCHAR2);

  PROCEDURE WHPR_PROC_CONFI_DEVO(pv_TipMovim      IN VARCHAR2,
                                 pv_NumOrden      IN VARCHAR2,
                                 pv_FecOrden      IN VARCHAR2,
                                 pv_CodCliente    IN VARCHAR2,
                                 pv_IdTransaccion IN VARCHAR2,
                                 S_ERRO           OUT NUMBER,
                                 S_MENS           OUT VARCHAR2);

  PROCEDURE WHPR_VALI_CONFI_ORCO(pv_CiaVenta      IN VARCHAR2,
                                 pv_TipMovim      IN VARCHAR2,
                                 pv_NumOrden      IN VARCHAR2,
                                 pv_FecOrden      IN VARCHAR2,
                                 pv_CodProveedor  IN VARCHAR2,
                                 pv_IdTransaccion IN VARCHAR2,
                                 S_ERRO           OUT NUMBER,
                                 S_MENS           OUT VARCHAR2);

  FUNCTION WHFU_OBTI_CORR_MOVI(pv_cod_cia varchar2, pv_tip_movi varchar2)
    RETURN NUMBER;

  PROCEDURE WHPR_OBTI_LOTE_INTE(pv_cod_alma      IN varchar2,
                                pv_cod_item      IN varchar2,
                                pv_cod_esta      IN varchar2,
                                pv_cod_lote_prov IN varchar2,
                                pd_fec_vcto      IN date,
                                pn_cod_lote      OUT number,
                                S_ERRO           OUT NUMBER,
                                S_MENS           OUT VARCHAR2);

  PROCEDURE WHPR_ACTU_ORDE_COMP(pv_cod_prov      varchar2,
                                pv_nro_orde      varchar2,
                                pv_cod_item      varchar2,
                                pn_sec_orde_deta orden_compra_det.c_sec_det_oc%TYPE,
                                pv_cod_umed_orde varchar2,
                                pn_can_reci_stoc number,
                                S_ERRO           OUT NUMBER,
                                S_MENS           OUT VARCHAR2);
  FUNCTION WHFU_VALI_PERI_VALO(pv_cod_cia varchar2, pd_fec_mov date)
    RETURN NUMBER;
  
  PROCEDURE WHPR_PROC_CONFI_INGR_PTER(pn_NroOP         IN NUMBER,
                                      pv_IdTransaccion IN VARCHAR2,
                                      S_ERRO           OUT NUMBER,
                                      S_MENS           OUT VARCHAR2);  
                                      
  PROCEDURE WHPR_PROC_CONFI_CONS_INSU(pn_NroOP         IN NUMBER,
                                      pv_IdTransaccion IN VARCHAR2,
                                      S_ERRO           OUT NUMBER,
                                      S_MENS           OUT VARCHAR2);                                      
  
  PROCEDURE WHPR_VALI_CONFI_DESP(pv_cod_cia  IN VARCHAR2,
                                 pv_cod_alma IN VARCHAR2,
                                 pv_nro_orde IN VARCHAR2,
                                 S_ERRO      OUT NUMBER,
                                 S_MENS      OUT VARCHAR2);
  
  FUNCTION WHFU_VALI_PEDI(pv_nro_docu varchar2) RETURN NUMBER;   
  
  FUNCTION WHFU_VALI_ALMA_INAC(pv_cod_cia varchar2, pv_cod_alm varchar2) RETURN NUMBEr;
                                   
end WHPG_INTE_CUBI;
/
create or replace package body WHPG_INTE_CUBI is
  /***************************************************************************
     Creado  : Raul Caballero - 05-07-21
     Descrip.: Consulta un documento electronico emitido por la empresa
  ****************************************************************************/
  PROCEDURE WHPR_CONS_DOCU_ELEC(pv_Empresa         IN VARCHAR2,
                                pv_TipoDocumento   IN VARCHAR2,
                                pv_SerieDocumento  IN VARCHAR2,
                                pv_NumeroDocumento IN VARCHAR2,
                                pc_refc            OUT SYS_REFCURSOR,
                                S_ERRO             OUT NUMBER,
                                S_MENS             OUT VARCHAR2) AS
    ERRO_NOT_FOUND EXCEPTION;
    lv_TipDocuCxC         documento_cxc.cdcc_tipodoc%type;
    ln_Count              number;
    ln_TipDocuSerieValido number;
  BEGIN
    S_ERRO := 0;
    -- Consulta por empresa
    IF pv_Empresa = '01' THEN
      -- Valido si existe equivalencia para el tipo de documento (SUNAT)
      select count(1)
        into ln_Count
        from tablas t
       where t.categoria = '958'
         and t.desc1 = pv_TipoDocumento;
    
      IF ln_Count > 0 THEN
        begin
          -- Consulto Documento_CxC por serie y numero de documento
          select d.cdcc_tipodoc
            into lv_TipDocuCxC
            from documento_cxc d
           where d.ndcc_serie = pv_SerieDocumento
             and d.ndcc_preimpreso = pv_NumeroDocumento
             and d.sdcc_status != 'AN';
        exception
          when no_data_found then
            S_MENS := pv_SerieDocumento || '-' || pv_NumeroDocumento ||
                      ': No existe el documento o se encuentra anulado';
            raise ERRO_NOT_FOUND;
        end;
      ELSE
        S_MENS := pv_TipoDocumento || ': Tipo de documento inválido';
        raise ERRO_NOT_FOUND;
      END IF;
    
      -- Tipo de Documento_CxC es válido
      select count(1)
        into ln_TipDocuSerieValido
        from dual
       where lv_TipDocuCxC in
             (select t.llave
                from tablas t
               where t.categoria = '958'
                 and t.desc1 = pv_TipoDocumento);
    
      -- Si el tipo de documento SUNAT es coherente con la serie
      IF ln_TipDocuSerieValido = 1 THEN
        -- 01 TABERNERO
        OPEN pc_refc FOR
          select pv_Empresa empresa,
                 pv_TipoDocumento tipoDocumento, -- tipo de documento_cxc
                 c.ndcc_serie serieDocumento,
                 c.ndcc_preimpreso numeroDocumento,
                 TO_CHAR(c.fdcc_emision, 'YYYY-mm-dd') fechaDocumento,
                 c.cdcc_cliente codCliente,
                 c.xdcc_cliente desCliente,
                 CASE
                   WHEN REGEXP_LIKE(c.ndcc_rucliente, '^[[:digit:]]+$') THEN
                    CASE
                   WHEN length(c.ndcc_rucliente) = 8 THEN
                    '01' -- DNI
                   WHEN length(c.ndcc_rucliente) = 11 THEN
                    '06' -- RUC
                   ELSE
                    '00' -- OTROS
                 END ELSE '00' END tipDocCliente,
                 c.ndcc_rucliente numDocCliente,
                 d.cdemo_secuencia nroItem,
                 d.cdemo_itemart codArticulo,
                 a.desc_item desArticulo,
                 d.cdemo_unidadart umArticulo,
                 d.qdemo_cantidadart cantidad
            from tabernero.documento_cxc c
           inner join tabernero.doccxc_motivo d on c.cdcc_compania =
                                                   d.cdemo_companiadcc
                                               and c.cdcc_tipodoc =
                                                   d.cdemo_tipodocdcc
                                               and c.cdcc_secuencia =
                                                   d.cdemo_secuenciadcc
           inner join tabernero.articulos a on a.cod_cia = '00'
                                           and a.cod_item = d.cdemo_itemart
           where c.cdcc_compania = '01'
             and c.cdcc_tipodoc = lv_TipDocuCxC
             and c.ndcc_serie = pv_SerieDocumento
             and c.ndcc_preimpreso = pv_NumeroDocumento
             and c.sdcc_status != 'AN';
      
      ELSE
        S_MENS := pv_TipoDocumento ||
                  ': Tipo de documento no es valido para la serie ' ||
                  pv_SerieDocumento;
        raise ERRO_NOT_FOUND;
      END IF;
    ELSE
      S_MENS := 'La consulta para la empresa ' || pv_Empresa ||
                ' no esta habilitada.';
      raise ERRO_NOT_FOUND;
    END IF;
    S_MENS := 'Consulta satisfactoria';
  EXCEPTION
    WHEN ERRO_NOT_FOUND THEN
      S_ERRO := 1;
      S_MENS := S_MENS;
    WHEN OTHERS THEN
      S_ERRO := 1;
      S_MENS := 'Error al consultar documento electronico: ' || 'Empresa: ' ||
                pv_Empresa || ', Tipo Doc.:' || pv_TipoDocumento || ' | ' ||
                pv_SerieDocumento || '-' || pv_NumeroDocumento;
    
  END WHPR_CONS_DOCU_ELEC;

  /***************************************************************************
     Creado  : Raul Caballero - 12-08-21
     Descrip.: Genera la lista de Ing. x Producción (IU) a enviar
  ****************************************************************************/
  PROCEDURE WHPR_INTE_ENVI_ACON_PTER(pc_refc OUT SYS_REFCURSOR) AS
  BEGIN
    OPEN pc_refc FOR
      SELECT TO_CHAR(o.n_secuencia) numorden,
             TO_CHAR(o.f_emision, 'YYYY-MM-DD') fechaOrden,
             1 idestado,
             (SELECT TO_CHAR(NVL(TO_NUMBER(T1.NUM3), 99)) COD_ALMA
                FROM TABERNERO.TABLAS T1
               WHERE T1.CATEGORIA = '038'
                 AND SUBSTR(T1.LLAVE, 3, 2) = o.c_almacen) codigoAlmacen,
             o.c_almacen codigoSubalmacen,
             d.c_producto codigoArticulo,
             TO_CHAR(o.n_secuencia) || TRIM(o.cod_maqu) loteProducto,
             d.c_umbase codigoUM,
             d.n_aproducirumbase cantidadPedida,
             CAST(NVL(a.qart_x_pallet, 0) AS NUMBER(13, 0)) cantidadXPallet,
             o.x_glosa observacion,
             1 idcuenta,
             (SELECT TO_CHAR(NVL(TO_NUMBER(T2.NUM3), 99)) COD_ALMA
                FROM TABERNERO.TABLAS T2
               WHERE T2.CATEGORIA = '038'
                 AND SUBSTR(T2.LLAVE, 3, 2) = d.c_almacen) codigoAlmacenDestino,
             d.c_almacen codigoSubAlmacenDestino,
             o.cod_maqu codigoLinea,
             1 accion,
             -- Detalle receta
             re.c_articulo insumos_codigoarticulo,
             '' insumos_lotearticulo,
             re.c_umbase insumos_codigoUM,
             re.n_necesitadaumbase insumos_cantidadpedida,
             (SELECT AC.QPTC_CANTIDAD
                FROM ARTPROD_COMPONENTE AC
               WHERE AC.CPTC_ARTICULO = d.c_producto
                 AND AC.CPTC_COMPONENTE = re.c_articulo) insumos_cantidadXPT,
             (SELECT TO_CHAR(NVL(TO_NUMBER(T3.NUM3), 99)) COD_ALMA
                FROM TABERNERO.TABLAS T3
               WHERE T3.CATEGORIA = '038'
                 AND SUBSTR(T3.LLAVE, 3, 2) = re.c_almacen) insumos_codigoAlmacenInsumo,
             re.c_almacen insumos_codigoSubAlmacenInsumo
        FROM prd_ordprod o
       inner join prd_ordprod_prod d on o.c_compania = d.c_compania
                                    and o.n_secuencia = d.n_secuencia
       inner join articulos a on d.c_producto = a.cod_item
       inner join prd_ordprod_det re on o.c_compania = re.c_compania
                                    and o.n_secuencia = re.n_secuencia
       WHERE o.c_compania = '01'
         and o.f_emision >= '05-jul-2021'
         and o.ind_envi_pter_dine = 'PE'
         and o.c_almacen in ('77')
         and o.c_status IN ('AP');
  
  END WHPR_INTE_ENVI_ACON_PTER;

  /***************************************************************************
     Creado  : Raul Caballero - 06-08-2021
     Descrip.: Actualiza el estado de una orden de recepcion enviada a CUBICO
     Modif.  : 
     Creac.  : 
  ****************************************************************************/
  PROCEDURE WHPR_UPDT_ORDE_PROD(pv_NroSecuencia IN VARCHAR2,
                                pv_Estado       IN VARCHAR2,
                                S_ERRO          OUT NUMBER,
                                S_MENS          OUT VARCHAR2) IS
  BEGIN
    S_ERRO := 0;
    S_MENS := 'Registro actualizado correctamente: ' || pv_NroSecuencia;
  
    UPDATE prd_ordprod o
       SET o.ind_envi_pter_dine = pv_Estado, o.fec_envi_pter_dine = sysdate
     WHERE o.c_compania = '01'
       and o.n_secuencia = pv_NroSecuencia;
  
    commit;
  EXCEPTION
    WHEN OTHERS THEN
      S_ERRO := 1;
      S_MENS := 'Error al actualizar registro de orden de produccion: ' ||
                pv_NroSecuencia;
  END WHPR_UPDT_ORDE_PROD;

  /***************************************************************************
     Creado  : Raul Caballero - 15-07-21
     Descrip.: Genera la lista de Recepcion de OC. Las devoluciones se generaran desde Cubico
  ****************************************************************************/
  PROCEDURE WHPR_INTE_ENVI_RECE(pc_refc OUT SYS_REFCURSOR) AS
  BEGIN
    OPEN pc_refc FOR
      SELECT TO_CHAR(c.C_SEC_OC) NumOrden, --
             TO_CHAR(c.f_ordencompra, 'YYYY-MM-DD') FechaOrden,
             (SELECT TO_CHAR(NVL(TO_NUMBER(T2.NUM3), 99)) COD_ALMA
                FROM TABERNERO.TABLAS T2
               WHERE T2.CATEGORIA = '038'
                 AND SUBSTR(T2.LLAVE, 3, 2) = cd.c_almacen) CodigoAlmacen,
             TO_CHAR(c.f_entrega, 'YYYY-MM-DD') FechaLlegada,
             c.C_PROVEEDOR CodigoERPProveedor,
             CASE
               WHEN c.c_tipo_oc IN ('01', '03') THEN -- NACIONAL
                2 -- Ingreso compras nacionales
               WHEN c.c_tipo_oc = '02' THEN -- IMPORTADO
                1 -- Ingreso por importaciones
               ELSE
                0 -- Error
             END IdTipo,
             CASE
               WHEN c.c_tipo_oc IN ('01', '03') THEN -- NACIONAL
                2 -- Orden de compra
               WHEN c.c_tipo_oc = '02' THEN -- IMPORTADO
                1 -- Orden de importacion
               ELSE
                0 -- Error
             END IdTipoDoc,
             c.X_GLOSA observacion,
             1 IdCuenta,
             1 Accion,
             CAST(cd.c_sec_det_oc AS NUMBER(15, 0)) Item,
             cd.cod_item CodigoArticulo,
             cd.c_um_item_oc CodigoUM,
             --cd.Q_UM_CTRL_STOCK,
             '' LoteProducto,
             cd.Q_PEDIDA_OC CantidadPedida,
             0 CantidadBulto,
             0 CantidadXBulto,
             cd.c_almacen CodigoSubAlmacen,
             1 AccionLinea,
             'admin' usuario,
             'OC' tipo
        FROM ORDEN_COMPRA_CAB c
       INNER JOIN ORDEN_COMPRA_DET cd ON c.cod_cia = cd.cod_cia
                                     AND c.cod_cia_vta = cd.cod_cia_vta
                                     and c.c_proveedor = cd.c_proveedor
                                     and c.f_ordencompra = cd.f_ordencompra
                                     and c.c_sec_oc = cd.c_sec_oc
       WHERE c.cod_cia = '00'
         and c.cod_cia_vta = '01'
         --and nvl(c.b_servicio, '0') = '0'
         AND nvl(cd.cod_item,'0')!='0'
         AND nvl(c.s_oc, '0') = 'AP'
         AND C.F_ORDENCOMPRA >= '01-jul-2021'
         and c.ind_envi_dine = 'PE' -- PENDIENTE DE ENVIAR
         AND cd.c_almacen IN
             ('67', '69', '55', '75', '71', '70', '68', '76', '77')
       order by 1, 2, 5;
    /*UNION ALL
    select TO_CHAR(li.nro_autorizacion) NumOrden,
           TO_CHAR(li.fec_aprobacion, 'YYYY-MM-DD') FechaOrden,
           CASE
             WHEN li.cod_almacen IN ('67', '69', '55', '75', '71', '70') THEN
              '01' -- SDA
             WHEN li.cod_almacen IN ('68', '76') THEN
              '02' -- ALDEA
             WHEN li.cod_almacen IN ('72') THEN
              '03' -- CORPAC
             ELSE
              '99' -- SUBALMACEN NO EXISTE
           END CodigoAlmacen,
           TO_CHAR(li.fec_ingreso_esperado, 'YYYY-MM-DD') FechaLlegada,
           li.cod_cliente_origen CodigoERPProveedor,
           3 IdTipo, -- 3 Devoluciones
           5 IdTipoDoc, -- 5 Orden por devolucion de venta
           li.observaciones Observacion,
           1 Idcuenta,
           1 Accion,
           ld.nro_secuencia Item,
           ld.cod_item CodigoArticulo,
           ld.um_item CodigoUM,
           '' LoteProducto,
           ld.cantidad_a_ingresar CantidadPedida,
           0 CantidadBulto,
           0 CantidadXBulto,
           li.cod_almacen CodigoSubAlmacen,
           1 AccionLinea,
           'admin' usuario,
           'LI' Tipo
      from litc_auto_ingr li
     inner join litd_auto_ingr ld on li.cod_compania = ld.cod_compania
                                 and li.nro_autorizacion =
                                     ld.nro_autorizacion
     where li.cod_compania = '01'
       and li.cod_tipo_logi_inve in ('DR', 'RE')
       and li.estado_auto_ingr = 'AP'
       and li.ind_envi_dine = '0'
     order by NumOrden, Item;*/
  
  END WHPR_INTE_ENVI_RECE;

  /***************************************************************************
     Creado  : Raul Caballero - 19-07-21
     Descrip.: Actualiza los registros de recepcion enviados (solo OC)
  ****************************************************************************/
  PROCEDURE WHPR_UPDT_ORDE_RECE(pv_TipoMov      IN VARCHAR2,
                                pv_CodProveedor IN VARCHAR2,
                                pv_NroOrden     IN VARCHAR2,
                                pv_FecOrden     IN VARCHAR2,
                                pv_Estado       IN VARCHAR2,
                                S_ERRO          OUT NUMBER,
                                S_MENS          OUT VARCHAR2) AS
  BEGIN
    S_ERRO := 0;
    S_MENS := 'Registro actualizado correctamente: ' || pv_TipoMov || ' ' ||
              pv_NroOrden;
  
    IF pv_TipoMov = 'OC' THEN
      UPDATE orden_compra_cab oc
         SET oc.ind_envi_dine = pv_Estado, oc.fec_envi_dine = sysdate
       WHERE oc.cod_cia = '00'
         and oc.cod_cia_vta = '01'
         and oc.c_proveedor = pv_CodProveedor
         and oc.c_sec_oc = TO_NUMBER(pv_NroOrden)
         and oc.f_ordencompra = TO_DATE(pv_FecOrden, 'YYYY-mm-dd');
      /*ELSIF pv_Tipo = 'LI' THEN
      UPDATE litc_auto_ingr li
         SET li.ind_envi_dine = pv_Estado, li.fec_envi_dine = sysdate
       WHERE li.cod_compania = '01'
         and li.nro_autorizacion = pn_NroOrden;*/
    ELSE
      S_ERRO := 1;
      S_MENS := 'Error al actualizar registro de recepcion: ' || pv_TipoMov || ' ' ||
                pv_NroOrden;
    END IF;
  
    commit;
  EXCEPTION
    WHEN OTHERS THEN
      S_ERRO := 1;
      S_MENS := 'Error al actualizar registro de recepcion: ' || pv_TipoMov || ' ' ||
                pv_NroOrden;
  END WHPR_UPDT_ORDE_RECE;

  /***************************************************************************
   Creado  : Raul Caballero - 05-08-2021
   Descrip.: Valida la cabecera de una Orden de Repecion (OC, Devolucion)
   Modif.  :
   Descrip.:
  ****************************************************************************/
  PROCEDURE WHPR_VALI_ORDE_RECE(pv_Tipo               IN VARCHAR2,
                                pv_CodProveedor       IN VARCHAR2,
                                pv_CodCliente         IN VARCHAR2,
                                pv_FecOrden           IN VARCHAR2,
                                pv_NroOrden           IN VARCHAR2,
                                ln_ValidacionExistosa OUT NUMBER,
                                S_ERRO                OUT NUMBER,
                                S_MENS                OUT VARCHAR2) AS
    ERRO_VALI_DOCU EXCEPTION;
    ln_ItemsPendientes    NUMBER;
    ln_PorcToleranciaProv NUMBER;
    lv_UMStock            articulos.um_control_stock%TYPE;
    lv_UMVenta            articulos.um_venta%TYPE;
    lv_UMOrig             orden_compra_det.c_um_item_oc%TYPE;
    ln_CantUM             orden_compra_det.q_pedida_oc%TYPE;
    ln_CantPedidadUMStock orden_compra_det.q_pedida_oc%TYPE;
  
    CURSOR c1_oc IS
      SELECT cd.f_ordencompra,
             cd.c_proveedor,
             cd.c_sec_oc,
             cd.c_sec_det_oc,
             cd.cod_item,
             cd.c_um_item_oc,
             cd.q_recibida_oc,
             cd.q_pedida_oc
        FROM ORDEN_COMPRA_CAB c
       INNER JOIN ORDEN_COMPRA_DET cd ON c.cod_cia = cd.cod_cia
                                     AND c.cod_cia_vta = cd.cod_cia_vta
                                     and c.c_proveedor = cd.c_proveedor
                                     and c.f_ordencompra = cd.f_ordencompra
                                     and c.c_sec_oc = cd.c_sec_oc
       WHERE c.cod_cia = '00'
         AND c.cod_cia_vta = '01'
         --AND nvl(c.b_servicio, '0') = '0'
         AND nvl(cd.cod_item,'0')!='0'
         AND nvl(c.s_oc, '0') = 'AP'
         AND C.C_PROVEEDOR = pv_CodProveedor
         AND c.f_ordencompra = TO_DATE(pv_FecOrden, 'YYYY-mm-dd')
         AND c.c_sec_oc = TO_NUMBER(pv_NroOrden);
    det_oc c1_oc%ROWTYPE;
  
  BEGIN
    -- ln_ValidacionExistosa: Retorna 1 si validacion OK, 0 en otro caso
    S_ERRO                := 0;
    S_MENS                := '';
    ln_ValidacionExistosa := 0;
    -- Se procede a validar segun tipo de movimiento
    IF pv_Tipo IN ('01', '02') THEN
      -- OC Importada o Nacional
      SELECT COUNT(1)
        INTO ln_ValidacionExistosa
        FROM PROVEEDOR P
       WHERE P.COD_CIA = '00'
         AND P.C_PROVEEDOR = pv_CodProveedor;
    
      IF ln_ValidacionExistosa = 0 THEN
        S_MENS := S_MENS || 'El codigo de proveedor no existe en ERP: ' ||
                  pv_CodProveedor;
        RAISE ERRO_VALI_DOCU;
      END IF;
    
      SELECT COUNT(1)
        INTO ln_ValidacionExistosa
        FROM ORDEN_COMPRA_CAB OC
       WHERE OC.COD_CIA = '00'
         AND OC.COD_CIA_VTA = '01'
         AND OC.C_PROVEEDOR = pv_CodProveedor
         AND OC.F_ORDENCOMPRA = TO_DATE(pv_FecOrden, 'YYYY-mm-dd')
         AND OC.C_SEC_OC = pv_NroOrden;
    
      IF ln_ValidacionExistosa = 0 THEN
        S_MENS := S_MENS || 'La Orden de compra no existe en ERP: ' ||
                  pv_Tipo || ' ' || pv_FecOrden || ' ' || pv_NroOrden || ' ' ||
                  pv_CodProveedor;
        RAISE ERRO_VALI_DOCU;
      END IF;
    
      -- Validando que hayan cantidades pendientes de confirmar y que esten dentro de la tolerancia por proveedor
      SELECT p1.por_tole_ingr
        INTO ln_PorcToleranciaProv
        FROM PROVEEDOR p1
       WHERE p1.COD_CIA = '00'
         AND p1.c_proveedor = pv_CodProveedor;
    
      ln_ItemsPendientes := 0;
      FOR det_oc IN c1_oc LOOP
        -- Obtengo la UM Stock y Venta del articulo
        select a.um_control_stock, a.um_venta
          into lv_UMStock, lv_UMVenta
          from articulos a
         where a.cod_item = det_oc.cod_item;
      
        -- Conversión de Q a UM Stock
        lv_UMOrig := det_oc.c_um_item_oc;
        ln_CantUM := det_oc.q_pedida_oc;
      
        P_CONVIERTE_UM(lv_UMOrig, lv_UMStock, ln_CantUM);
        ln_CantPedidadUMStock := ln_CantUM;
      
        IF det_oc.q_Recibida_Oc <
           (ln_CantPedidadUMStock +
           ln_PorcToleranciaProv * ln_CantPedidadUMStock) THEN
          ln_ItemsPendientes := ln_ItemsPendientes + 1;
        END IF;
      END LOOP;
    
      IF ln_ItemsPendientes = 0 THEN
        S_MENS := S_MENS || 'La OC ya fue recibida en su totalidad: ' ||
                  pv_Tipo || ' ' || pv_FecOrden || ' ' || pv_NroOrden || ' ' ||
                  pv_CodProveedor;
        RAISE ERRO_VALI_DOCU;
      END IF;
      --
      ln_ValidacionExistosa := 1;
      --
    ELSIF pv_Tipo IN ('03') THEN
      -- pv_Tipo 03 Devolucion
      ln_ValidacionExistosa := 1;
      --
    ELSE
      S_MENS := 'Tipo de movimiento invalido: ' || pv_Tipo;
      RAISE ERRO_VALI_DOCU;
    END IF;
  
  EXCEPTION
    WHEN ERRO_VALI_DOCU THEN
      S_MENS := S_MENS;
      S_ERRO := WHPG_CTE_DINE.ERRO;
    WHEN OTHERS THEN
      S_ERRO := WHPG_CTE_DINE.ERRO;
      S_MENS := S_MENS ||
                'PCK: WHPG_INTE_CUBI - PRC: WHPR_VALI_ORDE_RECE || ERROR NUMERO: ' ||
                SQLCODE || ' - DESCRIPCION: ' || SQLERRM ||
                ' ERROR_BACKTRACE: ' || DBMS_UTILITY.format_error_backtrace;
  END WHPR_VALI_ORDE_RECE;

  /***************************************************************************
     Creado  : Raul Caballero - 26-07-21
     Descrip.: Procesar recepcion de Orden de Compra (importado y nacional)
  ****************************************************************************/
  PROCEDURE WHPR_PROC_CONFI_ORCO(pv_TipMovim      IN VARCHAR2,
                                 pv_NumOrden      IN VARCHAR2,
                                 pv_FecOrden      IN VARCHAR2,
                                 pv_CodProveedor  IN VARCHAR2,
                                 pv_IdTransaccion IN VARCHAR2,
                                 S_ERRO           OUT NUMBER,
                                 S_MENS           OUT VARCHAR2) IS
    ERRO_GENE_DOCU EXCEPTION;
    ERRO_NOT_FOUND EXCEPTION;
    S_ERRO_AUX  number;
    S_MENS_AUX  varchar2(4000);
    lv_Step     varchar2(3);
    ln_Corre    number;
    lv_CiaVenta varchar2(2);
    --
    ln_LongMovAlm    tablas.num1%TYPE;
    lv_UMStock       articulos.um_control_stock%TYPE;
    lv_UMVenta       articulos.um_venta%TYPE;
    lv_NroDocu       mov_inventarios.nro_documento%TYPE;
    ld_FecTran       mov_inventarios.fecha_transaccion%TYPE;
    ld_HorTran       mov_inventarios.hora_transaccion%TYPE;
    lv_CodAlma       mov_inventarios.almacen_venta%TYPE;
    lv_GlosMovi      mov_inventarios.observacion%TYPE;
    ln_CantRecibida  mov_inventarios.cantidad%TYPE;
    ln_CantUMStock   mov_inventarios.cantidad%TYPE;
    lv_NroGuiaTransp mov_inventarios.c_guia_transp%TYPE;
    ln_SecOC         mov_inventarios.c_Sec_Oc%TYPE;
    lv_DocRef1       mov_inventarios.doc_ref_1%TYPE;
    lv_TipDocRef     mov_inventarios.tipo_doc_ref%TYPE;
    lv_CodItem       mov_inventarios.cod_item_2%TYPE;
    lv_TipMovi       mov_inventarios.tipo_movimiento%TYPE;
    lv_TipDocu       mov_inventarios.tipo_documento%TYPE;
    lv_Motivo        mov_inventarios.motivo%TYPE;
    ln_SecDetOC      mov_inventarios.c_sec_det_oc%TYPE;
    --
    ln_CodLote det_mov_ubicacion.cod_lote%TYPE;
    ln_CantMov det_mov_ubicacion.cantidad%TYPE;
    lv_CodEsta det_mov_ubicacion.cod_estado%TYPE;
    lv_CodZona det_mov_ubicacion.zona%TYPE;
    lv_CodRack det_mov_ubicacion.rack%TYPE;
    lv_CodNive det_mov_ubicacion.nivel%TYPE;
    lv_CodCasi det_mov_ubicacion.casillero%TYPE;
    lv_CodPale det_mov_ubicacion.pallet%TYPE;
    --
    lv_CodLoteProv saldo_lote.cod_lote_proveedor%TYPE;
    ld_FecVcto     saldo_lote.fecha_vencimiento%TYPE;
    ld_FecOrde     orden_compra_cab.f_ordencompra%TYPE;
    lv_Moneda      orden_compra_cab.c_moneda%TYPE;
    lv_UMOrde      Orden_Compra_Det.c_Um_Item_Oc%TYPE;
    lv_UMOrig      mov_inventarios.um_item_3%TYPE;
    lv_CodProvXray proveedor.c_proveedor%TYPE;
  
    -- 1. Variables para la cabecera del documento (recepciones)
    lv_TmpNumOrden   CUWD_CONF_ORDE_RECE_API.numorden%type;
    lv_TmpFechaOrden CUWD_CONF_ORDE_RECE_API.fechaorden%type;
    --lv_TmpCodigoAlmacen                CUWD_CONF_ORDE_RECE_API.codigoalmacen%type;
    --lv_TmpFechahoraIngresoOrden        CUWD_CONF_ORDE_RECE_API.fechahoraingresoorden%type;
    lv_TmpFechahoraTransaccion CUWD_CONF_ORDE_RECE_API.fechahoratransaccionorden%type;
    lv_TmpCodigoProveedor      CUWD_CONF_ORDE_RECE_API.codigoproveedor%type;
    lv_TmpTipoMovimiento       CUWD_CONF_ORDE_RECE_API.tipomovimiento%type;
    --lv_TipoDocumento                CUWD_CONF_ORDE_RECE_API.tipodocumento%type;
    --lv_NumDocumento                 CUWD_CONF_ORDE_RECE_API.numdocumento%type;
    lv_TmpUsuario CUWD_CONF_ORDE_RECE_API.usuario%type;
  
    -- 2. Detalle del documento             
    CURSOR c2 IS
      select tm.numorden,
             tm.fechaorden,
             --tm.codigoalmacen,
             tm.fechahoraingresoorden,
             tm.fechahoratransaccionorden,
             tm.codigoproveedor,
             --tm.codigocliente,
             --tm.tipomovimiento,
             tm.tipodocumento,
             tm.numdocumento,
             --tm.motivodevolucion,
             --tm.responsabledevolucion,
             --tm.tipoincidenciadevolucion,
             tm.idcuenta,
             tm.usuario,
             tm.item,
             tm.codigoarticulo,
             tm.codigoum,
             tm.loteproveedor,
             tm.fechavencimiento,
             tm.anyocosecha,
             tm.cantidadpedida,
             tm.cantidadrecibida,
             tm.codigosubalmacen
      --tm.tipoincidenciaitem
        from CUWD_CONF_ORDE_RECE_API tm
       where tm.tipomovimiento = pv_TipMovim
         and tm.numorden = pv_NumOrden
         and tm.fechaorden = pv_FecOrden
         and tm.codigoproveedor = pv_CodProveedor
         and tm.idtransaccion = pv_IdTransaccion
         and tm.est_regi = 'IN'; -- IN: Ingresado y pendiente de procesar
    c2_rec c2%ROWTYPE;
  
  BEGIN
    -- *********************************************************************
    -- ********************* CABECERA DE INGRESO *************************
    -- *********************************************************************
    lv_Step     := '1';
    lv_CiaVenta := '01';
    S_ERRO      := WHPG_CTE_DINE.OK;
    S_MENS      := '';
  
    select distinct tm.numorden,
                    tm.fechaorden,
                    tm.fechahoratransaccionorden,
                    tm.codigoproveedor,
                    tm.tipomovimiento,
                    tm.usuario
      into lv_TmpNumOrden,
           lv_TmpFechaOrden,
           lv_TmpFechahoraTransaccion,
           lv_TmpCodigoProveedor,
           lv_TmpTipoMovimiento,
           lv_TmpUsuario
      from CUWD_CONF_ORDE_RECE_API tm
     where tm.tipomovimiento = pv_TipMovim
       and tm.numorden = pv_NumOrden
       and tm.fechaorden = pv_FecOrden
       and tm.codigoproveedor = pv_CodProveedor
       and tm.idtransaccion = pv_IdTransaccion
       and tm.est_regi = 'IN';
  
    -- Validación detalle de OC
    WHPR_VALI_CONFI_ORCO(lv_CiaVenta,
                         lv_TmpTipoMovimiento,
                         lv_TmpNumOrden,
                         lv_TmpFechaOrden,
                         lv_TmpCodigoProveedor,
                         pv_IdTransaccion,
                         S_ERRO,
                         S_MENS);
  
    -- Si pasa las validaciones de registro OC
    IF S_ERRO = WHPG_CTE_DINE.OK THEN
      lv_Step := '2';
      -- ************************** INGRESO POR COMPRA *******************************
      lv_TipMovi     := WHPG_CTE_DINE.MOVI_IC;
      lv_TipDocu     := WHPG_CTE_DINE.DOCU_PI;
      lv_Motivo      := '01'; -- INGRESO POR COMPRA
      ln_SecOC       := TO_NUMBER(lv_TmpNumOrden);
      lv_CodProvXray := lv_TmpCodigoProveedor;
      lv_TipDocRef   := WHPG_CTE_DINE.DOC_REF_OC;
      lv_DocRef1     := TRIM(lv_TmpNumOrden);
    
      lv_GlosMovi := 'Ingreso por Compra ' || TO_CHAR(ln_SecOC) ||
                     ' - WMS CUBICO - ' ||
                     to_char(sysdate, 'dd/mm/yy HH:Mi AM');
    
      ld_FecOrde := TO_DATE(lv_TmpFechaOrden, 'YYYY-mm-dd');
      ld_FecTran := TO_DATE(SUBSTR(lv_TmpFechahoraTransaccion, 0, 10),
                            'YYYY-mm-dd');
      ld_HorTran := TO_DATE(lv_TmpFechahoraTransaccion,
                            'YYYY-mm-dd"T"HH24:MI:SS');
    
      S_MENS := 'OC: ' || to_char(ln_SecOC) || '|' || lv_TmpFechaOrden || '|' ||
                lv_CodProvXray || '|| ';
    
      -- Obteniendo la moneda de la OC
      Select oc.c_moneda
        into lv_Moneda
        from orden_compra_cab oc
       where oc.cod_cia = '00'
         and oc.cod_cia_vta = lv_CiaVenta
         and oc.c_proveedor = lv_CodProvXray
         and oc.c_sec_oc = ln_SecOC
         and oc.f_ordencompra = ld_FecOrde;
    
      -- Obtiene longitud de número de documento
      select nvl(num1, 6)
        into ln_LongMovAlm
        from tablas
       where categoria = '051'
         and llave = '92';
    
      if ln_LongMovAlm > 8 then
        S_MENS := S_MENS ||
                  '=> Error de definicion XRAY: La longitud de documento definida en la categoria 051, llave 92 no puede ser mayor a 8';
        raise ERRO_GENE_DOCU;
      end if;
    
      -- *********************************************************************
      -- ********************** DETALLE DE ORDEN COMPRA **********************
      -- *********************************************************************
      lv_Step := '3';
      FOR c2_rec IN c2 LOOP
        -- Datos
        ln_SecDetOC      := c2_rec.item;
        lv_CodItem       := c2_rec.codigoarticulo;
        lv_NroGuiaTransp := c2_rec.numdocumento;
        lv_UMOrde        := c2_rec.codigoum;
        lv_CodAlma       := c2_rec.codigosubalmacen;
      
        S_MENS := S_MENS || 'Detalle: ' || to_char(ln_SecDetOC) || '|' ||
                  lv_CodItem || '|';
      
        -- Obtengo la UM Stock y Venta del articulo
        select a.um_control_stock, a.um_venta
          into lv_UMStock, lv_UMVenta
          from articulos a
         where a.cod_item = lv_CodItem;
      
        -- Conversión de la cantidad recibida (que esta en UMOrig) a UM Stock
        lv_UMOrig       := c2_rec.codigoum;
        ln_CantRecibida := c2_rec.cantidadrecibida;
      
        P_CONVIERTE_UM(lv_UMOrig, lv_UMStock, ln_CantRecibida);
        ln_CantUMStock := ln_CantRecibida;
      
        -- Obtiene Correlativo de Ingreso 
        ln_Corre := WHFU_OBTI_CORR_MOVI(lv_CiaVenta, 'IM');
        IF ln_Corre = 0 THEN
          S_MENS := S_MENS || '=> Error al generar correlativo de ingreso';
          RAISE ERRO_GENE_DOCU;
        END IF;
      
        -- Se genera el nro. de documento de ingreso
        lv_NroDocu := LPAD(ln_Corre, ln_LongMovAlm, '0');
        -- ********************  A. Inserta en MOV_INVENTARIOS ***********************
        lv_Step := '4';
        insert into Mov_Inventarios
          (cod_cia,
           compania_venta_3,
           almacen_venta,
           tipo_movimiento,
           tipo_documento,
           nro_documento,
           cod_item_2,
           proveedor,
           almacen_destino,
           cantidad,
           costo_unitario,
           doc_ref_1,
           fecha_transaccion,
           motivo,
           precio_unitario,
           tipo_doc_ref,
           um_item_3,
           usuario,
           moneda,
           costo_unitario_me,
           cos_unit_est,
           cos_unit_me_est,
           hora_transaccion,
           f_ordencompra,
           c_sec_oc,
           c_sec_det_oc,
           observacion,
           ingreso_salida,
           fecha_real,
           tiempo_garantia,
           reversado,
           n_guia_prov,
           f_guia_prov,
           c_guia_transp)
        values
          (WHPG_CTE_DINE.COD_CIA,
           lv_CiaVenta,
           lv_CodAlma,
           lv_TipMovi,
           lv_TipDocu,
           lv_NroDocu,
           lv_CodItem,
           lv_CodProvXray,
           '', -- almacen_destino
           ln_CantUMStock,
           0, -- costo_unitario
           lv_DocRef1,
           ld_FecTran,
           lv_Motivo,
           0, -- precio_unitario
           lv_TipDocRef,
           lv_UMStock,
           lv_TmpUsuario,
           lv_Moneda, -- moneda
           0, -- costo_unitario_me
           0, -- cost_unit_est
           0, -- cost_unit_me_est
           ld_HorTran,
           ld_FecOrde,
           ln_SecOC,
           ln_SecDetOC,
           lv_GlosMovi,
           lv_TipMovi,
           sysdate, -- fecha_real
           '0', -- tiempo_garantia
           '0', -- reservado
           lv_NroGuiaTransp, -- n_guia_prov
           null, -- f_guia_prov
           lv_NroGuiaTransp);
        if SQL%NOTFOUND then
          S_MENS := S_MENS ||
                    '=> Ocurrio un error al insertar en Mov_Inventarios';
          raise ERRO_NOT_FOUND;
        end if;
        -- ******************** Fin Inserta en MOV_UBICACION ***********************
      
        -- ******************** B. Inserta en DET_MOV_UBICACION ***********************
        lv_CodLoteProv := c2_rec.loteproveedor;
        lv_CodZona     := WHPG_CTE_CUBI.UBI_RECEP;
        lv_CodRack     := '     ';
        lv_CodNive     := '     ';
        lv_CodCasi     := '     ';
        lv_CodPale     := '     ';
        ln_CantMov     := ln_CantUMStock;
        ld_FecVcto     := TO_DATE(c2_rec.fechavencimiento, 'YYYY-mm-dd');
        lv_CodEsta     := WHPG_CTE_CUBI.EST_BE;
        --
        lv_Step := '5';
        --
        WHPR_OBTI_LOTE_INTE(lv_CodAlma,
                            lv_CodItem,
                            lv_CodEsta,
                            lv_CodLoteProv,
                            ld_FecVcto,
                            ln_CodLote,
                            S_ERRO_AUX,
                            S_MENS_AUX);
      
        IF S_ERRO_AUX != 0 THEN
          S_MENS := S_MENS || '=> El lote interno para el lote proveedor ' ||
                    lv_CodLoteProv || ' no pudo ser generado';
          RAISE ERRO_GENE_DOCU;
        END IF;
        --
        lv_Step := '6';
        --
        insert into Det_Mov_Ubicacion
          (cod_cia,
           compania_venta_3,
           almacen_venta,
           tipo_movimiento,
           tipo_documento,
           nro_documento,
           cod_item_2,
           cod_lote,
           cod_estado,
           zona,
           rack,
           nivel,
           casillero,
           pallet,
           cantidad,
           um_mov,
           c_secdetoc,
           cantidad_um_stock)
        values
          (WHPG_CTE_DINE.COD_CIA,
           lv_CiaVenta,
           lv_CodAlma,
           lv_TipMovi,
           lv_TipDocu,
           lv_NroDocu,
           lv_CodItem,
           ln_CodLote,
           lv_CodEsta,
           lv_CodZona,
           lv_CodRack,
           lv_CodNive,
           lv_CodCasi,
           lv_CodPale,
           ln_CantMov,
           lv_UMStock,
           ln_SecDetOC,
           ln_CantUMStock);
        if SQL%NOTFOUND then
          S_MENS := S_MENS ||
                    '=> Ocurrio un error al insertar Det_Mov_Ubicacion';
          raise ERRO_NOT_FOUND;
        end if;
      
        -- Actualizo el año de cosecha en saldo_lote creado (o existente)
        update saldo_lote sl
           set sl.ano_produc = c2_rec.anyocosecha
         where sl.cod_cia = '00'
           and sl.compania_venta = lv_CiaVenta
           and sl.cod_almacen = lv_CodAlma
           and sl.cod_item = lv_CodItem
           and sl.cod_lote = ln_CodLote
           and sl.cod_estado = lv_CodEsta;
      
        -- ******************** Fin Inserta en DET_MOV_UBICACION ***********************
      
        -- ******************** C. Actualizo la Cant. Recibida de OC ***********************
        lv_Step := '7';
        WHPR_ACTU_ORDE_COMP(lv_CodProvXray,
                            lv_TmpNumOrden,
                            lv_CodItem,
                            ln_SecDetOC,
                            lv_UMOrde,
                            ln_CantUMStock,
                            S_ERRO_AUX,
                            S_MENS_AUX);
        if S_ERRO_AUX != 0 then
          S_MENS := S_MENS || '=> Se produjo un error al actualizar la OC';
          raise ERRO_GENE_DOCU;
        end if;
      
        -- ******************** Actualizo Procesamiento de Interface ***********************
        lv_Step := '8';
        update CUWD_CONF_ORDE_RECE_API tmp
           set tmp.est_regi = 'PR', tmp.fec_crea = sysdate
         where tmp.numorden = lv_TmpNumOrden
           and tmp.fechaorden = lv_TmpFechaOrden
           and tmp.tipomovimiento = lv_TmpTipoMovimiento
           and tmp.codigoproveedor = lv_TmpCodigoProveedor
           and tmp.item = ln_SecDetOC
           and tmp.codigoarticulo = lv_CodItem
           and tmp.idtransaccion = pv_IdTransaccion;
        --
      END LOOP;
      --
    ELSE
      S_MENS := S_MENS;
      RAISE ERRO_GENE_DOCU;
    END IF;
    --
    S_MENS := 'La recepción de OC se realizó de forma satisfactoria.';
    --
    COMMIT;
    --
  EXCEPTION
    WHEN ERRO_NOT_FOUND THEN
      S_MENS := 'Ocurrio un error al insertar registro: ' || S_MENS;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      ROLLBACK;
      update CUWD_CONF_ORDE_RECE_API tmp
         set tmp.est_regi = 'ER', tmp.fec_crea = sysdate
       where tmp.numorden = lv_TmpNumOrden
         and tmp.fechaorden = lv_TmpFechaOrden
         and tmp.tipomovimiento = lv_TmpTipoMovimiento
         and tmp.codigoproveedor = lv_TmpCodigoProveedor
         and tmp.idtransaccion = pv_IdTransaccion;
    WHEN ERRO_GENE_DOCU THEN
      S_MENS := 'Ocurrio un error al confirmar la recepción de OC: ' ||
                S_MENS;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      ROLLBACK;
      update CUWD_CONF_ORDE_RECE_API tmp
         set tmp.est_regi = 'ER', tmp.fec_crea = sysdate
       where tmp.numorden = lv_TmpNumOrden
         and tmp.fechaorden = lv_TmpFechaOrden
         and tmp.tipomovimiento = lv_TmpTipoMovimiento
         and tmp.codigoproveedor = lv_TmpCodigoProveedor
         and tmp.idtransaccion = pv_IdTransaccion;
    WHEN OTHERS THEN
      S_MENS := 'PCK: WHPG_INTE_CUBI - PRC: WHPR_PROC_CONFI_ORCO || ERROR: ' ||
                SQLCODE || ' - DESCRIPCION: ' || SQLERRM || ' - PASO : ' ||
                lv_Step;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      ROLLBACK;
      update CUWD_CONF_ORDE_RECE_API tmp
         set tmp.est_regi = 'ER', tmp.fec_crea = sysdate
       where tmp.numorden = lv_TmpNumOrden
         and tmp.fechaorden = lv_TmpFechaOrden
         and tmp.tipomovimiento = lv_TmpTipoMovimiento
         and tmp.codigoproveedor = lv_TmpCodigoProveedor
         and tmp.idtransaccion = pv_IdTransaccion;
  END WHPR_PROC_CONFI_ORCO;

  /***************************************************************************
     Creado  : Raul Caballero - 02-08-21
     Descrip.: Procesar recepcion de Devolucion
  ****************************************************************************/
  PROCEDURE WHPR_PROC_CONFI_DEVO(pv_TipMovim        IN VARCHAR2,
                                 pv_NumOrden        IN VARCHAR2,
                                 pv_FecOrden        IN VARCHAR2,
                                 pv_CodCliente      IN VARCHAR2,
                                 pv_IdTransaccion   IN VARCHAR2,
                                 S_ERRO             OUT NUMBER,
                                 S_MENS             OUT VARCHAR2) IS
    ERRO_GENE_DOCU EXCEPTION;
    ERRO_NOT_FOUND EXCEPTION;
    S_ERRO_AUX     number;
    S_MENS_AUX     varchar2(4000);
    lv_Step        varchar2(3);
    ln_LongMovAlm  tablas.num1%TYPE;
    ln_Corre       number;
    lv_CiaVenta    varchar2(2);
    lv_Position    number;
    --
    lv_UMStock     articulos.um_control_stock%TYPE;
    lv_UMVenta     articulos.um_venta%TYPE;
    lv_NroDocu     mov_inventarios.nro_documento%TYPE;
    ld_FecTran     mov_inventarios.fecha_transaccion%TYPE;
    ld_HorTran     mov_inventarios.hora_transaccion%TYPE;
    lv_CodAlma     mov_inventarios.almacen_venta%TYPE;
    lv_GlosMovi    mov_inventarios.observacion%TYPE;
    ln_CantRecibida mov_inventarios.cantidad%TYPE;
    ln_CantUMStock   mov_inventarios.cantidad%TYPE;
    ln_SecDevo       mov_inventarios.c_Sec_Oc%TYPE;
    ln_SecDetDevo  mov_inventarios.c_sec_det_oc%TYPE;
    lv_NumDocRef1  mov_inventarios.doc_ref_1%TYPE;
    lv_TipDocRef   mov_inventarios.tipo_doc_ref%TYPE;
    lv_CodItem     mov_inventarios.cod_item_2%TYPE;
    lv_TipMovi     mov_inventarios.tipo_movimiento%TYPE;
    lv_TipDocu     mov_inventarios.tipo_documento%TYPE;
    lv_Motivo      mov_inventarios.motivo%TYPE;
    lv_NroGuiaProv mov_inventarios.c_guia_transp%TYPE;
    lv_NroOrde     mov_inventarios.doc_ref_1%TYPE;
    lv_CodClie     mov_inventarios.proveedor%TYPE;
    lv_CodRespDevo mov_inventarios.cod_orig_inci_devo%TYPE;
    lv_TipInciDevo mov_inventarios.cod_subg_inci%TYPE;
    --
    ln_CodLote     det_mov_ubicacion.cod_lote%TYPE;
    ln_CantMov     det_mov_ubicacion.cantidad%TYPE;
    lv_CodEsta     det_mov_ubicacion.cod_estado%TYPE;
    lv_CodZona     det_mov_ubicacion.zona%TYPE;
    lv_CodRack     det_mov_ubicacion.rack%TYPE;
    lv_CodNive     det_mov_ubicacion.nivel%TYPE;
    lv_CodCasi     det_mov_ubicacion.casillero%TYPE;
    lv_CodPale     det_mov_ubicacion.pallet%TYPE;
    --
    lv_CodLoteProv      saldo_lote.cod_lote_proveedor%TYPE;
    ld_FecVcto          saldo_lote.fecha_vencimiento%TYPE;
    ld_FecOrde          orden_compra_cab.f_ordencompra%TYPE;
    lv_UMOrig           mov_inventarios.um_item_3%TYPE;
    --
    lv_TipDocuCXC       documento_cxc.CDCC_TIPODOC%TYPE;
    lv_NroSecuCXC       documento_cxc.CDCC_SECUENCIA%TYPE;
    lv_CodLoca          documento_cxc.CDCC_ORIGPEDIDOOIH%TYPE;
    lv_CodClienteXray   documento_cxc.cdcc_cliente%TYPE;

    -- 1. Variables para la cabecera del documento (recepciones)
    lv_TmpNumOrden                     CUWD_CONF_ORDE_RECE_API.numorden%type;
    lv_TmpFechaOrden                   CUWD_CONF_ORDE_RECE_API.fechaorden%type;
    --lv_TmpCodigoAlmacen                CUWD_CONF_ORDE_RECE_API.codigoalmacen%type;
    lv_TmpFechahoraIngresoOrden        CUWD_CONF_ORDE_RECE_API.fechahoraingresoorden%type;
    lv_TmpFechahoraTransOrden          CUWD_CONF_ORDE_RECE_API.fechahoratransaccionorden%type;
    lv_TmpCodigoCliente                CUWD_CONF_ORDE_RECE_API.codigoproveedor%type;
    lv_TmpTipoMovimiento               CUWD_CONF_ORDE_RECE_API.tipomovimiento%type;
    --lv_TmpTipoDocumento                CUWD_CONF_ORDE_RECE_API.tipodocumento%type;
    lv_TmpNumDocumento                 CUWD_CONF_ORDE_RECE_API.numdocumento%type;
    --ln_TmpIdCuenta                     CUWD_CONF_ORDE_RECE_API.idcuenta%type;
    lv_TmpUsuario                      CUWD_CONF_ORDE_RECE_API.usuario%type;
    lv_TmpMotivoDevo                   CUWD_CONF_ORDE_RECE_API.motivoDevolucion%TYPE;
    lv_TmpResponsableDevo              CUWD_CONF_ORDE_RECE_API.responsableDevolucion%TYPE;
    lv_TmpTipoIncidenciaDevo           CUWD_CONF_ORDE_RECE_API.tipoIncidenciaDevolucion%TYPE;

    -- 2. Detalle del documento             
    CURSOR c2 IS
      select tm.numorden,
            tm.fechaorden,
            --tm.codigoalmacen,
            tm.fechahoraingresoorden,
            tm.fechahoratransaccionorden,
            --tm.codigoproveedor,
            tm.codigocliente,
            --tm.tipomovimiento,
            tm.tipodocumento,
            tm.numdocumento,
            tm.motivodevolucion,
            tm.responsabledevolucion,
            tm.tipoincidenciadevolucion,
            --tm.idcuenta,
            tm.usuario,
            tm.item,
            tm.codigoarticulo,
            tm.codigoum,
            tm.loteproveedor,
            tm.fechavencimiento,
            --tm.anyocosecha,
            tm.cantidadpedida,
            tm.cantidadrecibida,
            tm.codigosubalmacen,
            tm.tipoincidenciaitem
        from CUWD_CONF_ORDE_RECE_API tm
       where tm.tipomovimiento = pv_TipMovim
         and tm.numorden = pv_NumOrden
         and tm.fechaorden = pv_FecOrden
         and tm.codigocliente = pv_CodCliente
         and tm.idtransaccion = pv_IdTransaccion
         and tm.est_regi = 'IN'; -- Registro pendiente de procesar
    c2_rec c2%ROWTYPE;
  
  BEGIN
    -- *********************************************************************
    -- ********************* CABECERA DE INGRESO *************************
    -- *********************************************************************
    lv_Step     := '1';
    lv_CiaVenta := '01';
    S_ERRO      := WHPG_CTE_DINE.OK;
    S_MENS      := '';
    
    select distinct tm.numorden,
            tm.fechaorden,
            tm.fechahoraingresoorden,
            tm.fechahoratransaccionorden,
            tm.codigocliente,
            tm.tipomovimiento,
            tm.usuario,
            tm.motivoDevolucion,
            tm.responsableDevolucion,
            tm.tipoincidenciadevolucion
        into lv_TmpNumOrden,
            lv_TmpFechaOrden,
            lv_TmpFechahoraIngresoOrden,
            lv_TmpFechahoraTransOrden,
            lv_TmpCodigoCliente,
            lv_TmpTipoMovimiento,
            lv_TmpUsuario,
            lv_TmpMotivoDevo,
            lv_TmpResponsableDevo,
            lv_TmpTipoIncidenciaDevo
        from CUWD_CONF_ORDE_RECE_API tm
       where tm.tipomovimiento = pv_TipMovim
         and tm.numorden = pv_NumOrden
         and tm.fechaorden = pv_FecOrden
         and tm.codigocliente = pv_CodCliente
         and tm.idtransaccion = pv_IdTransaccion
         and tm.est_regi = 'IN';
    
    lv_NroOrde := TRIM(lv_TmpNumOrden);
    lv_CodClie := TRIM(lv_TmpCodigoCliente);
    
    -- Si pasa las validaciones de Devolucion
    IF S_ERRO = WHPG_CTE_DINE.OK THEN
        lv_Step := '2';
        -- ************************** INGRESO POR DEVOLUCION *******************************
        lv_TipMovi     := WHPG_CTE_DINE.MOVI_ID;
        lv_TipDocu     := WHPG_CTE_DINE.DOCU_PI;
        ln_SecDevo       := TO_NUMBER(lv_TmpNumOrden);

        lv_GlosMovi    := 'Devolución por Logi.Inve. ' || lv_NroOrde || ' - WS CUBICO - ' ||
                         to_char(sysdate, 'dd/mm/yy HH:Mi AM');

        ld_FecOrde     := TO_DATE(lv_TmpFechaOrden, 'YYYY-mm-dd');
        ld_FecTran     := TO_DATE(SUBSTR(lv_TmpFechahoraTransOrden, 0, 10), 'YYYY-mm-dd');
        ld_HorTran     := TO_DATE(lv_TmpFechahoraTransOrden, 'YYYY-mm-dd"T"HH24:MI:SS');

        lv_Motivo := lv_TmpMotivoDevo;
        lv_CodRespDevo := lv_TmpResponsableDevo;
        lv_TipInciDevo := lv_TmpTipoIncidenciaDevo;
        
        S_MENS := 'LI:' || to_char(ln_SecDevo) || '|' || lv_TmpFechaOrden || '|' || lv_CodClie || '|| ';
        
        -- Obtiene longitud de número de documento
        select nvl(num1, 6)
          into ln_LongMovAlm
          from tablas
         where categoria = '051'
           and llave = '92';

        if ln_LongMovAlm > 8 then
          S_MENS := S_MENS || '=> Error de definicion XRAY: La longitud de documento definida en la categoria 051, llave 92 no puede ser mayor a 8';
          raise ERRO_GENE_DOCU;
        end if;

        begin 
            lv_Position := INSTR(lv_TmpNumDocumento,'-');
            select C.CDCC_TIPODOC,
                    C.CDCC_SECUENCIA,
                    C.CDCC_ORIGPEDIDOOIH,
                    c.cdcc_cliente
                into lv_TipDocuCXC,
                    lv_NroSecuCXC,
                    lv_CodLoca,
                    lv_CodClienteXray
            from documento_cxc c
            where c.ndcc_serie = SUBSTR(lv_TmpNumDocumento, 1, lv_Position-1) 
              and c.ndcc_preimpreso = SUBSTR(lv_TmpNumDocumento, 1, lv_Position+1)
              and c.sdcc_status != 'AN';
        exception
          when no_data_found then
            S_MENS := S_MENS || '=> No existe el documento relacionado o se encuentra anulado: ' || lv_TmpNumDocumento;
            raise ERRO_NOT_FOUND;
        end;

        lv_NumDocRef1 := lv_NroSecuCXC;
        select t.desc2
            into lv_TipDocRef -- Tipo de documento almacen FA, BL, etc
            from tablas t
        where t.categoria = '020'
            and t.llave = lv_TipDocuCXC;
       
        -- *********************************************************************
        -- ********************** DETALLE DE DEVOLUCION **********************
        -- *********************************************************************
        lv_Step := '3';
        FOR c2_rec IN c2 LOOP
            -- Datos
            ln_SecDetDevo  := c2_rec.item;
            lv_CodItem     := c2_rec.codigoarticulo;
            lv_NroGuiaProv := c2_rec.numdocumento;
            lv_CodAlma     := c2_rec.codigosubalmacen;
            
            -- Obtengo la UM Stock y Venta del articulo
            select a.um_control_stock, a.um_venta
                into lv_UMStock, lv_UMVenta
                from articulos a
              where a.cod_item = lv_CodItem;

            -- Conversión de cantidad recibida a UM Stock
            lv_UMOrig := c2_rec.codigoum; 
            ln_CantRecibida := c2_rec.cantidadrecibida;

            P_CONVIERTE_UM(lv_UMOrig, lv_UMStock, ln_CantRecibida);
            ln_CantUMStock := ln_CantRecibida;

            -- Obtiene Correlativo Ingreso 
            ln_Corre := WHFU_OBTI_CORR_MOVI(lv_CiaVenta, 'IM');
            IF ln_Corre = 0 THEN
                S_MENS := S_MENS || '=> Error al generar correlativo de ingreso';
                RAISE ERRO_GENE_DOCU;
            END IF;

            -- Se genera el nro. de documento de ingreso
            lv_NroDocu := LPAD(ln_Corre, ln_LongMovAlm, '0');

            -- ********************  A. Inserta en MOV_INVENTARIOS ***********************
            lv_Step := '4';
            insert into Mov_Inventarios
                (cod_cia,
                compania_venta_3,
                almacen_venta,
                tipo_movimiento,
                tipo_documento,
                nro_documento,
                cod_item_2,
                proveedor,
                almacen_destino,
                cantidad,
                costo_unitario,
                doc_ref_1,
                doc_ref_2,
                fecha_transaccion,
                motivo,
                precio_unitario,
                tipo_doc_ref,
                um_item_3,
                usuario,
                moneda,
                costo_unitario_me,
                cos_unit_est,
                cos_unit_me_est,
                hora_transaccion,
                f_ordencompra,
                c_sec_oc,
                c_sec_det_oc,
                tipo_doc_ref_2,
                observacion,
                ingreso_salida,
                fecha_real,
                tiempo_garantia,
                reversado,
                cod_localidad,
                n_guia_prov,
                f_guia_prov,
                c_guia_transp,
                cod_orig_inci_devo,
                cod_subg_inci)
            values
                (WHPG_CTE_DINE.COD_CIA,
                lv_CiaVenta,
                lv_CodAlma,
                lv_TipMovi,
                lv_TipDocu,
                lv_NroDocu,
                lv_CodItem,
                lv_CodClienteXray,
                '', -- almacen_destino
                ln_CantUMStock,
                0, -- costo_unitario
                lv_NumDocRef1,
                lv_NroSecuCXC,
                ld_FecTran,
                lv_Motivo, -- motivo devolucion
                0, -- precio_unitario
                lv_TipDocRef,
                lv_UMStock,
                lv_TmpUsuario,
                '', -- moneda
                0, -- costo_unitario_me
                0, -- cost_unit_est
                0, -- cost_unit_me_est
                ld_HorTran,
                ld_FecOrde,
                ln_SecDevo,
                ln_SecDetDevo,
                 lv_TipDocuCXC,
                lv_GlosMovi,
                lv_TipMovi,
                sysdate, -- fecha_real
                '0', -- tiempo_garantia
                '0', -- reservado
                lv_CodLoca,
                '', -- n_guia_prov
                null, -- f_guia_prov
                lv_NroGuiaProv, -- Factura, Boleta
                lv_CodRespDevo, -- Codigo responsable
                lv_TipInciDevo); -- Tipo de incidencia devolucion
            if SQL%NOTFOUND then
                S_MENS := S_MENS || '=> Ocurrio un error al insertar en Mov_Inventarios';
                raise ERRO_NOT_FOUND;
            end if;
            -- ******************** Fin Inserta en MOV_UBICACION ***********************
            
            -- ******************** B. Inserta en DET_MOV_UBICACION ***********************
            lv_CodLoteProv := c2_rec.loteproveedor;
            lv_CodZona     := WHPG_CTE_CUBI.UBI_RECEP;
            lv_CodRack     := '     ';
            lv_CodNive     := '     ';
            lv_CodCasi     := '     ';
            lv_CodPale     := '     ';
            ln_CantMov     := ln_CantUMStock;
            ld_FecVcto     := TO_DATE(c2_rec.fechavencimiento, 'YYYY-mm-dd');
            lv_CodEsta     := WHPG_CTE_CUBI.EST_BE;
            --
            lv_Step := '5';
            --
            WHPR_OBTI_LOTE_INTE(lv_CodAlma,
                                lv_CodItem,
                                lv_CodEsta,
                                lv_CodLoteProv,
                                ld_FecVcto,
                                ln_CodLote,
                                S_ERRO_AUX,
                                S_MENS_AUX);

            IF S_ERRO_AUX != 0 THEN
                S_MENS := S_MENS || '=> El lote interno del lote proveedor ' ||
                        lv_CodLoteProv || ' no pudo ser generado';
                RAISE ERRO_GENE_DOCU;
            END IF;
            --
            lv_Step := '6';
            --
            insert into Det_Mov_Ubicacion
                (cod_cia,
                compania_venta_3,
                almacen_venta,
                tipo_movimiento,
                tipo_documento,
                nro_documento,
                cod_item_2,
                cod_lote,
                cod_estado,
                zona,
                rack,
                nivel,
                casillero,
                pallet,
                cantidad,
                um_mov,
                c_secdetoc,
                cantidad_um_stock)
            values
                (WHPG_CTE_DINE.COD_CIA,
                lv_CiaVenta,
                lv_CodAlma,
                lv_TipMovi,
                lv_TipDocu,
                lv_NroDocu,
                lv_CodItem,
                ln_CodLote,
                lv_CodEsta,
                lv_CodZona,
                lv_CodRack,
                lv_CodNive,
                lv_CodCasi,
                lv_CodPale,
                ln_CantMov,
                lv_UMStock,
                ln_SecDetDevo,
                ln_CantUMStock);
            if SQL%NOTFOUND then
                S_MENS := S_MENS || '=> Ocurrio un error al insertar Det_Mov_Ubicacion';
                raise ERRO_NOT_FOUND;
            end if;
            -- ******************** Fin Inserta en DET_MOV_UBICACION ***********************
                        
            -- ******************** Actualizo Procesamiento de Interface ***********************
            lv_Step := '7';
            update CUWD_CONF_ORDE_RECE_API tmp
                set tmp.est_regi = 'PR',
                    tmp.fec_crea = sysdate
            where tmp.numorden = lv_TmpNumOrden
                and tmp.fechaorden = lv_TmpFechaOrden
                and tmp.tipomovimiento = lv_TmpTipoMovimiento
                and tmp.codigocliente = lv_TmpCodigoCliente
                and tmp.item = ln_SecDetDevo
                and tmp.codigoarticulo = lv_CodItem
                and tmp.idtransaccion = pv_IdTransaccion;
        --
        END LOOP;
        --
    ELSE
        S_MENS := S_MENS;
        RAISE ERRO_GENE_DOCU;
    END IF;
    --
    S_MENS := 'La recepción de devolución se realizó de forma satisfactoria.';
    --
    COMMIT;
    --
  EXCEPTION
    WHEN ERRO_NOT_FOUND THEN
      S_MENS := 'Ocurrio un error al insertar registro: ' || S_MENS;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      ROLLBACK;
      update CUWD_CONF_ORDE_RECE_API tmp
          set tmp.est_regi = 'ER',
            tmp.fec_crea = sysdate
      where tmp.numorden = lv_TmpNumOrden
          and tmp.fechaorden = lv_TmpFechaOrden
          and tmp.tipomovimiento = lv_TmpTipoMovimiento
          and tmp.codigocliente = lv_TmpCodigoCliente
          and tmp.idtransaccion = pv_IdTransaccion;
    WHEN ERRO_GENE_DOCU THEN
      S_MENS := 'Ocurrio un error al confirmar la recepción de Devolución: ' || S_MENS;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      ROLLBACK;
      update CUWD_CONF_ORDE_RECE_API tmp
          set tmp.est_regi = 'ER',
            tmp.fec_crea = sysdate
      where tmp.numorden = lv_TmpNumOrden
          and tmp.fechaorden = lv_TmpFechaOrden
          and tmp.tipomovimiento = lv_TmpTipoMovimiento
          and tmp.codigocliente = lv_TmpCodigoCliente
          and tmp.idtransaccion = pv_IdTransaccion;
    WHEN OTHERS THEN
      S_MENS := 'PCK: WHPG_INTE_CUBI - PRC: WHPR_PROC_CONFI_DEVO || ERROR NUMERO: ' ||
                SQLCODE || ' - DESCRIPCION: ' || SQLERRM || ' - PASO : ' || lv_Step;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      ROLLBACK;
      update CUWD_CONF_ORDE_RECE_API tmp
          set tmp.est_regi = 'ER',
            tmp.fec_crea = sysdate
      where tmp.numorden = lv_TmpNumOrden
          and tmp.fechaorden = lv_TmpFechaOrden
          and tmp.tipomovimiento = lv_TmpTipoMovimiento
          and tmp.codigocliente = lv_TmpCodigoCliente
          and tmp.idtransaccion = pv_IdTransaccion;
  END WHPR_PROC_CONFI_DEVO;
  
  /***************************************************************************
     Creado  : Raul Caballero - 26-07-21
     Descrip.: Validacion de datos de Orden de compra
  ****************************************************************************/
  PROCEDURE WHPR_VALI_CONFI_ORCO(pv_CiaVenta      IN VARCHAR2,
                                 pv_TipMovim      IN VARCHAR2,
                                 pv_NumOrden      IN VARCHAR2,
                                 pv_FecOrden      IN VARCHAR2,
                                 pv_CodProveedor  IN VARCHAR2,
                                 pv_IdTransaccion IN VARCHAR2,
                                 S_ERRO           OUT NUMBER,
                                 S_MENS           OUT VARCHAR2) IS
  
    ERRO_VALI_ORCO EXCEPTION;
    ERRO_NOT_FOUND EXCEPTION;
    lv_Step               varchar2(3);
    ln_Cont               number;
    ln_PorcToleranciaProv NUMBER;
    ln_LimiteRecepItem    NUMBER;
    --
    ld_FecTran mov_inventarios.fecha_transaccion%TYPE;
    lv_CodProv mov_inventarios.proveedor%TYPE;
    --
    lv_CodEsta     det_mov_ubicacion.cod_estado%TYPE;
    --
    ld_FecOrden     orden_compra_cab.f_ordencompra%type;
    lv_NroOrde      mov_inventarios.doc_ref_1%TYPE;
    lv_CodAlma      orden_compra_det.c_almacen%TYPE;
    lv_CodAlmaOri   orden_compra_det.c_almacen%TYPE;
    ln_SecDetOC     orden_compra_det.c_sec_det_oc%TYPE;
    lv_CodItem      orden_compra_det.cod_item%TYPE;
    ln_CantRecibida orden_compra_det.q_Recibida_Oc%type;
    --
    lv_UMStock    articulos.um_control_stock%TYPE;
    lv_UMVenta    articulos.um_venta%TYPE;
    lv_UMOrig     mov_inventarios.um_item_3%TYPE;
    ln_CantPedida mov_inventarios.cantidad%TYPE;
    --
    ln_CantPedidadUMStock orden_compra_det.q_pedida_oc%TYPE;
  
    -- 1. Variables para la cabecera del documento (recepciones)
    lv_TmpNumOrden              CUWD_CONF_ORDE_RECE_API.numorden%type;
    lv_TmpFechaOrden            CUWD_CONF_ORDE_RECE_API.fechaorden%type;
    --lv_TmpCodigoAlmacen         CUWD_CONF_ORDE_RECE_API.codigoalmacen%type;
    --lv_TmpFechahoraIngresoOrden CUWD_CONF_ORDE_RECE_API.fechahoraingresoorden%type;
    lv_TmpFechahoraTransaccion  CUWD_CONF_ORDE_RECE_API.fechahoratransaccionorden%type;
    lv_TmpCodigoProveedor       CUWD_CONF_ORDE_RECE_API.codigoproveedor%type;
    lv_TmpTipoMovimiento        CUWD_CONF_ORDE_RECE_API.tipomovimiento%type;
    --lv_TipoDocumento                CUWD_CONF_ORDE_RECE_API.tipodocumento%type;
    --lv_NumDocumento                 CUWD_CONF_ORDE_RECE_API.numdocumento%type;
    --lv_Usuario                      CUWD_CONF_ORDE_RECE_API.usuario%type;
  
    -- 2. Cursor detallado por documento               
    CURSOR c2 IS
      select tm.numorden,
             tm.fechaorden,
             tm.codigoalmacen,
             tm.fechahoraingresoorden,
             tm.fechahoratransaccionorden,
             --tm.codigoproveedor,
             tm.codigocliente,
             tm.tipomovimiento,
             tm.tipodocumento,
             tm.numdocumento,
             tm.motivodevolucion,
             tm.responsabledevolucion,
             tm.tipoincidenciadevolucion,
             tm.idcuenta,
             tm.usuario,
             tm.item,
             tm.codigoarticulo,
             tm.codigoum,
             --tm.loteproveedor,
             --tm.fechavencimiento,
             --tm.anyocosecha,
             tm.cantidadpedida,
             tm.cantidadrecibida,
             tm.codigosubalmacen,
             tm.tipoincidenciaitem
        from CUWD_CONF_ORDE_RECE_API tm
       where tm.tipomovimiento = pv_TipMovim
         and tm.numorden = pv_NumOrden
         and tm.fechaorden = pv_FecOrden
         and tm.codigoproveedor = pv_CodProveedor
         and tm.idtransaccion = pv_IdTransaccion
         and tm.est_regi = 'IN'; -- IN: Ingresado y pendiente de procesar
    c2_rec c2%ROWTYPE;
  
  BEGIN
    S_ERRO := WHPG_CTE_DINE.OK;
    S_MENS := '';
  
    lv_Step := '1';
    select distinct tm.numorden,
                    tm.fechaorden,
                    --tm.codigoalmacen,
                    --tm.fechahoraingresoorden,
                    tm.fechahoratransaccionorden,
                    tm.codigoproveedor,
                    tm.tipomovimiento
      into lv_TmpNumOrden,
           lv_TmpFechaOrden,
           --lv_TmpCodigoAlmacen,
           --lv_TmpFechahoraIngresoOrden,
           lv_TmpFechahoraTransaccion,
           lv_TmpCodigoProveedor,
           lv_TmpTipoMovimiento
      from CUWD_CONF_ORDE_RECE_API tm
     where tm.tipomovimiento = pv_TipMovim
       and tm.numorden = pv_NumOrden
       and tm.fechaorden = pv_FecOrden
       and tm.codigoproveedor = pv_CodProveedor
       and tm.idtransaccion = pv_IdTransaccion
       and tm.est_regi = 'IN';
  
    -- ************************* A. VALIDACION DE CABECERA ***************************
    lv_Step     := '2';
    lv_CodProv  := TRIM(lv_TmpCodigoProveedor);
    lv_NroOrde  := TRIM(lv_TmpNumOrden);
    ld_FecOrden := TO_DATE(lv_TmpFechaOrden, 'YYYY-mm-dd');
    ld_FecTran  := TO_DATE(SUBSTR(lv_TmpFechahoraTransaccion, 0, 10),
                           'YYYY-mm-dd');
  
    S_MENS := 'OC: ' || lv_NroOrde || '|' || lv_TmpFechaOrden || '|' ||
              lv_CodProv || '|| ';
  
    -- Validacion de periodo de valorización
    ln_Cont := WHFU_VALI_PERI_VALO(pv_CiaVenta, ld_FecTran);
    IF ln_Cont = 0 THEN
      S_MENS := S_MENS ||
                '=> No existe ó no esta abierto el periódo de valorización para el mes.' ||
                TO_CHAR(ld_FecTran, 'MM');
      RAISE ERRO_VALI_ORCO;
    END IF;
  
    -- *********************** B. VALIDACION DE DETALLE ****************************
    lv_Step := '3';
    -- Se obtiene la tolerancia por proveedor
    SELECT p1.por_tole_ingr
      INTO ln_PorcToleranciaProv
      FROM PROVEEDOR p1
     WHERE p1.COD_CIA = '00'
       AND p1.c_proveedor = pv_CodProveedor;
  
    FOR c2_rec IN c2 LOOP
      -- Datos
      ln_SecDetOC := c2_rec.item;
      lv_CodItem  := c2_rec.codigoarticulo;
      lv_CodEsta  := WHPG_CTE_CUBI.EST_BE;
      lv_CodAlma  := c2_rec.codigosubalmacen;
    
      S_MENS := S_MENS || 'Detalle: ' || to_char(ln_SecDetOC) || '|' ||
                lv_CodItem || '|';
    
      -- Validacion de detalle OC
      select count(1)
        into ln_Cont
        from orden_compra_det d
       where d.cod_cia = WHPG_CTE_DINE.COD_CIA
         and d.cod_cia_vta = pv_CiaVenta
         and d.c_proveedor = lv_CodProv
         and d.c_sec_oc = TO_NUMBER(lv_NroOrde)
         and d.f_ordencompra = ld_FecOrden
         and d.cod_item = lv_CodItem
         and d.c_sec_det_oc = ln_SecDetOC;
    
      if ln_Cont = 0 then
        S_MENS := S_MENS ||
                  '=> No existe o no coincide con registro original.';
        RAISE ERRO_VALI_ORCO;
      end if;
    
      -- Validacion de almacen
      select nvl(d.c_almacen, 'XX')
        into lv_CodAlmaOri
        from orden_compra_det d
       where d.cod_cia = WHPG_CTE_DINE.COD_CIA
         and d.cod_cia_vta = pv_CiaVenta
         and d.c_proveedor = lv_CodProv
         and d.c_sec_oc = TO_NUMBER(lv_NroOrde)
         and d.f_ordencompra = ld_FecOrden
         and d.cod_item = lv_CodItem
         and d.c_sec_det_oc = ln_SecDetOC;
    
      IF (lv_CodAlma != lv_CodAlmaOri) then
        S_MENS := S_MENS || '=> El almacén ' || lv_CodAlma ||
                  ' no concide con el almacen original ' || lv_CodAlmaOri;
        RAISE ERRO_VALI_ORCO;
      END IF;
    
      if lv_CodAlma not in ('67', '76') THEN
        S_MENS := S_MENS || '=> El almacén ' || lv_CodAlma ||
                  ' no está configurado para trabajar con CUBICO';
        RAISE ERRO_VALI_ORCO;
      END IF;
    
      -- Validación de existencia de código articulo
      select count(1)
        into ln_Cont
        from articulos a
       where a.cod_item = lv_CodItem;
    
      IF ln_Cont = 0 THEN
        S_MENS := S_MENS || '=> El codigo de articulo no existe en XRAY';
        RAISE ERRO_VALI_ORCO;
      END IF;
    
      /*-- Validación de estado del articulo
       select Count(1)
        into ln_Cont
        from tablas t
       where t.categoria = '404'
         and t.llave = lv_CodEsta;
      
      IF ln_Cont = 0 THEN
        S_MENS     := S_MENS || '=> El estado ' || lv_CodEsta || ' no existe en XRAY';
        RAISE ERRO_VALI_ORCO;
      END IF;*/
    
      -- Valida que la cantidad recibida no exceda el limite
      select d.c_um_item_oc, d.q_pedida_oc, d.q_Recibida_Oc
        into lv_UMOrig, ln_CantPedida, ln_CantRecibida
        from orden_compra_det d
       where d.cod_cia = WHPG_CTE_DINE.COD_CIA
         and d.cod_cia_vta = pv_CiaVenta
         and d.c_proveedor = lv_CodProv
         and d.c_sec_oc = TO_NUMBER(lv_NroOrde)
         and d.f_ordencompra = ld_FecOrden
         and d.cod_item = lv_CodItem
         and d.c_sec_det_oc = ln_SecDetOC;
    
      select a.um_control_stock, a.um_venta
        into lv_UMStock, lv_UMVenta
        from articulos a
       where a.cod_item = lv_CodItem;
    
      -- Conversión de Cantidad pedida OC a UM Stock    
      P_CONVIERTE_UM(lv_UMOrig, lv_UMStock, ln_CantPedida);
      ln_CantPedidadUMStock := ln_CantPedida;
    
      ln_LimiteRecepItem := ln_CantPedidadUMStock +
                            (ln_PorcToleranciaProv * ln_CantPedidadUMStock);
    
      IF ln_CantRecibida + c2_rec.cantidadrecibida > ln_LimiteRecepItem THEN
        S_MENS := S_MENS ||
                  '=> La cantidad a recepcionar excede el limite permitido: Cant. recibida : ' ||
                  to_char(ln_CantRecibida) || ', Cant. Máxima a recibir: ' ||
                  to_char(ln_LimiteRecepItem) || ', Cant. a recibir: ' ||
                  to_char(c2_rec.cantidadrecibida) ||
                  ', Cant. recibida después: ' ||
                  to_char(ln_CantRecibida + c2_rec.cantidadrecibida);
        RAISE ERRO_VALI_ORCO;
      END IF;
    
    END LOOP;
    --
  EXCEPTION
    WHEN ERRO_VALI_ORCO THEN
      S_MENS := 'Ocurrio un error al validar la OC: ' || S_MENS;
      S_ERRO := WHPG_CTE_DINE.ERRO;
    WHEN OTHERS THEN
      S_MENS := 'PCK: WHPG_INTE_CUBI - PRC: WHPR_VALI_CONFI_ORCO || ERROR NUMERO: ' ||
                SQLCODE || ' - DESCRIPCION: ' || SQLERRM || ' - PASO : ' ||
                lv_Step;
      S_ERRO := WHPG_CTE_DINE.ERRO;
  END WHPR_VALI_CONFI_ORCO;

  /***************************************************************************
   Creado  : Nilton Santos - 20-05-2020
   Descrip.: Obtiene correlativo de movimiento de almacenes
   Modif.  :
   Descrip.:
  ****************************************************************************/
  FUNCTION WHFU_OBTI_CORR_MOVI(pv_cod_cia varchar2, pv_tip_movi varchar2)
    RETURN NUMBER IS
    ln_Corre correlativo_alma.n_correlativo%TYPE;
  
  BEGIN
    --
    UPDATE CORRELATIVO_ALMA
       SET N_CORRELATIVO = NVL(N_CORRELATIVO, 0) + 1
     WHERE C_CODCIA = pv_cod_cia
       AND C_CODIGO = pv_tip_movi;
  
    SELECT NVL(N_CORRELATIVO, 0)
      INTO ln_Corre
      FROM CORRELATIVO_ALMA
     WHERE C_CODCIA = pv_cod_cia
       AND C_CODIGO = pv_tip_movi;
    --
    RETURN ln_Corre;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 0;
  END WHFU_OBTI_CORR_MOVI;

  /***************************************************************************
   Creado  : Nilton Santos - 21-05-2020
   Descrip.: Obtiene lote interno de Saldo Lote
   Modif.  : Raul Caballero
   Descrip.: Refactor
  ****************************************************************************/
  PROCEDURE WHPR_OBTI_LOTE_INTE(pv_cod_alma      IN varchar2,
                                pv_cod_item      IN varchar2,
                                pv_cod_esta      IN varchar2,
                                pv_cod_lote_prov IN varchar2,
                                pd_fec_vcto      IN date,
                                pn_cod_lote      OUT number,
                                S_ERRO           OUT NUMBER,
                                S_MENS           OUT VARCHAR2) IS
  
    ln_CodLote saldo_lote.cod_lote%TYPE;
    ln_Cont    NUMBER;
  
  BEGIN
    --
    S_ERRO := WHPG_CTE_DINE.OK;
    -- Valido si artículo maneja lotes
    select Count(1)
      into ln_Cont
      from articulos a
     where a.cod_item = pv_cod_item
       and nvl(a.flag_lote, '0') = '1';
    --
    IF ln_Cont > 0 THEN
      -- Validación si ya existe un lote interno para el Lote Proveedor indicado
      select Count(1)
        into ln_Cont
        from saldo_lote l
       where l.compania_venta = WHPG_CTE_DINE.COD_COMP
         and l.cod_almacen = pv_cod_alma
         and l.cod_cia = WHPG_CTE_DINE.COD_CIA
         and l.cod_item = pv_cod_item
         and l.cod_estado = pv_cod_esta
         and l.cod_lote_proveedor = pv_cod_lote_prov;
      --
      if ln_Cont = 0 then
        -- Si no existe un lote interno para el lote proveedor
        -- Valido si el producto tiene lotes anteriores
        select Count(1)
          into ln_Cont
          from saldo_lote l
         where l.compania_venta = WHPG_CTE_DINE.COD_COMP
           and l.cod_almacen = pv_cod_alma
           and l.cod_cia = WHPG_CTE_DINE.COD_CIA
           and l.cod_item = pv_cod_item
           and l.cod_estado = pv_cod_esta;
        --
        if ln_Cont > 0 then
          -- Incremento el cod_lote (nro lote interno) en 1
          select max(nvl(l.cod_lote, 0)) + 1
            into ln_CodLote
            from saldo_lote l
           where l.compania_venta = WHPG_CTE_DINE.COD_COMP
             and l.cod_almacen = pv_cod_alma
             and l.cod_cia = WHPG_CTE_DINE.COD_CIA
             and l.cod_item = pv_cod_item
             and l.cod_estado = pv_cod_esta;
        else
          ln_CodLote := 1;
        end if;
      else
        -- Si existe un lote interno para el lote proveedor
        select l.cod_lote
          into ln_CodLote
          from saldo_lote l
         where l.compania_venta = WHPG_CTE_DINE.COD_COMP
           and l.cod_almacen = pv_cod_alma
           and l.cod_cia = WHPG_CTE_DINE.COD_CIA
           and l.cod_item = pv_cod_item
           and l.cod_estado = pv_cod_esta
           and l.cod_lote_proveedor = pv_cod_lote_prov;
      end if;
    else
      ln_CodLote := 0;
    end if;
  
    -- Validación de existencia de Lote interno obtenido
    select Count(1)
      into ln_Cont
      from saldo_lote l
     where l.compania_venta = WHPG_CTE_DINE.COD_COMP
       and l.cod_almacen = pv_cod_alma
       and l.cod_cia = WHPG_CTE_DINE.COD_CIA
       and l.cod_item = pv_cod_item
       and l.cod_estado = pv_cod_esta
       and l.cod_lote = ln_CodLote;
    --
    if ln_Cont = 0 then
      -- Inserto en Saldo Lote
      insert into saldo_lote
        (compania_venta,
         cod_almacen,
         cod_cia,
         cod_item,
         cod_lote,
         cod_estado,
         c_proveedor,
         saldo,
         fecha_vencimiento,
         cod_lote_proveedor,
         rowversion)
      values
        (WHPG_CTE_DINE.COD_COMP,
         pv_cod_alma,
         WHPG_CTE_DINE.COD_CIA,
         pv_cod_item,
         ln_CodLote,
         pv_cod_esta,
         null, -- NSC  24-11-20 Antes pv_cod_prov
         0,
         pd_fec_vcto,
         pv_cod_lote_prov,
         0);
      --
    end if;
    --
    pn_cod_lote := ln_CodLote;
    --
  EXCEPTION
    WHEN OTHERS THEN
      S_MENS      := 'PCK: WHPG_INTE_DINE - PRC: WHPR_OBTI_LOTE_INTE || ERROR NUMERO: ' ||
                     SQLCODE || ' - DESCRIPCION: ' || SQLERRM;
      S_ERRO      := WHPG_CTE_DINE.ERRO;
      pn_cod_lote := -1;
      ROLLBACK;
  END WHPR_OBTI_LOTE_INTE;

  /***************************************************************************
   Creado  : Nilton Santos - 21-05-2020
   Descrip.: Actualiza la OC
   Modif.  :
   Descrip.:
  ****************************************************************************/
  PROCEDURE WHPR_ACTU_ORDE_COMP(pv_cod_prov      varchar2,
                                pv_nro_orde      varchar2,
                                pv_cod_item      varchar2,
                                pn_sec_orde_deta orden_compra_det.c_sec_det_oc%TYPE,
                                pv_cod_umed_orde varchar2,
                                pn_can_reci_stoc number,
                                S_ERRO           OUT NUMBER,
                                S_MENS           OUT VARCHAR2) AS
  
    ln_Cont    NUMBER;
    ln_CanPedi orden_compra_det.q_pedida_oc%TYPE;
    ln_CanReci orden_Compra_det.q_Recibida_Oc%TYPE;
    lv_UMOrde  orden_compra_det.c_um_item_oc%TYPE;
    lv_UMStoc  orden_compra_det.c_um_item_oc%TYPE;
    lv_EstDeta orden_compra_Det.s_Item_Oc%TYPE;
    lv_EstOC   orden_compra_cab.s_oc%TYPE;
  
  BEGIN
    lv_UMOrde := pv_cod_umed_orde;
    -- Obtengo los datos de cantidad pedida y recibida actual
    select nvl(d.q_pedida_oc, '0'),
           nvl(d.q_recibida_oc, 0),
           a.um_control_stock
      into ln_CanPedi, ln_CanReci, lv_UMStoc
      from Orden_Compra_Det d
     inner join articulos a on d.cod_item = a.cod_item
     where d.cod_cia = WHPG_CTE_DINE.COD_CIA
       and d.cod_cia_vta = WHPG_CTE_DINE.COD_COMP
       and d.c_proveedor = pv_cod_prov
       and d.c_sec_oc = pv_nro_orde
       and d.c_sec_det_oc = pn_sec_orde_deta
       and d.cod_item = pv_cod_item
       and d.c_um_item_oc = lv_UMOrde;
  
    -- Procedimiento que convierte la cantidad pedida en UM stock
    P_CONVIERTE_UM(lv_UMOrde, lv_UMStoc, ln_CanPedi);
    --
    if (ln_CanReci + pn_can_reci_stoc) >= ln_CanPedi then
      lv_EstDeta := 'DT';
    else
      lv_EstDeta := 'DP';
    end if;
  
    -- Actualizo la cantidad recibida
    Update orden_compra_det d
       set q_recibida_oc = nvl(q_recibida_oc, 0) + pn_can_reci_stoc,
           s_item_oc     = lv_EstDeta
     where cod_cia = WHPG_CTE_DINE.COD_CIA
       and cod_cia_vta = WHPG_CTE_DINE.COD_COMP
       and c_proveedor = pv_cod_prov
       and c_sec_oc = pv_nro_orde
       and cod_item = pv_cod_item
       and d.c_sec_det_oc = pn_sec_orde_deta
       and c_um_item_oc = lv_UMOrde;
  
    -- Cuantos items en el detalle de la OC faltan completar su recepcion
    select Count(1)
      into ln_Cont
      from ORDEN_COMPRA_DET d
     where cod_cia = WHPG_CTE_DINE.COD_CIA
       and cod_cia_vta = WHPG_CTE_DINE.COD_COMP
       and c_proveedor = pv_cod_prov
       and c_sec_oc = pv_nro_orde
       and d.s_item_oc != 'DT';
    --
    if ln_Cont > 0 then
      lv_EstOC := 'DP';
    else
      lv_EstOC := 'DT';
    end if;
  
    -- Actualización de cabecera de OC
    update ORDEN_COMPRA_CAB c
       set c.s_oc = lv_EstOC
     where cod_cia = WHPG_CTE_DINE.COD_CIA
       and cod_cia_vta = WHPG_CTE_DINE.COD_COMP
       and c_proveedor = pv_cod_prov
       and c_sec_oc = pv_nro_orde;
    --  
  EXCEPTION
    WHEN OTHERS THEN
      S_MENS := 'PCK: WHPG_INTE_DINE - PRC: WHPR_ACTU_ORDE_COMP || ERROR NUMERO: ' ||
                SQLCODE || ' - DESCRIPCION: ' || SQLERRM;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      ROLLBACK;
  END WHPR_ACTU_ORDE_COMP;
  /***************************************************************************
   Creado  : Nilton Santos - 20-05-2020
   Descrip.: Validando Periodo de Valorización
   Modif.  :
   Descrip.:
  ****************************************************************************/
  FUNCTION WHFU_VALI_PERI_VALO(pv_cod_cia varchar2, pd_fec_mov date)
    RETURN NUMBER IS
    ln_Val NUMBER(1);
    lv_Per VARCHAR2(7);
  BEGIN
    ln_Val := 0;
    lv_Per := TO_CHAR(pd_fec_mov, 'YYYY') || '-' ||
              TO_CHAR(pd_fec_mov, 'MM');
    --
    SELECT count(1)
      INTO ln_Val
      FROM PERIODO_COSTEADO
     WHERE COD_CIA = WHPG_CTE_DINE.COD_CIA
       AND COMPANIA_VENTA = pv_cod_cia
       AND PERIODO = lv_Per
       AND ESTADO = 'A';
  
    RETURN ln_Val;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 0;
  END WHFU_VALI_PERI_VALO;
  
  /***************************************************************************
     Creado  : Nilton Santos - 01-09-21
     Descrip.: Procesar confirmación de Ingreso de PTS
     Modif.  : 
     Descrip.: 
  ****************************************************************************/
  PROCEDURE WHPR_PROC_CONFI_INGR_PTER(pn_NroOP         IN NUMBER,
                                      pv_IdTransaccion IN VARCHAR2,
                                      S_ERRO           OUT NUMBER,
                                      S_MENS           OUT VARCHAR2) IS
    ERRO_GENE_DOCU EXCEPTION;
    ERRO_NOT_FOUND EXCEPTION;
    S_ERRO_AUX    number;
    S_MENS_AUX    varchar2(4000);
    lv_Step       varchar2(3);
    ln_LongMovAlm tablas.num1%TYPE;
    ln_Corre      number;
    lv_CiaVenta   varchar2(2);
    --
    lv_UMStock      articulos.um_control_stock%TYPE;
    lv_UMVenta      articulos.um_venta%TYPE;
    lv_NroDocu      mov_inventarios.nro_documento%TYPE;
    ld_FecTran      mov_inventarios.fecha_transaccion%TYPE;
    ld_HorTran      mov_inventarios.hora_transaccion%TYPE;
    lv_CodAlma      mov_inventarios.almacen_venta%TYPE;
    lv_GlosMovi     mov_inventarios.observacion%TYPE;
    ln_CantRecibida mov_inventarios.cantidad%TYPE;
    ln_CantStock    mov_inventarios.cantidad%TYPE;
    ln_SecOC        mov_inventarios.c_Sec_Oc%TYPE;
    ln_SecDetOC     mov_inventarios.c_sec_det_oc%TYPE;
    lv_NumDocRef1   mov_inventarios.doc_ref_1%TYPE;
    lv_NumDocRef3   mov_inventarios.doc_ref_3%TYPE;
    lv_NroNota      mov_inventarios.nro_nota%TYPE;
    lv_TipDocRef    mov_inventarios.tipo_doc_ref%TYPE;
    lv_CodItem      mov_inventarios.cod_item_2%TYPE;
    lv_TipMovi      mov_inventarios.tipo_movimiento%TYPE;
    lv_TipDocu      mov_inventarios.tipo_documento%TYPE;
    lv_Motivo       mov_inventarios.motivo%TYPE;
    lv_NroGuiaProv  mov_inventarios.c_guia_transp%TYPE;
    lv_NroOrde      mov_inventarios.doc_ref_1%TYPE;
    lv_CodRespDevo mov_inventarios.cod_orig_inci_devo%TYPE;
    --
    ln_CodLote det_mov_ubicacion.cod_lote%TYPE;
    ln_CantMov det_mov_ubicacion.cantidad%TYPE;
    lv_CodEsta det_mov_ubicacion.cod_estado%TYPE;
    lv_CodZona det_mov_ubicacion.zona%TYPE;
    lv_CodRack det_mov_ubicacion.rack%TYPE;
    lv_CodNive det_mov_ubicacion.nivel%TYPE;
    lv_CodCasi det_mov_ubicacion.casillero%TYPE;
    lv_CodPale det_mov_ubicacion.pallet%TYPE;
    --
    lv_CodLoteProv    saldo_lote.cod_lote_proveedor%TYPE;
    ld_FecVcto        saldo_lote.fecha_vencimiento%TYPE;
    lv_UMDevo         orden_compra_det.c_Um_Item_Oc%TYPE;
    lv_UMOrig         mov_inventarios.um_item_3%TYPE;
    lv_UMDest         mov_inventarios.um_item_3%TYPE;
    lv_CodClienteXray proveedor.c_proveedor%TYPE;
    --
    lv_TipDocuCXC       documento_cxc.CDCC_TIPODOC%TYPE;
    lv_NroSecuCXC       documento_cxc.CDCC_SECUENCIA%TYPE;
    lv_CodLoca          documento_cxc.CDCC_ORIGPEDIDOOIH%TYPE;
    
    -- 1. Variables para la cabecera del documento (recepciones)
    ln_TmpNroOP         cuwd_conf_opro_pter_api.NUMEROOP%type;
    lv_TmpFecEntr       cuwd_conf_opro_pter_api.FECHAENTREGA%TYPE;
    lv_TmpCodAlma       cuwd_conf_opro_pter_api.CODIGOALMACEN%TYPE;
    lv_TmpCodSubAlma    cuwd_conf_opro_pter_api.CODIGOSUBALMACEN%TYPE;
    ln_TmpSecu          cuwd_conf_opro_pter_api.SECUENCIA%TYPE;
    lv_TmpUsuario       cuwd_conf_opro_pter_api.USUARIO%TYPE;
    lv_TmpFecTran       cuwd_conf_opro_pter_api.Fechahoratransaccion%TYPE;
    --
    lv_CodMaqu          prd_ordprod.cod_maqu%TYPE; 
    lv_UsuConeWS        usr_conexion.usuario%TYPE;
    lv_PasConeWS        usr_conexion.clave%TYPE;
    ln_SecProdOP        prd_ordprod_prod.n_secarticulo%TYPE;    
    ln_Cont             number;    

  
    -- 2. Detalle del documento             
    CURSOR c2 IS
      select tm.fechaentrega,
             tm.codigosubalmacen,
             tm.codigoalmacen,
             tm.secuencia,
             tm.codigoproducto,
             tm.unidadmedida,
             tm.cantidadproducida,
             tm.usuario,
             tm.fechahoratransaccion
        from cuwd_conf_opro_pter_api tm
       where tm.numeroop = pn_NroOP
         and tm.sk_orde_prod_pter_api = pv_IdTransaccion
         and tm.est_regi = 'IN'; -- Registro pendiente de procesar
    c2_rec c2%ROWTYPE;
  
  BEGIN
    -- *********************************************************************
    -- ********************* CABECERA DE INGRESO *************************
    -- *********************************************************************
    lv_Step     := '1';
    lv_CiaVenta := '01';
    S_ERRO      := WHPG_CTE_DINE.OK;
    S_MENS      := '';
  
    select distinct tm.numeroop,
                    tm.fechaentrega,
                    tm.codigoalmacen,
                    tm.codigosubalmacen,
                    tm.secuencia,
                    tm.usuario,
                    tm.fechahoratransaccion
      into ln_TmpNroOP,
           lv_TmpFecEntr,
           lv_TmpCodAlma,
           lv_TmpCodSubAlma,
           ln_TmpSecu,
           lv_TmpUsuario,
           lv_TmpFecTran
      from cuwd_conf_opro_pter_api tm
     where tm.numeroop = pn_NroOP
       and tm.sk_orde_prod_pter_api = pv_IdTransaccion
       and tm.est_regi = 'IN';
  
    -- Si pasa las validaciones de Devolucion
    IF S_ERRO = WHPG_CTE_DINE.OK THEN
      lv_Step := '2';
      -- ************************** INGRESO POR DEVOLUCION *******************************
      lv_TipMovi    := WHPG_CTE_DINE.MOVI_IU;
      lv_TipDocu    := WHPG_CTE_DINE.DOCU_PI;
      lv_NumDocRef3 := ln_TmpNroOP;
      lv_NroNota    := 'PRDNEW';
    
      lv_GlosMovi := 'Ingreso de Producto Terminado - Nro. O/P ' || lv_NroOrde ||
                     ' - WS CUBICO - ' ||
                     to_char(sysdate, 'dd/mm/yy HH:Mi AM');
    
      ld_FecTran := TO_DATE(SUBSTR(lv_TmpFecTran, 0, 10),
                            'YYYY-mm-dd');
      ld_HorTran := TO_DATE(lv_TmpFecTran,
                            'YYYY-mm-dd"T"HH24:MI:SS');
      -- Usuario y Clave de WS para usar Package de Ordenes de Producción
      SELECT usuario, clave
        INTO lv_UsuConeWS, lv_PasConeWS
        FROM USR_CONEXION
       WHERE rownum = 1;
      -- Obtengo el Nro. de Maquina de OP y Tipo de Orden
      select p.cod_maqu, p.c_tipo_op
        into lv_CodMaqu, lv_Motivo
        from prd_ordprod p
       where p.c_compania = lv_CiaVenta
         and p.n_secuencia = ln_TmpNroOP;
      --           
      lv_CodLoteProv := ln_TmpNroOP ||lv_CodMaqu ;
      -- Obtiene longitud de número de documento
      select nvl(num1, 6)
        into ln_LongMovAlm
        from tablas
       where categoria = '051'
         and llave = '92';
    
      if ln_LongMovAlm > 8 then
        S_MENS := S_MENS ||
                  'Error de definicion XRAY: La longitud de documento definida en la categoria 051, llave 92 no puede ser mayor a 8';
        raise ERRO_GENE_DOCU;
      end if;
      --    
      S_MENS := 'Ingreso de OP : '||ln_TmpNroOP||' Grabada Satisfactoriamente';
    
      -- *********************************************************************
      -- ********************** DETALLE DE DEVOLUCION **********************
      -- *********************************************************************
      lv_Step := '3';
      FOR c2_rec IN c2 LOOP
        -- Datos
        ln_SecDetOC    := c2_rec.secuencia;
        lv_CodItem     := c2_rec.codigoproducto;
        lv_UMDevo      := c2_rec.unidadmedida;
        lv_CodAlma     := c2_rec.codigosubalmacen;
      
        -- Obtengo la UM Stock y Venta del articulo
        select a.um_control_stock, a.um_venta
          into lv_UMStock, lv_UMVenta
          from articulos a
         where a.cod_item = lv_CodItem;
      
        -- Conversión de cantidad a UM Stock
        lv_UMOrig       := c2_rec.unidadmedida;
        lv_UMDest       := lv_UMStock;
        ln_CantRecibida := c2_rec.cantidadproducida;
      
        P_CONVIERTE_UM(lv_UMOrig, lv_UMDest, ln_CantRecibida);
        ln_CantStock := ln_CantRecibida;
      
        -- Obtiene Correlativo Ingreso 
        ln_Corre := WHFU_OBTI_CORR_MOVI(lv_CiaVenta, 'IM');
        IF ln_Corre = 0 THEN
          S_MENS := S_MENS || 'Error al generar correlativo de ingreso';
          RAISE ERRO_GENE_DOCU;
        END IF;
      
        -- Se genera el nro. de documento de ingreso
        lv_NroDocu := LPAD(ln_Corre, ln_LongMovAlm, '0');
        
        -- ********************  A. Inserta en MOV_INVENTARIOS ***********************
        lv_Step := '4';
        insert into Mov_Inventarios
          (cod_cia,
           compania_venta_3,
           almacen_venta,
           tipo_movimiento,
           tipo_documento,
           nro_documento,
           cod_item_2,
           proveedor,
           almacen_destino,
           cantidad,
           compania_destino,
           costo_unitario,
           doc_ref_1,
           doc_ref_2,
           fecha_transaccion,
           motivo,
           precio_unitario,
           tipo_doc_ref,
           um_item_3,
           nro_nota,
           usuario,
           moneda,
           costo_unitario_me,
           cos_unit_est,
           cos_unit_me_est,
           hora_transaccion,
           f_ordencompra,
           c_sec_oc,
           c_sec_det_oc,
           observacion,
           doc_ref_3,
           ingreso_salida,
           tiempo_garantia,
           reversado,
           cod_pto_atencion,
           fecha_real,
           factor_deal,
           cod_localidad,
           tipo_doc_ref_2,
           n_guia_prov,
           f_guia_prov,
           c_guia_transp,
           cod_orig_inci_devo)
        values
          (WHPG_CTE_DINE.COD_CIA,
           lv_CiaVenta,
           lv_CodAlma,
           lv_TipMovi,
           lv_TipDocu,
           lv_NroDocu,
           lv_CodItem,
           lv_CodClienteXray,
           '',
           ln_CantStock,
           '',
           0,
           lv_NumDocRef1,
           lv_NroSecuCXC,
           ld_FecTran,
           lv_Motivo,
           0,
           lv_TipDocRef,
           lv_UMStock,
           lv_NroNota,
           lv_TmpUsuario,
           '',
           0,
           0,
           0,
           ld_HorTran,
           '',
           ln_SecOC,
           ln_SecDetOC,
           lv_GlosMovi,
           lv_NumDocRef3,
           lv_TipMovi,
           '0',
           '0',
           '',
           sysdate,
           '',
           lv_CodLoca,
           lv_TipDocuCXC,
           '',
           null,
           lv_NroGuiaProv, -- Factura, Boleta
           lv_CodRespDevo);
        if SQL%NOTFOUND then
          S_MENS := S_MENS ||
                    'Ocurrio un error al insertar en Mov_Inventarios';
          raise ERRO_NOT_FOUND;
        end if;
        -- ******************** Fin Inserta en MOV_UBICACION ***********************
      
        -- ******************** B. Inserta en DET_MOV_UBICACION ***********************
        lv_CodLoteProv := lv_CodLoteProv;
        lv_CodZona     := WHPG_CTE_CUBI.UBI_RECEP;
        lv_CodRack     := '     ';
        lv_CodNive     := '     ';
        lv_CodCasi     := '     ';
        lv_CodPale     := '     ';
        ln_CantMov     := ln_CantStock;
        ld_FecVcto     := null; --TO_DATE(c2_rec.fechavencimiento, 'YYYY-mm-dd');
        lv_CodEsta     := WHPG_CTE_CUBI.EST_BE;
        --
        lv_Step := '5';
        --
        WHPR_OBTI_LOTE_INTE(lv_CodAlma,
                            lv_CodItem,
                            lv_CodEsta,
                            lv_CodLoteProv,
                            ld_FecVcto,
                            ln_CodLote,
                            S_ERRO_AUX,
                            S_MENS_AUX);
      
        IF S_ERRO_AUX != 0 THEN
          S_MENS := S_MENS || 'El lote interno del lote proveedor ' ||
                    lv_CodLoteProv || ' no pudo ser generado';
          RAISE ERRO_GENE_DOCU;
        END IF;
        --
        lv_Step := '6';
        --
        insert into Det_Mov_Ubicacion
          (cod_cia,
           compania_venta_3,
           almacen_venta,
           tipo_movimiento,
           tipo_documento,
           nro_documento,
           cod_item_2,
           cod_lote,
           cod_estado,
           zona,
           rack,
           nivel,
           casillero,
           pallet,
           cantidad,
           um_mov,
           c_secdetoc,
           cantidad_um_stock)
        values
          (WHPG_CTE_DINE.COD_CIA,
           lv_CiaVenta,
           lv_CodAlma,
           lv_TipMovi,
           lv_TipDocu,
           lv_NroDocu,
           lv_CodItem,
           ln_CodLote,
           lv_CodEsta,
           lv_CodZona,
           lv_CodRack,
           lv_CodNive,
           lv_CodCasi,
           lv_CodPale,
           ln_CantMov,
           lv_UMStock,
           ln_SecDetOC,
           ln_CantStock);
        if SQL%NOTFOUND then
          S_MENS := S_MENS ||
                    'Ocurrio un error al insertar Det_Mov_Ubicacion';
          raise ERRO_NOT_FOUND;
        end if;
        -- ******************** Fin Inserta en DET_MOV_UBICACION ***********************
        -- ******************** E. Actualizo Mov. de OP por Ingreso de PT  ***********************
        lv_Step := '7';
        -- Obtengo la secuencia de producto de PT
        SELECT pd.n_secarticulo
          INTO ln_SecProdOP
          FROM PRD_ORDPROD_PROD PD
         WHERE pd.c_compania = lv_CiaVenta
           AND pd.n_secuencia = ln_TmpNroOP
           AND pd.c_producto = lv_CodItem;
        -- Actualizo Paquete de Ordenes de Producción para que actualice el movimiento
        PKGPROD_ORDPROD.P_InsIngresoPT(lv_UsuConeWS,
                                       lv_PasConeWS,
                                       lv_CiaVenta,
                                       ln_TmpNroOP,
                                       lv_CodItem,
                                       ln_SecProdOP,
                                       lv_CodAlma,
                                       lv_TipMovi,
                                       lv_TipDocu,
                                       lv_NroDocu,
                                       '',
                                       '',
                                       0,
                                       S_ERRO_AUX,
                                       S_MENS_AUX);
        -- S_ERRO = p_exito (evaluación al contrario)
        if S_ERRO_AUX = 0 then
          S_MENS := S_MENS ||
                    '. Se produjeron errores al actualizar Acon. Ing PT '||S_MENS_AUX;
          raise ERRO_GENE_DOCU;
        end if;
        --
      END LOOP;
      -- ************************** Liquidación de OP *********************
      lv_Step := '8';
      -- Validación si tiene Consumos Completos de Insumos
      select count(1)
        into ln_Cont
        from prd_ordprod_det d
       where d.c_compania = lv_CiaVenta
         and d.n_secuencia = ln_TmpNroOP
         and d.n_necesitadaumbase - d.n_usadaumbase != 0;
      -- 
      if ln_Cont = 0 then
        -- Validación si tiene ingreso total o parcial de PT
        select count(1)
          into ln_Cont
          from prd_ordprod_prod p
         where p.c_compania = lv_CiaVenta
           and p.n_secuencia = ln_TmpNroOP
           and nvl(p.n_producidaumbase, 0) = 0;
        --
        if ln_Cont = 0 then
          -- Si no está liquidado, lo liquido
          select count(1)
            into ln_Cont
            from prd_ordprod p
           where p.c_compania = lv_CiaVenta
             and p.n_secuencia = ln_TmpNroOP
             and p.c_status = 'CE';
          -- 
          if ln_Cont = 0 then
            -- Liquido de OP si cumple validaciones anteriores
            PKGPROD_ORDPROD.P_CerrarOrdProd(lv_UsuConeWS,
                                            lv_PasConeWS,
                                            lv_CiaVenta,
                                            ln_TmpNroOP,
                                            lv_TmpUsuario,
                                            '1',
                                            S_ERRO_AUX,
                                            S_MENS_AUX);
            --
            if S_ERRO_AUX = 0 then
              S_MENS := S_MENS ||
                        '. Se produjeron errores al liquidar OP '||S_MENS_AUX;
              raise ERRO_GENE_DOCU;
            end if;
            --     
          end if;
        end if;
        --
      end if;
      -- ******************** Actualizo Procesamiento de Interface ***********************
      lv_Step := '9';
      update cuwd_conf_opro_pter_api tm
         set tm.est_regi = 'PR', 
             tm.fec_crea = sysdate
       where tm.numeroop = pn_NroOP
       and tm.sk_orde_prod_pter_api = pv_IdTransaccion;
      --
    ELSE
      S_MENS := 'Error al validar datos de Devolucion: ' || S_MENS;
      RAISE ERRO_GENE_DOCU;
    END IF;
    --
    S_MENS := WHPG_CTE_DINE.MENS_OK;
    --
    COMMIT;
    --
  EXCEPTION
    WHEN ERRO_NOT_FOUND THEN
      S_MENS := 'PCK: WHPG_INTE_CUBI - PRC: WHPR_PROC_CONFI_DEVO - DESCRIPCION: ' ||
                SQLERRM || ' - ' || ' - PASO : ' || lv_Step;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      --
      update cuwd_conf_opro_pter_api tmp
           set tmp.est_regi = 'PR', 
               tmp.fec_crea = sysdate
         where tmp.numeroop = pn_NroOP
         and tmp.sk_orde_prod_pter_api = pv_IdTransaccion;
      --     
    WHEN ERRO_GENE_DOCU THEN
      S_MENS := 'Ocurrio un error al confirmar el ingreso de PT: ' || S_MENS;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      ROLLBACK;
      --
      update cuwd_conf_opro_pter_api tmp
           set tmp.est_regi = 'ER', 
               tmp.fec_crea = sysdate
         where tmp.numeroop = pn_NroOP
           and tmp.sk_orde_prod_pter_api = pv_IdTransaccion;
      --
      COMMIT;     
    WHEN OTHERS THEN
      S_MENS := 'PCK: WHPG_INTE_CUBI - PRC: WHPR_PROC_CONFI_DEVO || ERROR NUMERO: ' ||
                SQLCODE || ' - DESCRIPCION: ' || SQLERRM || ' - PASO : ' ||
                lv_Step;
      S_ERRO := WHPG_CTE_DINE.ERRO;
      ROLLBACK;
      --
      update cuwd_conf_opro_pter_api tmp
           set tmp.est_regi = 'ER', 
               tmp.fec_crea = sysdate
         where tmp.numeroop = pn_NroOP
          and tmp.sk_orde_prod_pter_api = pv_IdTransaccion;
      --     
      COMMIT;
  END WHPR_PROC_CONFI_INGR_PTER;

  /***************************************************************************
     Creado  : Nilton Santos - 01-09-21
     Descrip.: Procesar confirmación de Consumo de Insumos
     Modif.  : 
     Descrip.: 
  ****************************************************************************/
  PROCEDURE WHPR_PROC_CONFI_CONS_INSU(pn_NroOP         IN NUMBER,
                                      pv_IdTransaccion IN VARCHAR2,
                                      S_ERRO           OUT NUMBER,
                                      S_MENS           OUT VARCHAR2) IS
    ERRO_GENE_DOCU EXCEPTION;
    ERRO_NOT_FOUND EXCEPTION;
    S_ERRO_AUX     number;
    S_MENS_AUX     varchar2(4000);
    lv_Step        varchar2(3);
    ln_LongMovAlm  tablas.num1%TYPE;
    ln_Corre       number;
    lv_CiaVenta    varchar2(2);
    --
    lv_UMStock      articulos.um_control_stock%TYPE;
    lv_UMVenta      articulos.um_venta%TYPE;
    lv_NroDocu      mov_inventarios.nro_documento%TYPE;
    ld_FecTran      mov_inventarios.fecha_transaccion%TYPE;
    ld_HorTran      mov_inventarios.hora_transaccion%TYPE;
    lv_CodAlma      mov_inventarios.almacen_venta%TYPE;
    lv_GlosMovi     mov_inventarios.observacion%TYPE;
    ln_CantRecibida mov_inventarios.cantidad%TYPE;
    ln_CantStock    mov_inventarios.cantidad%TYPE;
    lv_NumDocRef1   mov_inventarios.doc_ref_1%TYPE;
    lv_NumDocRef3   mov_inventarios.doc_ref_3%TYPE;
    lv_NroNota      mov_inventarios.nro_nota%TYPE;
    lv_TipDocRef    mov_inventarios.tipo_doc_ref%TYPE;
    lv_CodItem      mov_inventarios.cod_item_2%TYPE;
    lv_TipMovi      mov_inventarios.tipo_movimiento%TYPE;
    lv_TipDocu      mov_inventarios.tipo_documento%TYPE;
    lv_Motivo       mov_inventarios.motivo%TYPE;
    lv_NroGuiaProv  mov_inventarios.c_guia_transp%TYPE;
    lv_NroOrde      mov_inventarios.doc_ref_1%TYPE;
    --
    ln_CodLote det_mov_ubicacion.cod_lote%TYPE;
    ln_CantMov det_mov_ubicacion.cantidad%TYPE;
    lv_CodEsta det_mov_ubicacion.cod_estado%TYPE;
    lv_CodZona det_mov_ubicacion.zona%TYPE;
    lv_CodRack det_mov_ubicacion.rack%TYPE;
    lv_CodNive det_mov_ubicacion.nivel%TYPE;
    lv_CodCasi det_mov_ubicacion.casillero%TYPE;
    lv_CodPale det_mov_ubicacion.pallet%TYPE;
    --
    lv_CodLoteProv    saldo_lote.cod_lote_proveedor%TYPE;
    ld_FecVcto        saldo_lote.fecha_vencimiento%TYPE;
    lv_UMDevo         orden_compra_det.c_Um_Item_Oc%TYPE;
    lv_UMOrig         mov_inventarios.um_item_3%TYPE;
    lv_UMDest         mov_inventarios.um_item_3%TYPE;
    
    -- 1. Variables para la cabecera del documento (recepciones)
    ln_TmpNroOP         cuwd_conf_opro_pter_api.NUMEROOP%type;
    lv_TmpFecEntr       cuwd_conf_opro_pter_api.FECHAENTREGA%TYPE;
    lv_TmpCodAlma       cuwd_conf_opro_pter_api.CODIGOALMACEN%TYPE;
    lv_TmpCodSubAlma    cuwd_conf_opro_pter_api.CODIGOSUBALMACEN%TYPE;
    ln_TmpSecu          cuwd_conf_opro_pter_api.SECUENCIA%TYPE;
    lv_TmpUsuario       cuwd_conf_opro_pter_api.USUARIO%TYPE;
    lv_TmpFecTran       cuwd_conf_opro_pter_api.Fechahoratransaccion%TYPE;
    --
    lv_CodMaqu          prd_ordprod.cod_maqu%TYPE; 
    lv_UsuConeWS        usr_conexion.usuario%TYPE;
    lv_PasConeWS        usr_conexion.clave%TYPE;
    ln_Cont             number;    
    lv_NroInteGuia      guias_header.nro_interno_guia%TYPE;
    lv_CodUsua          guias_header.usuario%TYPE;
    lv_IndBoni          guias_detalle.flag_bonificado%TYPE;
    lv_NueGuia          boolean;
    ln_Factor           guias_detalle.factor%TYPE;
    lv_IngrSali         mov_inventarios.ingreso_salida%TYPE;
    ln_CantAcum         number;
    --
    lb_SigBusc          boolean;
    
    -- 2. Detalle del documento             
    CURSOR c2 IS
      select tm.fechaconsumo,
             tm.codigosubalmacen,
             tm.codigoalmacen,
             tm.secuencia,
             tm.codigoinsumo,
             tm.unidadmedida,
             tm.cantidadconsumida,
             tm.cantidadmerma,
             tm.cantidaddevuelta,
             tm.usuario,
             tm.fechahoratransaccion
        from cuwd_conf_opro_insu_api_v2 tm
       where tm.numeroop = pn_NroOP
         and tm.sk_orde_prod_insu_api = pv_IdTransaccion
         and tm.est_regi = 'IN'; -- Registro pendiente de procesar
    c2_rec c2%ROWTYPE;
    
    -- 3. Consulta para Saldos por Lote para Descarga de Stock
    CURSOR c3 IS
      Select s.cod_lote,
             s.cod_estado,
             s.zona,
             s.rack,
             s.nivel,
             s.casillero,
             s.pallet,
             s.um_stock,
             s.saldo - nvl(s.qty_reservada_od, 0) saldo,
             s.saldo_ums - nvl(s.qty_reservada_ums, 0) saldo_ums,
             l.cod_lote_proveedor,
             l.fecha_vencimiento
        From saldo_ubicacion s, saldo_lote l
       Where s.cod_cia = WHPG_CTE_CUBI.COD_CIA
         and s.compania_venta = lv_CiaVenta
         and s.cod_almacen = lv_TmpCodSubAlma
         and s.cod_item = lv_CodItem
         and s.saldo - nvl(s.qty_reservada_od, 0) > 0
         and s.um_stock = lv_UMStock
         and nvl(s.saldo, 0) > 0
         and s.cod_estado = lv_CodEsta
         and s.zona = lv_CodZona
         and s.cod_cia = l.cod_cia
         and s.compania_venta = l.compania_venta
         and s.cod_almacen = l.cod_almacen
         and s.cod_lote = l.cod_lote
         and s.cod_estado = l.cod_estado
         and s.cod_item = l.cod_item
       Order by l.cod_lote;
    c3_rec c3%ROWTYPE;
  
  BEGIN
    -- *********************************************************************
    -- ********************* CABECERA DE INGRESO *************************
    -- *********************************************************************
    lv_Step     := '1';
    lv_CiaVenta := '01';
    S_ERRO      := WHPG_CTE_CUBI.OK;
    S_MENS      := '';
  
    select distinct tm.numeroop,
                    tm.fechaconsumo,
                    tm.codigoalmacen,
                    tm.codigosubalmacen,
                    tm.secuencia,
                    tm.usuario,
                    tm.fechahoratransaccion
      into ln_TmpNroOP,
           lv_TmpFecEntr,
           lv_TmpCodAlma,
           lv_TmpCodSubAlma,
           ln_TmpSecu,
           lv_TmpUsuario,
           lv_TmpFecTran
      from cuwd_conf_opro_insu_api_v2 tm
     where tm.numeroop = pn_NroOP
       and tm.sk_orde_prod_insu_api = pv_IdTransaccion
       and tm.est_regi = 'IN';
  
    -- Si pasa las validaciones de Devolucion
    IF S_ERRO = WHPG_CTE_CUBI.OK THEN
      lv_Step := '2';
      -- ************************** INGRESO POR DEVOLUCION *******************************
      --ln_NroOD      := -1; -- Consumo Directo no tiene pickings
      lv_TipMovi    := WHPG_CTE_CUBI.MOVI_SU;
      lv_TipDocu    := WHPG_CTE_CUBI.DOCU_GU;
      lv_NumDocRef3 := ln_TmpNroOP;
      lv_NroNota    := 'PRDNEW';
      lv_CodUsua    := 'WS_CUBI';
      lv_IndBoni    := '0';
      lv_IngrSali   := WHPG_CTE_CUBI.MOVI_SU;
      --
      lv_GlosMovi := 'Salida por Consumo de O/P ' || lv_NroOrde ||
                     ' - WS CUBICO - ' ||
                     to_char(sysdate, 'dd/mm/yy HH:Mi AM');
    
      ld_FecTran := TO_DATE(SUBSTR(lv_TmpFecTran, 0, 10),
                            'YYYY-mm-dd');
      ld_HorTran := TO_DATE(lv_TmpFecTran,
                            'YYYY-mm-dd"T"HH24:MI:SS');
      -- Usuario y Clave de WS para usar Package de Ordenes de Producción
      SELECT usuario, clave
        INTO lv_UsuConeWS, lv_PasConeWS
        FROM USR_CONEXION
       WHERE rownum = 1;
      -- Obtengo el Nro. de Maquina de OP y Tipo de Orden
      select p.cod_maqu, p.c_tipo_op
        into lv_CodMaqu, lv_Motivo
        from prd_ordprod p
       where p.c_compania = lv_CiaVenta
         and p.n_secuencia = ln_TmpNroOP;
      --           
      -- Obtiene longitud de número de documento
      select nvl(num1, 6)
        into ln_LongMovAlm
        from tablas
       where categoria = '051'
         and llave = '92';
    
      if ln_LongMovAlm > 8 then
        S_MENS := S_MENS ||
                  'Error de definicion XRAY: La longitud de documento definida en la categoria 051, llave 92 no puede ser mayor a 8';
        raise ERRO_GENE_DOCU;
      end if;
      --    
      S_MENS := 'Consumo de OP : '||ln_TmpNroOP||' Grabada Satisfactoriamente';
    
      -- *********************************************************************
      -- ********************** DETALLE DE CONSUMO ***************************
      -- *********************************************************************
      lv_Step := '3';
      lv_NueGuia  := true;
      --
      FOR c2_rec IN c2 LOOP
        -- Datos
        --ln_SecDetOC    := c2_rec.secuencia;
        lv_CodItem     := c2_rec.codigoinsumo;
        lv_UMDevo      := c2_rec.unidadmedida;
        lv_CodAlma     := c2_rec.codigosubalmacen;
        lv_CodLoteProv := '';
        ld_FecVcto     := '';
      
        -- Obtengo la UM Stock y Venta del articulo
        select a.um_control_stock, a.um_venta
          into lv_UMStock, lv_UMVenta
          from articulos a
         where a.cod_item = lv_CodItem;
      
        -- Conversión de cantidad a UM Stock
        /*IF SUBSTR(lv_UMOrig, 1, 4) = 'CAJA' THEN
          lv_UMOrig := lv_UMVenta;
        END IF;*/
        lv_UMOrig       := c2_rec.unidadmedida;
        lv_UMDest       := lv_UMStock;
        ln_CantRecibida := c2_rec.cantidadconsumida;
      
        P_CONVIERTE_UM(lv_UMOrig, lv_UMDest, ln_CantRecibida);
        ln_CantStock := ln_CantRecibida;
         
        -- ******************** A. Inserta en GUIAS HEADER  ***********************
        lv_Step := '4';
        -- Obtiene Correlativo Salida
        UPDATE TABLAS
           SET num1 = num1 + 1
         WHERE CATEGORIA = '053'
           AND LLAVE = lv_CiaVenta || '1';
        ---- Correlativo para el numero de guia
        SELECT num1
          INTO ln_Corre
          FROM TABLAS
         WHERE CATEGORIA = '053'
           AND LLAVE = lv_CiaVenta || '1';
        -- Se genera el nro. de guia
        lv_NroInteGuia := LPAD(ln_Corre, ln_LongMovAlm, '0');
        lv_NroDocu     := lv_NroInteGuia;
        -- ******************** A.1 INSERCION DE GUIA HEADER ******************
        IF lv_NueGuia THEN
          insert into Guias_Header
            (compania_venta_3,
             nro_interno_guia,
             cod_cia,
             nro_pedido,
             fecha_aprobacion_2,
             fecha_guiado_2,
             nro_impresion,
             status_guia,
             imp_auto_os,
             imprime_os,
             rowversion,
             glosa,
             cod_almacen,
             nro_back_order,
             hold_error_inv,
             urgente,
             flag_liquidada,
             flag_motivo_otros,
             flag_dism_stock,
             tipo_salida,
             sin_dism_stock,
             usuario,
             fecha_transaccion,
             ind_liqu_driv)
          values
            (lv_CiaVenta,
             lv_NroInteGuia,
             WHPG_CTE_CUBI.COD_CIA,
             '0',
             ld_HorTran,
             ld_HorTran,
             '0',
             'T',
             '0',
             '0',
             '0',
             lv_GlosMovi,
             lv_CodAlma,
             '0',
             '0',
             '0',
             '1',
             '1',
             '1',
             lv_TipMovi,
             '0',
             lv_CodUsua,
             sysdate,
             '0');
          if SQL%NOTFOUND then
            S_MENS := 'Se produjeron errores al insertar Guias_Heasder';
            raise ERRO_NOT_FOUND;
          end if;
          lv_NueGuia := false;
        END IF;
        -- ******************* A.2 Inserta GUIAS_DETALLE ********************
        INSERT INTO Guias_Detalle
          (compania_venta_3,
           nro_interno_guia,
           cod_cia,
           cod_item_2,
           cod_almacen,
           nro_guia_3,
           qty_aprobada_2,
           qty_despachada_2,
           qty_pedida_2,
           qty_desp_parcial,
           UM_Item,
           qty_back_order,
           factor,
           flag_bonificado)
        values
          (lv_CiaVenta,
           lv_NroInteGuia,
           WHPG_CTE_CUBI.COD_CIA,
           lv_CodItem,
           lv_CodAlma,
           '',
           ln_CantStock,
           ln_CantStock,
           ln_CantStock,
           ln_CantStock,
           lv_UMDest,
           0,
           ln_Factor,
           lv_IndBoni);
        if SQL%NOTFOUND then
          S_MENS := 'Se produjeron errores al insertar Guias_Detallle';
          raise ERRO_NOT_FOUND;
        end if; -- FIN DE GUIAS
        -- ********************  A. Inserta en MOV_INVENTARIOS ***********************
        lv_Step := '5';
        -- Valido si existe movimiento, si exise actualizo cantidad en mov_inventarios
        select Count(1)
          into ln_Cont
          from mov_inventarios m
         where m.cod_cia = WHPG_CTE_CUBI.COD_CIA
           and m.compania_venta_3 = lv_CiaVenta
           and m.almacen_venta = lv_CodAlma
           and m.tipo_movimiento = lv_TipMovi
           and m.nro_documento = lv_NroDocu
           and m.cod_item_2 = lv_CodItem;
        --  
        if ln_cont = 0 then
          insert into Mov_Inventarios
            (cod_cia,
             compania_venta_3,
             almacen_venta,
             tipo_movimiento,
             tipo_documento,
             nro_documento,
             cod_item_2,
             proveedor,
             almacen_destino,
             cantidad,
             compania_destino,
             costo_unitario,
             doc_ref_1,
             doc_ref_2,
             fecha_transaccion,
             motivo,
             precio_unitario,
             tipo_doc_ref,
             um_item_3,
             nro_nota,
             usuario,
             moneda,
             costo_unitario_me,
             cos_unit_est,
             cos_unit_me_est,
             hora_transaccion,
             f_ordencompra,
             c_sec_oc,
             c_sec_det_oc,
             observacion,
             doc_ref_3,
             ingreso_salida,
             tiempo_garantia,
             reversado,
             cod_pto_atencion,
             fecha_real,
             factor_deal,
             cod_localidad,
             tipo_doc_ref_2,
             n_guia_prov,
             f_guia_prov,
             c_guia_transp)
          values
            (WHPG_CTE_CUBI.COD_CIA,
             lv_CiaVenta,
             lv_CodAlma,
             lv_TipMovi,
             lv_TipDocu,
             lv_NroDocu,
             lv_CodItem,
             '',
             '',
             ln_CantStock,
             '',
             0,
             lv_NumDocRef1,
             '',
             ld_FecTran,
             lv_Motivo,
             0,
             lv_TipDocRef,
             lv_UMStock,
             lv_NroNota,
             lv_CodUsua,
             '',
             0,
             0,
             0,
             ld_HorTran,
             '',
             '', 
             '', -- c_sec_det_oc
             lv_GlosMovi,
             lv_NumDocRef3,
             lv_IngrSali,
             '0',
             '0',
             '',
             sysdate,
             '',
             '',
             '',
             '',
             null,
             lv_NroGuiaProv);
          if SQL%NOTFOUND then
            S_MENS := 'Se produjeron errores al insertar Mov_Inventarios';
            raise ERRO_NOT_FOUND;
          end if;
        else
          -- NSC 28-09-20 Si existe, actualizo la cantidad
          update mov_inventarios m
             set m.cantidad = NVL(m.cantidad, 0) + ln_CantStock
           where m.cod_cia = WHPG_CTE_CUBI.COD_CIA
             and m.compania_venta_3 = lv_CiaVenta
             and m.almacen_venta = lv_CodAlma
             and m.tipo_movimiento = lv_TipMovi
             and m.nro_documento = lv_NroDocu
             and m.cod_item_2 = lv_CodItem;
        end if; -- FIN NSC 28-09-02 Si existe actualizo
        -- ******************** Fin Inserta en MOV_UBICACION ***********************
      
        -- ******************** B. Inserta en DET_MOV_UBICACION ***********************
        ln_CantMov     := ln_CantStock;
        ld_FecVcto     := null; --TO_DATE(c2_rec.fechavencimiento, 'YYYY-mm-dd');
        lv_CodEsta     := WHPG_CTE_CUBI.EST_PR;
        lv_CodZona     := WHPG_CTE_CUBI.UBI_PRODU;
        --
        lv_Step := '5';
        --
        lb_SigBusc     := TRUE;
        ln_CantAcum    := 0;
        --
        FOR c3_rec IN c3 LOOP
          IF (ln_CantAcum + c3_rec.saldo) >= ln_CantStock THEN
            ln_CantMov := ln_CantStock - ln_CantAcum;
            lb_SigBusc := FALSE;
          ELSE
            ln_CantMov := c3_rec.saldo;
          END IF;
          ln_CantAcum := ln_CantAcum + ln_CantMov;
          --
          ln_CodLote := c3_rec.cod_lote;
          lv_CodEsta := c3_rec.cod_estado;
          lv_CodZona := c3_rec.zona;
          lv_CodRack := c3_rec.rack;
          lv_CodNive := c3_rec.nivel;
          lv_CodCasi := c3_rec.casillero;
          lv_CodPale := c3_rec.pallet;
          --lv_OrdIngr := '0'; -- FEFO no tien orden de ingreso
          --
          insert into Det_Mov_Ubicacion
            (cod_cia,
             compania_venta_3,
             almacen_venta,
             tipo_movimiento,
             tipo_documento,
             nro_documento,
             cod_item_2,
             cod_lote,
             cod_estado,
             zona,
             rack,
             nivel,
             casillero,
             pallet,
             cantidad,
             um_mov,
             c_secdetoc,
             cantidad_um_stock)
          values
            (WHPG_CTE_CUBI.COD_CIA,
             lv_CiaVenta,
             lv_CodAlma,
             lv_TipMovi,
             lv_TipDocu,
             lv_NroDocu,
             lv_CodItem,
             ln_CodLote,
             lv_CodEsta,
             lv_CodZona,
             lv_CodRack,
             lv_CodNive,
             lv_CodCasi,
             lv_CodPale,
             ln_CantMov,
             lv_UMStock,
             null,
             ln_CantMov);
          if SQL%NOTFOUND then
            S_MENS := 'Se produjeron errores al insertar Det_Mov_Ubicacion';
            raise ERRO_NOT_FOUND;
          end if;
          -- Si ya cubrio todo el saldo, salgo de cursor
          IF not lb_SigBusc THEN
            EXIT;
          END IF;
        END LOOP;
        -- ******************** Fin Inserta en DET_MOV_UBICACION ***********************
        -- ******************** E. Actualizo Mov. de Consumo de Insumo  ***********************
        lv_Step := '6';
        -- Actualizo Paquete de Ordenes de Producción para que actualice el movimiento
        PKGPROD_ORDPROD.P_InsConsumoComp(lv_UsuConeWS,
                                         lv_PasConeWS,
                                         lv_CiaVenta,
                                         ln_TmpNroOP,
                                         lv_CodAlma,
                                         lv_TipMovi,
                                         lv_TipDocu,
                                         lv_NroDocu,
                                         S_ERRO_AUX,
                                         S_MENS_AUX);
        if S_ERRO_AUX = 0 then
          S_MENS := S_MENS ||
                    '. Se produjeron errores al actualizar Acon. Ing PT '||S_MENS_AUX;
          raise ERRO_GENE_DOCU;
        end if;
        -- ************************** Liquidación de OP *********************
        -- Validación si tiene Consumos Completos de Insumos
        select count(1)
          into ln_Cont
          from prd_ordprod_det d
         where d.c_compania = lv_CiaVenta
           and d.n_secuencia = ln_TmpNroOP
           and d.n_necesitadaumbase - d.n_usadaumbase != 0;
        -- 
        if ln_Cont = 0 then
          -- Validación si tiene ingreso total o parcial de PT
          select count(1)
            into ln_Cont
            from prd_ordprod_prod p
           where p.c_compania = lv_CiaVenta
             and p.n_secuencia = ln_TmpNroOP
             and nvl(p.n_producidaumbase, 0) = 0;
          --
          if ln_Cont = 0 then
            -- Liquido de OP si cumple validaciones anteriores
            PKGPROD_ORDPROD.P_CerrarOrdProd(lv_UsuConeWS,
                                            lv_PasConeWS,
                                            lv_CiaVenta,
                                            ln_TmpNroOP,
                                            lv_CodUsua,
                                            '1',
                                            S_ERRO_AUX,
                                            S_MENS_AUX);
            --
            if S_ERRO_AUX = 0 then
              S_MENS := S_MENS ||
                        '. Se produjeron errores al liquidar OP '||S_MENS_AUX;
              raise ERRO_GENE_DOCU;
            end if;
            --                                
          end if;
          --
        end if;
        --
      END LOOP;
     
      -- ******************** Actualizo Procesamiento de Interface ***********************
      lv_Step := '9';
      update cuwd_conf_opro_insu_api_v2 tm
         set tm.est_regi = 'PR', 
             tm.fec_crea = sysdate
       where tm.numeroop = ln_TmpNroOP
       and   tm.numeroop = ln_TmpNroOP
       and tm.sk_orde_prod_insu_api = pv_IdTransaccion;
      --
    ELSE
      S_MENS := 'Error al validar datos de Devolucion: ' || S_MENS;
      RAISE ERRO_GENE_DOCU;
    END IF;
    --
    S_MENS := WHPG_CTE_CUBI.MENS_OK;
    --
    COMMIT;
    --
  EXCEPTION
    WHEN ERRO_NOT_FOUND THEN
      S_MENS := 'PCK: WHPG_INTE_CUBI - PRC: WHPR_PROC_CONFI_CONS_INSU - DESCRIPCION: ' ||
                SQLERRM || ' - ' || ' - PASO : ' || lv_Step;
      S_ERRO := WHPG_CTE_CUBI.ERRO;
      ROLLBACK;
      --
      update cuwd_conf_opro_insu_api_v2 tmp
           set tmp.est_regi = 'PR', 
               tmp.fec_crea = sysdate
         where tmp.numeroop = pn_NroOP
         and tmp.sk_orde_prod_insu_api = pv_IdTransaccion;
      --     
      COMMIT;
    WHEN ERRO_GENE_DOCU THEN
      S_MENS := 'Ocurrio un error al confirmar el consumo de insumo: ' || S_MENS;
      S_ERRO := WHPG_CTE_CUBI.ERRO;
      ROLLBACK;
      --
      update cuwd_conf_opro_insu_api_v2 tmp
           set tmp.est_regi = 'ER', 
               tmp.fec_crea = sysdate
         where tmp.numeroop = pn_NroOP
           and tmp.sk_orde_prod_insu_api = pv_IdTransaccion;
      --
      COMMIT;     
    WHEN OTHERS THEN
      S_MENS := 'PCK: WHPG_INTE_CUBI - PRC: WHPR_PROC_CONFI_CONS_INSU || ERROR NUMERO: ' ||
                SQLCODE || ' - DESCRIPCION: ' || SQLERRM || ' - PASO : ' ||
                lv_Step;
      S_ERRO := WHPG_CTE_CUBI.ERRO;
      ROLLBACK;
      --
      update cuwd_conf_opro_insu_api_v2 tmp
           set tmp.est_regi = 'ER', 
               tmp.fec_crea = sysdate
         where tmp.numeroop = pn_NroOP
          and tmp.sk_orde_prod_insu_api = pv_IdTransaccion;
      --
      COMMIT;     
  END WHPR_PROC_CONFI_CONS_INSU;

  /***************************************************************************
     Creado  : Nilton Santos - 28-09-2021
     Descrip.: Validación de Confirmación de Despachos
     Modif.  : 
     Descrip.: 
  ****************************************************************************/
  PROCEDURE WHPR_VALI_CONFI_CONS_INSU(pn_NroOP         IN NUMBER,
                                      pv_IdTransaccion IN VARCHAR2,
                                      S_ERRO           OUT NUMBER,
                                      S_MENS           OUT VARCHAR2) IS
  
    ERRO_GENE_DOCU EXCEPTION;
    ERRO_NOT_FOUND EXCEPTION;
    lv_Step        varchar2(3);
    ln_Cont        number;
    lv_NroDocu     mov_inventarios.nro_documento%TYPE;
    ld_FecTran     mov_inventarios.fecha_transaccion%TYPE;
    lv_CodAlma     mov_inventarios.almacen_venta%TYPE;
    lv_CodProv     mov_inventarios.proveedor%TYPE;
    lv_CodItem     mov_inventarios.cod_item_2%TYPE;
    lv_TipMovi     mov_inventarios.tipo_movimiento%TYPE;
    lv_NroOrde     mov_inventarios.doc_ref_1%TYPE;
    ln_CantMov     det_mov_ubicacion.cantidad%TYPE;
    lv_CodEsta     det_mov_ubicacion.cod_estado%TYPE;
    lv_CodLoteProv saldo_lote.cod_lote_proveedor%TYPE;
    ld_FecVcto     saldo_lote.fecha_vencimiento%TYPE;
    lv_ItemProc    whtd_logs_inte_dine.des_item_proc%TYPE;
    lv_NomArch     whtd_conf_rece_dine.nom_arch%TYPE;
    ld_FecArch     whtd_conf_rece_dine.fec_arch%TYPE;
    lv_EstDocu     whtd_conf_rece_dine.est_proc%TYPE;
    lv_DesItem     articulos.desc_item%TYPE;
    lv_UMVenta     mov_inventarios.um_item_3%TYPE;
    lv_UMStock     mov_inventarios.um_item_3%TYPE;
    lv_UMOrig      mov_inventarios.um_item_3%TYPE;
    lv_UMDest      mov_inventarios.um_item_3%TYPE;
    lv_NroInteGuia guias_header.nro_interno_guia%TYPE;
    ln_CantStock   mov_inventarios.cantidad%TYPE;
    ld_HorTran     mov_inventarios.hora_transaccion%TYPE;
    lv_ErroSaldo   varchar2(2);
    ld_FecMov      date;
    -- NSC 15-08-20
    ln_CanConsInsu prd_ordprod_det.n_usadaumbase%TYPE;
    ln_NroOP       prd_ordprod.n_secuencia%TYPE;
    ln_SecGuia     guias_detalle.secuencia%TYPE;
    lv_IndBoni     guias_detalle.flag_bonificado%TYPE;
    -- NSC 20-08-20
    ln_SldUbic det_mov_ubicacion.cantidad%TYPE;
  
    -- 1. Cursor consolidado de documentos
    CURSOR c1 IS
     select distinct tm.numeroop,
                    tm.fechaconsumo,
                    tm.codigoalmacen,
                    tm.codigosubalmacen,
                    tm.secuencia,
                    tm.usuario,
                    tm.fechahoratransaccion
      from cuwd_conf_opro_insu_api_v2 tm
     where tm.numeroop = pn_NroOP
       and tm.sk_orde_prod_insu_api = pv_IdTransaccion
       and tm.est_regi = 'IN';
    c1_rec c1%ROWTYPE;
  
    -- 2. Cursor detallado por documento               
    CURSOR c2 IS
      select tm.fechaconsumo,
             tm.codigosubalmacen,
             tm.codigoalmacen,
             tm.secuencia,
             tm.codigoinsumo,
             tm.unidadmedida,
             tm.cantidadconsumida,
             tm.cantidadmerma,
             tm.cantidaddevuelta,
             tm.usuario,
             tm.fechahoratransaccion
        from cuwd_conf_opro_insu_api_v2 tm
       where tm.numeroop = pn_NroOP
         and tm.sk_orde_prod_insu_api = pv_IdTransaccion
         and tm.est_regi = 'IN';
    c2_rec c2%ROWTYPE;
  
  BEGIN
    lv_Step    := '1';
    lv_EstDocu := '';
    S_ERRO     := WHPG_CTE_DINE.OK;
    --
    FOR c1_rec IN c1 LOOP
      -- Almacen 
      lv_CodAlma := c1_rec.codigosubalmacen;
      ld_FecTran := TRUNC(c1_rec.env_fec_transaccion_aaaammdd);
      -- NSC 20-07-20 Ahora llega con hora de transaccion desde DINET
      ld_HorTran := c1_rec.env_fec_transaccion_aaaammdd;
      -- NSC 11-08-20 Se pregunta por Motivo DINET
      --********************************* MOTIVOS DE DEPACHO *********************
      IF c1_rec.dato2 IN ('VE') THEN
        -- VENTA
        lv_TipMovi := WHPG_CTE_CUBI.MOVI_SM;
        -- Obtienes la Guía Interna de Pedido
        lv_NroInteGuia := DIPG_RUTE_AUTO.DIFU_OBTI_GUIA(lv_NroOrde, 'I');
        lv_CodEsta     := WHPG_CTE_CUBI.EST_BE; -- NSC 20-08-20
      ELSIF c1_rec.dato2 IN ('DG', 'DM') THEN
        -- DEGUSTACIONES y POP, CAMBIO MANO A MANO
        lv_TipMovi := WHPG_CTE_CUBI.MOVI_SC;
        -- Obtienes la Guía Interna de Pedido
        lv_NroInteGuia := DIPG_RUTE_AUTO.DIFU_OBTI_GUIA(lv_NroOrde, 'I');
        lv_CodEsta     := WHPG_CTE_CUBI.EST_BE; -- NSC 20-08-20    
      ELSIF c1_rec.dato2 IN ('DD','DX') THEN -- NSC 23-12-20
        -- ECOMMERCE
        lv_TipMovi     := WHPG_CTE_CUBI.MOVI_SM;
        lv_NroInteGuia := DIPG_RUTE_AUTO.DIFU_OBTI_GUIA(lv_NroOrde, 'I');
        lv_CodEsta     := WHPG_CTE_CUBI.EST_BE; -- NSC 20-08-20 
      ELSIF c1_rec.dato2 IN ('PE') THEN
        -- CONSUMO DE INSUMOS
        lv_TipMovi     := WHPG_CTE_CUBI.MOVI_SU;
        ln_NroOP       := TO_NUMBER(lv_NroOrde);
        lv_NroInteGuia := '';
        lv_CodEsta     := WHPG_CTE_CUBI.EST_PR; -- NSC 20-08-20
      ELSIF c1_rec.dato2 IN ('TR') THEN
        -- TRANSFERENCIA DE DINET HACIA OTROS ALMACENES NSC 15-09-20 
        lv_TipMovi     := WHPG_CTE_CUBI.MOVI_ST;
        lv_NroInteGuia := '';
        lv_CodEsta     := WHPG_CTE_CUBI.EST_BE;
      END IF;
      -- 1. Validación de Almacén
      -- NSC 05-10-20 Se agrega almcén TO
      /*IF pv_cod_alma not in
         (WHPG_CTE_CUBI.COD_ALMA_DINE_HU, WHPG_CTE_CUBI.COD_ALMA_DINE_SB) THEN
        S_MENS := 'El almacén ' || pv_cod_alma ||
                  ' no tiene equivalencias en XRAY';
        lv_EstDocu := 'ER';
        S_ERRO     := WHPG_CTE_CUBI.ERRO;
      END IF;*/
      -- NSC 12-08-20 Validación por tipo de movimiento
      -- NSC 07-10-20 Se agrega SC
      if lv_TipMovi IN (WHPG_CTE_CUBI.MOVI_SM, WHPG_CTE_CUBI.MOVI_SC) then
        -- 2. Validacion de existencia de Pedido
        ln_Cont := WHFU_VALI_PEDI(lv_NroOrde);
        IF ln_Cont = 0 THEN
          S_MENS := 'El número de Pedido ' || lv_NroOrde ||
                    ' no existe o está anulado'; -- NSC 15-10-20
          lv_EstDocu := 'ER';
          S_ERRO     := WHPG_CTE_CUBI.ERRO;
        ELSE
          -- 3. Validación de Existencia de Guia
          lv_NroInteGuia := DIPG_RUTE_AUTO.DIFU_OBTI_GUIA(lv_NroOrde, 'I');
          -- NSC 29-07-20 
          IF NVL(lv_NroInteGuia, '0') = '0' THEN
            S_MENS := 'Guía ' || lv_NroInteGuia || ' no existe';
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
          ELSE
            -- Si existe, obtengo el Almacén de Guia
            select g.cod_almacen
              into lv_CodAlma
              from guias_header g
             where g.compania_venta_3 = '01'
               and g.nro_interno_guia = lv_NroInteGuia;
           /* -- NSC 05-10-20 Se agrega el almacén de TO
            IF lv_CodAlma NOT IN
               (WHPG_CTE_CUBI.COD_ALMA_DINE, WHPG_CTE_CUBI.COD_ALMA_DINE_EC,
                WHPG_CTE_CUBI.COD_ALMA_DINE_TO) THEN
              S_MENS := 'El almacén ' || lv_CodAlma ||
                        ' no está configurado en DINET';
              lv_EstDocu := 'ER';
              S_ERRO     := WHPG_CTE_CUBI.ERRO;
            END IF;*/
            -- NSC 15-08-20 Validación de Guia Atendida
            select Count(1)
              into ln_Cont
              from guias_header g
             where g.compania_venta_3 = '01'
               and g.nro_interno_guia = lv_NroInteGuia
               and g.status_guia = 'T';
            --
            IF ln_Cont > 0 then
              S_MENS := 'El guia interna ' || lv_NroInteGuia ||
                        ' ya fue atendida por DINET';
              lv_EstDocu := 'ER';
              S_ERRO     := WHPG_CTE_CUBI.ERRO;
            END IF;
          
          END IF;
        END IF;
        -- NSC 19-11-20 Validación de Cant.Confirmada no debe ser mayor a Cant. Pedida (SPEDIDO_FECENTREGA)
        /*select sum(nvl(d.qty_pedida_2, 0) -
                   (select nvl(sum(c.env_cant_unmov), 0)
                      from whtd_conf_desp_dine c
                     where c.env_nro_pedido_cliente = lv_NroOrde
                       and c.env_cod_item2 = d.cod_item_2
                       and c.env_sec_item = d.secuencia)) dif
         into ln_Cont              
          from guias_header g, guias_detalle d
         where g.compania_venta_3 = '01'
           and g.nro_interno_guia = lv_NroInteGuia
           and g.status_guia != 'A'
           and g.compania_venta_3 = d.compania_venta_3
           and g.nro_interno_guia = d.nro_interno_guia;
         -- NSC 24-11-20 Se valida solo cuando es menor o cuando DINET confirma más que lo pedido
         IF ln_Cont != 0 then
            S_MENS := 'El pedido '||lv_NroOrde||' confirmado, tiene cantidades atendidas mayores a pedidas, guia interna ' || lv_NroInteGuia;
            WHPR_INSE_LOGS_CONF(lv_NomArch,
                                ld_FecArch,
                                ln_CantStock,
                                lv_ItemProc,
                                lv_TipMovi,
                                ld_FecTran,
                                lv_NroDocu,
                                lv_CodProv,
                                lv_NroOrde,
                                0,
                                S_MENS,
                                S_ERRO,
                                S_MENS);
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
         END IF;*/
        
        
      elsif lv_TipMovi = WHPG_CTE_CUBI.MOVI_SU then
        -- Obtengo el Almacen de OP
        select d.c_almacen
          into lv_CodAlma
          from prd_ordprod_det d
         where d.c_compania = '01'
           and d.n_secuencia = ln_NroOP
           and rownum = 1;
        --
        /*IF lv_CodAlma NOT IN
           (WHPG_CTE_CUBI.COD_ALMA_DINE, WHPG_CTE_CUBI.COD_ALMA_DINE_EC) THEN
          S_MENS := 'El almacén ' || lv_CodAlma ||
                    ' no está configurado en DINET, para la OP ' ||
                    TO_CHAR(ln_NroOP);
          lv_EstDocu := 'ER';
          S_ERRO     := WHPG_CTE_CUBI.ERRO;
        END IF;*/
      
      elsif lv_TipMovi = WHPG_CTE_CUBI.MOVI_ST then
        -- NSC 15-09-20 Validacion de ST
        -- NSC 28-09-20 Validación de Guia de ST
        select Count(1)
          into ln_Cont
          from guias_header g
         where g.compania_venta_3 = '01'
           and g.nro_interno_guia = lv_NroOrde;
        --
        IF ln_Cont = 0 then
          S_MENS := 'El guia interna ' || lv_NroOrde || ' no existe';
          lv_EstDocu := 'ER';
          S_ERRO     := WHPG_CTE_CUBI.ERRO;
        ELSE
          -- Obtengo el Almacen de OP
          select g.almacen_destino
            into lv_CodAlma
            from guias_header g
           where g.compania_venta_3 = '01'
             and g.nro_interno_guia = lv_NroOrde
             and rownum = 1;
          --
         /* IF lv_CodAlma IN
             (WHPG_CTE_CUBI.COD_ALMA_DINE, WHPG_CTE_CUBI.COD_ALMA_DINE_EC) THEN
            S_MENS := 'El almacén ' || lv_CodAlma ||
                      ' no debe ser direccionado para DINET, en la ST ' ||
                      TO_CHAR(lv_NroOrde);
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
          END IF;*/
        END IF;
      end if;
      -- 4. Validacion de Almacen Inactivo
      ln_Cont := WHFU_VALI_ALMA_INAC(pv_cod_cia, lv_CodAlma);
      IF ln_Cont = 0 THEN
        S_MENS := 'El almacén ' || lv_CodAlma ||
                  ' no existe o esta inactivo para la compañia ' ||
                  pv_cod_cia;
        lv_EstDocu := 'ER';
        S_ERRO     := WHPG_CTE_CUBI.ERRO;
      END IF;
      -- 5. Validacion de periodo de valorizaciòn
      ln_Cont := WHFU_VALI_PERI_VALO(pv_cod_cia, ld_FecTran);
      IF ln_Cont = 0 THEN
        S_MENS := 'No existe ó no esta abierto el periódo de valorización para el mes ' ||
                  TO_CHAR(TO_NUMBER(TO_CHAR(ld_FecTran, 'MM')));
        lv_EstDocu := 'ER';
        S_ERRO     := WHPG_CTE_CUBI.ERRO;
      END IF;
         
      -- Si cabecera de documento tiene incidencias, ya no voy hasta el detalle
      IF lv_EstDocu = 'ER' THEN
        -- ******************** Actualizo Procesamiento de Interface ***********************
        update whtd_conf_desp_dine i
           set i.est_proc = lv_EstDocu, i.fec_proc = sysdate
         where i.env_almacen_cliente = pv_cod_alma
           and TRUNC(i.env_fec_transaccion_aaaammdd) = ld_FecTran
           and i.env_nro_pedido_cliente = lv_NroOrde;
        --
        EXIT;
      END IF;
      -- *****************************************************************************
      -- ********************** VALIDACION DE DETALLE ********************************
      -- *****************************************************************************
      lv_Step := '3';
      FOR c2_rec IN c2 LOOP
        -- Datos
        lv_CodItem     := c2_rec.env_cod_item2;
        lv_CodLoteProv := c2_rec.dato1;
        ln_CantMov     := c2_rec.env_cantidad_pedido;
        ln_CantStock   := c2_rec.env_cant_unmov;
        lv_UMOrig      := c2_rec.env_unidad_mov;
        ld_FecVcto     := null;
        ln_SecGuia     := c2_rec.env_sec_item;
        -- Validación de existencia de código
        select count(1)
          into ln_Cont
          from articulos a
         where a.cod_item = lv_CodItem;
      
        --
        if ln_Cont = 0 then
          S_MENS := 'El artículo enviado ' || lv_CodEsta || ' no existe';
          lv_EstDocu := 'ER';
          S_ERRO     := WHPG_CTE_CUBI.ERRO;
        end if;
        -- NSC Validación que exista Saldo en Almacén
        -- NSC 18-09-20 Solo valida saldos para movim. distintos a ST, porque estos se realizan previamente
        if lv_TipMovi != WHPG_CTE_CUBI.MOVI_ST then
        
          -- Obtengo la UM Stock y Venta
          select a.desc_item, a.um_control_stock, a.um_venta
            into lv_DesItem, lv_UMStock, lv_UMVenta
            from articulos a
           where a.cod_item = lv_CodItem;
          -- Conversión de Q a UM Stock 
          IF SUBSTR(lv_UMOrig, 1, 4) = 'CAJA' THEN
            lv_UMOrig := lv_UMVenta;
          END IF;
          lv_UMDest := lv_UMStock;
          -- 
          Select count(1)
            into ln_cont
            from saldos_almacen
           where cod_cia = '00'
             and compania_venta_3 = pv_cod_cia
             and almacen = lv_codalma
             and cod_item = lv_coditem
             and nvl(qty_fisica, 0) + nvl(qty_no_disp_venta, 0) > 0;
          IF ln_Cont = 0 THEN
            S_MENS := 'NO EXISTE STOCK DISPONIBLE PARA ARTICULO: ' ||lv_CodItem||' - '||
                      lv_DesItem || ', U.M. : ' || lv_UMOrig;
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
          END IF;
          -- Validar de Saldos Negativos
          P_VALIDA_SALDO(pv_cod_cia,
                         lv_CodAlma,
                         lv_CodItem,
                         ln_CantStock,
                         ld_FecTran,
                         ld_HorTran,
                         lv_ErroSaldo,
                         ld_FecMov);
          IF lv_ErroSaldo = '1' THEN
            -- SALDO NEGATIVO POR HORA DE MOVIMIENTO
            /*S_MENS := 'NO EXISTE STOCK DISPONIBLE ' || ' PARA ARTICULO : ' ||
            lv_DesItem || ', U.M. : ' || lv_UMVenta;*/
            S_MENS := 'NO EXISTE STOCK DISPONIBLE ' || CHR(10) || CHR(9) ||
                      '  La salida ' || lv_TipMovi || ' - ' ||
                      WHPG_CTE_CUBI.DOCU_GU || ' - ' || lv_NroDocu ||
                      ' que esta realizando con fecha ' ||
                      to_char(ld_HorTran, 'dd/MM/yyyy hh:mm:ss AM') ||
                      ' por ' || TO_CHAR(ln_CantStock) || ' ' || lv_UMStock ||
                      ' está ' ||
                      ' llevando el saldo a negativo para la fecha ' ||
                      to_char(ld_FecMov, 'dd/MM/yyyy hh:mm:ss AM') ||
                      ' PARA ARTICULO : ' || lv_DesItem || ', U.M. : ' ||
                      lv_UMVenta;
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
          END IF;
          -- Valida que no se realiacen movimientos con monto cero
          IF NVL(ln_CantStock, 0) <= 0 THEN
            S_MENS := 'La cantidad no puede ser negativa para el artículo ' ||
                      lv_CodItem || ' nro movimiento ' || lv_NroInteGuia;
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
          END IF;
          -- NSC 20-08-20 Validación de Saldo Ubicación para inserción de DET_MOV_UBICACION
          ln_SldUbic := 0;
          SELECT NVL(SUM(SALDO), 0)
            INTO ln_SldUbic
            FROM SALDO_UBICACION
           WHERE COD_CIA = '00'
             AND COD_ITEM = lv_CodItem
             AND COD_ESTADO = lv_CodEsta
             AND COMPANIA_VENTA = pv_cod_cia
             AND COD_ALMACEN = lv_CodAlma
             AND UM_STOCK = lv_UMStock;
          --
          IF ln_SldUbic < ln_CantStock THEN
            S_MENS := 'Insuficiente stock en saldo por ubicacion para articulo: ' ||
                      lv_CodItem || ' - ' || lv_DesItem || ', UM: ' ||
                      lv_UMStock || ', Estado : ' || lv_CodEsta;
            
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
          END IF;
        
        end if; -- FIN NSC 18-09-20 No valida stock para ST porque esta se ha realizado previamente
      
        -- NSC 15-08-20 Validación de Despacho de Guia de Venta
        -- NSC 07-10-20 Se agrega SC
        if lv_TipMovi IN (WHPG_CTE_CUBI.MOVI_SM, WHPG_CTE_CUBI.MOVI_SC) then
          -- Validación de Estado
          IF lv_CodEsta != WHPG_CTE_CUBI.EST_BE THEN
            S_MENS := 'El estado enviado ' || lv_CodEsta ||
                      ' es diferente a BE';
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
          END IF;
          --
          select d.flag_bonificado
            into lv_IndBoni
            from guias_detalle d
           where d.compania_venta_3 = '01'
             and d.nro_interno_guia = lv_NroInteGuia
             and d.secuencia = ln_SecGuia;
          --
          select Count(1)
            into ln_Cont
            from guias_header g, guias_detalle d
           where g.compania_venta_3 = '01'
             and g.nro_interno_guia = lv_NroInteGuia
             and g.compania_venta_3 = d.compania_venta_3
             and g.nro_interno_guia = d.nro_interno_guia
             and d.flag_bonificado = lv_IndBoni
             and d.cod_item_2 = lv_CodItem
             and g.status_guia != 'A'
             and nvl(d.qty_picking, 0) > 0;
          --
          if ln_Cont > 0 then
            S_MENS := 'El artículo ' || lv_CodItem ||
                      ' ya tiene picking en guia ' || lv_NroInteGuia;
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
          end if;
          --
        end if;
        -- NSC 15-08-20 Validación de Consumo de OP
        if lv_TipMovi = WHPG_CTE_CUBI.MOVI_SU then
          select nvl(d.n_usadaumbase, 0)
            into ln_CanConsInsu
            from prd_ordprod_det d
           where d.c_compania = '01'
             and d.n_secuencia = ln_NroOP
             and d.c_articulo = lv_CodItem;
          if ln_CanConsInsu >= ln_CantStock then
            S_MENS := 'La cantidad a despachar del artículo ' || lv_CodItem ||
                      ' es mayor a la consumida en la OP ' ||
                      to_char(ln_NroOP);
            lv_EstDocu := 'ER';
            S_ERRO     := WHPG_CTE_CUBI.ERRO;
          end if;
        end if;
        -- ******************** Actualizo Procesamiento de Interface ***********************
        IF lv_EstDocu = 'ER' THEN
          update whtd_conf_desp_dine i
             set i.est_proc = lv_EstDocu, i.fec_proc = sysdate
           where i.env_almacen_cliente = pv_cod_alma
             and TRUNC(i.env_fec_transaccion_aaaammdd) = ld_FecTran
             and i.env_nro_pedido_cliente = lv_NroOrde;
        END IF;
        --
      END LOOP;
      --
    END LOOP;
    --
  EXCEPTION
    WHEN ERRO_NOT_FOUND THEN
      S_MENS := 'PCK: WHPG_INTE_DINE - PRC: WHPR_VALI_CONFI_CONS_INSU - DESCRIPCION: ' ||
                SQLERRM || ' - ' || ' - PASO : ' || lv_Step;
      S_ERRO := WHPG_CTE_CUBI.ERRO;
    WHEN ERRO_GENE_DOCU THEN
      S_MENS := S_MENS;
      S_ERRO := WHPG_CTE_CUBI.ERRO;
      --ROLLBACK;
    WHEN OTHERS THEN
      S_MENS := 'PCK: WHPG_INTE_DINE - PRC: WHPR_VALI_CONFI_CONS_INSU || ERROR NUMERO: ' ||
                SQLCODE || ' - DESCRIPCION: ' || SQLERRM || ' - PASO : ' ||
                lv_Step;
      S_ERRO := WHPG_CTE_CUBI.ERRO;
      ROLLBACK;
  END WHPR_VALI_CONFI_CONS_INSU;
  
  /***************************************************************************
   Creado  : Nilton Santos - 21-09-2021
   Descrip.: Validando de Documentos: OC, TR y OP
   Modif.  : 
   Descrip.: 
  ****************************************************************************/
  FUNCTION WHFU_VALI_PEDI(pv_nro_docu varchar2) RETURN NUMBER IS
  
    lv_NroInteGuia guias_header.nro_interno_guia%TYPE;
    ln_Val         NUMBER(1);
    ln_Cont        number;
  
  BEGIN
    --
    ln_Val := 1;
    --
    lv_NroInteGuia := DIPG_RUTE_AUTO.DIFU_OBTI_GUIA(pv_nro_docu, 'I');
    --
    IF NVL(lv_NroInteGuia, '0') = '0' THEN
      ln_Val := 0;
    END IF;
    -- Valido Pedido
    if ln_Val > 0 then
      select Count(1)
        into ln_Cont
        from guias_header g, spedido_header s
       where g.compania_venta_3 = '01'
         and g.nro_interno_guia = lv_NroInteGuia
         and g.nro_pedido = s.nro_pedido
         and s.status_pedido != 'A'; -- NSC 15-10-20
      --
      if ln_Cont = 0 then
        ln_Val := 0;
      end if;
    end if;
    --
    RETURN ln_Val;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 0;
  END WHFU_VALI_PEDI;
  /***************************************************************************
   Creado  : Nilton Santos - 21-09-2021
   Descrip.: Validando Almacen Inactivo
   Modif.  :
   Descrip.:
  ****************************************************************************/
  FUNCTION WHFU_VALI_ALMA_INAC(pv_cod_cia varchar2, pv_cod_alm varchar2) RETURN NUMBER IS
    ln_Val NUMBER(1);
  BEGIN
    --
    SELECT count(1)
      into ln_Val
      FROM TABLAS
     WHERE CATEGORIA = '038'
       AND LLAVE = pv_cod_cia || pv_cod_alm;
    --
    IF ln_Val > 0 THEN
      SELECT count(1)
        into ln_Val
        FROM TABLAS
       WHERE CATEGORIA = '038'
         AND LLAVE = pv_cod_cia || pv_cod_alm
         AND nvl(FLAG3, '0') = '1';
      IF ln_Val > 0 THEN
        ln_Val := 0;
      ELSE
        ln_Val := 1;
      END IF;
    END IF;
    --
    RETURN ln_Val;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 0;
  END WHFU_VALI_ALMA_INAC;
  

end WHPG_INTE_CUBI;
/

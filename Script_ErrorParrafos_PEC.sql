DECLARE      
--prueba     

--PROCEDURE SP_IMP_CONSULTAR_PARRAFOS (
      ivaPoliza      VARCHAR2(20) := '090000929119';
      ivaRamo        VARCHAR2(5) := '090';
      ivaSubRamo     VARCHAR2(5) := '055';
      
      OtaParrafos        Pck_Parrafos.gtabimpparrafo;
      ovaMensajeTecnico  VARCHAR2(500);
      ovaMensajeUsuario  VARCHAR2(500);
      ivaRecibo          VARCHAR2(500) := '11724161';
--      ) AS

  --JOSEBOMO 20110425 MVEE FASE II
  lvaAfibenestcodA TABBEN.AFIBENESTCOD%TYPE := 'A';
  lvaAfibenestcodC TABBEN.AFIBENESTCOD%TYPE := 'C';
  lvaAfiexcestcodA TABEXC.AFIEXCESTCOD%TYPE := 'A';
  lnuCrtcon TABCRT.CRTCON%TYPE;
  ldasysdate DATE := SYSDATE;
  lvaOperacion TABREC.RECOPECOD%TYPE;

  ldaFechaDefecto DATE := TO_DATE('1900/01/01','YYYY/MM/DD'); --JOSEBOMO 20140822 Barrido observaciones

  CURSOR POLIZA(pvaTipcarcod TABCRT.TIPCARCOD%TYPE, pvaProducto TABCRT.PROCOD%TYPE) IS --elizpego
       SELECT *
       FROM TABCRT
       WHERE CRTNUMCON = LTRIM(SUBSTR(ivaPoliza,4),'0')
       AND   TIPCARCOD = pvaTipcarcod
       AND  PROCOD = pvaProducto;--elizpego
       
  CURSOR lCursorParrafos(pCodtabcsg TABOBP.CODTABCSG%TYPE, pProcod TABOBP.PROCOD%TYPE, pAfiobpcod TABOBP.AFIOBPCOD%TYPE,pnuPafpk TABCRT.PAFPK%TYPE) IS
       SELECT *
     FROM   TABOBP
     WHERE  CODTABCSG = pCodtabcsg
     AND    PAFPK = pnuPafpk
     AND    PROCOD = pProcod
     AND    AFIOBPCOD = pAfiobpcod
     AND    FEBAJA = ldaFechaDefecto; --JOSEBOMO 20140822 Barrido observaciones

  CURSOR curAsegurados(pnuCrtcon TABCRT.CRTCON%TYPE) IS
     SELECT TEVA.DSNOMBRE_EXCLUSION, TEVA.DSALCANCE_EXCLUSION, EXC.EVARIECOD
     ,Pck_Gwb_Gestion_Personas.FN_CONSULTA_DATO_PERSONA(BEN.NATIDE , 'C' ,'CDTIPO_IDENTIFICACION' ,'VARCHAR2') CDTIPO_IDENTIFICACION
     ,Pck_Gwb_Gestion_Personas.FN_CONSULTA_DATO_PERSONA(BEN.NATIDE , 'C' ,'NMIDENTIFICACION' ,'VARCHAR2') NMIDENTIFICACION
     FROM TABBEN BEN, TABEXC EXC, TEVA_EXCLUSIONES_SALUDMP TEVA
     WHERE BEN.CRTCON = pnuCrtcon
     AND BEN.AFIBENESTCOD IN(lvaAfibenestcodA, lvaAfibenestcodC)
     AND EXC.CRTCON = BEN.CRTCON
     AND EXC.SBCCON = BEN.SBCCON
     AND EXC.BENCON = BEN.BENCON
     AND EXC.AFIEXCESTCOD = lvaAfiexcestcodA
     AND EXC.EXCFINVIG >= ldasysdate --YOHAORCA, 20120917 solucion a bug 49364
     AND TEVA.CDCODIGO_EXCLUSION = EXC.EVARIECOD;

  CURSOR curCampanias(pnuCrtcon TABCRT.CRTCON%TYPE,
                      pramo VARCHAR2,
                      psubramo VARCHAR2,
                      poperacion TABREC.RECOPECOD%TYPE) IS

    SELECT  CAM.CDCAMPANIA,CAM.DSCAMPANIA
    FROM    TGWB_CAMPANIAS CAM,TGWB_CAMPANIAS_CONTRATOS CON
    WHERE   CAM.CDCAMPANIA = CON.CDCAMPANIA
    AND     CON.NMCONSECUTIVO_CONTRATO = pnuCrtcon
    AND     CAM.CDRAMO = pramo
    AND     CAM.CDSUBRAMO = psubramo
    AND     INSTR(CAM.CDOPERACION,poperacion) > 0
    AND     ldasysdate BETWEEN CON.FEINICIOVIGENCIA AND CON.FEFINVIGENCIA;


  lvaNombreObjeto             VARCHAR2(30):='SP_IMP_CONSULTAR_PARRAFOS';
  lnuCodTabCsg                TABOBP.CODTABCSG%TYPE := 1;
  lvaProCod                   TABOBP.PROCOD%TYPE; --elizpego
  lvaAfiobpcod                TABOBP.AFIOBPCOD%TYPE := '3';
  lnuCont                     NUMBER(10);
  lvaTipcarcod                TABCRT.TIPCARCOD%TYPE;
  lnuPafpk                    TABCRT.PAFPK%TYPE;
  lvaMensajeTecnico           VARCHAR2(255):=NULL;
  lvaMensajeUsuario           VARCHAR2(255):=NULL;
  lexErrorProcedimiento       EXCEPTION;
  lvaLineas                   Pck_Utilitarios_Clob.gtabDatos;
  lvaParrafosOrdenados        VARCHAR2(500);
BEGIN
   BEGIN
      lvaTipcarcod := Pck_Gestion_Salud.FN_GET_TIPOCARATULA(ivaSubramo,ivaRamo,lvaProcod);
      IF TRIM(lvaTipcarcod) IS NULL THEN
         lvaMensajeTecnico :='No se pudo recuperar tipo de contrato '||ivaRamo||' '||ivaSubramo;
         lvaMensajeUsuario := 'No se pudo recuperar tipo de contrato';
         RAISE lexErrorProcedimiento;
      END IF;
   EXCEPTION WHEN OTHERS THEN
     lvaMensajeTecnico := 'No se pudo recuperar tipo de contrato '||ivaRamo||' '||ivaSubramo||' '||SQLERRM;
     lvaMensajeUsuario := 'No se pudo recuperar tipo de contrato';
     RAISE lexErrorProcedimiento;
   END;
   lvaProCod := Pck_Gestion_Salud.FN_GET_PRODUCTO(ivaRamo,ivaSubRamo); --ELIZPEGO
   lnuCont := 0;
   FOR lcurpol IN POLIZA(lvaTipcarcod,lvaProCod) LOOP
     lnuPafpk  := lcurpol.PAFPK;
     lnuCont   := lnuCont + 1;
     lnuCrtcon := lcurpol.CRTCON; --JOSEBOMO 20110425 MVEE FASE II
   END LOOP;
   IF lnuCont = 0 THEN
       lvaMensajeTecnico :='La póliza ' || ivaPoliza || 'NO existe en TABCRT';
       lvaMensajeUsuario := 'La póliza ' || ivaPoliza || 'NO existe';
       RAISE lexErrorProcedimiento;
   END IF;
   lnuCont := 0;
     FOR curparraf IN lCursorParrafos(lnuCodTabCsg, lvaProCod, lvaAfiobpcod,lnuPafpk) LOOP
       lvaParrafosOrdenados := Pck_Gestion_Salud.FN_ORDENAR_PARRAFO(curparraf.obptex);
       ops$procedim.Pck_Imp_Llevar_Vindi.SP_PARTIR_LINEA(lvaParrafosOrdenados,80,Pck_Gestion_Salud.gtaLineas, lvaMensajeTecnico, lvaMensajeUsuario);
        FOR lnucontador IN 1.. Pck_Gestion_Salud.gtaLineas.COUNT LOOP
           lnuCont := lnuCont + 1;
           OtaParrafos(lnuCont).cdparrafo := curparraf.AFIOBPCOD;
           --En salud las observaciones están VARCHAR2(500)
           OtaParrafos(lnuCont).dstexto   := Pck_Gestion_Salud.gtaLineas(lnuContador);
       END LOOP;
   END LOOP;
   --JOSEBOMO 20110425 MVEE FASE II
   IF lvaTipcarcod = 'F' THEN
      FOR lcurExc IN curAsegurados(lnuCrtcon) LOOP
         lvaParrafosOrdenados := Pck_Gestion_Salud.FN_ORDENAR_PARRAFO(SUBSTR('El asegurado con identificación '|| lcurExc.Cdtipo_Identificacion||lcurExc.Nmidentificacion|| ' presenta Anexo de Exclusión: ' ||lcurExc.dsnombre_exclusion ,1,500));
         Pck_Gestion_Salud.gtaLineas := lvaLineas;
         ops$procedim.Pck_Imp_Llevar_Vindi.SP_PARTIR_LINEA(lvaParrafosOrdenados,80,Pck_Gestion_Salud.gtaLineas, lvaMensajeTecnico, lvaMensajeUsuario);
         FOR lnucontador IN 1.. Pck_Gestion_Salud.gtaLineas.COUNT LOOP
           lnuCont := lnuCont + 1;
           OtaParrafos(lnuCont).cdparrafo := lcurExc.EVARIECOD;
           OtaParrafos(lnuCont).dstexto   := Pck_Gestion_Salud.gtaLineas(lnuContador);
         END LOOP;

         lvaParrafosOrdenados := Pck_Gestion_Salud.FN_ORDENAR_PARRAFO(SUBSTR('Alcance de la exclusión: '|| lcurExc.dsalcance_exclusion,1,500));
         Pck_Gestion_Salud.gtaLineas := lvaLineas;
         ops$procedim.Pck_Imp_Llevar_Vindi.SP_PARTIR_LINEA(lvaParrafosOrdenados,80,Pck_Gestion_Salud.gtaLineas, lvaMensajeTecnico, lvaMensajeUsuario);
         FOR lnucontador IN 1.. Pck_Gestion_Salud.gtaLineas.COUNT LOOP
           lnuCont := lnuCont + 1;
           OtaParrafos(lnuCont).cdparrafo := lcurExc.EVARIECOD;
           OtaParrafos(lnuCont).dstexto   := Pck_Gestion_Salud.gtaLineas(lnuContador);
         END LOOP;
      END LOOP;
   END IF;
   --JOSEBOMO 20110425

   --JESUSCAAL 20141104
   IF (ivaRecibo IS NOT NULL) THEN
     BEGIN
       SELECT T.RECOPECOD
       INTO lvaOperacion
       FROM TABREC T
       WHERE TIPCARCOD = lvaTipCarCod
         AND RECNUM = ivaRecibo
         AND PROCOD = lvaProcod;
     EXCEPTION
     WHEN OTHERS THEN
       NULL;
     END;
   END IF;
   FOR regCampanias IN curCampanias(lnuCrtcon,ivaRamo,ivaSubRamo,lvaOperacion) LOOP
       lvaParrafosOrdenados := regCampanias.DSCAMPANIA;
       Pck_Gestion_Salud.gtaLineas := lvaLineas;
       ops$procedim.Pck_Imp_Llevar_Vindi.SP_PARTIR_LINEA(lvaParrafosOrdenados,80,Pck_Gestion_Salud.gtaLineas, lvaMensajeTecnico, lvaMensajeUsuario);
        FOR lnucontador IN 1.. Pck_Gestion_Salud.gtaLineas.COUNT LOOP
           lnuCont := lnuCont + 1;
           OtaParrafos(lnuCont).cdparrafo := regCampanias.CDCAMPANIA;
           OtaParrafos(lnuCont).dstexto   := Pck_Gestion_Salud.gtaLineas(lnuContador);
       END LOOP;
   END LOOP;
   --/JESUCAAL 20141104

   Pck_Gestion_Salud.gtaLineas := lvaLineas;
   DBMS_OUTPUT.PUT_LINE(SUBSTR('TERMINO ...',1,500));
  EXCEPTION
  WHEN lexErrorProcedimiento THEN
    ovaMensajeTecnico:= SUBSTR(lvaNombreObjeto||':' ||lvaMensajeTecnico,1,255);
    ovaMensajeUsuario:= SUBSTR(lvaNombreObjeto||':' ||lvaMensajeUsuario,1,255);
    DBMS_OUTPUT.PUT_LINE(SUBSTR('lexErrorProcedimiento: '||ovaMensajeTecnico||' - '||ovaMensajeUsuario,1,500));
    WHEN OTHERS THEN
     ovaMensajeTecnico:= SUBSTR(lvaNombreObjeto ||':' || LPAD(TO_CHAR(ABS(SQLCODE)),7,'0' )||':'||lvaMensajeTecnico,1,255);
     ovaMensajeUsuario:= 'TRANSACCION NO DISPONIBLE ' ;
     DBMS_OUTPUT.PUT_LINE(SUBSTR('OTHERS: '||ovaMensajeTecnico||' - '||ovaMensajeUsuario,1,500));
END SP_IMP_CONSULTAR_PARRAFOS;
/

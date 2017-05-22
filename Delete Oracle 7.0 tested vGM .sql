 --Tested locally in MS SQL CC 6.5 MR 2 Branch.
--Extracted/Modified from the delendpoint sproc. Org ID hardcoded to 1
--9/21/2016. GMarmat. Do not send to customer. Use at your own risk.
--Replace all EID with your comma seperated list of endPointId
--Replace all MID with your comma seperated list of meteriId
--Use this script to get EID and MID when you know the meterno:
select e.endpointid, m.meterid, m.meterno from endpoints e inner join meters m on e.meterid=m.meterid
where m.meterno in ('MeterNo1','Meterno2');


DECLARE
v_temp NUMBER(1, 0) := 0;
BEGIN
   --Delete the AuditTransaction tables 
      DELETE AuditMCODetails 
      WHERE oldEndPointId in (EID); 
 
      DELETE AuditEndPointGroups 
      WHERE ROWID IN 
      ( SELECT  AEG.rowid 
--        JOIN AuditLog AL 
  --       ON AL.auditLogId = AEG.auditLogId.ROWID 
        FROM AuditEndPointGroups AEG 
               JOIN AuditLog AL 
                ON AL.auditLogId = AEG.auditLogId 
 
          WHERE AL.endPointId in (EID)); 
 
      DELETE AuditMomentaryIntLog 
      WHERE ROWID IN 
      ( SELECT  AML.rowid 
       -- JOIN AuditLog AL 
         --ON AL.auditLogId = AML.auditLogId.ROWID 
        FROM AuditMomentaryIntLog AML 
               JOIN AuditLog AL 
                ON AL.auditLogId = AML.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditEndPointProperties 
      WHERE ROWID IN 
      ( SELECT  AEP.rowid 
--        JOIN AuditLog AL 
  --       ON AL.auditLogId = AEP.auditLogId.ROWID 
        FROM AuditEndPointProperties AEP 
               JOIN AuditLog AL 
                ON AL.auditLogId = AEP.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditAMREndPointProperties 
      WHERE ROWID IN 
      ( SELECT  AMR.rowid 
--        JOIN AuditLog AL 
  --       ON AL.auditLogId = AMR.auditLogId.ROWID 
        FROM AuditAMREndPointProperties AMR 
               JOIN AuditLog AL 
                ON AL.auditLogId = AMR.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditTS1AMREndPointProperties 
      WHERE ROWID IN 
      ( SELECT  AMR.rowid 
       -- JOIN AuditLog AL 
         --ON AL.auditLogId = AMR.auditLogId.ROWID 
        FROM AuditTS1AMREndPointProperties AMR 
               JOIN AuditLog AL 
                ON AL.auditLogId = AMR.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
      DELETE AuditTS2AMREndPointProperties 
      WHERE ROWID IN 
      ( SELECT  AMR.rowid 
        --JOIN AuditLog AL 
         --ON AL.auditLogId = AMR.auditLogId.ROWID 
        FROM AuditTS2AMREndPointProperties AMR 
               JOIN AuditLog AL 
                ON AL.auditLogId = AMR.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditIconEndPointProperties 
      WHERE ROWID IN 
      ( SELECT  AIP.rowid 
        --JOIN AuditLog AL 
         --ON AL.auditLogId = AIP.auditLogId.ROWID 
        FROM AuditIconEndPointProperties AIP 
               JOIN AuditLog AL 
                ON AL.auditLogId = AIP.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AudTS1EOLVMEdptProp 
      WHERE ROWID IN 
      ( SELECT  AMR.rowid 
        --JOIN AuditLog AL 
         --ON AL.auditLogId = AMR.auditLogId.ROWID 
        FROM AudTS1EOLVMEdptProp AMR 
               JOIN AuditLog AL 
                ON AL.auditLogId = AMR.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditLCEndPointProperties 
      WHERE ROWID IN 
      ( SELECT ALC.rowid 
       -- JOIN AuditLog AL 
        -- ON AL.auditLogId = ALC.auditLogId.ROWID 
        FROM AuditLCEndPointProperties ALC 
               JOIN AuditLog AL 
                ON AL.auditLogId = ALC.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditRSSEndPointProperties 
      WHERE ROWID IN 
      ( SELECT  AREP.rowid 
--        JOIN AuditLog AL 
  --       ON AL.auditLogId = AREP.auditLogId.ROWID 
        FROM AuditRSSEndPointProperties AREP 
               JOIN AuditLog AL 
                ON AL.auditLogId = AREP.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditMeterProperties 
      WHERE ROWID IN 
      ( SELECT  AMP.rowid 
        --JOIN AuditLog AL 
         --ON AL.auditLogId = AMP.auditLogId.ROWID 
        FROM AuditMeterProperties AMP 
               JOIN AuditLog AL 
                ON AL.auditLogId = AMP.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditMCODetails 
      WHERE ROWID IN 
      ( SELECT  AMCO.rowid 
       -- JOIN AuditLog AL 
         --ON AL.auditLogId = AMCO.auditLogId.ROWID 
        FROM AuditMCODetails AMCO 
               JOIN AuditLog AL 
                ON AL.auditLogId = AMCO.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditKV2EndPointProperties 
      WHERE ROWID IN 
      ( SELECT  AK2.rowid 
        --JOIN AuditLog AL 
         --ON AL.auditLogId = AK2.auditLogId.ROWID 
        FROM AuditKV2EndPointProperties AK2 
               JOIN AuditLog AL 
                ON AL.auditLogId = AK2.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditS4EndPointProperties 
      WHERE ROWID IN 
      ( SELECT  AS4.rowid 
       -- JOIN AuditLog AL 
        -- ON AL.auditLogId = AS4.auditLogId.ROWID 
        FROM AuditS4EndPointProperties AS4 
               JOIN AuditLog AL 
                ON AL.auditLogId = AS4.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditKV2Readings 
      WHERE ROWID IN 
      ( SELECT  AK2.rowid 
        --JOIN AuditLog AL 
         --ON AL.auditLogId = AK2.auditLogId.ROWID 
        FROM AuditKV2Readings AK2 
               JOIN AuditLog AL 
                ON AL.auditLogId = AK2.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditKV2KReadings 
      WHERE ROWID IN 
      ( SELECT  AK2.rowid 
      --  JOIN AuditLog AL 
        -- ON AL.auditLogId = AK2.auditLogId.ROWID 
        FROM AuditKV2KReadings AK2 
               JOIN AuditLog AL 
                ON AL.auditLogId = AK2.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditKV2TReadings 
      WHERE ROWID IN 
      ( SELECT  AK2.rowid 
        --JOIN AuditLog AL 
        -- ON AL.auditLogId = AK2.auditLogId.ROWID 
        FROM AuditKV2TReadings AK2 
               JOIN AuditLog AL 
                ON AL.auditLogId = AK2.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditKV2KTReadings 
      WHERE ROWID IN 
      ( SELECT  AK2.rowid 
       -- JOIN AuditLog AL 
         --ON AL.auditLogId = AK2.auditLogId.ROWID 
        FROM AuditKV2KTReadings AK2 
               JOIN AuditLog AL 
                ON AL.auditLogId = AK2.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditKV2DiagnosticReadings 
      WHERE ROWID IN 
      ( SELECT  AK2.rowid 
     --   JOIN AuditLog AL 
       --  ON AL.auditLogId = AK2.auditLogId.ROWID 
        FROM AuditKV2DiagnosticReadings AK2 
               JOIN AuditLog AL 
                ON AL.auditLogId = AK2.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditTS2AMRDemandReadings 
      WHERE ROWID IN 
      ( SELECT  AME.rowid 
        --JOIN AuditLog AL 
         --ON AL.auditLogId = AME.auditLogId.ROWID 
        FROM AuditTS2AMRDemandReadings AME 
               JOIN AuditLog AL 
                ON AL.auditLogId = AME.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditMUEndPointProperties 
      WHERE ROWID IN 
      ( SELECT  AME.rowid 
       -- JOIN AuditLog AL 
        -- ON AL.auditLogId = AME.auditLogId.ROWID 
        FROM AuditMUEndPointProperties AME 
               JOIN AuditLog AL 
                ON AL.auditLogId = AME.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditTS2VoltageReadings 
      WHERE ROWID IN 
      ( SELECT  AME.rowid 
       -- JOIN AuditLog AL 
        -- ON AL.auditLogId = AME.auditLogId.ROWID 
        FROM AuditTS2VoltageReadings AME 
               JOIN AuditLog AL 
                ON AL.auditLogId = AME.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditS4KMReadings 
      WHERE ROWID IN 
      ( SELECT  AME.rowid 
       -- JOIN AuditLog AL 
        -- ON AL.auditLogId = AME.auditLogId.ROWID 
        FROM AuditS4KMReadings AME 
               JOIN AuditLog AL 
                ON AL.auditLogId = AME.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 
 
      DELETE AuditIdReadings 
      WHERE ROWID IN 
      ( SELECT  AIR.rowid 
       -- JOIN AuditLog AL 
        -- ON AL.auditLogId = AME.auditLogId.ROWID 
        FROM AuditIdReadings AIR 
               JOIN AuditLog AL 
                ON AL.auditLogId = AIR.auditLogId 
 
          WHERE AL.endPointId in (EID) ); 

      DELETE AuditLog 
      WHERE endPointId in (EID); 
 
      DELETE EndpointLog 
      WHERE ROWID IN 
      ( SELECT a.rowid 
       -- JOIN Readings a 
         --ON Readings.readingId = EndPointLog.readingId.ROWID 
        FROM EndPointLog a 
               JOIN Readings 
                ON Readings.readingId = a.readingId 
 
          WHERE Readings.endPointId in (EID) ); 
 
      DELETE TS1PriorEndPointLatestValues 
 
        WHERE endPointId in (EID); 
 
      DELETE TS1EndPointLatestValues 
 
        WHERE endPointId in (EID); 
 
      DELETE MUDiagnosticReadings 
      WHERE ROWID IN 
      ( SELECT  MDR.rowid 
       -- JOIN Readings R 
         --ON R.readingId = MDR.readingId.ROWID 
        FROM MUDiagnosticReadings MDR 
               JOIN Readings R 
                ON R.readingId = MDR.readingId 
 
          WHERE R.endPointId in (EID) 
                  AND MDR.readingsTableId = 2 ); 
 
      DELETE MUDiagnosticReadings 
      WHERE ROWID IN 
      ( SELECT  MDR.rowid 
       -- JOIN KV2Readings R 
        -- ON R.kv2ReadingId = MDR.readingId.ROWID 
        FROM MUDiagnosticReadings MDR 
               JOIN KV2Readings R 
                ON R.kv2ReadingId = MDR.readingId 
 
          WHERE R.endPointId in (EID) 
                  AND MDR.readingsTableId = 3 ); 
 
      DELETE MUReadings 
      WHERE ROWID IN 
      ( SELECT  MR.rowid 
        --JOIN Readings R 
         --ON R.readingId = MR.readingId.ROWID 
        FROM MUReadings MR 
               JOIN Readings R 
                ON R.readingId = MR.readingId 
 
          WHERE R.endPointId in (EID) 
                  AND MR.readingsTableId = 2 ); 
 
      DELETE MUReadings 
      WHERE ROWID IN 
      ( SELECT  MR.rowid 
        --JOIN KV2Readings R 
         --ON R.kv2ReadingId = MR.readingId.ROWID 
        FROM MUReadings MR 
               JOIN KV2Readings R 
                ON R.kv2ReadingId = MR.readingId 
 
          WHERE R.endPointId in (EID) 
                  AND MR.readingsTableId = 3 ); 
 
      DELETE TS2DiagReadings 
      WHERE ROWID IN 
      ( SELECT  TSR.rowid 
       -- JOIN Readings R 
        -- ON R.readingId = TSR.readingId.ROWID 
        FROM TS2DiagReadings TSR 
               JOIN Readings R 
                ON R.readingId = TSR.readingId 
 
          WHERE R.endPointId in (EID) ); 
 
      DELETE VoltageMonitoringReadings 
      WHERE ROWID IN 
      ( SELECT  VR.ROWID 
        --JOIN Readings R 
         --ON R.readingId = VR.readingId.ROWID 
        FROM VoltageMonitoringReadings VR 
               JOIN Readings R 
                ON R.readingId = VR.readingId 
 
          WHERE R.endPointId in (EID) ); 
 
      DELETE Readings 
 
        WHERE endpointId in (EID); 
 
      DELETE TS1OpticReadings 
      WHERE ROWID IN 
      ( SELECT  tor.ROWID 
       -- JOIN TS1Readings r 
       --  ON tor.readingId = r.readingId.ROWID 
        FROM TS1OpticReadings tor 
               JOIN TS1Readings r 
                ON tor.readingId = r.readingId 
 
          WHERE r.endpointId in (EID) ); 
 
      DELETE TS1EOLVMReadings 
      WHERE ROWID IN 
      ( SELECT  ter.ROWID 
        --JOIN TS1Readings r 
         --ON ter.readingId = r.readingId.ROWID 
        FROM TS1EOLVMReadings ter 
               JOIN TS1Readings r 
                ON ter.readingId = r.readingId 
 
          WHERE r.endpointId in (EID) ); 
 
      /*DELETE TS1EOLV4Readings 
      WHERE ROWID IN 
      ( SELECT  ter.ROWID 
        --JOIN TS1Readings r 
        -- ON ter.readingId = r.readingId.ROWID 
        FROM TS1EOLV4Readings ter 
               JOIN TS1Readings r 
                ON ter.readingId = r.readingId 
 
          WHERE r.endpointId in (EID) ); 
 */
      DELETE TS1Readings 
 
        WHERE endpointId in (EID); 
 
      DELETE TS1OutageHistory 
 
        WHERE endpointId in (EID); 
 
      DELETE LCEndpointLog 
      WHERE ROWID IN 
      ( SELECT LCR.rowid 
        --JOIN LCReadings 
         --ON LCReadings.lcReadingId = LCEndPointLog.lcReadingId.ROWID 
        FROM LCEndPointLog LCR 
               JOIN LCReadings 
                ON LCReadings.lcReadingId = LCR.lcReadingId 
 
          WHERE LCReadings.endPointId in (EID) ); 
 
      DELETE LCEndPointLatestValues 
 
        WHERE endPointId in (EID); 
 
      DELETE LCReadings 
 
        WHERE endpointId in (EID); 
 
      DELETE KV2KReadings 
      WHERE ROWID IN 
      ( SELECT  k.ROWID 
       -- JOIN KV2Readings r 
       --  ON r.kv2readingId = k.kv2readingId.ROWID 
        FROM KV2KReadings k 
               JOIN KV2Readings r 
                ON r.kv2readingId = k.kv2readingId 
 
          WHERE r.endPointId in (EID) ); 
 
      DELETE KV2TReadings 
      WHERE ROWID IN 
      ( SELECT  k.ROWID 
        --JOIN KV2Readings r 
         --ON r.kv2readingId = k.kv2readingId.ROWID 
        FROM KV2TReadings k 
               JOIN KV2Readings r 
                ON r.kv2readingId = k.kv2readingId 
 
          WHERE r.endPointId in (EID) ); 
 
      DELETE KV2KTReadings 
      WHERE ROWID IN 
      ( SELECT  k.ROWID 
        --JOIN KV2Readings r 
        -- ON r.kv2readingId = k.kv2readingId.ROWID 
        FROM KV2KTReadings k 
               JOIN KV2Readings r 
                ON r.kv2readingId = k.kv2readingId 
 
          WHERE r.endPointId in (EID) ); 
 
      DELETE KV2DiagnosticReadings 
      WHERE ROWID IN 
      ( SELECT  k.ROWID 
       -- JOIN KV2Readings r 
        -- ON r.kv2readingId = k.kv2readingId.ROWID 
        FROM KV2DiagnosticReadings k 
               JOIN KV2Readings r 
                ON r.kv2readingId = k.kv2readingId 
 
          WHERE r.endPointId in (EID) ); 
 
      DELETE KV2Readings 
 
        WHERE endPointId in (EID); 
 
      DELETE KV2EndPointLatestValues 
 
        WHERE endPointId in (EID); 
 
      DELETE IntervalData WHERE endpointId in (EID); 
      DELETE CommModChngOuts WHERE endpointId in (EID);
      DELETE IDReadingLogs WHERE endpointId in (EID); 
 
      DELETE EndpointGroupAssoc 
 
        WHERE EndPointId in (EID); 
        
      DELETE EndpointGroupAssocHistory 
        WHERE endpointId in (EID); 
 
	 

      DELETE EndPointHistory 
 
        WHERE EndPointId in (EID); 
 
      BEGIN 
         SELECT 1 INTO v_temp 
           FROM DUAL 
          WHERE EXISTS ( SELECT 1 
                         FROM EndPoints 
                           WHERE meterId in (MID) 
 
                            HAVING COUNT(*) = 1 ); 
      EXCEPTION 
         WHEN OTHERS THEN 
            NULL; 
      END; 
 
      --Only delete if there is only one endpoint attached to the meter. 
      IF v_temp = 1 THEN 
         DELETE EndPointHistory 
 
           WHERE meterId in (EID); 
 
      END IF; 
 
      DELETE EventLog WHERE EndPointId in (EID); 
 
      DELETE TS1EndpointDataImportErrors 
      WHERE ROWID IN 
      ( SELECT  TE.ROWID 
       -- JOIN ErrorLog E 
        -- ON E.errorLogId = TE.errorLogId.ROWID 
        FROM TS1EndpointDataImportErrors TE 
               JOIN ErrorLog E 
                ON E.errorLogId = TE.errorLogId 
 
          WHERE E.endPointId in (EID) ); 
 
      DELETE ReadingErrors 
      WHERE ROWID IN 
      ( SELECT  RE.ROWID 
       -- JOIN ErrorLog EL 
       --  ON EL.errorLogId = RE.errorLogId.ROWID 
        FROM ReadingErrors RE 
               JOIN ErrorLog EL 
                ON EL.errorLogId = RE.errorLogId 
 
          WHERE EL.endPointId in (EID) ); 
 
      DELETE MultiSpeakTransactionErrors 
      WHERE ROWID IN 
      ( SELECT  MTE.ROWID 
       -- JOIN ErrorLog E 
       --  ON E.errorLogId = MTE.errorLogId.ROWID 
        FROM MultiSpeakTransactionErrors MTE 
               JOIN ErrorLog E 
                ON E.errorLogId = MTE.errorLogId 
 
          WHERE E.endPointId in (EID) ); 
          
      DELETE ReadingProcessingErrors 
      WHERE ROWID IN 
      ( SELECT  RPE.ROWID 
       -- JOIN ErrorLog EL 
       --  ON EL.errorLogId = RE.errorLogId.ROWID 
        FROM ReadingProcessingErrors RPE 
               JOIN ErrorLog EL 
                ON EL.errorLogId = RPE.errorLogId 
 
          WHERE EL.endPointId in (EID) ); 
 
      DELETE ERRORLOG WHERE EndPointId in (EID); 
 
      -- In case this is an RSS endpoint, delete from the RSSDeviceCommandHistory table and the 
      -- RSSDevices table 
      DELETE RSSDeviceCommandHistory 
 
        WHERE rssDeviceId = ( SELECT rssDeviceId 
                              FROM RSSDevices 
                                WHERE endPointId in (EID) ); 
 
      DELETE RSSDevices 
 
        WHERE endPointId in (EID); 
 
      -- In case this is an LC endpoint, delete from the LCCommandHistory table 
      DELETE CommandHistory WHERE endPointId in (EID); 
 
      DELETE LCDevices 
 
        WHERE endPointId in (EID); 
 
      DELETE LCEndPointProperties 
 
        WHERE endPointId in (EID); 
 
      -- Delete any RE commands from the CmdRECommands table that were endpoint addressed to this endpoint 
      DELETE CmdRECommands 
      WHERE ROWID IN 
      ( SELECT  CR.ROWID 
        --JOIN CommandLog CL 
        -- ON CL.commandLogId = CR.commandLogId.ROWID 
        FROM CmdRECommands CR 
               JOIN CommandLog CL 
                ON CL.commandLogId = CR.commandLogId 
 
          WHERE CL.endPointId in (EID) ); 
 
      DELETE CmdSetGroupsParameters 
      WHERE ROWID IN 
      ( SELECT  CR.ROWID 
       -- JOIN  CL 
       --  ON CL.commandLogId = CR.commandLogId.ROWID 
        FROM CmdSetGroupsParameters CR 
               JOIN CommandLog CL 
                ON CL.commandLogId = CR.commandLogId 
 
          WHERE CL.endPointId in (EID) ); 
 
      DELETE CmdLCOverrideParameters 
      WHERE ROWID IN 
      ( SELECT  CLO.ROWID 
        --JOIN CommandLog CL 
        -- ON CL.commandLogId = CLO.commandLogId.ROWID 
        FROM CmdLCOverrideParameters CLO 
               JOIN CommandLog CL 
                ON CL.commandLogId = CLO.commandLogId 
 
          WHERE CL.endPointId in (EID) ); 
 
      DELETE LCTempScheduleCommandLog 
      WHERE ROWID IN 
      ( SELECT  LSC.ROWID 
        --JOIN CommandLog CL 
        -- ON CL.commandLogId = LSC.commandLogId.ROWID 
        FROM LCTempScheduleCommandLog LSC 
               JOIN CommandLog CL 
                ON CL.commandLogId = LSC.commandLogId 
 
          WHERE CL.endPointId in (EID) ); 
 
      DELETE CmdLCCommand 
      WHERE ROWID IN 
      ( SELECT  CLC.ROWID 
       -- JOIN CommandLog CL 
       --  ON CL.commandLogId = CLC.commandLogId.ROWID 
        FROM CmdLCCommand CLC 
               JOIN CommandLog CL 
                ON CL.commandLogId = CLC.commandLogId 
 
          WHERE CL.endPointId in (EID) ); 
 
      DELETE CmdTOUOverrideParameters 
      WHERE ROWID IN 
      ( SELECT CTOU.rowid 
   --     JOIN CommandLog CL 
   --      ON CL.commandLogId = CTOU.commandLogId.ROWID 
        FROM CmdTOUOverrideParameters CTOU 
               JOIN CommandLog CL 
                ON CL.commandLogId = CTOU.commandLogId 
 
          WHERE CL.endPointId in (EID) ); 
          
       DELETE cmdRegisterHANDeviceParameters 
      WHERE ROWID IN 
      ( SELECT CHD.rowid 
          FROM cmdRegisterHANDeviceParameters CHD 
               JOIN CommandLog CL 
                ON CL.commandLogId = CHD.commandLogId 
          WHERE CL.endPointId in (EID) ); 
          
          
       DELETE PLCTS2FocusWorkaroundHistory WHERE endPointId in (EID); 
                    
 
      -- Adusted relationship to come from CommandLog table (ejk) 
	  
      DELETE CommandResponse 
		WHERE commandLogId IN ( SELECT commandLogId FROM CommandLog WHERE EndPointId in (EID) )
			OR endpointId in (EID); 
                                  
      DELETE MultiSpuCommandDownstreamLog 
      WHERE ROWID IN 
      ( SELECT MSCDL.rowid 
   --     JOIN CommandLog CL 
   --      ON CL.commandLogId = CTOU.commandLogId.ROWID 
        FROM MultiSpuCommandDownstreamLog MSCDL 
               JOIN CommandLog CL 
                ON CL.commandLogId = MSCDL.commandLogId 
 
          WHERE CL.endPointId in (EID) ); 
 
      DELETE CommandDownstreamLog 
      WHERE ROWID IN 
      ( SELECT  CDL.ROWID 
       -- JOIN CommandsSent CS 
       --  ON CS.commandSentId = CDL.commandSentId.ROWID 
        FROM CommandDownstreamLog CDL 
               JOIN CommandsSent CS 
                ON CS.commandSentId = CDL.commandSentId 
 
          WHERE CS.commandLogId IN ( SELECT commandLogId 
                                     FROM CommandLog 
                                       WHERE EndPointId in (EID) ) ); 
 
      DELETE RFCommandStaging 
      WHERE ROWID IN 
      ( SELECT  RCS.ROWID 
        --JOIN CommandsSent CS 
        -- ON CS.commandSentId = RCS.commandSentId.ROWID 
        FROM RFCommandStaging RCS 
               JOIN CommandsSent CS 
                ON CS.commandSentId = RCS.commandSentId 
 
          WHERE CS.commandLogId IN ( SELECT commandLogId 
                                     FROM CommandLog 
                                       WHERE EndPointId in (EID) ) ); 
	  
      DELETE CommandsSent 
 
        WHERE commandLogId IN ( SELECT commandLogId 
                                FROM CommandLog 
                                  WHERE EndPointId in (EID) ); 
	  
      DELETE CommandLogNotes 
        WHERE commandLogId IN ( SELECT commandLogId 
                                FROM CommandLog 
                                  WHERE endPointId in (EID) ); 
       DELETE CmdGetLPParameters 
  
        WHERE commandLogId IN ( SELECT commandLogId 
                                FROM CommandLog 
                                  WHERE endPointId in (EID) ); 
  
	  --PLX table - doesn't currently exist
	  DELETE from CmdPLCSetEvent 
	  WHERE commandLogId IN ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID) );	

      DELETE CmdFEBatches WHERE endPointId in (EID); 
 
      DELETE OutageProbabilityLog WHERE endPointId in (EID); 
 
      DELETE ODEventRequests 
 
        WHERE meterId in (MID); 
 
      DELETE CommandLog WHERE EndPointId in (EID); 
 
      DELETE MomentaryInterruptionLog 
 
        WHERE EndPointId in (EID); 
 
      DELETE VirtualGroupAssoc 
 
        WHERE objectId in (EID) 
                AND objectTypeId = 1; 
 
      DELETE EndPointStatusCodeHistory 
 
        WHERE endPointId in (EID); 
 
      DELETE EndPointLatestValues 
 
        WHERE endPointId in (EID); 
 
      DELETE PreviousDemand 
 
        WHERE endpointId in (EID); 
        
 
	DELETE tt_ts1FreqAllocationChange;
	-- Hold onto any entries that need to be reinserted as a blocked allocation
	INSERT INTO tt_ts1FreqAllocationChange (spuId, frequency, blockedById)
	  (SELECT spuId, frequency, blockedById 
		FROM TS1FreqAllocation tfa
		WHERE endPointId in (EID) AND
		  blockedById IS NOT NULL AND EXISTS (SELECT 1 FROM TS1AllocationBlocks TB WHERE TB.blockId = tfa.blockedById)
	   );

	-- DELETE all the freq allocations
	DELETE TS1FreqAllocation WHERE endPointId in (EID);

	-- Insert the blocked allocations
	INSERT INTO TS1FreqAllocation (spuId, frequency, blockId, state)
	  SELECT spuId, frequency, blockedById, 1
		FROM tt_ts1FreqAllocationChange tfac;
 
      DELETE TS1EndpointCards 
 
        WHERE endPointId in (EID); 
 
      DELETE TS1Endpoints 
 
        WHERE endPointId in (EID); 
 
      DELETE LCEndpointProperties 
      WHERE endPointId in (EID); 
 
      DELETE RFEndpointProperties 
      WHERE endpointId in (EID); 
 
      DELETE RFMeshHistory 
      WHERE endpointId in (EID); 
 
      DELETE AsyncRequests 
      WHERE endPointId in (EID); 
 
      DELETE MUPreviousDemand 
      WHERE endPointId in (EID); 
 
      DELETE MUEndpoints 
      WHERE endPointId in (EID); 
 
      DELETE EndpointNotes 
 
        WHERE endPointId in (EID); 
 
      DELETE EndpointProperties 
 
        WHERE endPointId in (EID); 
 
      DELETE FinalReadings 
 
        WHERE endpointId in (EID); 
 
      DELETE TS1LatchDataLatest 
 
        WHERE endPointid in (EID); 
 
      DELETE TS1LatchData 
 
        WHERE endPointid in (EID); 
 
      DELETE PredeployedEndpoints 
 
        WHERE endPointId in (EID); 
 
      DELETE AutoSwitchHistory 
 
        WHERE endPointId in (EID); 
 
      DELETE GPSLocations 
 
        WHERE endpointId in (EID); 
 
      DELETE RFAMRRegistrationHistory 
 
        WHERE endpointId in (EID); 
 
      DELETE RFEndpointLatestValues 
 
        WHERE endpointId in (EID); 
 
      DELETE IDReadingLatestValues 
 
        WHERE endpointId in (EID); 
 
      DELETE IntervalDataLatestValues 
 
        WHERE endpointId in (EID); 
 
      DELETE EndpointMeteringProperties 
 
        WHERE endpointId in (EID); 
 
      DELETE EndpointDeploymentStatistics 
 
        WHERE endpointId in (EID); 
 
      DELETE TempEdptDeploymentStatistics 
 
        WHERE endpointId in (EID); 
 
      DELETE TempDailyReadings 
 
        WHERE endpointId in (EID); 
 
      DELETE EndpointAlerts 
 
        WHERE endpointId in (EID); 
 
      DELETE EndpointDeploymentStatistics 
 
        WHERE endpointId in (EID); 
 
      DELETE PDRReadingHistory 
 
        WHERE endpointId in (EID); 
        
        DELETE PDRCalendarHistory 
 
        WHERE endpointId in (EID); 
        
        DELETE EndpointAlerts2 
 
        WHERE endpointId in (EID); 
 
      DELETE IDOnDemandReadings 
 
        WHERE endpointId in (EID); 
 
      DELETE toumodeconfirmation 
 
        WHERE endpointId in (EID); 
 
      DELETE IDUnCrtdRdgLatestValues 
 
        WHERE endpointId in (EID); 
        
      DELETE IntervalDataGaps 
 
        WHERE endpointId in (EID); 
        
	  DELETE IntervalUncorrectedData 
 
        WHERE endpointId in (EID);   
              
	DELETE IdReadingLogGaps WHERE endpointId in (EID);
	
	  DELETE RFEndpointReadingStats 
 
        WHERE endpointId in (EID);
        
      DELETE GPRSEndpointProperties 
 
        WHERE endpointId in (EID);
        
		DELETE EndPointStatusCodeHistory 
		WHERE endpointid in (SELECT hanendpointid FROM HANDevices WHERE endpointId in (EID));

		DELETE EndpointProperties 
		WHERE endpointid in (SELECT hanendpointid FROM HANDevices WHERE endpointId in (EID));

		DELETE Endpoints 
		WHERE endpointid  in (SELECT hanendpointid FROM HANDevices WHERE endpointId in (EID));

         DELETE HANDevices 
 
        WHERE hanEndpointId in (EID);
       
        DELETE HANDevices 
 
        WHERE endpointId in (EID);  


	DELETE EndpointMeterConfiguration WHERE endpointId in (EID);
      
      DELETE CollectorTransitionHistory where endpointId in (EID);
        
      DELETE EndpointDaySet WHERE endpointId in (EID);
	DELETE DailyEndpointReadingStats WHERE endpointId in (EID);
	DELETE LCSDelaySettingTracking WHERE endpointId in (EID);
	DELETE EdptScrty WHERE endpointId in (EID);
	DELETE temproutebids WHERE endpointId in (EID);
	DELETE GapReq WHERE endpointId in (EID);
	DELETE GapRqstCmds WHERE endpointId in (EID);
    DELETE SnapReadTimes WHERE endpointId in (EID);
	DELETE NMMDevice WHERE endpointId in (EID);
	DELETE EvntGapReq WHERE endpointId in (EID);

	  UPDATE RFEndpointProperties set bestneighborEndpointId = NULL WHERE bestNeighborEndpointId in (EID);
	  UPDATE Collectors SET cltrAppEdptId = NULL WHERE cltrAppEdptId in (EID);
	  
	 
	  DECLARE 
		v_ipaddr NUMBER(20);
    v_ipaddrV6 VARCHAR(45);
    v_IPVersion INT(10);
      BEGIN
		BEGIN
        SELECT SettingsValue INTO v_IPVersion  FROM Settings S INNER JOIN 
        SettingsValues SV ON S.SettingsId = SV.SettingsId
        WHERE enumName = 'IPVersion';
        EXCEPTION
        WHEN NO_DATA_FOUND THEN
        v_IPVersion := NULL;
		End;
        IF v_IPVersion = 1 THEN
          BEGIN
            SELECT ipaddr INTO v_ipaddr 
            From Endpoints 
            WHERE endpointId in (EID) 
            AND organizationID = 1
            AND endPointTypeId != 15;
          EXCEPTION 
          WHEN OTHERS THEN NULL; 
          END;
	  
          IF v_ipaddr IS NOT NULL THEN
            UPDATE IPAddrs SET ASSIGNEDFLG = 0 WHERE IPAddr = v_IPAddr;
          END IF;
        END IF;
        IF v_IPVersion = 2 THEN
          BEGIN
            SELECT ipaddrV6 INTO v_ipaddrV6 
            From Endpoints 
            WHERE endpointId in (EID) 
            AND organizationID = 1
            AND endPointTypeId != 15;
          EXCEPTION 
          WHEN OTHERS THEN NULL; 
          END;
	  
          IF v_ipaddrV6 IS NOT NULL THEN
            UPDATE IPAddrsV6 SET ASSIGNEDFLG = 0 WHERE IPAddr = v_IPAddrV6;
          END IF;
        END IF;
        
    END;

	
	--PLX table - doesn't currently exist 
	DELETE PLCPacketProcessError WHERE endpointId in (EID);
	DELETE PLCEdptAsyncPktStateHist WHERE endpointId in (EID);
	DELETE PLCEdptPktProcessState WHERE endpointId in (EID);
	DELETE PLCEdptPktProcessStateHist WHERE endpointId in (EID);
	DELETE PLCEndpointAlarm WHERE endpointId in (EID);
	DELETE PLCEdptConfigProfile WHERE endpointId in (EID);
	DELETE FROM PLCWFFindEndpointHist WHERE plcWorkflowId IN (SELECT plcWorkflowId FROM PLCWorkflowHist WHERE endpointId in (EID));
	DELETE PLCWorkflowHist WHERE endpointId in (EID);
	DELETE FROM PLCWFFindEndpoint WHERE plcWorkflowId IN (SELECT plcWorkflowId FROM PLCWorkflowHist WHERE endpointId in (EID));
	DELETE PLCWorkflow WHERE endpointId in (EID);
	

  
      DELETE Endpoints 
 
        WHERE EndPointId in (EID); 
 
      BEGIN 
         SELECT 1 INTO v_temp 
           FROM DUAL 
          WHERE NOT EXISTS ( SELECT 1 
                             FROM EndPoints 
                               WHERE meterId in (MID) )
			AND NOT EXISTS ( SELECT 1
							 FROM IDReadingLogs
							   WHERE meterId in (MID) ); 
      EXCEPTION 
         WHEN OTHERS THEN 
            NULL; 
      END; 
 
      -- There may still be an endpoint attached to the meter. 
      IF v_temp = 1 THEN 
      BEGIN 
         DELETE stageRebuildMeters 
         WHERE meterId in (MID); 
 
         DELETE Meters 
         WHERE meterId in (MID); 
 
      END; 
      END IF; 
 /*
      BEGIN 
         SELECT 1 INTO v_temp 
           FROM DUAL 
          WHERE v_muSlot1MeterId IS NOT NULL 
        AND NOT EXISTS ( SELECT 1 
                         FROM MUEndpoints 
                           WHERE meterId = v_muSlot1MeterId ) 
        AND NOT EXISTS ( SELECT 1 
                         FROM MUReadings 
                           WHERE meterId = v_muSlot1MeterId ); 
      EXCEPTION 
         WHEN OTHERS THEN 
            NULL; 
      END; 
 */
      -- There may still be MUEndpoints attached to the meter 
	  /*
      IF v_temp = 1 THEN 
      BEGIN 
         DELETE stageRebuildMeters 
         WHERE meterId = v_muSlot1MeterId; 
 
         DELETE Meters 
         WHERE meterId = v_muSlot1MeterId; 
 
      END; 
      END IF; 
 
      BEGIN 
         SELECT 1 INTO v_temp 
           FROM DUAL 
          WHERE v_muSlot1MeterId IS NOT NULL 
        AND NOT EXISTS ( SELECT 1 
                         FROM MUEndpoints 
                           WHERE meterId = v_muSlot2MeterId ) 
        AND NOT EXISTS ( SELECT 1 
                         FROM MUReadings 
                           WHERE meterId = v_muSlot2MeterId ); 
      EXCEPTION 
         WHEN OTHERS THEN 
            NULL; 
      END; 
 
      IF v_temp = 1 THEN 
      BEGIN 
         DELETE stageRebuildMeters 
         WHERE meterId = v_muSlot2MeterId; 
 
         DELETE Meters 
         WHERE meterId = v_muSlot2MeterId; 
 
      END; 
      END IF; 

	  
	v_temp :=0;
   IF v_serialnumber IS NOT NULL THEN
   BEGIN
	BEGIN
      SELECT ETI.endpointTransactionImportId
        INTO v_endpointImportTransId
        FROM EndpointInstallInfoTrans ETI
       WHERE ETI.installedEndpointSN = v_serialnumber;
	    EXCEPTION
         WHEN OTHERS THEN
            NULL;
      END;

	  DELETE EndpointInstallInfoTrans
       WHERE installedEndpointSN = v_serialnumber;

    		 DELETE from EndpointTransactionImport 
			where endpointTransactionImportId = v_endpointImportTransId
			AND NOT EXISTS (SELECT 1 FROM EndpointInstallInfoTrans ETI where ETI.endpointTransactionImportId = v_endpointImportTransId);

		DELETE from RFUHeadEndRadios 
		where serialNumber = v_serialnumber;
		
		DELETE from EdptKeyGen
       where serialNumber = v_serialnumber;

		DELETE from RouteBIds
		where serialNumber = v_serialnumber;

 */
END;

--commit after running the previous script the end
commit;
  
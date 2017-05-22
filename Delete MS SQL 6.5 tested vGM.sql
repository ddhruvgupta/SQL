--Tested locally in MS SQL CC 6.5 MR 2 Branch.
--Extracted/Modified from the delendpoint sproc. Org ID hardcoded to 1
--9/21/2016. GMarmat. Do not send to customer. Use at your own risk.
--Replace all EID with your comma seperated list of endpointids
--Replace all MID with your comma seperated list of meterids
--Use this script to get EID and MID when you know the meterno:
select e.endpointid, m.meterid, m.meterno from endpoints e inner join meters m on e.meterid=m.meterid
where m.meterno in ('MeterNo1','Meterno2')

BEGIN TRAN



       -- Delete the AuditTransaction tables
       DELETE AuditMCODetails
       FROM AuditMCODetails AMCO
       WHERE AMCO.oldEndPointId in (EID) 

       DELETE AuditEndPointGroups
       FROM AuditEndPointGroups AEG INNER JOIN AuditLog AL ON AL.auditLogId = AEG.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditMomentaryIntLog
       FROM AuditMomentaryIntLog AML INNER JOIN AuditLog AL ON AL.auditLogId = AML.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditEndPointProperties
       FROM AuditEndPointProperties AEP INNER JOIN AuditLog AL ON AL.auditLogId = AEP.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditAMREndPointProperties
       FROM AuditAMREndPointProperties AMR INNER JOIN AuditLog AL ON AL.auditLogId = AMR.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditTS1AMREndPointProperties
       FROM AuditTS1AMREndPointProperties AMR INNER JOIN AuditLog AL ON AL.auditLogId = AMR.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditTS2AMREndPointProperties
       FROM AuditTS2AMREndPointProperties AMR INNER JOIN AuditLog AL ON AL.auditLogId = AMR.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditIconEndPointProperties
       FROM AuditIconEndPointProperties AIP INNER JOIN AuditLog AL ON AL.auditLogId = AIP.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AudTS1EOLVMEdptProp
       FROM AudTS1EOLVMEdptProp AMR INNER JOIN AuditLog AL ON AL.auditLogId = AMR.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditLCEndPointProperties
       FROM AuditLCEndPointProperties ALC INNER JOIN AuditLog AL ON AL.auditLogId = ALC.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditRSSEndPointProperties
       FROM AuditRSSEndPointProperties AREP INNER JOIN AuditLog AL ON AL.auditLogId = AREP.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditMeterProperties
       FROM AuditMeterProperties AMP INNER JOIN AuditLog AL ON AL.auditLogId = AMP.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditMCODetails
       FROM AuditMCODetails AMCO INNER JOIN AuditLog AL ON AL.auditLogId = AMCO.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditKV2EndPointProperties
       FROM AuditKV2EndPointProperties AK2 INNER JOIN AuditLog AL ON AL.auditLogId = AK2.auditLogId
       WHERE AL.endPointId in (EID) 
       
       DELETE AuditS4EndPointProperties
       FROM AuditS4EndPointProperties AS4 INNER JOIN AuditLog AL ON AL.auditLogId = AS4.auditLogId
       WHERE AL.endPointId in (EID) 
       
       DELETE AuditKV2Readings
       FROM AuditKV2Readings AK2 INNER JOIN AuditLog AL ON AL.auditLogId = AK2.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditKV2KReadings
       FROM AuditKV2KReadings AK2 INNER JOIN AuditLog AL ON AL.auditLogId = AK2.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditKV2TReadings
       FROM AuditKV2TReadings AK2 INNER JOIN AuditLog AL ON AL.auditLogId = AK2.auditLogId
       WHERE AL.endPointId in (EID) 
       
       DELETE AuditKV2KTReadings
       FROM AuditKV2KTReadings AK2 INNER JOIN AuditLog AL ON AL.auditLogId = AK2.auditLogId
       WHERE AL.endPointId in (EID) 

       DELETE AuditKV2DiagnosticReadings
       FROM AuditKV2DiagnosticReadings AK2 INNER JOIN AuditLog AL ON AL.auditLogId = AK2.auditLogId
       WHERE AL.endPointId in (EID) 
       
       DELETE AuditTS2AMRDemandReadings
       FROM AuditTS2AMRDemandReadings AME INNER JOIN AuditLog AL ON AL.auditLogId = AME.auditLogId
       WHERE AL.endPointId in (EID) 
       
       DELETE AuditMUEndPointProperties
       FROM AuditMUEndPointProperties AME INNER JOIN AuditLog AL ON AL.auditLogId = AME.auditLogId
       WHERE AL.endPointId in (EID) 
       
       DELETE AuditTS2VoltageReadings
       FROM  AuditTS2VoltageReadings AME INNER JOIN AuditLog AL ON AL.auditLogId = AME.auditLogId
       WHERE AL.endPointId in (EID) 
       
       DELETE AuditS4KMReadings
       FROM  AuditS4KMReadings AME INNER JOIN AuditLog AL ON AL.auditLogId = AME.auditLogId
       WHERE AL.endPointId in (EID) 
       
       DELETE AuditIdReadings 
       FROM AuditIdReadings AIR INNER JOIN AuditLog AL ON AL.auditLogId = AIR.AuditLogId
       WHERE AL.endpointId in (EID) 
       
       DELETE AuditLog 
       FROM AuditLog AL
       WHERE AL.endPointId in (EID) 

       DELETE EndpointLog 
              FROM EndpointLog INNER JOIN Readings ON Readings.ReadingId = EndpointLog.ReadingId
              WHERE Readings.endpointId in (EID) 
       
       DELETE FROM TS1PriorEndPointLatestValues WHERE endPointId in (EID) 

       DELETE FROM TS1EndPointLatestValues WHERE endPointId in (EID) 
       
       DELETE MUDiagnosticReadings
       FROM MUDiagnosticReadings MDR 
       INNER JOIN Readings R ON R.readingId = MDR.readingId
       WHERE R.endPointId in (EID) 
       AND MDR.readingsTableId = 2
       
       DELETE MUDiagnosticReadings
       FROM MUDiagnosticReadings MDR
       INNER JOIN KV2Readings R ON R.kv2ReadingId = MDR.readingId
       WHERE R.endPointId in (EID) 
       AND MDR.readingsTableId = 3
       
       DELETE MUReadings
       FROM MUReadings MR
       INNER JOIN Readings R ON R.readingId = MR.readingId
       WHERE R.endPointId in (EID) 
       AND MR.readingsTableId = 2
       
       DELETE MUReadings
       FROM MUReadings MR
       INNER JOIN KV2Readings R ON R.kv2ReadingId = MR.readingId
       WHERE R.endPointId in (EID) 
       AND MR.readingsTableId = 3

       DELETE FROM TS2DiagReadings
       FROM TS2DiagReadings TSR 
       INNER JOIN Readings R ON R.readingId = TSR.readingId
       WHERE  R.endPointId in (EID) 
       
       DELETE VoltageMonitoringReadings
       FROM VoltageMonitoringReadings VR
       INNER JOIN Readings R ON R.readingId = VR.readingId
       WHERE R.endPointId in (EID) 

       DELETE FROM Readings WHERE endpointId in (EID) 
       
       DELETE TS1OpticReadings
       FROM TS1OpticReadings tor INNER JOIN TS1Readings r ON tor.readingId = r.readingId
       WHERE r.endpointId in (EID) 

       DELETE TS1EOLVMReadings
       FROM TS1EOLVMReadings ter INNER JOIN TS1Readings r ON ter.readingId = r.readingId
       WHERE r.endpointId in (EID) 

       DELETE TS1EOLVMIDReadings
       FROM TS1EOLVMIDReadings ter INNER JOIN TS1Readings r ON ter.readingId = r.readingId
       WHERE r.endpointId in (EID) 

       DELETE FROM TS1Readings WHERE endpointId in (EID) 

       DELETE TS1OutageHistory
       WHERE endpointId in (EID) 

       DELETE LCEndpointLog 
              FROM LCEndpointLog INNER JOIN LCReadings ON LCReadings.lcReadingId = LCEndpointLog.lcReadingId
              WHERE LCReadings.endpointId in (EID) 

       DELETE FROM LCEndPointLatestValues WHERE endPointId in (EID) 

       DELETE FROM LCReadings WHERE endpointId in (EID) 

       DELETE KV2KReadings
       FROM KV2KReadings k INNER JOIN KV2Readings r ON r.kv2readingId = k.kv2readingId
       WHERE r.endPointId in (EID) 

       DELETE KV2TReadings
       FROM KV2TReadings k INNER JOIN KV2Readings r ON r.kv2readingId = k.kv2readingId
       WHERE r.endPointId in (EID) 

       DELETE KV2KTReadings
       FROM KV2KTReadings k INNER JOIN KV2Readings r ON r.kv2readingId = k.kv2readingId
       WHERE r.endPointId in (EID) 

       DELETE KV2DiagnosticReadings
       FROM KV2DiagnosticReadings k INNER JOIN KV2Readings r ON r.kv2readingId = k.kv2readingId
       WHERE r.endPointId in (EID) 

       DELETE KV2Readings 
       WHERE endPointId in (EID) 

       DELETE KV2EndPointLatestValues
       WHERE endPointId in (EID) 
       
       DELETE IntervalData WHERE endpointId in (EID) 
       DELETE CommModChngOuts WHERE endpointId in (EID) 
       DELETE IDReadingLogs
       FROM IDReadingLogs IDR
       WHERE IDR.endpointId in (EID) 
       
       DELETE FROM EndpointGroupAssoc where EndPointId in (EID) 
       DELETE FROM EndpointGroupAssocHistory where endpointId in (EID) 

       DELETE FROM EndPointHistory   where EndPointId in (EID) 
       --Only delete if there is only one endpoint attached to the meter.
       IF EXISTS (SELECT 1 FROM Endpoints WHERE meterId  in (MID)  HAVING COUNT(*) = 1)
              DELETE FROM EndPointHistory   where meterId  in (MID) 

       DELETE FROM EventLog    where EndPointId in (EID) 
       DELETE TS1EndpointDataImportErrors
       FROM TS1EndpointDataImportErrors TE 
       INNER JOIN ErrorLog E ON E.errorLogId = TE.errorLogId
       WHERE E.endPointId in (EID)  

       DELETE ReadingErrors 
       FROM ReadingErrors RE 
       INNER JOIN ErrorLog EL ON EL.errorLogId = RE.errorLogId 
       WHERE EL.endPointId in (EID) 
       
       DELETE MultiSpeakTransactionErrors
       FROM MultiSpeakTransactionErrors MTE
       INNER JOIN ErrorLog E ON E.errorLogId = MTE.errorLogId
       WHERE E.endPointId in (EID)       
       
       DELETE ReadingProcessingErrors
    FROM ReadingProcessingErrors RPE
    INNER JOIN ErrorLog EL ON EL.errorLogId = RPE.errorLogId
    WHERE EL.endPointId in (EID) 

       DELETE FROM ERRORLOG where EndPointId in (EID) 

       -- In case this is an RSS endpoint, delete from the RSSDeviceCommandHistory table and the 
       -- RSSDevices table
       DELETE FROM RSSDeviceCommandHistory WHERE rssDeviceId = (SELECT rssDeviceId FROM 
                           RSSDevices WHERE endPointId in (EID) )
       DELETE FROM RSSDevices WHERE endPointId in (EID) 

       -- In case this is an LC endpoint, delete from the LCCommandHistory table
       DELETE FROM CommandHistory WHERE endPointId in (EID) 
       DELETE FROM LCDevices WHERE endPointId in (EID) 
       DELETE FROM LCEndPointProperties WHERE endPointId in (EID) 

       -- Delete any RE commands from the CmdRECommands table that were endpoint addressed to this endpoint
       DELETE CmdRECommands
       FROM CmdRECommands CR INNER JOIN CommandLog CL ON CL.commandLogId = CR.commandLogId
       WHERE CL.endPointId in (EID)  
       
       DELETE CmdSetGroupsParameters
       FROM CmdSetGroupsParameters CR INNER JOIN CommandLog CL ON CL.commandLogId = CR.commandLogId
       WHERE CL.endPointId in (EID)  
              
       DELETE CmdLCOverrideParameters
       FROM CmdLCOverrideParameters CLO INNER JOIN CommandLog CL ON CL.commandLogId = CLO.commandLogId
       WHERE CL.endPointId in (EID)  
       
       DELETE LCTempScheduleCommandLog
       FROM LCTempScheduleCommandLog LSC INNER JOIN CommandLog CL ON CL.commandLogId = LSC.commandLogId
       WHERE CL.endPointId in (EID) 

       DELETE CmdLCCommand
       FROM CmdLCCommand CLC INNER JOIN CommandLog CL ON CL.commandLogId = CLC.commandLogId
       WHERE CL.endPointId in (EID) 
       
       DELETE CmdTOUOverrideParameters
       FROM CmdTOUOverrideParameters CTOU INNER JOIN CommandLog CL ON CL.commandLogId = CTOU.commandLogId
       WHERE CL.endPointId in (EID)  
       
       DELETE cmdRegisterHANDeviceParameters
       FROM cmdRegisterHANDeviceParameters CHP INNER JOIN CommandLog CL ON CL.commandLogId = CHP.commandLogId
       WHERE CL.endPointId in (EID)  
       
       DELETE PLCTS2FocusWorkaroundHistory
       FROM PLCTS2FocusWorkaroundHistory 
       WHERE endpointId in (EID) 
       
       -- Adusted relationship to come from CommandLog table (ejk) 
       DELETE FROM CommandResponse WHERE  commandLogId IN ( SELECT commandLogId  FROM CommandLog  WHERE EndPointId in (EID)   )  
              OR endpointId in (EID) 
       
       DELETE FROM MultiSpuCommandDownstreamLog
       FROM MultiSpuCommandDownstreamLog MSCDL
       INNER JOIN CommandLog CL ON CL.commandLogId = MSCDL.commandLogId
       WHERE CL.EndPointId in (EID) 

       DELETE FROM CommandDownstreamLog
       FROM CommandDownstreamLog CDL
       INNER JOIN CommandsSent CS ON CS.commandSentId = CDL.commandSentId
       WHERE CS.commandLogId IN ( SELECT commandLogId  FROM CommandLog  WHERE EndPointId in (EID)   ) 
       
       DELETE FROM RFCommandStaging
       FROM RFCommandStaging RCS
       INNER JOIN CommandsSent CS ON CS.commandSentId = RCS.commandSentId
       WHERE CS.commandLogId IN ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID)   )

       DELETE FROM CommandsSent WHERE  commandLogId IN ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID)  )     
       DELETE FROM CommandLogNotes WHERE commandLogId IN ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID)  )     
       DELETE from CmdGetLPParameters where commandLogId IN ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID)  )      
       DELETE from CmdPLCSetEvent where commandLogId in ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID)  )     
       DELETE from CmdPLCAsyncRequest where commandLogId in ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID)  )      
       DELETE from CmdPLCConfig where commandLogId in ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID)  )     
       DELETE from CmdPLCHANCommand where commandLogId in ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID)  )     
       DELETE from HANSEPRetryTracking where commandLogId in ( SELECT commandLogId FROM CommandLog WHERE endPointId in (EID)  )      
       
       DELETE FROM CmdFEBatches where endPointId in (EID) 
       DELETE OutageProbabilityLog WHERE endPointId in (EID) 
       DELETE ODEventRequests WHERE meterId  in (MID) 
       
       DELETE FROM CommandLog WHERE EndPointId in (EID) 
       DELETE FROM MomentaryInterruptionLog WHERE EndPointId in (EID) 
       DELETE FROM VirtualGroupAssoc WHERE objectId in (EID)  AND objectTypeId = 1
       DELETE FROM EndPointStatusCodeHistory WHERE endPointId in (EID) 
       DELETE FROM EndPointLatestValues WHERE endPointId in (EID) 
       DELETE FROM PreviousDemand WHERE endpointId in (EID) 
       
       DECLARE @ts1FreqAllocationChange TABLE
       (
         spuId smallint,
         frequency int,
         blockedById int
       )

       -- Hold onto any entries that need to be reinserted as a blocked allocation
       INSERT INTO @ts1FreqAllocationChange (spuId, frequency, blockedById)
         (SELECT spuId, frequency, blockedById 
              FROM TS1FreqAllocation tfa
              WHERE endPointId in (EID)  AND
                blockedById IS NOT NULL AND EXISTS (SELECT 1 FROM TS1AllocationBlocks TB WHERE TB.blockId = tfa.blockedById)
          )

       -- DELETE all the freq allocations
       DELETE TS1FreqAllocation WHERE endPointId in (EID) 

       -- Insert the blocked allocations
       INSERT TS1FreqAllocation (spuId, frequency, blockId, state)
         SELECT spuId, frequency, blockedById, 1
              FROM @ts1FreqAllocationChange tfac
              
       
       DELETE FROM TS1EndpointCards WHERE endPointId in (EID) 
       DELETE FROM TS1Endpoints WHERE endPointId in (EID) 
       DELETE LCEndpointProperties FROM LCEndpointProperties WHERE  endPointId in (EID) 
       DELETE RFEndpointProperties FROM RFEndpointProperties WHERE  endpointId in (EID) 
       DELETE RFMeshHistory FROM RFMeshHistory WHERE endpointId in (EID) 
       DELETE AsyncRequests FROM AsyncRequests WHERE endPointId in (EID) 
       DELETE MUPreviousDemand FROM MUPreviousDemand WHERE endPointId in (EID) 
       DELETE MUEndpoints FROM MUEndpoints WHERE endPointId in (EID) 
       DELETE FROM EndpointNotes WHERE endPointId in (EID) 
       DELETE FROM EndpointProperties WHERE endPointId in (EID) 
       DELETE FROM FinalReadings WHERE endpointId in (EID) 
       DELETE FROM TS1LatchDataLatest WHERE endPointid in (EID) 
       DELETE FROM TS1LatchData WHERE endPointid in (EID) 
       DELETE PredeployedEndpoints WHERE endPointId in (EID) 
       DELETE FROM AutoSwitchHistory WHERE endPointId in (EID) 
       DELETE FROM GPSLocations WHERE endpointId in (EID) 
       DELETE RFAMRRegistrationHistory WHERE endpointId in (EID) 
       DELETE RFEndpointLatestValues WHERE endpointId in (EID) 
       DELETE IDReadingLatestValues WHERE endpointId in (EID) 
       DELETE IntervalDataLatestValues WHERE endpointId in (EID) 
       DELETE EndpointMeteringProperties WHERE endpointId in (EID) 
       DELETE EndpointDeploymentStatistics WHERE endpointId in (EID) 
       DELETE TempEdptDeploymentStatistics WHERE endpointId in (EID) 
       DELETE TempDailyReadings WHERE endpointId in (EID) 
       DELETE EndpointAlerts WHERE endpointId in (EID) 
       DELETE FROM EndpointDeploymentStatistics WHERE endpointId in (EID) 
       DELETE FROM EndpointAlerts2 WHERE endpointId in (EID) 
       DELETE FROM PDRReadingHistory WHERE endpointId in (EID) 
       DELETE FROM PDRCalendarHistory WHERE endpointId in (EID) 
       DELETE IDOnDemandReadings WHERE endpointId in (EID) 
       DELETE toumodeconfirmation WHERE endpointId in (EID) 
       DELETE IDUnCrtdRdgLatestValues WHERE endpointId in (EID) 
       DELETE IntervalDataGaps WHERE endpointId in (EID) 
       DELETE IntervalUncorrectedData WHERE endpointId in (EID) 
       DELETE RFEndpointReadingStats WHERE endpointId in (EID) 
       DELETE GPRSEndpointProperties WHERE endpointId in (EID) 
       DELETE IdReadingLogGaps WHERE endpointId in (EID) 
       
       DELETE EndPointStatusCodeHistory WHERE endpointId in (SELECT hanEndpointId FROM HANDevices WHERE endpointid in (EID) )
       DELETE EndpointProperties WHERE endpointId in (SELECT hanEndpointId FROM HANDevices WHERE endpointid in (EID) )
       DELETE Endpoints WHERE endpointId in (SELECT hanEndpointId FROM HANDevices WHERE endpointid in (EID) )

       DELETE HANDevices WHERE hanEndpointId in (EID) 
       DELETE HANDevices WHERE endpointId in (EID) 
       DELETE EndpointMeterConfiguration WHERE endpointId in (EID) 
       DELETE CollectorTransitionHistory where endpointId in (EID) 
       DELETE EndpointDaySet WHERE endpointId in (EID) 
       DELETE DailyEndpointReadingStats WHERE endpointId in (EID) 
       DELETE LCSDelaySettingTracking WHERE endpointId in (EID) 
       DELETE EdptScrty WHERE endpointId in (EID)  
       DELETE temproutebids WHERE endpointId in (EID) 
       DELETE GapReq WHERE endpointId in (EID)  
       DELETE GapRqstCmds WHERE endpointId in (EID) 
    DELETE SnapReadTimes WHERE endpointId in (EID) 
       DELETE NMMDevice WHERE endpointId in (EID) 
       DELETE EvntGapReq WHERE endpointId in (EID) 

       --Set the best neighbor to null if this endpoint was a best neighbor for some other endpoint
       UPDATE RFEndpointProperties set bestneighborEndpointId = NULL WHERE bestNeighborEndpointId in (EID) 
       UPDATE Collectors SET cltrAppEdptId = NULL WHERE cltrAppEdptId in (EID) 
       
       DECLARE @IPVersion INT
       SELECT @IPVersion = SettingsValue FROM Settings S INNER JOIN 
              SettingsValues SV ON S.SettingsId = SV.SettingsId
              WHERE enumName = 'IPVersion'      
       IF (1 = @IPVersion /* IP Version 4, Update IPAddrs table */) 
       BEGIN
              --     Update the status flag to "not used"
              DECLARE @IPAddr BIGINT  
              SELECT @IPAddr = IPADDR FROM ENDPOINTS  
                     WHERE (EndpointId in (EID)   
                     AND OrganizationId = 1
                     AND EndpointTypeId <> 15) --exclude Cellular
              IF (@IPAddr IS NOT NULL)  
              BEGIN  
                     UPDATE IPAddrs SET AssignedFlg = 0 WHERE IPAddr = @IPAddr
              END    
       END  
       IF(2 = @IPVersion /* IP Version 6, Update IPAddrsV6 table */)
       BEGIN
              DECLARE @IPAddrV6 VARCHAR (45)
              SELECT @IPAddrV6 = IPADDRV6 FROM ENDPOINTS  
                     WHERE (EndpointId in (EID)   
                     AND OrganizationId = 1
                     AND EndpointTypeId <> 15) --exclude Cellular
              IF (@IPAddrV6 IS NOT NULL)  
              BEGIN
                     --     Update the status flag to "not used"              
                     UPDATE IPAddrsV6 SET AssignedFlg = 0 WHERE IPAddr = @IPAddrV6
              END    
       END


       DELETE PLCPacketProcessError WHERE endpointId in (EID) 
       DELETE PLCEdptAsyncPktStateHist WHERE endpointId in (EID) 
       DELETE PLCEdptPktProcessState WHERE endpointId in (EID) 
       DELETE PLCEdptPktProcessStateHist WHERE endpointId in (EID) 
       DELETE PLCEndpointAlarm WHERE endpointId in (EID) 
       DELETE PLCEdptConfigProfile WHERE endpointId in (EID) 
       DELETE FROM PLCWFFindEndpointHist WHERE plcWorkflowId IN (SELECT plcWorkflowId FROM PLCWorkflowHist WHERE endpointId in (EID) )
       DELETE PLCWorkflowHist WHERE endpointId in (EID) 
       DELETE FROM PLCWFFindEndpoint WHERE plcWorkflowId IN (SELECT plcWorkflowId FROM PLCWorkflowHist WHERE endpointId in (EID) )
       DELETE PLCWorkflow WHERE endpointId in (EID) 

       DELETE FROM Endpoints where EndPointId in (EID) 

       -- There may still be an endpoint attached to the meter.
       IF NOT EXISTS(SELECT 1 FROM EndPoints WHERE meterId  in (MID) ) AND
          NOT EXISTS(SELECT 1 FROM IDReadingLogs WHERE meterId  in (MID) )  
       BEGIN
           DELETE stageRebuildMeters FROM stageRebuildMeters WHERE meterId  in (MID) 
           DELETE Meters FROM Meters WHERE meterId  in (MID) 
       END

       -- There may still be MUEndpoints attached to the meter
       
       /* Serial number stuff ignore for now
       IF @serialnumber IS NOT NULL
       BEGIN         
              SELECT @endpointImportTransId = ETI.endpointTransactionImportId FROM EndpointInstallInfoTrans ETI 
                     WHERE ETI.installedEndpointSN = @serialnumber 

              DELETE from EndpointInstallInfoTrans where installedEndpointSN = @serialnumber
              
              DELETE from EndpointTransactionImport 
                     where endpointTransactionImportId = @endpointImportTransId
                     AND NOT EXISTS (SELECT 1 FROM EndpointInstallInfoTrans ETI where ETI.endpointTransactionImportId = @endpointImportTransId)
              
              DELETE from RFUHeadEndRadios 
              where serialNumber = @serialnumber

       DELETE from EdptKeyGen
        where serialNumber = @serialnumber  
 
              DELETE from RouteBIds
              where serialNumber = @serialnumber


       END
       */
       
COMMIT TRAN

{ included in JVCLData.inc (at the bottom) }
// $Id$

{------------------------------------------------------------------------------}
{ QuickReport }
function Opt_QuickReport4(Target: TCompileTarget): Boolean;
begin
  Result := FileExists(Target.RootDir + Format('\bin\qr4run%s%d.bpl', [LowerCase(TargetTypes[Target.IsBCB]), Target.Version]));
end;

function Opt_QuickReport(Target: TCompileTarget): Boolean;
begin
  Result := FileExists(Target.RootDir + Target.VersionedDCPs('\lib\qrpt.dcp')) or
            FileExists(Target.RootDir + Target.VersionedDCPs('\lib\obj\qrpt.dcp')) or
            Opt_QuickReport4(Target);
end;

{------------------------------------------------------------------------------}
{ Internet Components }
function Opt_InternetComponents(Target: TCompileTarget): Boolean;
begin
  Result := (FileExists(Target.RootDir + Target.VersionedDCPs('\lib\vclie.dcp')) and
             FileExists(Target.RootDir + Target.VersionedDCPs('\lib\inet.dcp')))
            or
            (FileExists(Target.RootDir + Target.VersionedDCPs('\lib\obj\vclie.dcp')) and
             FileExists(Target.RootDir + Target.VersionedDCPs('\lib\obj\inet.dcp')));
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

procedure TTargetConfig.UpdateOptions;
begin
  // quickreport configuration
  EnableOption('QREPORT4', Opt_QuickReport4(Target));
  EnableOption('JVCL_UseQuickReport', Opt_QuickReport(Target));

  // internet components
  EnableOption('INTERNET_COMPONENTS', Opt_InternetComponents(Target));
end;


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
  Result := FileExists(Target.RootDir + Target.VersionedPackage('\lib\qrpt.dcp')) or Opt_QuickReport4(Target);
end;

{------------------------------------------------------------------------------}
{ Internet Components }
function Opt_InternetComponents(Target: TCompileTarget): Boolean;
begin
  Result := FileExists(Target.RootDir + Target.VersionedPackage('\lib\vclie.dcp'));
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

procedure TTargetConfig.UpdateJVCLConfig;
begin
  // quickreport configuration
  JVCLConfig.Enabled['QREPORT4'] := JVCLConfig.Enabled['QREPORT4'] and Opt_QuickReport4(Target);
  JVCLConfig.Enabled['JVCL_UseQuickReport'] := JVCLConfig.Enabled['JVCL_UseQuickReport'] and Opt_QuickReport(Target);

  // internet components
  JVCLConfig.Enabled['INTERNET_COMPONENTS'] := JVCLConfig.Enabled['INTERNET_COMPONENTS'] and Opt_InternetComponents(Target);
end;


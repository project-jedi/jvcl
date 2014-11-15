{ included in JVCLData.pas (at the bottom) }
// $Id$

function GetSystemDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetSystemDirectory(PChar(Result), Length(Result)));
end;

{------------------------------------------------------------------------------}
{ QuickReport }
function Opt_QuickReport4(Target: TCompileTarget): Boolean;
begin
  if Target.IsBDS then
    Result := FileExists(Target.RootDir + Format('\bin\qr4run%s%d.bpl', [LowerCase(Target.TargetType), Target.Version]))
  else
    Result := FileExists(Target.RootDir + Format('\bin\qr4run%s%d.bpl', [LowerCase(Target.TargetType), Target.Version]));
end;

function Opt_QuickReport(Target: TCompileTarget): Boolean;
begin
  Result := FileExists(Target.RootDir + Target.VersionedDCP('\lib\qrpt.dcp')) or
            FileExists(Target.RootDir + Target.VersionedDCP('\lib\obj\qrpt.dcp')) or
            Opt_QuickReport4(Target);
end;

{------------------------------------------------------------------------------}
{ Internet Components }
function Opt_InternetComponents(Target: TCompileTarget): Boolean;
begin
  Result := (FileExists(Target.RootDir + Target.VersionedDCP('\lib\vclie.dcp')) and
             FileExists(Target.RootDir + Target.VersionedDCP('\lib\inet.dcp')))
            or
            (FileExists(Target.RootDir + Target.VersionedDCP('\lib\obj\vclie.dcp')) and
             FileExists(Target.RootDir + Target.VersionedDCP('\lib\obj\inet.dcp')));
end;

{------------------------------------------------------------------------------}
{ SMExport Components }
function Opt_SMExportComponents(Target: TCompileTarget): Boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------}
{ SMImport Components }
function Opt_SMImportComponents(Target: TCompileTarget): Boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------}
{ Indy Components }
function Opt_Indy9Components(Target: TCompileTarget): Boolean;
begin
//  Result := FileExists(Target.RootDir + Target.VersionedDCP('\lib\indy.dcp')) or
//            FileExists(Target.RootDir + Target.VersionedDCP('\lib\Indy9\indy.dcp')) or
//            FileExists(Target.RootDir + Target.VersionedDCP('\lib\Indy10\IndyCore.dcp')) or
//            FileExists(Target.RootDir + Target.VersionedDCP('\lib\IndyCore.dcp'))
  Result := true;
end;

function Opt_Indy10Components(Target: TCompileTarget): Boolean;
begin
//  Result := (FileExists(Target.RootDir + Target.VersionedDCP('\lib\Indy10\IndyCore.dcp')) or
//             FileExists(Target.RootDir + Target.VersionedDCP('\lib\IndyCore.dcp'))) and
//            (Target.FindPackageEx('dclIndyCore') <> nil);
  Result := true;
end;

{------------------------------------------------------------------------------}
{ ICS Components }
function Opt_ICSComponents(Target: TCompileTarget): Boolean;
begin
  if not Target.SupportsPersonalities([persDelphi]) then
    Result := False
  else
    if (Target.Version >= 9) then
      Result := FileExists(Target.BDSProjectsDir + Format('\bpl\IcsDel%d%d.dcp', [Target.Version,0]))
    else
      Result := FileExists(Target.RootDir + Format('\Projects\bpl\IcsDel%d%d.dcp', [Target.Version,0]));//Target.VersionedDCP('\Projects\bpl\IcsDel.dcp'));
end;

{------------------------------------------------------------------------------}
{ DevExpress CX-Editors }
function Opt_CXEditorComponents(Target: TCompileTarget): Boolean;
begin
//  if (Target.Version >= 9) then
//    Result := FileExists(GetSystemDir+(Format('\cxEditors%s%d.bpl', [LowerCase(Target.TargetType), Target.Version])))
//  else
//    Result := FileExists(GetSystemDir+(Format('\cxEditorsVCL%s%d.bpl', [LowerCase(Target.TargetType), Target.Version])));
  Result := true;
end;

{------------------------------------------------------------------------------}
{ DevExpress CX-Grid }
function Opt_CXGridComponents(Target: TCompileTarget): Boolean;
begin
//  if (Target.Version >= 9) then
//    Result := FileExists(GetSystemDir+(Format('\cxGrid%s%d.bpl', [LowerCase(Target.TargetType), Target.Version])))
//  else
//    Result := FileExists(GetSystemDir+(Format('\cxGridVCL%s%d.bpl', [LowerCase(Target.TargetType), Target.Version])));
  Result := true;
end;

{------------------------------------------------------------------------------}
{ DOA Components }
function Opt_DOAComponents(Target: TCompileTarget): Boolean;
begin
//  if Target.Version = 9 then
//    Result := FileExists(Target.BDSProjectsDir + '\bpl\doa41d2005.dcp')
//  else if Target.Version = 10 then
//    Result := FileExists(Target.BDSProjectsDir + '\bpl\doa41d2006.dcp')
//  else if Target.Version = 11 then
//    Result := FileExists(Target.BDSProjectsDir + '\bpl\doa41d2007.dcp')
//  else
//    Result := FileExists(Target.RootDir + Format('\Projects\bpl\doa41%s%d.bpl', [LowerCase(Target.TargetType), Target.Version]));
  Result := true;
end;

{------------------------------------------------------------------------------}
{ ODAC Components }
function Opt_ODACComponents(Target: TCompileTarget): Boolean;
begin
//  if Target.Version = 10 then
//    Result := FileExists(Target.RootDir + '\Bin\OdacVcl100.bpl')
//  else if Target.Version = 11 then
//    Result := FileExists(Target.RootDir + '\Bin\OdacVcl105.bpl')
//  else if Target.Version = 12 then
//    Result := FileExists(Target.RootDir + '\Bin\OdacVcl120.bpl')
//  else if Target.Version = 14 then
//    Result := FileExists(Target.RootDir + '\Bin\OdacVcl140.bpl')
//  else if Target.Version = 15 then
//    Result := FileExists(Target.RootDir + '\Bin\OdacVcl150.bpl')
//  else if Target.Version = 16 then
//    Result := FileExists(Target.RootDir + '\Bin\OdacVcl160.bpl')
//  else
//    Result := FileExists(Target.RootDir + Format('\Odac\Bin\OdacVcl%d.bpl', [Target.Version]));
  Result := true;
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

  // Third Party Components
  { Indy is shipped with Delphi 6 and newer. But the Indy9 version that comes
    with Delphi 6 is not interface compatible with the Indy9 that Delphi 7 uses.
    The JVCL's Indy 9 support is based on Delphi7. }
  EnableOption('USE_3RDPARTY_INDY', Opt_Indy9Components(Target){, Target.Version >= 7});
  { Indy 10 is shipped with Delphi 2005 and newer }
  EnableOption('USE_3RDPARTY_INDY10', Opt_Indy10Components(Target){, Target.Version >= 9});

  EnableOption('USE_3RDPARTY_ICS', Opt_ICSComponents(Target));

  EnableOption('USE_3RDPARTY_DOA', Opt_DOAComponents(Target));

  EnableOption('USE_3RDPARTY_DEVART_ODAC', Opt_ODACComponents(Target));

  EnableOption('USE_3RDPARTY_SMIMPORT', Opt_SMImportComponents(Target));
  EnableOption('USE_3RDPARTY_SMEXPORT', Opt_SMExportComponents(Target));

//  EnableOption('USE_3RDPARTY_DEVEXPRESS_CXEDITOR_SHARED', Opt_CXEditorComponents(Target));
//  EnableOption('USE_3RDPARTY_DEVEXPRESS_CXGRID_SHARED', Opt_CXGridComponents(Target));
end;


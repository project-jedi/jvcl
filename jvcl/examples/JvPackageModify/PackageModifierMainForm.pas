{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit PackageModifierMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  EInvalidPackageFormat = class(Exception);
  TPackageModifierMainFrm = class(TForm)
    PageControl1: TPageControl;
    tabOptions: TTabSheet;
    tabFiles: TTabSheet;
    GroupBox1: TGroupBox;
    rbImplicitBuildOff: TRadioButton;
    rbImplicitBuildOn: TRadioButton;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    chkVarStringChecks: TCheckBox;
    chkBoolEval: TCheckBox;
    chkExtendedSyntax: TCheckBox;
    chkTypedAddress: TCheckBox;
    chkOpenStrings: TCheckBox;
    ckhOptimization: TCheckBox;
    chkStackFrames: TCheckBox;
    chkSafeDivide: TCheckBox;
    chkLongStrings: TCheckBox;
    chkWriteableConst: TCheckBox;
    chkOverflowChecks: TCheckBox;
    chkIOChecks: TCheckBox;
    chkRangeChecks: TCheckBox;
    chkDebugInfo: TCheckBox;
    chkLocalSymbols: TCheckBox;
    chkReferenceInfo: TCheckBox;
    chkAssertions: TCheckBox;
    reFiles: TRichEdit;
    btnAdd: TButton;
    OpenDialog1: TOpenDialog;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure reFilesChange(Sender: TObject);
  private
    { Private declarations }
    procedure ModifyPackages;
    function ModifyStrings(Strings:TStrings):boolean;
    procedure LoadSettings;
    procedure SaveSettings;
  protected
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  end;

var
  PackageModifierMainFrm: TPackageModifierMainFrm;

implementation

uses
  IniFiles, ShellAPI;
  
{$R *.dfm}

procedure TPackageModifierMainFrm.btnOKClick(Sender: TObject);
begin
  ModifyPackages;
end;

procedure TPackageModifierMainFrm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TPackageModifierMainFrm.ModifyPackages;
var i:integer;S:TStringlist;
begin
  S := TStringlist.Create;
  try
    // remove duplicate filenames:
    S.Sorted := true;
    for i := 0 to reFiles.Lines.Count - 1 do
      S.Add(reFiles.Lines[i]);
    reFiles.Lines.Assign(S);
    S.Sorted := false;
    S.Clear;
    // now do the real job
    for i := 0 to reFiles.Lines.Count - 1 do
      if FileExists(reFiles.Lines[i]) then
      begin
        S.LoadFromFile(reFiles.Lines[i]);
        try
          if ModifyStrings(S) then
            S.SaveToFile(reFiles.Lines[i]);
        except
          on E:EInvalidPackageFormat do
            EInvalidPackageFormat.CreateFmt('Error in "%s":'#13#10'%s',[reFiles.Lines[i],E.Message]);
        end;
      end;
  finally
    S.Free;
  end;
end;


function TPackageModifierMainFrm.ModifyStrings(Strings: TStrings):boolean;
const
  cDirectives:array[0..17] of PChar =
    ('ASSERTIONS','BOOLEVAL','DEBUGINFO','EXTENDEDSYNTAX','IOCHECKS','LOCALSYMBOLS',
     'LONGSTRINGS','OPENSTRINGS','OPTIMIZATION','OVERFLOWCHECKS','RANGECHECKS',
    'REFERENCEINFO','SAFEDIVIDE','STACKFRAMES','TYPEDADDRESS','VARSTRINGCHECKS',
    'WRITEABLECONST','IMPLICITBUILD');

  cChecked:array [boolean] of PChar = ('OFF','ON');
var
  i:integer;
function SameValue(Directive:integer;IsOn:boolean;var Index:integer; var Changed:boolean):boolean;
var k:integer;tmp:string;
begin
  tmp := ' ' + cChecked[IsOn] + '}';
  for k := 0 to Strings.Count - 1 do
    if Pos(cDirectives[Directive],Strings[k]) > 1 then
    begin
      Index := k;
      // does this directive already have the right value?
      Result := Pos(tmp,Strings[k]) <> 0;
      if Result then
        Changed := true; // only set changed if Result is true
      Exit;
    end;
  raise EInvalidPackageFormat.CreateFmt('Directive "%s" is missing, unable to continue.',[cDirectives[Directive]]);
end;
begin
  Result := false;
  if not SameValue(0,chkAssertions.Checked,i, Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[0],cChecked[chkAssertions.Checked]]);
  if not SameValue(1,chkBoolEval.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[1],cChecked[chkBoolEval.Checked]]);
  if not SameValue(2,chkDebugInfo.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[2],cChecked[chkDebugInfo.Checked]]);
  if not SameValue(3,chkExtendedSyntax.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[3],cChecked[chkExtendedSyntax.Checked]]);
  if not SameValue(4,chkIOChecks.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[4],cChecked[chkIOChecks.Checked]]);
  if not SameValue(5,chkLocalSymbols.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[5],cChecked[chkLocalSymbols.Checked]]);
  if not SameValue(6,chkLongStrings.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[6],cChecked[chkLongStrings.Checked]]);
  if not SameValue(7,chkOpenStrings.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[7],cChecked[chkOpenStrings.Checked]]);
  if not SameValue(8,ckhOptimization.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[8],cChecked[ckhOptimization.Checked]]);
  if not SameValue(9,chkOverflowChecks.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[9],cChecked[chkOverflowChecks.Checked]]);
  if not SameValue(10,chkRangeChecks.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[10],cChecked[chkRangeChecks.Checked]]);
  if not SameValue(11,chkReferenceInfo.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[11],cChecked[chkReferenceInfo.Checked]]);
  if not SameValue(12,chkSafeDivide.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[12],cChecked[chkSafeDivide.Checked]]);
  if not SameValue(13,chkStackFrames.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[13],cChecked[chkStackFrames.Checked]]);
  if not SameValue(14,chkTypedAddress.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[14],cChecked[chkTypedAddress.Checked]]);
  if not SameValue(15,chkVarStringChecks.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[15],cChecked[chkVarStringChecks.Checked]]);
  if not SameValue(16,chkWriteableConst.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[16],cChecked[chkWriteableConst.Checked]]);
  if not SameValue(17,rbImplicitBuildOn.Checked,i,Result) then
    Strings[i] := Format('{$%s %s}',[cDirectives[17],cChecked[rbImplicitBuildOn.Checked]]);
end;

procedure TPackageModifierMainFrm.btnAddClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    reFiles.Lines.AddStrings(OpenDialog1.Files);
end;

procedure TPackageModifierMainFrm.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle,true);
  LoadSettings;
end;

procedure TPackageModifierMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveSettings;
  DragAcceptFiles(Handle,false);
end;

procedure TPackageModifierMainFrm.WMDropFiles(var Message: TWMDropFiles);
var
  Count:integer;buf:array[0..MAX_PATH] of char;
begin
  with Message do
  begin
    Count := DragQueryFile(Drop,DWORD(-1),nil,0) - 1;
    while Count >= 0 do
    begin
      DragQueryFile(Drop,Count,buf,sizeof(buf));
      if FileExists(string(buf)) then
        reFiles.Lines.Add(string(buf));
      Dec(Count);
    end;
    DragFinish(Drop);
  end;
end;

procedure TPackageModifierMainFrm.reFilesChange(Sender: TObject);
begin
  btnOK.Enabled := reFiles.Lines.Count > 0;
end;

procedure TPackageModifierMainFrm.LoadSettings;
begin
  with TIniFile.Create(ChangeFileExt(Application.Exename,'.ini')) do
  try
    rbImplicitBuildOn.Checked := ReadBool('Settings','IMPLICITBUILDON',rbImplicitBuildOn.Checked);
    rbImplicitBuildOff.Checked := not rbImplicitBuildOn.Checked;
    chkVarStringChecks.Checked := ReadBool('Settings','VARSTRINGCHECKS',chkVarStringChecks.Checked);
    chkBoolEval.Checked := ReadBool('Settings','BOOLEVAL',chkBoolEval.Checked);
    chkExtendedSyntax.Checked := ReadBool('Settings','EXTENDEDSYNTAX',chkExtendedSyntax.Checked);
    chkTypedAddress.Checked := ReadBool('Settings','TYPEDADDRESS',chkTypedAddress.Checked);
    chkOpenStrings.Checked := ReadBool('Settings','OPENSTRINGS',chkOpenStrings.Checked);
    ckhOptimization.Checked := ReadBool('Settings','OPTIMIZATION',ckhOptimization.Checked);
    chkStackFrames.Checked := ReadBool('Settings','STACKFRAMES',chkStackFrames.Checked);
    chkSafeDivide.Checked := ReadBool('Settings','SAFEDIVIDE',chkSafeDivide.Checked);
    chkLongStrings.Checked := ReadBool('Settings','LONGSTRINGS',chkLongStrings.Checked);
    chkWriteableConst.Checked := ReadBool('Settings','WRITEABLECONST',chkWriteableConst.Checked);
    chkOverflowChecks.Checked := ReadBool('Settings','OVERFLOWCHECKS',chkOverflowChecks.Checked);
    chkIOChecks.Checked := ReadBool('Settings','IOCHECKS',chkIOChecks.Checked);
    chkRangeChecks.Checked := ReadBool('Settings','RANGECHECKS',chkRangeChecks.Checked);
    chkDebugInfo.Checked := ReadBool('Settings','DEBUGINFO',chkDebugInfo.Checked);
    chkLocalSymbols.Checked := ReadBool('Settings','LOCALSYMBOLS',chkLocalSymbols.Checked);
    chkReferenceInfo.Checked := ReadBool('Settings','REFERENCEINFO',chkReferenceInfo.Checked);
    chkAssertions.Checked := ReadBool('Settings','ASSERTIONS',chkAssertions.Checked);
    ReadSection('Files',reFiles.Lines);
  finally
    Free;
  end;
end;

procedure TPackageModifierMainFrm.SaveSettings;
var i:integer;
begin
  with TIniFile.Create(ChangeFileExt(Application.Exename,'.ini')) do
  try
    WriteBool('Settings','IMPLICITBUILDON',rbImplicitBuildOn.Checked);
    WriteBool('Settings','VARSTRINGCHECKS',chkVarStringChecks.Checked);
    WriteBool('Settings','BOOLEVAL',chkBoolEval.Checked);
    WriteBool('Settings','EXTENDEDSYNTAX',chkExtendedSyntax.Checked);
    WriteBool('Settings','TYPEDADDRESS',chkTypedAddress.Checked);
    WriteBool('Settings','OPENSTRINGS',chkOpenStrings.Checked);
    WriteBool('Settings','OPTIMIZATION',ckhOptimization.Checked);
    WriteBool('Settings','STACKFRAMES',chkStackFrames.Checked);
    WriteBool('Settings','SAFEDIVIDE',chkSafeDivide.Checked);
    WriteBool('Settings','LONGSTRINGS',chkLongStrings.Checked);
    WriteBool('Settings','WRITEABLECONST',chkWriteableConst.Checked);
    WriteBool('Settings','OVERFLOWCHECKS',chkOverflowChecks.Checked);
    WriteBool('Settings','IOCHECKS',chkIOChecks.Checked);
    WriteBool('Settings','RANGECHECKS',chkRangeChecks.Checked);
    WriteBool('Settings','DEBUGINFO',chkDebugInfo.Checked);
    WriteBool('Settings','LOCALSYMBOLS',chkLocalSymbols.Checked);
    WriteBool('Settings','REFERENCEINFO',chkReferenceInfo.Checked);
    WriteBool('Settings','ASSERTIONS',chkAssertions.Checked);
    EraseSection('Files');
    for i := 0 to reFiles.Lines.Count - 1 do
      WriteString('Files',reFiles.Lines[i],'');
  finally
    Free;
  end;
end;

end.

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

{$I jvcl.inc}

unit fReports;

interface

uses
  Windows, Messages, SysUtils, Forms, Classes, Controls, JvInterpreter, JvInterpreterFm, JvComponent,
  JvFormPlacement, StdCtrls, JvHTControls, JvExStdCtrls;

type
  TReports = class(TForm)
    RAhtLabel1: TJvHTLabel;
    lbReports: TJvHtListBox;
    bReport: TButton;
    RegAuto2: TJvFormStorage;
    JvInterpreterFm1: TJvInterpreterFm;
    bCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure bReportClick(Sender: TObject);
    procedure JvInterpreterFm1GetValue(Sender: TObject; Identifer: string;
      var Value: Variant; Args: TJvInterpreterArgs; var Done: boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure Show;

implementation

uses
  {$IFDEF COMPILER6_UP} Variants, {$ENDIF}
  JvJVCLUtils, JvJCLUtils, JvInterpreter_all, JvInterpreter_Quickrpt;

{$R *.DFM}

procedure Show;
begin
  with TReports.Create(Application) do
  try
    ShowModal;
  finally { wrap up }
    Free;
  end; { try/finally }
end;

procedure TReports.FormCreate(Sender: TObject);
{
var
  Ss, Ss2: TStringlist;
  i: integer;
}
begin
(*
//!!!  RegAuto2.IniStrings.Clear;
  Ss := TStringlist.Create;
  Ss2 := TStringlist.Create;
  try
    ReadFolder(ExePath + 'Reports', '*.ini', Ss);
    Ss.Sort;
    for i := 0 to Ss.Count - 1 do { Iterate }
    begin
      Ss2.LoadFromFile(ExePath + 'Reports\' + Ss[i]);
      RegAuto2.IniStrings.AddStrings(Ss2);
    end; { for }
  finally { wrap up }
    Ss.Free;
    Ss2.Free;
  end; { try/finally }

  RegAuto2.ReadSection('Reports', lbReports.Items);
  if lbReports.Items.Count = 0 then
    RAhtLabel1.Caption := 'There are no reports';
  *)
//  RAhtLabel1.Caption := '<b><c:Red>ERROR:<c:WindowText></b> Fail reading file <b>' + RegAuto2.IniFile;
end;

procedure RunReport(JvInterpreterFmRunner: TJvInterpreterFm; const FileName: TFileName);
var
  F1: TFileName;
begin
  F1 := ChangeFileExt(AddPath(FileName, ExePath + 'Reports\'), '.pas');
  if not FileExists(F1) then
    raise Exception.Create('Can''t load report. '#13 +
      'File "' + F1 + '" not found.');
  JvInterpreterRunReportPreview(F1);
end; { RunReport }

procedure TReports.bReportClick(Sender: TObject);
{
var
  F1: TFileName;
  T1: string;
}
begin
(*
//!!!
  if lbReports.ItemIndex < 0 then Exit;
  F1 := Trim(RegAuto2.ReadString(lbReports.Items[lbReports.ItemIndex], 'File', ''));
  T1 := Trim(RegAuto2.ReadString(lbReports.Items[lbReports.ItemIndex], 'Type', 'Report'));
  F1 := ChangeFileExt(AddPath(F1, ExePath + 'Reports\'), '.pas');
  if not FileExists(F1) then
    raise Exception.Create('Can''t load report. '#13 +
      'File "' + F1 + '" not found.');
  if Cmp(T1, 'report') then
  begin
    RunReport(JvInterpreterFm1, F1);
  end
  else if Cmp(T1, 'formmodal') then
    JvInterpreterFm1.RunFormModal(F1)
  else
    raise Exception.Create('Unknown report type.');
*)    
end;

procedure TReports.JvInterpreterFm1GetValue(Sender: TObject; Identifer: string;
  var Value: Variant; Args: TJvInterpreterArgs; var Done: boolean);
var
  JvInterpreterFmRunner: TJvInterpreterFm;
begin
  if Cmp(Identifer, 'RunReport') then
  begin
    JvInterpreterFmRunner := TJvInterpreterFm.Create(Self);
    try
      RunReport(JvInterpreterFmRunner, Args.Values[0]);
    finally { wrap up }
      JvInterpreterFmRunner.Free;
    end; { try/finally }
    Value := NULL;
    Done := true;
  end;
end;

end.


{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing

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

unit OtherStandAlone;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvLabel, ComCtrls, JvExControls, JvComponent;

type
  TOtherMainForm = class(TForm)
    JvHotLink1: TJvLabel;
    Label1: TLabel;
    JvHotLink2: TJvLabel;
    JvHotLink4: TJvLabel;
    JvHotLink3: TJvLabel;
    JvHotLink5: TJvLabel;
    JvHotLink6: TJvLabel;
    JvHotLink7: TJvLabel;
    JvHotLink8: TJvLabel;
    JvHotLink9: TJvLabel;
    JvHotLink10: TJvLabel;
    Label2: TLabel;
    JvHotLink11: TJvLabel;
    JvHotLink12: TJvLabel;
    JvHotLink13: TJvLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    JvHotLink14: TJvLabel;
    JvHotLink15: TJvLabel;
    JvHotLink16: TJvLabel;
    Label6: TLabel;
    JvHotLink17: TJvLabel;
    JvHotLink18: TJvLabel;
    JvHotLink19: TJvLabel;
    Label7: TLabel;
    StatusBar: TStatusBar;
    Label8: TLabel;
    JvHotLink22: TJvLabel;
    JvHotLink23: TJvLabel;
    JvHotLink24: TJvLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    JvLabel1: TJvLabel;
    JvLabel2: TJvLabel;
    Label13: TLabel;
    JvLabel3: TJvLabel;
    Label14: TLabel;
    JvLabel4: TJvLabel;
    Label15: TLabel;
    procedure JvHotLinkMouseEnter(Sender: TObject);
    procedure JvHotLinkMouseLeave(Sender: TObject);
    procedure JvHotLinkClick(Sender: TObject);
    procedure JvHotLinkBuildJVCLClick(Sender: TObject);
    procedure JvLabelMouseEnter(Sender: TObject);
    procedure JvLabelClick(Sender: TObject);
    procedure JvLabelMouseLeave(Sender: TObject);
  end;

var
  OtherMainForm: TOtherMainForm;

implementation

uses
  {$IFNDEF COMPILER6_UP}
  FileCtrl,
  {$ENDIF}
  ShellAPI, JclStrings;

{$R *.dfm}

procedure TOtherMainForm.JvHotLinkMouseEnter(Sender: TObject);
var
  fileName : String;
  hotLnk : TJvLabel;
begin
 hotLnk := sender as TJvLabel;

 fileName := ExtractFilePath(Application.ExeName) + (hotLnk).Url;

 if not fileExists(fileName) then
 begin
   hotLnk.font.Color := clred;
   StatusBar.SimpleText := 'file ''' + fileName + ''' not found: run ''Build all JVCL examples now''';
 end
 else
 begin
   hotLnk.font.Color := clGreen;
   StatusBar.SimpleText := 'click to start file ''' + fileName + '''';
 end;
end;

procedure TOtherMainForm.JvHotLinkMouseLeave(Sender: TObject);
var
  hotLnk : TJvLabel;
begin
  hotLnk := sender as TJvLabel;
  if hotLnk.Tag = 1 then // already visited
    hotLnk.font.color := clNavy
  else
    hotLnk.font.color := clBlue;
end;

procedure TOtherMainForm.JvHotLinkClick(Sender: TObject);
var
  fileName : String;
  hotLnk : TJvLabel;
begin
 hotLnk := sender as TJvLabel;

 fileName := ExtractFilePath(Application.ExeName) + (hotLnk).Url;
 if fileExists(fileName) then
 begin
   hotLnk.font.Color := clNavy; // now it is visited
   hotLnk.Tag := 1;
   ShellExecute(0, nil, PChar('"' +fileName+ '"'), nil, nil, SW_SHOWNORMAL);
 end
 else
 begin
   MessageDlg('File "' + fileName + ' " was not found!', mtError, [mbOK], 0);
   hotLnk.font.Color := clBlue;
 end;
end;

procedure TOtherMainForm.JvHotLinkBuildJVCLClick(Sender: TObject);
var
  fileName : String;
  filePath : String;
begin

 fileName := ExtractFilePath(Application.ExeName) + 'examples\CompileExamples.bat';
 StrReplace(fileName, '\bin', '', [rfIgnoreCase]);

 filePath := extractFilePath(fileName);
 ChDir(filePath);

 if fileExists(fileName) then
   ShellExecute(0, nil, pChar (fileName), nil, pCHar(filePath), SW_NORMAL);

end;

procedure TOtherMainForm.JvLabelMouseEnter(Sender: TObject);
var
  fileName : String;
  aJvLbl : TJvLabel;
begin
 aJvLbl := sender as TJvLabel;

 fileName := ExtractFilePath(Application.ExeName) + (aJvLbl).hint;
 StrReplace(fileName, 'jvcl\bin\', '', [rfIgnoreCase]);

 if not fileExists(fileName) then
 begin
   aJvLbl.font.Color := clred;
   StatusBar.SimpleText := 'file ''' + fileName + ''' not found';
 end
 else
 begin
   aJvLbl.font.Color := clGreen;
   StatusBar.SimpleText := 'click to start file ''' + fileName + '''';
 end;

end;

procedure TOtherMainForm.JvLabelClick(Sender: TObject);
var
  fileOrDirName : String;
  aJvLbl : TJvLabel;
begin
 aJvLbl := sender as TJvLabel;

 fileOrDirName := ExtractFilePath(Application.ExeName) + (aJvLbl).hint;
 StrReplace(fileOrDirName, 'jvcl\bin\', '', [rfIgnoreCase]);

 if DirectoryExists(fileOrDirName) or FileExists(fileOrDirName) then
 begin
   aJvLbl.font.Color := clNavy; // now it is visited
   aJvLbl.Tag := 1;
   ShellExecute(0, nil, pChar (fileOrDirName), nil, nil, SW_NORMAL);
 end
 else
   aJvLbl.font.Color := clBlue;
end;

procedure TOtherMainForm.JvLabelMouseLeave(Sender: TObject);
var
  aJvLbl : TJvLabel;
begin
  aJvLbl := sender as TJvLabel;
  if aJvLbl.Tag = 1 then // already visited
    aJvLbl.font.color := clNavy
  else
    aJvLbl.font.color := clBlue;
end;

end.






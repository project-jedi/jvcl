unit OtherStandAlone;
{$I JVCL.INC}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvLabel, JvHotLink, ComCtrls;

type
  TOtherMainForm = class(TForm)
    JvHotLink1: TJvHotLink;
    Label1: TLabel;
    JvHotLink2: TJvHotLink;
    JvHotLink4: TJvHotLink;
    JvHotLink3: TJvHotLink;
    JvHotLink5: TJvHotLink;
    JvHotLink6: TJvHotLink;
    JvHotLink7: TJvHotLink;
    JvHotLink8: TJvHotLink;
    JvHotLink9: TJvHotLink;
    JvHotLink10: TJvHotLink;
    Label2: TLabel;
    JvHotLink11: TJvHotLink;
    JvHotLink12: TJvHotLink;
    JvHotLink13: TJvHotLink;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    JvHotLink14: TJvHotLink;
    JvHotLink15: TJvHotLink;
    JvHotLink16: TJvHotLink;
    Label6: TLabel;
    JvHotLink17: TJvHotLink;
    JvHotLink18: TJvHotLink;
    JvHotLink19: TJvHotLink;
    Label7: TLabel;
    StatusBar: TStatusBar;
    Label8: TLabel;
    JvHotLink22: TJvHotLink;
    JvHotLink23: TJvHotLink;
    JvHotLink24: TJvHotLink;
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
  hotLnk : TJvHotLink;
begin
 hotLnk := sender as TJvHotLink;

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
  hotLnk : TJvHotLink;
begin
  hotLnk := sender as TJvHotLink;
  if hotLnk.Tag = 1 then // already visited
    hotLnk.font.color := clNavy
  else
    hotLnk.font.color := clBlue;
end;

procedure TOtherMainForm.JvHotLinkClick(Sender: TObject);
var
  fileName : String;
  hotLnk : TJvHotLink;
begin
 hotLnk := sender as TJvHotLink;

 fileName := ExtractFilePath(Application.ExeName) + (hotLnk).Url;
 if fileExists(fileName) then
 begin
   hotLnk.font.Color := clNavy; // now it is visited
   hotLnk.Tag := 1;
 end
 else
   hotLnk.font.Color := clBlue;
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



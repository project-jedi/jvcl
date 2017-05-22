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

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvMultiHttpGrabber, StdCtrls, 
  ActnList, Menus, ComCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    JvMultiHttpGrabber1: TJvMultiHttpGrabber;
    Panel1: TPanel;
    Label1: TLabel;
    btnDownload: TButton;
    gbContent: TGroupBox;
    reContent: TRichEdit;
    gnLog: TGroupBox;
    reLog: TRichEdit;
    Splitter1: TSplitter;
    cbURL: TComboBox;
    btnDownloadAll: TButton;
    sbMain: TStatusBar;
    pbProgress: TProgressBar;
    acMainActions: TActionList;
    acDownload: TAction;
    acDownloadAll: TAction;
    acURLAdd: TAction;
    acURLDelete: TAction;
    acClearLog: TAction;
    acClearContent: TAction;
    popMain: TPopupMenu;
    Addtohistory1: TMenuItem;
    Deletefromhistory1: TMenuItem;
    N1: TMenuItem;
    Download1: TMenuItem;
    Downloadall1: TMenuItem;
    N2: TMenuItem;
    Clearcontent1: TMenuItem;
    Clearlog1: TMenuItem;
    acSaveToFile: TAction;
    N3: TMenuItem;
    acSaveToFile1: TMenuItem;
    Options1: TMenuItem;
    procedure JvMultiHttpGrabber1ClosedConnection(Sender: TObject;
      UserData: Integer; Url: String);
    procedure JvMultiHttpGrabber1ClosingConnection(Sender: TObject;
      UserData: Integer; Url: String);
    procedure JvMultiHttpGrabber1ConnectedToServer(Sender: TObject;
      UserData: Integer; Url: String);
    procedure JvMultiHttpGrabber1ConnectingToServer(Sender: TObject;
      UserData: Integer; Url: String);
    procedure JvMultiHttpGrabber1DoneStream(Sender: TObject;
      UserData: Integer; Stream: TStream; StreamSize: Integer;
      Url: String);
    procedure JvMultiHttpGrabber1Error(Sender: TObject; UserData: Integer;
      Url, Error: String);
    procedure JvMultiHttpGrabber1Progress(Sender: TObject; UserData,
      Position, TotalSize: Integer; Url: String; var Continue: Boolean);
    procedure JvMultiHttpGrabber1ReceivedResponse(Sender: TObject;
      UserData: Integer; Url: String; DataSize: Integer);
    procedure JvMultiHttpGrabber1ReceivingResponse(Sender: TObject;
      UserData: Integer; Url: String);
    procedure JvMultiHttpGrabber1Redirect(Sender: TObject;
      UserData: Integer; Url, NewUrl: String);
    procedure JvMultiHttpGrabber1RequestComplete(Sender: TObject;
      UserData: Integer; Url: String);
    procedure JvMultiHttpGrabber1RequestSent(Sender: TObject;
      UserData: Integer; Url: String; DataSize: Integer);
    procedure JvMultiHttpGrabber1ResolvedName(Sender: TObject;
      UserData: Integer; Url, Name: String);
    procedure JvMultiHttpGrabber1ResolvingName(Sender: TObject;
      UserData: Integer; Url: String);
    procedure JvMultiHttpGrabber1SendingRequest(Sender: TObject;
      UserData: Integer; Url: String);
    procedure FormCreate(Sender: TObject);
    procedure sbMainResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acDownloadExecute(Sender: TObject);
    procedure acDownloadAllExecute(Sender: TObject);
    procedure acMainActionsUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acURLAddExecute(Sender: TObject);
    procedure acURLDeleteExecute(Sender: TObject);
    procedure acClearLogExecute(Sender: TObject);
    procedure acClearContentExecute(Sender: TObject);
    procedure JvMultiHttpGrabber1DoneFile(Sender: TObject;
      UserData: Integer; FileName: String; FileSize: Integer; Url: String);
    procedure acSaveToFileExecute(Sender: TObject);
  private
    { Private declarations }
    procedure LoadSettings;
    procedure SaveSettings;
    function GetColor(UserData:integer):TColor;
    function MakeFileName(URL:string):string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  IniFiles, JvTypes;
{$R *.dfm}

procedure TForm1.JvMultiHttpGrabber1ClosedConnection(Sender: TObject;
  UserData: Integer; Url: String);
begin
  reLog.Lines.Add(Format('Closed Connection to %s',[Url]));
end;

procedure TForm1.JvMultiHttpGrabber1ClosingConnection(Sender: TObject;
  UserData: Integer; Url: String);
begin
  reLog.Lines.Add(Format('Closing Connection to %s',[Url]));
end;

procedure TForm1.JvMultiHttpGrabber1ConnectedToServer(Sender: TObject;
  UserData: Integer; Url: String);
begin
  reLog.Lines.Add(Format('Connected to %s',[Url]));
end;

procedure TForm1.JvMultiHttpGrabber1ConnectingToServer(Sender: TObject;
  UserData: Integer; Url: String);
begin
  reLog.Lines.Add(Format('Connecting to %s',[Url]));
end;

procedure TForm1.JvMultiHttpGrabber1DoneStream(Sender: TObject;
  UserData: Integer; Stream: TStream; StreamSize: Integer;
  Url: String);
var S:String;
begin
  reLog.Lines.Add(Format('Download complete from %s',[Url]));
//  Stream.Seek(0,soFromBeginning); // (p3) not needed, already reset 
  if UserData < 0 then // single download
  begin
    reContent.DefAttributes.Color := GetColor(UserData);
    reContent.Lines.LoadFromStream(Stream);
    reContent.Lines.Insert(0,'¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ' + Url + ' ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤');
  end
  else if StreamSize > 0 then // multi download
  begin
    SetLength(S,StreamSize);
    Stream.Read(S[1],StreamSize);
    reContent.SelAttributes.Color := GetColor(UserData);
    reContent.SelText := #13#10'¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ' + Url + ' ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤' + #13#10 + S;
  end;
  pbProgress.Position := 0;
  sbMain.Panels[1].Text := '  Ready';
end;

procedure TForm1.JvMultiHttpGrabber1Error(Sender: TObject;
  UserData: Integer; Url, Error: String);
begin
  reLog.Lines.Add(Format('Error at %s:'#13#10'%s',[Url, Error]));
end;

procedure TForm1.JvMultiHttpGrabber1Progress(Sender: TObject; UserData,
  Position, TotalSize: Integer; Url: String; var Continue: Boolean);
begin
  if TotalSize > 0 then
    pbProgress.Max := TotalSize
  else
    pbProgress.Max := Position;
  pbProgress.Position := Position;
  sbMain.Panels[1].Text := Format(' %dkB of %dkB completed',[Position div 1024, pbProgress.Max div 1024]);
//  pbProgress.Visible := Position > 0;
  pbProgress.Update;
  sbMain.Update;
end;

procedure TForm1.JvMultiHttpGrabber1ReceivedResponse(Sender: TObject;
  UserData: Integer; Url: String; DataSize: Integer);
begin
  reLog.Lines.Add(Format('Size of %s is %d',[Url, DataSize]));
end;

procedure TForm1.JvMultiHttpGrabber1ReceivingResponse(Sender: TObject;
  UserData: Integer; Url: String);
begin
  reLog.Lines.Add(Format('Recieving response from %s',[Url]));
end;

procedure TForm1.JvMultiHttpGrabber1Redirect(Sender: TObject;
  UserData: Integer; Url, NewUrl: String);
begin
  reLog.Lines.Add(Format('Redirected from %s to %s',[Url, NewUrl]));
end;

procedure TForm1.JvMultiHttpGrabber1RequestComplete(Sender: TObject;
  UserData: Integer; Url: String);
begin
  reLog.Lines.Add(Format('Request completed on %s',[Url]));
end;

procedure TForm1.JvMultiHttpGrabber1RequestSent(Sender: TObject;
  UserData: Integer; Url: String; DataSize: Integer);
begin
  reLog.Lines.Add(Format('Request sent to %s',[Url]));
end;

procedure TForm1.JvMultiHttpGrabber1ResolvedName(Sender: TObject;
  UserData: Integer; Url, Name: String);
begin
  reLog.Lines.Add(Format('Name %s resolved on %s',[Name, Url]));
end;

procedure TForm1.JvMultiHttpGrabber1ResolvingName(Sender: TObject;
  UserData: Integer; Url: String);
begin
  reLog.Lines.Add(Format('Resolving %s',[Url]));
end;

procedure TForm1.JvMultiHttpGrabber1SendingRequest(Sender: TObject;
  UserData: Integer; Url: String);
begin
  reLog.Lines.Add(Format('Sending request to %s',[Url]));
end;

function TForm1.GetColor(UserData: integer): TColor;
const
  cDataColorCount = 10;
  cDataColors:array [0..cDataColorCount-1] of TColor =
  (clMaroon, clNavy, clRed, clBlue, clPurple, clBlack, clGray, clTeal, clGreen, clOlive);
begin
  if UserData < 0 then
    Result := cDataColors[0]
  else
    Result := cDataColors[UserData mod cDataColorCount];
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadSettings;
  pbProgress.Parent := sbMain;
end;

procedure TForm1.sbMainResize(Sender: TObject);
begin
  pbProgress.SetBounds(2,2,sbMain.Panels[0].Width - 2,sbMain.ClientHeight - 2);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveSettings;
end;

procedure TForm1.LoadSettings;
begin
  with TIniFile.Create(ChangeFileExt(Application.Exename, '.ini')) do
  try
    if SectionExists('URL History') then // reset to default if key doesn't exist
    begin
      ReadSection('URL History',cbURL.Items);
      cbURL.ItemIndex := 0;
      acSaveToFile.Checked := ReadBool('Settings','Save To File',acSaveToFile.Checked);
    end;
  finally
    Free;
  end;
  reContent.WordWrap := true;
end;

procedure TForm1.SaveSettings;
var i:integer;
begin
  with TIniFile.Create(ChangeFileExt(Application.Exename, '.ini')) do
  try
    EraseSection('URL History');
    for i := 0 to cbURL.Items.Count - 1 do
      WriteString('URL History',cbURL.Items[i],'');
    WriteBool('Settings','Save To File',acSaveToFile.Checked);
  finally
    Free;
  end;
end;

procedure TForm1.acDownloadExecute(Sender: TObject);
begin
  reContent.WordWrap := false;
  pbProgress.Position := 0;
  sbMain.Panels[1].Text := '';
  acURLAdd.Execute;
  if acSaveToFile.Checked then
    JvMultiHttpGrabber1.OutputMode := omFile
  else
    JvMultiHttpGrabber1.OutputMode := omStream;
  JvMultiHttpGrabber1.Url := cbURL.Text;
  JvMultiHttpGrabber1.Filename := MakeFileName(cbURL.Text);
  JvMultiHttpGrabber1.Download(-1);
end;

procedure TForm1.acDownloadAllExecute(Sender: TObject);
var i:integer;
begin
  reContent.WordWrap := false;
  acClearContent.Execute;
  acURLAdd.Execute;
  if acSaveToFile.Checked then
    JvMultiHttpGrabber1.OutputMode := omFile
  else
    JvMultiHttpGrabber1.OutputMode := omStream;
  for i := 0 to cbURL.Items.Count - 1 do
  begin
    JvMultiHttpGrabber1.Url := cbURL.Items[i];
    JvMultiHttpGrabber1.Filename := MakeFileName(cbURL.Items[i]);
    JvMultiHttpGrabber1.Download(i);
  end;
end;

procedure TForm1.acMainActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if JvMultiHttpGrabber1.Working then
    Screen.Cursor := crAppStart
  else
    Screen.Cursor := crDefault;
  acDownload.Enabled := (cbURL.Text <> '') and not JvMultiHttpGrabber1.Working;
  acDownloadAll.Enabled := cbURL.Items.Count > 1;
//  pbProgress.Visible := JvMultiHttpGrabber1.Working;
  acURLAdd.Enabled := cbURL.Text <> '';
  acURLDelete.Enabled := acURLAdd.Enabled;
end;

procedure TForm1.acURLAddExecute(Sender: TObject);
begin
  if cbURL.Items.IndexOf(cbURL.Text) < 0 then
    cbURL.Items.Add(cbURL.Text);
end;

procedure TForm1.acURLDeleteExecute(Sender: TObject);
var i:integer;
begin
  i := cbURL.Items.IndexOf(cbURL.Text);
  if i >= 0 then cbURL.Items.Delete(i);
  cbURL.Text := '';
  if i >= cbURL.Items.Count then Dec(i);
  if cbURL.ItemIndex < 0 then
    cbURL.ItemIndex := i;
end;

procedure TForm1.acClearLogExecute(Sender: TObject);
begin
  reLog.Clear;
end;

procedure TForm1.acClearContentExecute(Sender: TObject);
begin
  reContent.Clear;
end;

procedure TForm1.JvMultiHttpGrabber1DoneFile(Sender: TObject;
  UserData: Integer; FileName: String; FileSize: Integer; Url: String);
begin
  reLog.Lines.Add(Format('%s was saved to file %s',[Url, Filename]));
  pbProgress.Position := 0;
end;

function TForm1.MakeFileName(URL: string): string;
var i:integer;
begin
  if Pos('http://',AnsiLowerCase(URL)) = 1 then
    i := Length('http://') + 1
  else
    i := 1;
  Result := '';
  while i <= Length(URL) do
  begin
    if URL[i] in [':','/','.'] then
      Result := Result + '_'
    else
      Result := Result + URL[i];
    Inc(i);
  end;
  if Result = '' then
    Result := 'default.htm';
  Result := ExtractFilePath(Application.ExeName) + ChangeFileExt(Result,'.htm');
end;

procedure TForm1.acSaveToFileExecute(Sender: TObject);
begin
  acSaveToFile.Checked := not acSaveToFile.Checked;
end;

end.

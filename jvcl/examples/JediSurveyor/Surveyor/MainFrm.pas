{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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
  Dialogs, StdCtrls, ExtCtrls, JvScrollBox, ComCtrls, JvProgressBar,
  JvBevel, Buttons, JvBitBtn, JvFooter, JvComponent, JvSurveyIntf,
  JvDialogs, ImgList, ActnList, JvActions, JvLinkLabel,
  JvRadioButton, JvCheckBox, JvMemo, Menus, JvExForms, JvExButtons,
  JvExExtCtrls, JvExControls, JvImageSquare;

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    lblTitle: TLabel;
    JvFooter1: TJvFooter;
    btnPrev: TJvFooterBtn;
    btnNext: TJvFooterBtn;
    btnClose: TJvFooterBtn;
    sbSurvey: TJvScrollBox;
    OpenSurveyDialog: TJvOpenDialog;
    JediLogo: TJvImageSquare;
    il48: TImageList;
    alMain: TActionList;
    acStartPage: TAction;
    acPrevPage: TAction;
    acNextPage: TAction;
    acLoadSurvey: TAction;
    acSendMail: TJvSendMailAction;
    acGotoJVCL: TJvWebAction;
    acLastPage: TAction;
    acExit: TAction;
    JvBevel1: TJvBevel;
    lblProgress: TLabel;
    lblDescription: TJvLinkLabel;
    lblSurveyTitle: TLabel;
    btnOpen: TButton;
    popMultiple: TPopupMenu;
    acCheckAll: TAction;
    acUncheckAll: TAction;
    acInvert: TAction;
    acCheckFirst: TAction;
    acCheckLast: TAction;
    popExclusive: TPopupMenu;
    btnComment: TButton;
    acComment: TAction;
    acAbout: TAction;
    popSend: TPopupMenu;
    acSaveSurvey: TAction;
    SaveSurveyDialog: TJvSaveDialog;
    Save1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acStartPageExecute(Sender: TObject);
    procedure acLastPageExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acPrevPageExecute(Sender: TObject);
    procedure alMainUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acNextPageExecute(Sender: TObject);
    procedure acLoadSurveyExecute(Sender: TObject);
    procedure lblDescriptionLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText: String);
    procedure acCheckAllExecute(Sender: TObject);
    procedure acUncheckAllExecute(Sender: TObject);
    procedure acInvertExecute(Sender: TObject);
    procedure acCheckFirstExecute(Sender: TObject);
    procedure acCheckLastExecute(Sender: TObject);
    procedure acCommentExecute(Sender: TObject);
    procedure sbSurveyContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure FormContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure acAboutExecute(Sender: TObject);
    procedure acSaveSurveyExecute(Sender: TObject);
  private
    FFilename,FTempSurveyFilename: string;
    FCompletedSurvey:boolean;
    FSurvey: IJvSurvey;
    FPageIndex: integer;
    edUserName, edUserEMail: TEdit;
    procedure DoExclusiveClick(Sender: TObject);
    procedure DoMultipleClick(Sender: TObject);

    procedure CreateEverything;
    procedure LoadSettings;
    procedure ReadCommandLine;
    procedure LoadSurvey(const AFilename: string);
    function GetTempSurveyFileName: string;
    procedure PromptForSurveyFile(const AFilename: string);
    procedure SaveSettings;
    procedure FreeEverything;
    procedure ClearScrollBox;
    procedure BuildExclusivePopUpMenu;
    procedure BuildMultiplePopUpMenu;

    function CheckPage(Index:integer): boolean;
    procedure StartPage;
    procedure EndPage;
    procedure CreateExclusivePage(Index:integer);
    procedure CreateMultiplePage(Index:integer);
    procedure CreateFreeFormPage(Index:integer);
    procedure SavePage;
    procedure UpdateProgress;
    procedure CreatePage(Index: integer);
    procedure DoSendMail(Sender: TObject);
    property Filename: string read FFilename write FFilename;
  public
  end;

var
  frmMain: TfrmMain;

resourcestring
  SMainFormCaptionFmt = 'JEDI Surveyor - (%s)';
  SDefaultSurveyName = 'No survey loaded!';
  SStartPageTitle = 'Welcome to the JEDI Surveyor';
  SStartPageDescription = 'The JEDI Surveyor is a tool to collect user input on various issues.' +
    ' Currently no survey is loaded. To load a survey, click the "Open" button and select a survey file from the dialog.';
  SEndPageTitle = 'Survey completed';
  SEndPageDescriptionFmt =
    'Click the "Send" button below to send an e-mail to %s (<link>mailto:%s</link>) with your answers. ' +
    'The results of the survey will be made available at the following location: <link>%s</link>. Enter a username and and e-mail address to help us keep track of participants (optional).';
  SExpiredSurveyCaption = 'Survey expired!';
  SExpiredSurveyTextFmt = 'This survey expired on %s, would you like to view it anyway?';
  SErrSurveyImplementationNotFound = 'Fatal Error: No IJvSurvey implementation found, cannot continue!';
  SUsername = '&Username:';
  SEmail = '&E-mail address:';
  SSend = '&Send';
  SSaveResponseCaption = 'Save responses';
  SSaveResponsePrompt = 'You haven''t completed the survey. Do you want to save your responses this far (responses are saved in the original survey file)?';
  SPageOfPageFmt = 'Page %d of %d';
  SDeleteResponseCaption = 'Delete response file';
  SFmtDeleteResponsePrompt = 'Do you want to delete the response file (%s)?';
  SAboutText = 'JEDI Surveyor version 1.0';
  SAboutTitle = 'About Surveyor...';

const
  cStartOffset = 24;
  cDefaultControlWidth = 125;

implementation

uses
  ComObj, Math, JclStrings, JclSysInfo,
  JvJCLUtils, JvSurveyUtils, CommentFrm;

{$R *.dfm}

function TextSize(Canvas: TCanvas; DefaultHeight:integer; const S: string): TSize;
var
  i: integer;
  T:TStringlist;
begin
  Result.cx := 0;
  Result.cy := 0;
  T := TStringlist.Create;
  try
    T.Text := S;
    for i := 0 to T.Count - 1 do
    begin
      Result.cx := Max(Result.cx, Canvas.TextWidth(T[i]));
      Inc(Result.cy, DefaultHeight);
    end;
  finally
    T.Free;
  end;
  if Result.cy <= 0 then Result.cy := DefaultHeight;
end;

{ TfrmMain }

procedure TfrmMain.CreatePage(Index: integer);
begin
  PopupMenu := nil;
  if (Index < 0) then
    StartPage
  else if (FSurvey.Items.Count > 0) and (Index >= FSurvey.Items.Count) then
    EndPage
  else
  begin
    lblTitle.Caption := FSurvey.Items[Index].Title;
    lblDescription.Caption := FSurvey.Items[Index].Description;
    case FSurvey.Items[Index].SurveyType of
      stExclusive:
        CreateExclusivePage(Index);
      stMultiple:
        CreateMultiplePage(Index);
      stFreeForm:
        CreateFreeFormPage(Index);
    end;
  end;
  UpdateProgress;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CreateEverything;
  LoadSettings;
  ReadCommandLine;
end;

procedure TfrmMain.CreateEverything;
begin
  FPageIndex := -1;
  if Assigned(CreateSurvey) then
    FSurvey := CreateSurvey
  else
  begin
    ShowMessage(SErrSurveyImplementationNotFound);
    Application.Terminate;
    Close;
  end;
end;

procedure TfrmMain.FreeEverything;
begin
  FSurvey := nil;
end;

procedure TfrmMain.LoadSettings;
begin
  JediLogo.Anchors := [akRight, akTop];
  JediLogo.Action := acGotoJVCL;
  // TODO: load additional properties
end;

procedure TfrmMain.ReadCommandLine;
var
  S:string;
  i: integer;
begin
  S := '';
  for i := 1 to ParamCount do
    if FileExists(ExpandUNCFileName(ParamStr(i))) then
    begin
      S := ExpandUNCFileName(ParamStr(i));
      Break;
    end;
  if S <> '' then
    LoadSurvey(S)
  else
    StartPage;
end;

procedure TfrmMain.SaveSettings;
begin
  // TODO: save settings
end;

procedure TfrmMain.PromptForSurveyFile(const AFilename: string);
begin
  OpenSurveyDialog.Filename := AFilename;
  acLoadSurvey.Execute;
end;

procedure TfrmMain.LoadSurvey(const AFilename: string);
begin
  if FileExists(AFilename) then
  begin
    FSurvey.LoadFromFile(AFilename);
    FFilename := AFilename;
    StartPage;
    if (FSurvey.ExpiryDate < Date) and not YesNo(SExpiredSurveyCaption, Format(SExpiredSurveyTextFmt,[DateToStr(FSurvey.ExpiryDate)])) then
      PromptForSurveyFile(AFilename)
  end
  else
    PromptForSurveyFile(AFilename);
  Caption := Format(SMainFormCaptionFmt, [ExtractFileName(Filename)]);
  lblSurveyTitle.Caption := FSurvey.Title;
end;

procedure TfrmMain.StartPage;
begin
  SavePage;
  ClearScrollBox;
  FPageIndex := -1;
  lblTitle.Caption := SStartPageTitle;
  if (FSurvey.Items.Count < 1) then
    lblDescription.Caption := SStartPageDescription
  else
    lblDescription.Caption := FSurvey.Description;
  lblProgress.Visible := false;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not FCompletedSurvey and (FSurvey.Items.Count > 0) and YesNo(SSaveResponseCaption,SSaveResponsePrompt) then
  begin
    if FSurvey.SurveyTaker.ID = '' then
      FSurvey.SurveyTaker.ID := CreateClassID;
    FSurvey.SaveToFile(Filename,ffBinary);
  end
  else if FileExists(FTempSurveyFilename) and YesNo(SDeleteResponseCaption,Format(SFmtDeleteResponsePrompt,[FTempSurveyFilename])) then
    DeleteFile(FTempSurveyFilename);
  SaveSettings;
  FreeEverything;
end;

procedure TfrmMain.ClearScrollBox;
begin
  if (edUserName <> nil) and (edUserEMail <> nil) then
  begin
    FSurvey.SurveyTaker.UserName := edUserName.Text;
    FSurvey.SurveyTaker.MailAddress := edUserEMail.Text;
  end;
  while sbSurvey.ControlCount > 0 do
    sbSurvey.Controls[0].Free;
  edUserName := nil;
  edUserEMail := nil;
  PopupMenu := nil;
end;

procedure TfrmMain.EndPage;
begin
  SavePage;
  ClearScrollBox;
  FPageIndex := FSurvey.Items.Count;
  lblTitle.Caption := SEndPageTitle;
  lblDescription.Caption := Format(SEndPageDescriptionFmt, [FSurvey.Recipient, FSurvey.RecipientMail, FSurvey.ResultHRef]);
  edUserName := TEdit.Create(self);
  with edUserName do
  begin
    Parent := sbSurvey;
    SetBounds(18, 36, sbSurvey.ClientWidth - 36, Height);
    TabOrder := 0;
    Text := FSurvey.SurveyTaker.UserName;
  end;
  with TLabel.Create(self) do
  begin
    Parent := sbSurvey;
    SetBounds(18, 18, Width, Height);
    Caption := SUserName;
    FocusControl := edUserName;
  end;
  edUserEmail := TEdit.Create(self);
  with edUserEmail do
  begin
    Parent := sbSurvey;
    SetBounds(18, 84, sbSurvey.ClientWidth - 36, Height);
    TabOrder := 1;
    Text := FSurvey.SurveyTaker.MailAddress;
  end;
  with TLabel.Create(self) do
  begin
    Parent := sbSurvey;
    SetBounds(18, 66, Width, Height);
    Caption := SEMail;
    FocusControl := edUserEmail;
  end;

  with TButton.Create(self) do
  begin
    Parent := sbSurvey;
    SetBounds(sbSurvey.ClientWidth - 105 - Width, 126, Width, Height);
    Action := acSaveSurvey;
    Anchors := [akRight,akTop];
    TabOrder := 2;
    Enabled := FSurvey.ExpiryDate >= Date;
  end;
  with TButton.Create(self) do
  begin
    Parent := sbSurvey;
    SetBounds(sbSurvey.ClientWidth - 25 - Width, 126, Width, Height);
    Caption := SSend;
    Anchors := [akRight,akTop];
    OnClick := DoSendMail;
    TabOrder := 3;
    Enabled := FSurvey.ExpiryDate >= Date;
  end;

  PopUpMenu := popSend;
  UpdateProgress;
end;

procedure TfrmMain.acStartPageExecute(Sender: TObject);
begin
  CreatePage(-1);
end;

procedure TfrmMain.acLastPageExecute(Sender: TObject);
begin
  CreatePage(FSurvey.Items.Count);
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acPrevPageExecute(Sender: TObject);
begin
  SavePage;
  Dec(FPageIndex);
  CreatePage(FPageIndex);
  UpdateProgress;
end;

procedure TfrmMain.acNextPageExecute(Sender: TObject);
begin
  SavePage;
  Inc(FPageIndex);
  CreatePage(FPageIndex);
  UpdateProgress;
end;

procedure TfrmMain.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acPrevPage.Enabled := (FPageIndex >= 0);
  acNextPage.Enabled := (Filename <> '') and (FSurvey.Items.Count > 0)
    and (FPageIndex < FSurvey.Items.Count) and CheckPage(FPageIndex);
  lblProgress.Visible := (FPageIndex >= 0) and (FPageIndex <= FSurvey.Items.Count);

  acLoadSurvey.Visible := FPageIndex = -1;
  acComment.Visible := (FPageIndex >= 0) and (FPageIndex < FSurvey.Items.Count);
  acLoadSurvey.Enabled := acLoadSurvey.Visible;
  acSaveSurvey.Enabled := FPageIndex = FSurvey.Items.Count;
  lblSurveyTitle.Visible := not acLoadSurvey.Visible;
end;

function TfrmMain.CheckPage(Index:integer): boolean;

  function HasCheckedItems: boolean;
  var
    i: integer;
  begin
    if (FSurvey.Items[Index].SurveyType = stFreeForm) or not FSurvey.Items[Index].Required then
      Result := true
    else
    begin
      with sbSurvey do
        for i := 0 to ControlCount - 1 do
          if ((Controls[i] is TRadioButton) and TRadioButton(Controls[i]).Checked) or
            ((Controls[i] is TCheckBox) and TCheckBox(Controls[i]).Checked) then
          begin
            Result := true;
            Exit;
          end;
      Result := false;
    end;
  end;
begin
  Result := (Index < 0) or (Index >= FSurvey.Items.Count) or HasCheckedItems;
end;

procedure TfrmMain.CreateExclusivePage(Index:integer);
var
  i, j, X, Y, AWidth: integer;
  ASize: TSize;
  RB: TJvRadioButton;
  S: TStringlist;
begin
  ClearScrollBox;
  X := cStartOffset;
  Y := cStartOffset;
  AWidth := cDefaultControlWidth;
  S := TStringlist.Create;
  try
//    S.CommaText := FSurvey.Items[Index].Choices;
    S.Text := DecodeChoice(FSurvey.Items[Index].Choices,stExclusive);
    j := 0;
    for i := 0 to S.Count - 1 do
    begin
      if S[i] = '' then Continue;
      RB := TJvRadioButton.Create(Self);
      RB.Caption := DecodeString(S[i]);
      ASize := TextSize(Canvas, RB.Height, RB.Caption);
      AWidth := Max(AWidth, ASize.cx + 32);
      RB.SetBounds(X, Y, AWidth, ASize.cy);
      RB.Checked := IsChecked(FSurvey.Items[Index], j);
      Inc(Y, ASize.cy + 4);
      if Y >= sbSurvey.ClientHeight - ASize.cy - cStartOffset then
      begin
        Y := cStartOffset;
        Inc(X, AWidth + cStartOffset);
      end;
      RB.Parent := sbSurvey;
//      if i = 0 then
//        ActiveControl := RB;
      Inc(j);
    end;
  finally
    S.Free;
  end;
  BuildExclusivePopUpMenu;
  PopupMenu := popExclusive;
end;

procedure TfrmMain.CreateFreeFormPage(Index:integer);
var RE:TJvMemo;
begin
  ClearScrollBox;
  RE := TJvMemo.Create(self);
  with RE do
  begin
    WordWrap := false;
    Parent := sbSurvey;
    SetBounds(cStartOffset, cStartOffset,
      sbSurvey.ClientWidth - cStartOffset * 2, sbSurvey.ClientHeight - cStartOffset * 2);
    Anchors := [akLeft..akBottom];
//    PlainText := true;
    ScrollBars := ssBoth;
    if FSurvey.Items[FPageIndex].Responses = '' then
      Lines.Text := DecodeChoice(FSurvey.Items[Index].Choices,stFreeForm)
    else
      Lines.Text := DecodeResponse(FSurvey.Items[Index].Responses,stFreeForm);
    ActiveControl := RE;
  end;
end;

procedure TfrmMain.CreateMultiplePage(Index:integer);
var
  i, j, X, Y, AWidth: integer;
  CB: TJvCheckBox;
  ASize: TSize;
  S: TStringlist;
begin
  ClearScrollBox;
  X := cStartOffset;
  Y := cStartOffset;
  AWidth := cDefaultControlWidth;
  S := TStringlist.Create;
  try
//    S.CommaText := FSurvey.Items[Index].Choices;
    S.Text := DecodeChoice(FSurvey.Items[Index].Choices,stMultiple);
    j := 0;
    for i := 0 to S.Count - 1 do
    begin
      if S[i] = '' then Continue;
      CB := TJvCheckBox.Create(Self);
      CB.Caption := S[i];
      ASize := TextSize(Canvas, CB.Height, CB.Caption);
      AWidth := Max(AWidth, ASize.cx + 32);
      CB.SetBounds(X, Y, AWidth, ASize.cy);
      CB.Checked := IsChecked(FSurvey.Items[Index], j);
      Inc(Y, ASize.cy + 4);
      if Y >= sbSurvey.ClientHeight - ASize.cy - cStartOffset then
      begin
        Y := cStartOffset;
        Inc(X, AWidth + cStartOffset);
      end;
      CB.Parent := sbSurvey;
      if i = 0 then
        ActiveControl := CB;
      Inc(j);
    end;
  finally
    S.Free;
  end;
  BuildMultiplePopUpMenu;
  PopupMenu := popMultiple;
end;

procedure TfrmMain.acLoadSurveyExecute(Sender: TObject);
begin
  if OpenSurveyDialog.Execute then
    LoadSurvey(OpenSurveyDialog.Filename)
  else
    StartPage;
end;

function TfrmMain.GetTempSurveyFileName: string;

  function DefaultStr(const S, Default: string): string;
  begin
    Result := S;
    if Result = '' then
      Result := Default;
  end;
begin
  // create a file name from the input filename, username, current date and time
  // and add a path
  Result := ChangeFileExt(ExtractFileName(Filename), '') +
    DefaultStr(GetLocalUserName,GetLocalComputerName) +
    FormatDateTime('yyyyMMddhhnnss', Now) + cResponseFileExt;
  //  ShowMessage(Result);
end;

procedure TfrmMain.SavePage;
var
  i: integer;S:string;
begin
  if (FPageIndex < 0) or (FPageIndex >= FSurvey.Items.Count) then Exit;
  S := '';
  with sbSurvey do
    for i := 0 to ControlCount - 1 do
    begin
      if Controls[i] is TRadioButton then
        S := S + Format('%d', [Ord(TRadioButton(Controls[i]).Checked)]) + cRecordSeparator
      else if Controls[i] is TCheckBox then
        S := S + Format('%d', [Ord(TCheckBox(Controls[i]).Checked)]) + cRecordSeparator
      else if Controls[i] is TCustomMemo then
        S := TCustomMemo(Controls[i]).Lines.Text
      else if Controls[i] is TCustomEdit then
        S := TCustomEdit(Controls[i]).Text;
    end;
  if MyAnsiLastChar(S) = cRecordSeparator then SetLength(S,Length(S)-1);
  FSurvey.Items[FPageIndex].Responses := S;
end;

procedure TfrmMain.UpdateProgress;
begin
  if FPageIndex >= 0 then
    lblProgress.Caption := Format(SPageOfPageFmt, [FPageIndex + 1, FSurvey.Items.Count + 1])
  else
    lblProgress.Caption := '';
end;

procedure TfrmMain.DoSendMail(Sender: TObject);
begin
  acSendMail.MailOptions.Recipients := FSurvey.RecipientMail;
  acSendMail.MailOptions.Subject := FSurvey.Title;
  if edUserName <> nil then
    FSurvey.SurveyTaker.UserName := edUserName.Text;
  if edUserEMail <> nil then
    FSurvey.SurveyTaker.MailAddress := edUserEMail.Text;
  // create and attach response file
  acSendMail.MailOptions.Attachments.Clear;
  if FTempSurveyFilename = '' then
    FTempSurveyFilename := GetTempSurveyFileName;
  if FSurvey.SurveyTaker.ID = '' then
    FSurvey.SurveyTaker.ID := CreateClassID;
  FSurvey.SaveToFile(FTempSurveyFilename,ffBinary);
  acSendMail.MailOptions.Attachments.Add(FTempSurveyFilename);

  acSendMail.MailOptions.ShowDialogs := true; // not acSendMail.Mail.UserLogged;
  if acSendMail.Execute then
    FCompletedSurvey := true;
end;

procedure TfrmMain.lblDescriptionLinkClick(Sender: TObject;
  LinkNumber: Integer; LinkText: String);
begin
  OpenObject(LinkText);
end;

procedure TfrmMain.acCheckAllExecute(Sender: TObject);
var i:integer;
begin
  for i := 0 to sbSurvey.ControlCount - 1 do
    if sbSurvey.Controls[i] is TCheckBox then
      TCheckBox(sbSurvey.Controls[i]).Checked := true;
end;

procedure TfrmMain.acUncheckAllExecute(Sender: TObject);
var i:integer;
begin
  for i := 0 to sbSurvey.ControlCount - 1 do
    if sbSurvey.Controls[i] is TCheckBox then
      TCheckBox(sbSurvey.Controls[i]).Checked := false;
end;

procedure TfrmMain.acInvertExecute(Sender: TObject);
var i:integer;
begin
  for i := 0 to sbSurvey.ControlCount - 1 do
    if sbSurvey.Controls[i] is TCheckBox then
      TCheckBox(sbSurvey.Controls[i]).Checked := not TCheckBox(sbSurvey.Controls[i]).Checked;
end;

procedure TfrmMain.acCheckFirstExecute(Sender: TObject);
var i:integer;
begin
  for i := 0 to sbSurvey.ControlCount - 1 do
    if sbSurvey.Controls[i] is TRadioButton then
    begin
      TCheckBox(sbSurvey.Controls[i]).Checked := true;
      Exit;
    end;
end;

procedure TfrmMain.acCheckLastExecute(Sender: TObject);
var i:integer;
begin
  for i := sbSurvey.ControlCount - 1 downto 0 do
    if sbSurvey.Controls[i] is TRadioButton then
    begin
      TCheckBox(sbSurvey.Controls[i]).Checked := true;
      Exit;
    end;
end;

procedure TfrmMain.DoExclusiveClick(Sender:TObject);
begin
  with (Sender as TMenuItem) do
  begin
    TRadioButton(Tag).Checked := true;
    Checked := true;
  end;
end;

procedure TfrmMain.BuildExclusivePopUpMenu;
var i:integer;m:TMenuItem;R:TRadioButton;
begin
  popExclusive.Items.Clear;
  for i := 0 to sbSurvey.ControlCount - 1 do
    if sbSurvey.Controls[i] is TRadioButton then
    begin
      R := TRadioButton(sbSurvey.Controls[i]);
      m := TMenuItem.Create(popExclusive);
      m.AutoHotkeys := maManual;
      m.Checked := R.Checked;
//      m.AutoCheck := true;  // only availale in D6+
      m.RadioItem := true;
      m.GroupIndex := 1;
      m.Caption := R.Caption;
      m.Tag     := integer(R);
      m.OnClick := DoExclusiveClick;
      popExclusive.Items.Add(m);
      if popExclusive.Items.Count < 10 then
        m.ShortCut := ShortCut(Ord('0') + popExclusive.Items.Count,[ssCtrl])
    end;
  // add standard items:
  m := TMenuItem.Create(popExclusive);
  m.Caption := '-';
  popExclusive.Items.Add(m);

  m := TMenuItem.Create(popExclusive);
  m.Action := acCheckFirst;
  popExclusive.Items.Add(m);

  m := TMenuItem.Create(popExclusive);
  m.Action := acCheckLast;
  popExclusive.Items.Add(m);
end;

procedure TfrmMain.DoMultipleClick(Sender:TObject);
begin
  with (Sender as TMenuItem) do
  begin
    TCheckBox(Tag).Checked := not TCheckBox(Tag).Checked;
    Checked := not Checked;
  end;
end;

procedure TfrmMain.BuildMultiplePopUpMenu;
var i:integer;m:TMenuItem;C:TCheckBox;
begin
  popMultiple.Items.Clear;
  for i := 0 to sbSurvey.ControlCount - 1 do
    if sbSurvey.Controls[i] is TCheckBox then
    begin
      C := TCheckBox(sbSurvey.Controls[i]);
      m := TMenuItem.Create(popMultiple);
      m.AutoHotkeys := maManual;
      m.Checked := C.Checked;
//      m.AutoCheck := true;
      m.Caption := C.Caption;
      m.Tag     := integer(C);
      m.OnClick := DoMultipleClick;
      popMultiple.Items.Add(m);
      if popMultiple.Items.Count < 10 then
        m.ShortCut := ShortCut(Ord('0') + popMultiple.Items.Count,[ssCtrl])
    end;
  // add standard items:
  m := TMenuItem.Create(popMultiple);
  m.Caption := '-';
  popMultiple.Items.Add(m);

  m := TMenuItem.Create(popMultiple);
  m.Action := acCheckAll;
  popMultiple.Items.Add(m);

  m := TMenuItem.Create(popMultiple);
  m.Action := acUnCheckAll;
  popMultiple.Items.Add(m);

  m := TMenuItem.Create(popMultiple);
  m.Caption := '-';
  popMultiple.Items.Add(m);

  m := TMenuItem.Create(popMultiple);
  m.Action := acInvert;
  popMultiple.Items.Add(m);
end;

procedure TfrmMain.acCommentExecute(Sender: TObject);
var S:string;
begin
  S := FSurvey.Items[FPageIndex].Comments;
  if TfrmComment.Comment(FSurvey.Items[FPageIndex].Title,S) then
      FSurvey.Items[FPageIndex].Comments := S;
end;

procedure TfrmMain.sbSurveyContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  //
end;

procedure TfrmMain.FormContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  if (PopupMenu <> nil) then
  begin
    if (MousePos.X < 0) or (MousePos.Y < 0) then
    begin
      GetCursorPos(MousePos);
//      MousePos := ScreenToClient(MousePos);
      if (MousePos.X < Left) or (MousePos.X > Left + Width) or
        (MousePos.Y < Top) or (MousePos.Y > Top + Height) then
          MousePos := Point(Left + Width div 2, Top + Height div 2);
      PopupMenu.Popup(MousePos.X,MousePos.Y);
      Handled := true;
    end;
  end;
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  Windows.MessageBox(GetActiveWindow,PChar(SAboutText),PChar(SAboutTitle),MB_OK or MB_ICONINFORMATION);
end;

procedure TfrmMain.acSaveSurveyExecute(Sender: TObject);
const
  cSurveyFormat:array[boolean] of TJvSurveyFileFormat = (ffText,ffBinary);
begin
  if FTempSurveyFilename = '' then
    FTempSurveyFilename := GetTempSurveyFileName;
  SaveSurveyDialog.FileName := FTempSurveyFilename;
  if SaveSurveyDialog.Execute then
  begin
    FTempSurveyFilename := SaveSurveyDialog.Filename;
    if FSurvey.SurveyTaker.ID = '' then
      FSurvey.SurveyTaker.ID := CreateClassID;
    FSurvey.SaveToFile(FTempSurveyFilename,cSurveyFormat[SaveSurveyDialog.FilterIndex = 1]);
    FCompletedSurvey := true;
  end;
end;

end.


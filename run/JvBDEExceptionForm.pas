{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDbExcpt.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

TJvBdeErrorDlg based on sample form
   DELPHI\DEMOS\DB\TOOLS\DBEXCEPT.PAS

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBDEExceptionForm;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBTables,
  JvComponent;

type
  TDBErrorEvent = procedure(Error: TDBError; var Msg: string) of object;

  TJvBdeErrorDlg = class(TJvForm)
    BasicPanel: TPanel;
    ErrorText: TLabel;
    IconPanel: TPanel;
    IconImage: TImage;
    TopPanel: TPanel;
    RightPanel: TPanel;
    DetailsPanel: TPanel;
    DbMessageText: TMemo;
    DbResult: TEdit;
    DbCatSub: TEdit;
    NativeResult: TEdit;
    Back: TButton;
    Next: TButton;
    ButtonPanel: TPanel;
    DetailsBtn: TButton;
    OKBtn: TButton;
    BDELabel: TLabel;
    NativeLabel: TLabel;
    BottomPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
  private
    FCurItem: Integer;
    FDetails: Boolean;
    FDetailsHeight: Integer;
    FDbException: EDbEngineError;
    FPrevOnException: TExceptionEvent;
    FOnErrorMsg: TDBErrorEvent;
    procedure GetErrorMsg(Error: TDBError; var Msg: string);
    procedure ShowError;
    procedure SetShowDetails(Value: Boolean);
  public
    procedure ShowException(Sender: TObject; E: Exception);
    property OnErrorMsg: TDBErrorEvent read FOnErrorMsg write FOnErrorMsg;
  end;

const
  DbErrorHelpCtx = THelpContext(0);

procedure DbErrorIntercept;

implementation

uses
  Consts, Windows, BDE,
  JvResources;

{$R *.dfm}

var
  DbEngineErrorDlg: TJvBdeErrorDlg = nil;

procedure DbErrorIntercept;
begin
  DbEngineErrorDlg.Free;
  DbEngineErrorDlg := TJvBdeErrorDlg.Create(Application);
end;

procedure TJvBdeErrorDlg.ShowException(Sender: TObject; E: Exception);
begin
  Screen.Cursor := crDefault;
  Application.NormalizeTopMosts;
  try
    if (E is EDbEngineError) and (FDbException = nil) and
      not Application.Terminated then
    begin
      FDbException := EDbEngineError(E);
      try
        ShowModal;
      finally
        FDbException := nil;
      end;
    end
    else
    begin
      if Assigned(FPrevOnException) then
        FPrevOnException(Sender, E)
      else
      if NewStyleControls then
        Application.ShowException(E)
      else
        MessageDlg(E.Message + '.', mtError, [mbOk], 0);
    end;
  except
    { ignore any exceptions }
  end;
  Application.RestoreTopMosts;
end;

procedure TJvBdeErrorDlg.ShowError;
var
  BDEError: TDBError;
  S: string;
  I: Integer;
begin
  Back.Enabled := (FCurItem > 0);
  Next.Enabled := (FCurItem < FDbException.ErrorCount - 1);
  BDEError := FDbException.Errors[FCurItem];
  { Fill BDE error information }
  BDELabel.Enabled := True;
  DbResult.Text := IntToStr(BDEError.ErrorCode);
  DbCatSub.Text := Format('[$%s] [$%s]', [IntToHex(BDEError.Category, 2),
    IntToHex(BDEError.SubCode, 2)]);
  { Fill native error information }
  NativeLabel.Enabled := BDEError.NativeError <> 0;
  if NativeLabel.Enabled then
    NativeResult.Text := IntToStr(BDEError.NativeError)
  else
    NativeResult.Clear;
  { The message text is common to both BDE and native errors }
  S := Trim(BDEError.Message);
  for I := 1 to Length(S) do
    if S[I] < ' ' then
      S[I] := ' ';
  {GetErrorMsg(BDEError, S);}
  DbMessageText.Text := Trim(S);
end;

procedure TJvBdeErrorDlg.SetShowDetails(Value: Boolean);
begin
  DisableAlign;
  try
    if Value then
    begin
      DetailsPanel.Height := FDetailsHeight;
      ClientHeight := DetailsPanel.Height + BasicPanel.Height;
      DetailsBtn.Caption := RsDetailsLeftCaption;
      FCurItem := 0;
      ShowError;
    end
    else
    begin
      ClientHeight := BasicPanel.Height;
      DetailsPanel.Height := 0;
      DetailsBtn.Caption := RsDetailsRightCaption;
    end;
    DetailsPanel.Enabled := Value;
    FDetails := Value;
  finally
    EnableAlign;
  end;
end;

procedure TJvBdeErrorDlg.GetErrorMsg(Error: TDBError; var Msg: string);
begin
  if Assigned(FOnErrorMsg) then
  try
    FOnErrorMsg(Error, Msg);
  except
  end;
end;

procedure TJvBdeErrorDlg.FormCreate(Sender: TObject);
begin
  FDetailsHeight := DetailsPanel.Height;
  Icon.Handle := LoadIcon(0, IDI_EXCLAMATION);
  IconImage.Picture.Icon := Icon;
  { Load string resources }
  Caption := RsDBExceptCaption;
  BDELabel.Caption := RsBDEErrorLabel;
  NativeLabel.Caption := RsServerErrorLabel;
  Next.Caption := RsNextButtonCaption;
  Back.Caption := RsPrevButtonCaption;
  OKBtn.Caption := SOKButton;
  { Set exception handler }
  FPrevOnException := Application.OnException;
  Application.OnException := ShowException;
end;

procedure TJvBdeErrorDlg.FormDestroy(Sender: TObject);
begin
  Application.OnException := FPrevOnException;
end;

procedure TJvBdeErrorDlg.FormShow(Sender: TObject);
var
  S: string;
  ErrNo: Integer;
begin
  if FDbException.HelpContext <> 0 then
    HelpContext := FDbException.HelpContext
  else
    HelpContext := DbErrorHelpCtx;
  FCurItem := 0;
  if (FDbException.ErrorCount > 1) and
    (FDbException.Errors[1].NativeError <> 0) and
    ((FDbException.Errors[0].ErrorCode = DBIERR_UNKNOWNSQL) or
    { General SQL error }
    (FDbException.Errors[0].ErrorCode = DBIERR_INVALIDUSRPASS)) then
    { Unknown username or password }
    ErrNo := 1
  else
    ErrNo := 0;
  S := Trim(FDbException.Errors[ErrNo].Message);
  GetErrorMsg(FDbException.Errors[ErrNo], S);
  ErrorText.Caption := S;
  SetShowDetails(False);
  DetailsBtn.Enabled := FDbException.ErrorCount > 0;
end;

procedure TJvBdeErrorDlg.DetailsBtnClick(Sender: TObject);
begin
  SetShowDetails(not FDetails);
end;

procedure TJvBdeErrorDlg.BackClick(Sender: TObject);
begin
  Dec(FCurItem);
  ShowError;
end;

procedure TJvBdeErrorDlg.NextClick(Sender: TObject);
begin
  Inc(FCurItem);
  ShowError;
end;

end.


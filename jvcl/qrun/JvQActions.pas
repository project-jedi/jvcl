{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvActions.Pas, released on 2002-10-04.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2002 Sébastien Buysse.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQActions;

interface

uses
  {$IFDEF MSWINDOWS}
  JclMapi,
  Windows, ShellAPI,
  {$ENDIF MSWINDOWS}  
  QActnList, 
  {$IFDEF LINUX}
  QWindows,
  {$ENDIF LINUX}
  Classes;

type
  {$IFDEF MSWINDOWS}

  TJvSendMailOptions = class(TPersistent)
  private
    FMailer: TJclEmail;
    FShowDialogs: Boolean;
    function GetAttachments: TStrings;
    function GetBody: string;
    function GetFindOptions: TJclEmailFindOptions;
    function GetHtmlBody: Boolean;
    function GetLogonOptions: TJclEmailLogonOptions;
    function GetReadMsg: TJclEmailReadMsg;
    function GetSubject: string;
    function GetUserLogged: Boolean;
    procedure SetAttachments(const Value: TStrings);
    procedure SetBody(const Value: string);
    procedure SetFindOptions(const Value: TJclEmailFindOptions);
    procedure SetHtmlBody(const Value: Boolean);
    procedure SetLogonOptions(const Value: TJclEmailLogonOptions);
    procedure SetSubject(const Value: string);
    function GetRecipients: string;
    procedure SetRecipients(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Execute: Boolean;
    property Mailer: TJclEmail read FMailer write FMailer;
  published
    property Attachments: TStrings read GetAttachments write SetAttachments;
    property Body: string read GetBody write SetBody;
    property FindOptions: TJclEmailFindOptions read GetFindOptions write SetFindOptions;
    property HtmlBody: Boolean read GetHtmlBody write SetHtmlBody;
    property LogonOptions: TJclEmailLogonOptions read GetLogonOptions write SetLogonOptions;
    property ReadMsg: TJclEmailReadMsg read GetReadMsg;
    property Recipients: string read GetRecipients write SetRecipients;
    property ShowDialogs: Boolean read FShowDialogs write FShowDialogs default True;
    property Subject: string read GetSubject write SetSubject;
    property UserLogged: Boolean read GetUserLogged;
  end;

  TJvSendMailAction = class(TAction)
  private
    FMailOptions: TJvSendMailOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
  published
    property MailOptions: TJvSendMailOptions read FMailOptions write FMailOptions;
  end;

  {$ENDIF MSWINDOWS}

  TJvWebAction = class(TAction)
  private
    FUrl: string;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    function Execute: Boolean; override;
  published
    property URL: string read FUrl write FUrl;
  end;


implementation

{$IFDEF MSWINDOWS}

//=== { TJvSendMailAction } ==================================================

constructor TJvSendMailAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisableIfNoHandler := False;
  FMailOptions := TJvSendMailOptions.Create;
end;

destructor TJvSendMailAction.Destroy;
begin
  FMailOptions.Free;
  inherited Destroy;
end;

function TJvSendMailAction.Execute: Boolean;
begin
  Result := MailOptions.Execute;
end;

//=== { TJvSendMailOptions } =================================================

constructor TJvSendMailOptions.Create;
begin
  inherited Create;
  FShowDialogs := True;
  FMailer := TJclEmail.Create;
end;

destructor TJvSendMailOptions.Destroy;
begin
  FMailer.Free;
  inherited Destroy;
end;

function TJvSendMailOptions.Execute: Boolean;
begin
  Result := Mailer.Send(ShowDialogs);
end;

function TJvSendMailOptions.GetAttachments: TStrings;
begin
  Result := Mailer.Attachments;
end;

function TJvSendMailOptions.GetBody: string;
begin
  Result := Mailer.Body;
end;

function TJvSendMailOptions.GetFindOptions: TJclEmailFindOptions;
begin
  Result := Mailer.FindOptions;
end;

function TJvSendMailOptions.GetHtmlBody: Boolean;
begin
  Result := Mailer.HtmlBody;
end;

function TJvSendMailOptions.GetLogonOptions: TJclEmailLogonOptions;
begin
  Result := Mailer.LogonOptions;
end;

function TJvSendMailOptions.GetReadMsg: TJclEmailReadMsg;
begin
  Result := Mailer.ReadMsg;
end;

function TJvSendMailOptions.GetRecipients: string;
begin
  if Mailer.Recipients.Count = 0 then
    Result := ''
  else
    Result := Mailer.Recipients.Items[0].Address;
end;

function TJvSendMailOptions.GetSubject: string;
begin
  Result := Mailer.Subject;
end;

function TJvSendMailOptions.GetUserLogged: Boolean;
begin
  Result := Mailer.UserLogged;
end;

procedure TJvSendMailOptions.SetAttachments(const Value: TStrings);
begin
  Mailer.Attachments.Assign(Value);
end;

procedure TJvSendMailOptions.SetBody(const Value: string);
begin
  Mailer.Body := Value;
end;

procedure TJvSendMailOptions.SetFindOptions(const Value: TJclEmailFindOptions);
begin
  Mailer.FindOptions := Value;
end;

procedure TJvSendMailOptions.SetHtmlBody(const Value: Boolean);
begin
  Mailer.HtmlBody := Value;
end;

procedure TJvSendMailOptions.SetLogonOptions(const Value: TJclEmailLogonOptions);
begin
  Mailer.LogonOptions := Value;
end;

procedure TJvSendMailOptions.SetRecipients(const Value: string);
begin
  Mailer.Recipients.Clear;
  Mailer.Recipients.Add(Value);
end;

procedure TJvSendMailOptions.SetSubject(const Value: string);
begin
  Mailer.Subject := Value;
end;

{$ENDIF MSWINDOWS}

//=== { TJvWebAction } =======================================================

function TJvWebAction.Execute: Boolean;
begin
  Result := ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL) > HINSTANCE_ERROR;
end;

function TJvWebAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

procedure TJvWebAction.UpdateTarget(Target: TObject);
begin
  Enabled := Url <> '';
end;

end.


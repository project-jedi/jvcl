{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvActions.Pas, released on 2002-10-04.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2002 Sébastien Buysse.
All Rights Reserved.

Contributor(s): -

Last Modified: 2000-10-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvActions;

interface

uses
  Windows, Classes, ActnList, ShellApi,
  JclMapi;

type
  TJvSendMailOptions = class(TPersistent)
  private
    FMail: TJclEmail;
    FShowDialogs: Boolean;
    function GetAttachments: TStrings;
    function GetBody: string;
    function GetFindOptions: TJclEmailFindOptions;
    function GetHtmlBody: Boolean;
    function GetLogonOptions: TJclEmailLogonOptions;
    function GetReadMsg: TJclEmailReadMsg;
    function GetSubject: string;
    function GetUserLogged: Boolean;
    procedure SetAttachment(const Value: TStrings);
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
    property MapiComponent: TJclEmail read FMail write FMail;
  published
    property ShowDialogs: Boolean read FShowDialogs write FShowDialogs default True;

    property Attachments: TStrings read GetAttachments write SetAttachment;
    property Body: string read GetBody write SetBody;
    property FindOptions: TJclEmailFindOptions read GetFindOptions write SetFindOptions;
    property HtmlBody: Boolean read GetHtmlBody write SetHtmlBody;
    property LogonOptions: TJclEmailLogonOptions read GetLogonOptions write SetLogonOptions;
    property ReadMsg: TJclEmailReadMsg read GetReadMsg;
    property Recipients: string read GetRecipients write SetRecipients; 
    property Subject: string read GetSubject write SetSubject;
    property UserLogged: Boolean read GetUserLogged;
  end;

  TJvSendMail = class(TAction)
  private
    FMail: TJvSendMailOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
  published
    property Mail: TJvSendMailOptions read FMail write FMail;
  end;

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

uses
  JvFunctions;

//=== TJvSendMail ============================================================

constructor TJvSendMail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisableIfNoHandler := False;
  FMail := TJvSendMailOptions.Create;
end;

destructor TJvSendMail.Destroy;
begin
  FMail.Free;
  inherited Destroy;
end;

function TJvSendMail.Execute: Boolean;
begin
  Result := FMail.Execute;
end;

//=== TJvSendMailOptions =====================================================

constructor TJvSendMailOptions.Create;
begin
  inherited Create;
  FShowDialogs := True;
  FMail := TJclEmail.Create;
end;

destructor TJvSendMailOptions.Destroy;
begin
  FMail.Free;
  inherited Destroy;
end;

function TJvSendMailOptions.Execute: Boolean;
begin
  Result := Fmail.Send(FShowDialogs);
end;

function TJvSendMailOptions.GetAttachments: TStrings;
begin
  Result := FMail.Attachments;
end;

function TJvSendMailOptions.GetBody: string;
begin
  Result := FMail.Body;
end;

function TJvSendMailOptions.GetFindOptions: TJclEmailFindOptions;
begin
  Result := FMail.FindOptions;
end;

function TJvSendMailOptions.GetHtmlBody: Boolean;
begin
  Result := FMail.HtmlBody;
end;

function TJvSendMailOptions.GetLogonOptions: TJclEmailLogonOptions;
begin
  Result := FMail.LogonOptions;
end;

function TJvSendMailOptions.GetReadMsg: TJclEmailReadMsg;
begin
  Result := FMail.ReadMsg;
end;

function TJvSendMailOptions.GetRecipients: string;
begin
  if FMail.Recipients.Count = 0 then
    Result := ''
  else
    Result := FMail.Recipients.Items[0].Address;
end;

function TJvSendMailOptions.GetSubject: string;
begin
  Result := FMail.Subject;
end;

function TJvSendMailOptions.GetUserLogged: Boolean;
begin
  Result := FMail.UserLogged;
end;

procedure TJvSendMailOptions.SetAttachment(const Value: TStrings);
begin
  FMail.Attachments.Assign(Value);
end;

procedure TJvSendMailOptions.SetBody(const Value: string);
begin
  FMail.Body := Value;
end;

procedure TJvSendMailOptions.SetFindOptions(const Value: TJclEmailFindOptions);
begin
  FMail.FindOptions := Value;
end;

procedure TJvSendMailOptions.SetHtmlBody(const Value: Boolean);
begin
  FMail.HtmlBody := Value;
end;

procedure TJvSendMailOptions.SetLogonOptions(const Value: TJclEmailLogonOptions);
begin
  FMail.LogonOptions := Value;
end;

procedure TJvSendMailOptions.SetRecipients(const Value: string);
begin
  Fmail.Recipients.Clear;
  FMail.Recipients.Add(Value);
end;

procedure TJvSendMailOptions.SetSubject(const Value: string);
begin
  FMail.Subject := Value;
end;

//=== TJvWebAction ===========================================================

function TJvWebAction.Execute: Boolean;
begin
  Result := OpenObject(FUrl);
end;

function TJvWebAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

procedure TJvWebAction.UpdateTarget(Target: TObject);
begin
  Enabled := (Length(FUrl) <> 0);
end;

end.

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgMailSlots.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Burov Dmitry, translation of russian text.

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgMailSlots;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls,
  JvComponent;

type
  TOnNewMessage = procedure(Sender: TObject; MessageText: string) of object;

  TJvgMailSlotServer = class(TJvComponent)
  private
    FMailSlotName: string;
    FLastMessage: string;
    FOnNewMessage: TOnNewMessage;
    FTimer: TTimer;
    FHandle: THandle;
    FEnabled: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
  protected
    procedure Loaded; override;
    procedure OnTimer(Sender: TObject);
  published
    property MailSlotName: string read FMailSlotName write FMailSlotName;
    property OnNewMessage: TOnNewMessage read FOnNewMessage write
      FOnNewMessage;
  end;

  TJvgMailSlotClient = class(TJvComponent)
  private
    FMailSlotName: string;
    FServerName: string;
    FOnNewMessage: TOnNewMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Send(str: string): Boolean;
  protected
    procedure Loaded; override;
    procedure ErrorCatch(Sender: TObject; Exc: Exception);
  published
    property ServerName: string read FServerName write FServerName;
    property MailSlotName: string read FMailSlotName write FMailSlotName;
    property OnNewMessage: TOnNewMessage read FOnNewMessage write FOnNewMessage;
  end;

implementation

uses
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvConsts,
  JvgUtils, JvgTypes;

{$IFNDEF USEJVCL}
resourcestring
  RsETJvgMailSlotServerErrorCreatingChan = 'TJvgMailSlotServer: Error creating channel!';
  RsETJvgMailSlotServerErrorGatheringInf = 'TJvgMailSlotServer: Error gathering information!';
  RsETJvgMailSlotServerErrorReadingMessa = 'TJvgMailSlotServer: Error reading message!';
{$ENDIF USEJVCL}

constructor TJvgMailSlotServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FMailSlotName := 'MailSlot';
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := OnTimer;
end;

destructor TJvgMailSlotServer.Destroy;
begin
  FTimer.Free;
  // закрытие канала
  { Closing channel [translated] }
  Close;
  inherited Destroy;
end;

procedure TJvgMailSlotServer.Loaded;
begin
  inherited Loaded;
  Open;
end;

procedure TJvgMailSlotServer.Open;
begin
  //  if not FEnabled then exit;
  // создание канала с именем MailSlotName - по этому имени к нему
  // будут обращаться клиенты
  { Creating channel named MailSlotName - that is the name clients will use to
    access the channel [translated] }
  FHandle := CreateMailSlot(PChar('\\.\mailslot\' + MailSlotName), 0,
    MAILSLOT_WAIT_FOREVER, nil);
  //FHandle:=CreateMailSlot('\\.\mailslot\MailSlot',0,MAILSLOT_WAIT_FOREVER,nil);

  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create(RsETJvgMailSlotServerErrorCreatingChan);
  FTimer.Enabled := True;
end;

procedure TJvgMailSlotServer.Close;
begin
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FHandle := 0;
end;

procedure TJvgMailSlotServer.OnTimer(Sender: TObject);
var
  MsgNext: DWORD;
  MsgNumber: DWORD;
  Read: DWORD;
  MessageText: string;
  Buffer: array [0..249] of Char;
begin
  //  if not FEnabled then exit;

  MessageText := '';
  // определение наличия сообщения в канале
  { Determining if there's message in channel [translated] }
  if not GetMailSlotInfo(FHandle, nil, MsgNext, @MsgNumber, nil) then
    raise Exception.Create(RsETJvgMailSlotServerErrorGatheringInf);
  if MsgNext <> MAILSLOT_NO_MESSAGE then
  begin
    Beep;
    // чтение сообщения из канала и добавление в текст протокола
    { Reading message from channel and adding it to text of log }
    if ReadFile(FHandle, Buffer, SizeOf(Buffer), Read, nil) then
      MessageText := Buffer
    else
      raise Exception.Create(RsETJvgMailSlotServerErrorReadingMessa);
  end;

  if (MessageText <> '') and Assigned(FOnNewMessage) then
    FOnNewMessage(Self, MessageText);

  FLastMessage := MessageText;
end;
//------------------------------------------------------------------------------

constructor TJvgMailSlotClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMailSlotName := 'MailSlot';
  FServerName := '';
end;

destructor TJvgMailSlotClient.Destroy;
begin
  inherited Destroy;
end;

procedure TJvgMailSlotClient.Loaded;
begin
  inherited Loaded;
  // (rom) this is not a good idea
  Application.OnException := ErrorCatch;
end;

procedure TJvgMailSlotClient.ErrorCatch(Sender: TObject; Exc: Exception);
var
  UserName: array [0..99] of Char;
  Size: DWORD;
begin
  // получение имени пользователя
  { Querying user name [translated] }
  Size := SizeOf(UserName);
  GetUserName(UserName, Size);

  Send('/' + UserName + '/' + FormatDateTime('hh:mm', Time) + '/' +
    Exc.Message);
  // вывод сообщения об ошибке пользователю
  { Showing message about error to user [translated] }
  Application.ShowException(Exc);
end;

function TJvgMailSlotClient.Send(str: string): Boolean;
var
  Buffer: array [0..249] of Char;
  FHandle: THandle;
  Written: DWORD;
begin
  // открытие канала : MyServer - имя сервера
  // (\\.\\mailslot\xxx - монитор работает на этом же ПК)
  // xxx - имя канала
  { Opening channel: MyServer - name of server
    (\\.\\mailslot\xxx - monitor is working on the same PC
    xxx is the name of channel [translated]
  }
  if FServerName = '' then
    FServerName := '.\';
  FHandle := CreateFile(PChar('\\' + FServerName + '\mailslot\' + FMailSlotName),
    GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    StrCopy(Buffer, PChar(Str));
    // передача текста ошибки (запись в канал и закрытие канала)
    { Transmitting text of error (putting into channel and closing channel) [translated] }
    WriteFile(FHandle, Buffer, Length(Str) + 1, Written, nil);
    CloseHandle(FHandle);
  end;
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

end.


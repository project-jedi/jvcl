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
  Windows, Messages, SysUtils, Classes, JvComponent, Graphics,
  Controls, Forms, Dialogs, ExtCtrls;

type
  TOnNewMessage = procedure(Sender: TObject; MessageText: string) of object;

  TJvgMailSlotServer = class(TJvComponent)
  private
    FMailSlotName, FLastMessage: string;
    FOnNewMessage: TOnNewMessage;

    Timer: TTimer;
    h: THandle;
    str: string[250];
    MsgNumber, MsgNext, Read: DWORD;
  public
    FEnabled: boolean;
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
    FMailSlotName, FServerName: string;
    FOnNewMessage: TOnNewMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Send(str: string): boolean;
  protected
    procedure Loaded; override;
    procedure ErrorCatch(Sender: TObject; Exc: Exception);
  published
    property ServerName: string read FServerName write FServerName;
    property MailSlotName: string read FMailSlotName write FMailSlotName;
    property OnNewMessage: TOnNewMessage read FOnNewMessage write
      FOnNewMessage;
  end;


resourcestring
  sTJvgMailSlotServerErrorCreatingChan = 'TJvgMailSlotServer: Error creating channel!';
  sTJvgMailSlotServerErrorGatheringInf = 'TJvgMailSlotServer: Error gathering information!';
  sTJvgMailSlotServerErrorReadingMessa = 'TJvgMailSlotServer: Error reading message!';

implementation
uses
  JvConsts, JvgUtils, JvgTypes;

constructor TJvgMailSlotServer.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := true;
  FMailSlotName := 'MailSlot';
  Timer := TTimer.Create(nil);
  Timer.Enabled := false;
  Timer.OnTimer := OnTimer;
end;

destructor TJvgMailSlotServer.Destroy;
begin
  Timer.Free;
  // закрытие канала
  { Closing channel [translated] }
  Close;
  inherited;
end;

procedure TJvgMailSlotServer.Loaded;
begin
  inherited;
  Open;
end;

procedure TJvgMailSlotServer.Open;
begin
  //  if not FEnabled then exit;
  // создание канала с именем MailSlotName - по этому имени к нему
  // будут обращаться клиенты
  { Creating channel named MailSlotName - that is the name clients will use to
    access the channel [translated] }
  h := CreateMailSlot(PChar('\\.\mailslot\' + MailSlotName), 0,
    MAILSLOT_WAIT_FOREVER, nil);
  //h:=CreateMailSlot('\\.\mailslot\MailSlot',0,MAILSLOT_WAIT_FOREVER,nil);

  if h = INVALID_HANDLE_VALUE then
  begin
    raise Exception.Create(sTJvgMailSlotServerErrorCreatingChan);
  end;
  Timer.Enabled := true;
end;

procedure TJvgMailSlotServer.Close;
begin
  if h <> 0 then
    CloseHandle(h);
  h := 0;
end;

procedure TJvgMailSlotServer.OnTimer(Sender: TObject);
var
  MessageText: string;
begin
  //  if not FEnabled then exit;

  MessageText := '';
  // определение наличия сообщения в канале
  { Determining if there's message in channel [translated] }
  if not GetMailSlotInfo(h, nil, DWORD(MsgNext), @MsgNumber, nil) then
  begin
    raise Exception.Create(sTJvgMailSlotServerErrorGatheringInf);
  end;
  if MsgNext <> MAILSLOT_NO_MESSAGE then
  begin
    beep;
    // чтение сообщения из канала и добавление в текст протокола
    { Reading message from channel and adding it to text of log }
    if ReadFile(h, str, 200, DWORD(Read), nil) then
      MessageText := str
    else
      raise
        Exception.Create(sTJvgMailSlotServerErrorReadingMessa);
  end;

  if (MessageText <> '') and Assigned(OnNewMessage) then
    OnNewMessage(self, MessageText);

  FLastMessage := MessageText;
end;
//------------------------------------------------------------------------------

constructor TJvgMailSlotClient.Create(AOwner: TComponent);
begin
  inherited;
  FMailSlotName := 'MailSlot';
  FServerName := '';
end;

destructor TJvgMailSlotClient.Destroy;
begin
  inherited;
end;

procedure TJvgMailSlotClient.Loaded;
begin
  inherited;
  Application.OnException := ErrorCatch;
end;

procedure TJvgMailSlotClient.ErrorCatch(Sender: TObject; Exc: Exception);
var
  UserName: array[0..99] of char;
  i: integer;
begin
  // получение имени пользователя
  { Querying user name [translated] }
  i := SizeOf(UserName);
  GetUserName(UserName, DWORD(i));

  Send('/' + UserName + '/' + FormatDateTime('hh:mm', Time) + '/' +
    Exc.Message);
  // вывод сообщения об ошибке пользователю
  { Showing message about error to user [translated] }
  Application.ShowException(Exc);
end;

function TJvgMailSlotClient.Send(str: string): boolean;
var
  strMess: string[250];
  h: THandle;
  i: integer;
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
  h := CreateFile(PChar('\\' + FServerName + '\mailslot\' + FMailSlotName),
    GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if h <> INVALID_HANDLE_VALUE then
  begin
    strMess := str;
    // передача текста ошибки (запись в канал и закрытие канала)
    { Transmitting text of error (putting into channel and closing channel) [translated] }
    WriteFile(h, strMess, Length(strMess) + 1, DWORD(i), nil);
    CloseHandle(h);
  end;
  Result := h <> INVALID_HANDLE_VALUE;
end;

end.


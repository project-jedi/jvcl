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

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

UNIT JvgMailSlots;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   JvComponent,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   extctrls;

TYPE
   TOnNewMessage = PROCEDURE(Sender: TObject; MessageText: STRING) OF OBJECT;

   TJvgMailSlotServer = CLASS(TJvComponent)
   PRIVATE
      FMailSlotName, FLastMessage: STRING;
      FOnNewMessage: TOnNewMessage;

      Timer: TTimer;
      h: THandle;
      str: STRING[250];
      MsgNumber, MsgNext, Read: DWORD;
   PUBLIC
      FEnabled: boolean;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Open;
      PROCEDURE Close;
   PROTECTED
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE OnTimer(Sender: TObject);
   PUBLISHED
      PROPERTY MailSlotName: STRING READ FMailSlotName WRITE FMailSlotName;
      PROPERTY OnNewMessage: TOnNewMessage READ FOnNewMessage WRITE
         FOnNewMessage;
   END;

   TJvgMailSlotClient = CLASS(TJvComponent)
   PRIVATE
      FMailSlotName, FServerName: STRING;
      FOnNewMessage: TOnNewMessage;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      FUNCTION Send(str: STRING): boolean;
   PROTECTED
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE ErrorCatch(Sender: TObject; Exc: Exception);
   PUBLISHED
      PROPERTY ServerName: STRING READ FServerName WRITE FServerName;
      PROPERTY MailSlotName: STRING READ FMailSlotName WRITE FMailSlotName;
      PROPERTY OnNewMessage: TOnNewMessage READ FOnNewMessage WRITE
         FOnNewMessage;
   END;

PROCEDURE Register;

IMPLEMENTATION
USES JvgUtils,
   JvgTypes;

PROCEDURE Register;
BEGIN
//   RegisterComponents('Gl Components', [TJvgMailSlotServer,
//      TJvgMailSlotClient]);
END;

CONSTRUCTOR TJvgMailSlotServer.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FEnabled := true;
   FMailSlotName := 'MailSlot';
   Timer := TTimer.Create(NIL);
   Timer.Enabled := false;
   Timer.OnTimer := OnTimer;
END;

DESTRUCTOR TJvgMailSlotServer.Destroy;
BEGIN
   Timer.Free;
   // закрытие канала
   Close;
   INHERITED;
END;

PROCEDURE TJvgMailSlotServer.Loaded;
BEGIN
   INHERITED;
   Open;
END;

PROCEDURE TJvgMailSlotServer.Open;
BEGIN
   //  if not FEnabled then exit;
   // создание канала с именем MailSlotName - по этому имени к нему
     // будут обращаться клиенты
   h := CreateMailSlot(PChar('\\.\mailslot\' + MailSlotName), 0,
      MAILSLOT_WAIT_FOREVER, NIL);
   //h:=CreateMailSlot('\\.\mailslot\MailSlot',0,MAILSLOT_WAIT_FOREVER,nil);

   IF h = INVALID_HANDLE_VALUE THEN
   BEGIN
      RAISE Exception.Create('TJvgMailSlotServer: Ошибка создания канала !');
   END;
   Timer.Enabled := true;
END;

PROCEDURE TJvgMailSlotServer.Close;
BEGIN
   IF h <> 0 THEN
      CloseHandle(h);
   h := 0;
END;

PROCEDURE TJvgMailSlotServer.OnTimer(Sender: TObject);
VAR
   MessageText                : STRING;
BEGIN
   //  if not FEnabled then exit;

   MessageText := '';
   // определение наличия сообщения в канале
   IF NOT GetMailSlotInfo(h, NIL, DWORD(MsgNext), @MsgNumber, NIL) THEN
   BEGIN
      RAISE Exception.Create('TJvgMailSlotServer: Ошибка сбора информации!');
   END;
   IF MsgNext <> MAILSLOT_NO_MESSAGE THEN
   BEGIN
      beep;
      // чтение сообщения из канала и добавление в текст протокола
      IF ReadFile(h, str, 200, DWORD(Read), NIL) THEN
         MessageText := str
      ELSE
         RAISE
            Exception.Create('TJvgMailSlotServer: Ошибка чтения сообщения !');
   END;

   IF (MessageText <> '') AND Assigned(OnNewMessage) THEN
      OnNewMessage(self, MessageText);

   FLastMessage := MessageText;
END;
//------------------------------------------------------------------------------

CONSTRUCTOR TJvgMailSlotClient.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FMailSlotName := 'MailSlot';
   FServerName := '';
END;

DESTRUCTOR TJvgMailSlotClient.Destroy;
BEGIN
   INHERITED;
END;

PROCEDURE TJvgMailSlotClient.Loaded;
BEGIN
   INHERITED;
   Application.OnException := ErrorCatch;
END;

PROCEDURE TJvgMailSlotClient.ErrorCatch(Sender: TObject; Exc: Exception);
VAR
   UserName                   : ARRAY[0..99] OF char;
   i                          : integer;
BEGIN
   // получение имени пользователя
   i := SizeOf(UserName);
   GetUserName(UserName, DWORD(i));

   Send('/' + UserName + '/' + FormatDateTime('hh:mm', Time) + '/' +
      Exc.Message);
   // вывод сообщения об ошибке пользователю
   Application.ShowException(Exc);
END;

FUNCTION TJvgMailSlotClient.Send(str: STRING): boolean;
VAR
   strMess                    : STRING[250];
   h                          : THandle;
   i                          : integer;
BEGIN
   // открытие канала : MyServer - имя сервера
   // (\\.\\mailslot\xxx - монитор работает на этом же ПК)
   // xxx - имя канала
   IF FServerName = '' THEN
      FServerName := '.\';
   h := CreateFile(PChar('\\' + FServerName + '\mailslot\' + FMailSlotName),
      GENERIC_WRITE,
      FILE_SHARE_READ, NIL, OPEN_EXISTING, 0, 0);
   IF h <> INVALID_HANDLE_VALUE THEN
   BEGIN
      strMess := str;
      // передача текста ошибки (запись в канал и закрытие канала)
      WriteFile(h, strMess, Length(strMess) + 1, DWORD(i), NIL);
      CloseHandle(h);
   END;
   Result := h <> INVALID_HANDLE_VALUE;
END;

END.


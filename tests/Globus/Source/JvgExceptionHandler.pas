{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgExceptionHandler.PAS, released on 2003-01-15.

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

UNIT JvgExceptionHandler;

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
   JvgMailSlots,
   jpeg,
   JvgSysInf;

TYPE
   TExceptionHandlerOption = (fehActiveDesignTime, fehActiveRunTime,
      fehFileLogging, fehSupressExceptions, feScreenShots);
   TExceptionHandlerOptions = SET OF TExceptionHandlerOption;

   EJvgHandlerException = CLASS(Exception)
   PUBLIC
      dummy: Integer;
   END;

   TJvgExceptionHandler = CLASS(TJvComponent)
   PRIVATE
      FActive: boolean;
      FLogFileName: STRING;
      FEMail: STRING;
      FMailSlot: TJvgMailSlotClient;
      { Private declarations }
      FOptions: TExceptionHandlerOptions;
      ScreenShotList: TList;
      SysInfo: TJvgSysInfo;
      PROCEDURE MakeScreenShot(CONST ID: STRING);
      PROCEDURE ShowMessage(E: Exception; CONST Message: STRING);
   PROTECTED
      PROCEDURE OnException(Sender: TObject; E: Exception); VIRTUAL;
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;

   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

      PROCEDURE Refresh;
      PROCEDURE ProcessMessage(CONST ExceptionMessage, Message, ID: STRING);

      PROCEDURE Log_(CONST ID: STRING); OVERLOAD;
      PROCEDURE Log_(E: Exception); OVERLOAD;
      PROCEDURE Log_(E: Exception; CONST ID: STRING); OVERLOAD;

      PROCEDURE Raise_(CONST ID: STRING); OVERLOAD;
      PROCEDURE Raise_(E: Exception); OVERLOAD;
      PROCEDURE Raise_(E: Exception; CONST ID: STRING); OVERLOAD;
      PROCEDURE Raise_(E: Exception; CONST Message, ID: STRING); OVERLOAD;

      PROCEDURE RaiseMessage(CONST Message: STRING);
   PUBLISHED
      PROPERTY Active: boolean READ FActive WRITE FActive DEFAULT true;
      PROPERTY EMail: STRING READ FEMail WRITE FEMail;
      PROPERTY LogFileName: STRING READ FLogFileName WRITE FLogFileName;
      PROPERTY MailSlot: TJvgMailSlotClient READ FMailSlot WRITE FMailSlot;
      PROPERTY Options: TExceptionHandlerOptions READ FOptions WRITE FOptions;
   END;


IMPLEMENTATION
USES JvgUtils,
   JvgFileUtils,
   FileCtrl;

VAR
   ExceptionHandler           : TJvgExceptionHandler;


PROCEDURE AssertErrorHandler_gl(CONST Message, Filename: STRING; LineNumber:
   Integer; ErrorAddr: Pointer);
BEGIN
   IF Assigned(ExceptionHandler) THEN
      ExceptionHandler.ProcessMessage(
         'ExceptAddr=' + Format('%p', [ErrorAddr]) + #13#10 +
         'LineNumber=' + IntToStr(LineNumber) + #13#10 +
         'FileName=' + Filename
         , 'internal JvgExceptionHandler message', '');
   ExceptionClass := EAssertionFailed;
   //  E := CreateAssertException(Message, Filename, LineNumber);
   //  RaiseAssertException(E, ErrorAddr, PChar(@ErrorAddr)+4);
END;

{ TJvgExceptionHandler }

CONSTRUCTOR TJvgExceptionHandler.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FActive := true;
   FOptions := [fehActiveRunTime, fehFileLogging];
   FLogFilename := '_errors_log.txt';
   ScreenShotList := TList.Create;
END;

DESTRUCTOR TJvgExceptionHandler.Destroy;
BEGIN
   ScreenShotList.Free;
   IF Assigned(SysInfo) THEN
      SysInfo.Free;
   Application.OnException := NIL;
   INHERITED;
END;

PROCEDURE TJvgExceptionHandler.OnException(Sender: TObject; E: Exception);
BEGIN
   IF E IS EJvgHandlerException THEN
      exit;

   ProcessMessage(E.Message, '', '');
   IF NOT (fehSupressExceptions IN Options) THEN
      Application.MessageBox(PChar(E.Message), PChar('Что-то случилось - ' +
         Application.Title), MB_OK + MB_ICONSTOP);
END;

PROCEDURE TJvgExceptionHandler.Loaded;
BEGIN
   INHERITED;
   IF Active THEN
      IF ((csDesigning IN ComponentState) AND (fehActiveDesignTime IN Options))
         OR
         (NOT (csDesigning IN ComponentState) AND (fehActiveRunTime IN Options))
            THEN
      BEGIN
         Application.OnException := OnException;
         // Настройка ссылки на глобальный обработчик процедуры Assert
         AssertErrorProc := @AssertErrorHandler_gl;
         ExceptionHandler := self;
      END;
END;

PROCEDURE TJvgExceptionHandler.ProcessMessage(CONST ExceptionMessage, Message,
   ID: STRING);
VAR
   fs                         : TFileStream;
   FileName, Msg              : STRING;
   Buffer                     : Pointer;
BEGIN
   FileName := IIF((pos('//', LogFileName) = 0) AND (pos(':', LogFileName) = 0),
      ExtractFilePath(ParamStr(0)) + LogFileName, LogFileName);

   Msg := '';
   TRY
      IF NOT FileExists(FileName) THEN  { первое создание лога }
      TRY
         IF NOT assigned(SysInfo) THEN
            SysInfo := TJvgSysInfo.Create(NIL);
         SysInfo.Refresh;
         Msg := '##################### exception log header';
         Msg := Msg + #13#10'ExeModule= ' + ParamStr(0);
         Msg := Msg + #13#10'OSPlatform= ' + SysInfo.OSPlatform;
         Msg := Msg + #13#10'CPUKind= ' + IntToStr(SysInfo.CPUKind);
         Msg := Msg + #13#10'CPUName= ' + SysInfo.CPUName;
         Msg := Msg + #13#10'TotalPhys= ' + IntToStr(SysInfo.TotalPhys);
         Msg := Msg + #13#10'AvailPhys= ' + IntToStr(SysInfo.AvailPhys);
         Msg := Msg + #13#10'TotalPageFile= ' + IntToStr(SysInfo.TotalPageFile);
         Msg := Msg + #13#10'AvailPageFile= ' + IntToStr(SysInfo.AvailPageFile);
         Msg := Msg + #13#10'TotalVirtual= ' + IntToStr(SysInfo.TotalVirtual);
         Msg := Msg + #13#10'AvailVirtual= ' + IntToStr(SysInfo.AvailVirtual);
         Msg := Msg + #13#10'ColorDepth= ' + IntToStr(SysInfo.ColorDepth);
         Msg := Msg + #13#10'SystemFont= ' + SysInfo.SystemFont;
         Msg := Msg + #13#10'VRefreshRate= ' + IntToStr(SysInfo.VRefreshRate);
         Msg := Msg + #13#10'GraphicResolution= ' + SysInfo.GraphicResolution;
         Msg := Msg +
            #13#10'##################### exception log header end'#13#10;
      EXCEPT
         { Сбои в работе TJvgSysInfo не должны мешать обработчику }
      END;

      IF ExceptObject <> NIL THEN
      BEGIN
         Msg := Msg + #13#10'Exception class= ' + ExceptObject.ClassName;
         Msg := Msg + #13#10'ExceptAddr= ' + Format('%p', [ExceptAddr]);
      END;
      Msg := Msg + #13#10'Computer= ' + ComputerName;
      Msg := Msg + #13#10'User= ' + UserName;
      Msg := Msg + #13#10'Datetime= ' + DateToStr(date) + '  ' +
         TimeToStr(time);

      IF ID <> '' THEN
         Msg := Msg + #13#10 + 'ID= ' + ID;
      IF ExceptionMessage <> '' THEN
         Msg := Msg + #13#10'ExceptionMessage= ' + ExceptionMessage;
      IF Message <> '' THEN
         Msg := Msg + #13#10'Message= ' + Message;
      Msg := Msg + #13#10;

      IF Assigned(MailSlot) THEN
         MailSlot.Send(Msg);
      IF (fehFileLogging IN Options) AND (trim(LogFileName) <> '') THEN
      BEGIN
         IF FileExists(FileName) THEN
            fs := TFileStream.Create(FileName, fmOpenReadWrite OR
               fmShareDenyNone)
         ELSE
            fs := TFileStream.Create(FileName, fmCreate OR fmShareDenyNone);

         TRY
            fs.Seek(0, soFromEnd);
            Buffer := @Msg[1];
            fs.Write(Buffer^, length(Msg));
         FINALLY
            fs.Free;
         END;

      END;

   EXCEPT
      ON E: Exception DO
         ShowMessage(E, 'Ошибка при сохранении протокола. '#13#10#13#10 +
            E.Message);
   END;

   IF feScreenShots IN Options THEN
      MakeScreenShot(ID);
END;

PROCEDURE TJvgExceptionHandler.MakeScreenShot(CONST ID: STRING);
VAR
   Jpeg                       : TJpegImage;
   BackImage                  : TBitmap;
   LogPath, ScreenShotName    : STRING;
   SysTime                    : TSystemTime;
   DC                         : HDC;
BEGIN
   GetLocalTime(SysTime);

   IF ScreenShotList.IndexOf(ExceptAddr) <> -1 THEN
      exit;                             //already done

   ScreenShotList.Add(ExceptAddr);

   Jpeg := TJpegImage.Create;
   BackImage := TBitmap.Create;
   TRY
      DC := GetDC(0);
      BackImage.Width := Screen.Width;
      BackImage.Height := Screen.Height;
      BitBlt(BackImage.Canvas.Handle, 0, 0, BackImage.Width, BackImage.Height,
         DC, 0, 0, SRCCOPY);
      ReleaseDC(0, DC);
      Jpeg.CompressionQuality := 50;
      Jpeg.Assign(BackImage);

      LogPath := ExtractFilePath(ParamStr(0)) +
         DelFileExt(ExtractFileName(ParamStr(0))) + '\';
      ScreenShotName := DelFileExt(ExtractFileName(ParamStr(0)) + ' ' +
         StringReplace(StringReplace(ID, '{', '', [rfReplaceAll]), '}', '',
         [rfReplaceAll]))
         + Format('  %2d-%2d-%4d %2d-%2d-%2d', [SysTime.wDay, SysTime.wMonth,
            SysTime.wYear, SysTime.wHour, SysTime.wMinute, SysTime.wSecond]);

      ForceDirectories(LogPath);

      Jpeg.SaveToFile(LogPath + ScreenShotName + '.jpg');
   FINALLY
      Jpeg.Free;
      BackImage.Free;
   END;
END;

PROCEDURE TJvgExceptionHandler.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
   IF (AComponent = MailSlot) AND (Operation = opRemove) THEN
   BEGIN
      FMailSlot := NIL;
   END;
END;

PROCEDURE TJvgExceptionHandler.Raise_(CONST ID: STRING);
BEGIN
   Log_(ID);
   IF ID <> '' THEN
      ShowMessage(NIL, ID);
   RAISE EJvgHandlerException.Create('');
END;

PROCEDURE TJvgExceptionHandler.Raise_(E: Exception);
BEGIN
   Log_(E);
   IF E.Message <> '' THEN
      ShowMessage(E, E.Message);
   RAISE EJvgHandlerException.Create('');
END;

PROCEDURE TJvgExceptionHandler.Raise_(E: Exception; CONST ID: STRING);
BEGIN
   Log_(E, ID);
   IF E.Message <> '' THEN
      ShowMessage(E, E.Message + #13#10#13#10 + ID);
   RAISE EJvgHandlerException.Create('');
END;

PROCEDURE TJvgExceptionHandler.Raise_(E: Exception; CONST Message, ID: STRING);
BEGIN
   ProcessMessage(E.Message, Message, ID);
   ShowMessage(E, Message);
   RAISE EJvgHandlerException.Create('');
END;

PROCEDURE TJvgExceptionHandler.Log_(CONST ID: STRING);
BEGIN
   ProcessMessage('', '', ID);
END;

PROCEDURE TJvgExceptionHandler.Log_(E: Exception);
BEGIN
   ProcessMessage(E.Message, '', '');
END;

PROCEDURE TJvgExceptionHandler.Log_(E: Exception; CONST ID: STRING);
BEGIN
   ProcessMessage(E.Message, '', ID);
END;

{ Отображает сообщение, если исключение не внутреннее - EJvgHandlerException }

PROCEDURE TJvgExceptionHandler.ShowMessage(E: Exception; CONST Message: STRING);
BEGIN
   IF NOT (E IS EJvgHandlerException) AND NOT (fehSupressExceptions IN Options)
      THEN
      IF Message <> '' THEN
         Dialogs.ShowMessage(Message)
      ELSE
         Application.ShowException(E);
END;

PROCEDURE TJvgExceptionHandler.Refresh;
BEGIN
   Application.OnException := OnException;
END;

{ Вызывает исключение с заданным сообщением без протоколирования }

PROCEDURE TJvgExceptionHandler.RaiseMessage(CONST Message: STRING);
BEGIN
   Dialogs.ShowMessage(Message);
   RAISE EJvgHandlerException.Create('');
END;

INITIALIZATION
   ExceptionHandler := NIL;

END.


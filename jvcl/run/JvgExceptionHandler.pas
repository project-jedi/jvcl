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

unit JvgExceptionHandler;

interface

uses
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

type
  TExceptionHandlerOption = (fehActiveDesignTime, fehActiveRunTime,
    fehFileLogging, fehSupressExceptions, feScreenShots);
  TExceptionHandlerOptions = set of TExceptionHandlerOption;

  EJvgHandlerException = class(Exception)
  public
    dummy: Integer;
  end;

  TJvgExceptionHandler = class(TJvComponent)
  private
    FActive: boolean;
    FLogFileName: string;
    FEMail: string;
    FMailSlot: TJvgMailSlotClient;
    { Private declarations }
    FOptions: TExceptionHandlerOptions;
    ScreenShotList: TList;
    SysInfo: TJvgSysInfo;
    procedure MakeScreenShot(const ID: string);
    procedure ShowMessage(E: Exception; const Message: string);
  protected
    procedure OnException(Sender: TObject; E: Exception); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Refresh;
    procedure ProcessMessage(const ExceptionMessage, Message, ID: string);

    procedure Log_(const ID: string); overload;
    procedure Log_(E: Exception); overload;
    procedure Log_(E: Exception; const ID: string); overload;

    procedure Raise_(const ID: string); overload;
    procedure Raise_(E: Exception); overload;
    procedure Raise_(E: Exception; const ID: string); overload;
    procedure Raise_(E: Exception; const Message, ID: string); overload;

    procedure RaiseMessage(const Message: string);
  published
    property Active: boolean read FActive write FActive default true;
    property EMail: string read FEMail write FEMail;
    property LogFileName: string read FLogFileName write FLogFileName;
    property MailSlot: TJvgMailSlotClient read FMailSlot write FMailSlot;
    property Options: TExceptionHandlerOptions read FOptions write FOptions;
  end;

implementation
uses JvgUtils,
  JvgFileUtils,
  FileCtrl;

var
  ExceptionHandler: TJvgExceptionHandler;

procedure AssertErrorHandler_gl(const Message, Filename: string; LineNumber:
  Integer; ErrorAddr: Pointer);
begin
  if Assigned(ExceptionHandler) then
    ExceptionHandler.ProcessMessage(
      'ExceptAddr=' + Format('%p', [ErrorAddr]) + #13#10 +
      'LineNumber=' + IntToStr(LineNumber) + #13#10 +
      'FileName=' + Filename
      , 'internal JvgExceptionHandler message', '');
  ExceptionClass := EAssertionFailed;
  //  E := CreateAssertException(Message, Filename, LineNumber);
  //  RaiseAssertException(E, ErrorAddr, PChar(@ErrorAddr)+4);
end;

{ TJvgExceptionHandler }

constructor TJvgExceptionHandler.Create(AOwner: TComponent);
begin
  inherited;
  FActive := true;
  FOptions := [fehActiveRunTime, fehFileLogging];
  FLogFilename := '_errors_log.txt';
  ScreenShotList := TList.Create;
end;

destructor TJvgExceptionHandler.Destroy;
begin
  ScreenShotList.Free;
  if Assigned(SysInfo) then
    SysInfo.Free;
  Application.OnException := nil;
  inherited;
end;

procedure TJvgExceptionHandler.OnException(Sender: TObject; E: Exception);
begin
  if E is EJvgHandlerException then
    exit;

  ProcessMessage(E.Message, '', '');
  if not (fehSupressExceptions in Options) then
    Application.MessageBox(PChar(E.Message), PChar('Что-то случилось - ' +
      Application.Title), MB_OK + MB_ICONSTOP);
end;

procedure TJvgExceptionHandler.Loaded;
begin
  inherited;
  if Active then
    if ((csDesigning in ComponentState) and (fehActiveDesignTime in Options))
      or
      (not (csDesigning in ComponentState) and (fehActiveRunTime in Options)) then
    begin
      Application.OnException := OnException;
      // Настройка ссылки на глобальный обработчик процедуры Assert
      AssertErrorProc := @AssertErrorHandler_gl;
      ExceptionHandler := self;
    end;
end;

procedure TJvgExceptionHandler.ProcessMessage(const ExceptionMessage, Message,
  ID: string);
var
  fs: TFileStream;
  FileName, Msg: string;
  Buffer: Pointer;
begin
  FileName := IIF((pos('//', LogFileName) = 0) and (pos(':', LogFileName) = 0),
    ExtractFilePath(ParamStr(0)) + LogFileName, LogFileName);

  Msg := '';
  try
    if not FileExists(FileName) then { первое создание лога }
    try
      if not assigned(SysInfo) then
        SysInfo := TJvgSysInfo.Create(nil);
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
    except
      { Сбои в работе TJvgSysInfo не должны мешать обработчику }
    end;

    if ExceptObject <> nil then
    begin
      Msg := Msg + #13#10'Exception class= ' + ExceptObject.ClassName;
      Msg := Msg + #13#10'ExceptAddr= ' + Format('%p', [ExceptAddr]);
    end;
    Msg := Msg + #13#10'Computer= ' + ComputerName;
    Msg := Msg + #13#10'User= ' + UserName;
    Msg := Msg + #13#10'Datetime= ' + DateToStr(date) + '  ' +
      TimeToStr(time);

    if ID <> '' then
      Msg := Msg + #13#10 + 'ID= ' + ID;
    if ExceptionMessage <> '' then
      Msg := Msg + #13#10'ExceptionMessage= ' + ExceptionMessage;
    if Message <> '' then
      Msg := Msg + #13#10'Message= ' + Message;
    Msg := Msg + #13#10;

    if Assigned(MailSlot) then
      MailSlot.Send(Msg);
    if (fehFileLogging in Options) and (trim(LogFileName) <> '') then
    begin
      if FileExists(FileName) then
        fs := TFileStream.Create(FileName, fmOpenReadWrite or
          fmShareDenyNone)
      else
        fs := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);

      try
        fs.Seek(0, soFromEnd);
        Buffer := @Msg[1];
        fs.Write(Buffer^, length(Msg));
      finally
        fs.Free;
      end;

    end;

  except
    on E: Exception do
      ShowMessage(E, 'Ошибка при сохранении протокола. '#13#10#13#10 +
        E.Message);
  end;

  if feScreenShots in Options then
    MakeScreenShot(ID);
end;

procedure TJvgExceptionHandler.MakeScreenShot(const ID: string);
var
  Jpeg: TJpegImage;
  BackImage: TBitmap;
  LogPath, ScreenShotName: string;
  SysTime: TSystemTime;
  DC: HDC;
begin
  GetLocalTime(SysTime);

  if ScreenShotList.IndexOf(ExceptAddr) <> -1 then
    exit; //already done

  ScreenShotList.Add(ExceptAddr);

  Jpeg := TJpegImage.Create;
  BackImage := TBitmap.Create;
  try
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
  finally
    Jpeg.Free;
    BackImage.Free;
  end;
end;

procedure TJvgExceptionHandler.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = MailSlot) and (Operation = opRemove) then
  begin
    FMailSlot := nil;
  end;
end;

procedure TJvgExceptionHandler.Raise_(const ID: string);
begin
  Log_(ID);
  if ID <> '' then
    ShowMessage(nil, ID);
  raise EJvgHandlerException.Create('');
end;

procedure TJvgExceptionHandler.Raise_(E: Exception);
begin
  Log_(E);
  if E.Message <> '' then
    ShowMessage(E, E.Message);
  raise EJvgHandlerException.Create('');
end;

procedure TJvgExceptionHandler.Raise_(E: Exception; const ID: string);
begin
  Log_(E, ID);
  if E.Message <> '' then
    ShowMessage(E, E.Message + #13#10#13#10 + ID);
  raise EJvgHandlerException.Create('');
end;

procedure TJvgExceptionHandler.Raise_(E: Exception; const Message, ID: string);
begin
  ProcessMessage(E.Message, Message, ID);
  ShowMessage(E, Message);
  raise EJvgHandlerException.Create('');
end;

procedure TJvgExceptionHandler.Log_(const ID: string);
begin
  ProcessMessage('', '', ID);
end;

procedure TJvgExceptionHandler.Log_(E: Exception);
begin
  ProcessMessage(E.Message, '', '');
end;

procedure TJvgExceptionHandler.Log_(E: Exception; const ID: string);
begin
  ProcessMessage(E.Message, '', ID);
end;

{ Отображает сообщение, если исключение не внутреннее - EJvgHandlerException }

procedure TJvgExceptionHandler.ShowMessage(E: Exception; const Message: string);
begin
  if not (E is EJvgHandlerException) and not (fehSupressExceptions in Options) then
    if Message <> '' then
      Dialogs.ShowMessage(Message)
    else
      Application.ShowException(E);
end;

procedure TJvgExceptionHandler.Refresh;
begin
  Application.OnException := OnException;
end;

{ Вызывает исключение с заданным сообщением без протоколирования }

procedure TJvgExceptionHandler.RaiseMessage(const Message: string);
begin
  Dialogs.ShowMessage(Message);
  raise EJvgHandlerException.Create('');
end;

initialization
  ExceptionHandler := nil;

end.

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgExceptionHandler.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgExceptionHandler;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, jpeg,
  JvTypes, JvComponent, JvgMailSlots, JvgSysInf;

type
  TExceptionHandlerOption = (fehActiveDesignTime, fehActiveRunTime,
    fehFileLogging, fehSupressExceptions, feScreenShots);
  TExceptionHandlerOptions = set of TExceptionHandlerOption;

  EJvgHandlerException = class(EJVCLException);

  TJvgExceptionHandler = class(TJvComponent)
  private
    FActive: Boolean;
    FLogFileName: string;
    FEMail: string;
    FMailSlot: TJvgMailSlotClient;
    FOptions: TExceptionHandlerOptions;
    FScreenShotList: TList;
    FSysInfo: TJvgSysInfo;
    procedure MakeScreenShot(const ID: string);
    procedure ShowMessage(E: Exception; const Msg: string);
  protected
    procedure OnException(Sender: TObject; E: Exception); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure ProcessMessage(const ExceptionMessage, Msg, ID: string);
    procedure Log_(const ID: string); overload;
    procedure Log_(E: Exception); overload;
    procedure Log_(E: Exception; const ID: string); overload;
    procedure Raise_(const ID: string); overload;
    procedure Raise_(E: Exception); overload;
    procedure Raise_(E: Exception; const ID: string); overload;
    procedure Raise_(E: Exception; const Msg, ID: string); overload;
    procedure RaiseMessage(const Msg: string);
  published
    property Active: Boolean read FActive write FActive default True;
    property EMail: string read FEMail write FEMail;
    property LogFileName: string read FLogFileName write FLogFileName;
    property MailSlot: TJvgMailSlotClient read FMailSlot write FMailSlot;
    property Options: TExceptionHandlerOptions read FOptions write FOptions
      default [fehActiveRunTime, fehFileLogging];
  end;

implementation

uses
  FileCtrl,
  JvConsts, JvgUtils, JvgFileUtils;

var
  ExceptionHandler: TJvgExceptionHandler = nil;

procedure AssertErrorHandler_gl(const Msg, FileName: string;
  LineNumber: Integer; ErrorAddr: Pointer);
begin
  if Assigned(ExceptionHandler) then
    ExceptionHandler.ProcessMessage(
      'ExceptAddr=' + Format('%p', [ErrorAddr]) + sLineBreak +
      'LineNumber=' + IntToStr(LineNumber) + sLineBreak +
      'FileName=' + FileName
      , 'internal JvgExceptionHandler message', '');
  ExceptionClass := EAssertionFailed;
  //  E := CreateAssertException(Msg, FileName, LineNumber);
  //  RaiseAssertException(E, ErrorAddr, PChar(@ErrorAddr)+4);
end;

//=== { TJvgExceptionHandler } ===============================================

constructor TJvgExceptionHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  FOptions := [fehActiveRunTime, fehFileLogging];
  FLogFileName := '_errors_log.txt';
  FScreenShotList := TList.Create;
end;

destructor TJvgExceptionHandler.Destroy;
begin
  FScreenShotList.Free;
  FSysInfo.Free;
  Application.OnException := nil;
  inherited Destroy;
end;

procedure TJvgExceptionHandler.OnException(Sender: TObject; E: Exception);
begin
  if E is EJvgHandlerException then
    Exit;

  ProcessMessage(E.Message, '', '');
  if not (fehSupressExceptions in Options) then
    Application.MessageBox(PChar(E.Message),
      PChar('Something has occured - ' + Application.Title),
      MB_OK + MB_ICONSTOP);
end;

procedure TJvgExceptionHandler.Loaded;
begin
  inherited Loaded;
  if Active then
    if ((csDesigning in ComponentState) and (fehActiveDesignTime in Options)) or
      (not (csDesigning in ComponentState) and (fehActiveRunTime in Options)) then
    begin
      Application.OnException := OnException;
      // Настройка ссылки на глобальный обработчик процедуры Assert
      { Setting up link to global Assert handler [translated] }
      AssertErrorProc := @AssertErrorHandler_gl;
      ExceptionHandler := Self;
    end;
end;

procedure TJvgExceptionHandler.ProcessMessage(const ExceptionMessage, Msg, ID: string);
var
  fs: TFileStream;
  FileName, Mesg: string;
begin
  FileName := IIF((Pos('//', LogFileName) = 0) and (Pos(':', LogFileName) = 0),
    ExtractFilePath(ParamStr(0)) + LogFileName, LogFileName);

  Mesg := '';
  try
    if not FileExists(FileName) then
    // { первое создание лога }
    { 1st creation of log }
    try
      if not Assigned(FSysInfo) then
        FSysInfo := TJvgSysInfo.Create(nil);
      FSysInfo.Refresh;
      Mesg := '##################### exception log header';
      Mesg := Mesg + sLineBreak + 'ExeModule= ' + ParamStr(0);
      Mesg := Mesg + sLineBreak + 'OSPlatform= ' + FSysInfo.OSPlatform;
      Mesg := Mesg + sLineBreak + 'CPUKind= ' + IntToStr(FSysInfo.CPUKind);
      Mesg := Mesg + sLineBreak + 'CPUName= ' + FSysInfo.CPUName;
      Mesg := Mesg + sLineBreak + 'TotalPhys= ' + IntToStr(FSysInfo.TotalPhys);
      Mesg := Mesg + sLineBreak + 'AvailPhys= ' + IntToStr(FSysInfo.AvailPhys);
      Mesg := Mesg + sLineBreak + 'TotalPageFile= ' + IntToStr(FSysInfo.TotalPageFile);
      Mesg := Mesg + sLineBreak + 'AvailPageFile= ' + IntToStr(FSysInfo.AvailPageFile);
      Mesg := Mesg + sLineBreak + 'TotalVirtual= ' + IntToStr(FSysInfo.TotalVirtual);
      Mesg := Mesg + sLineBreak + 'AvailVirtual= ' + IntToStr(FSysInfo.AvailVirtual);
      Mesg := Mesg + sLineBreak + 'ColorDepth= ' + IntToStr(FSysInfo.ColorDepth);
      Mesg := Mesg + sLineBreak + 'SystemFont= ' + FSysInfo.SystemFont;
      Mesg := Mesg + sLineBreak + 'VRefreshRate= ' + IntToStr(FSysInfo.VRefreshRate);
      Mesg := Mesg + sLineBreak + 'GraphicResolution= ' + FSysInfo.GraphicResolution;
      Mesg := Mesg + sLineBreak +
        '##################### exception log header end' + sLineBreak;
    except
      // { Сбои в работе TJvgSysInfo не должны мешать обработчику }
      { Handler should not to suffer from errors in TJvgSysInfo's working }
    end;

    if ExceptObject <> nil then
    begin
      Mesg := Mesg + sLineBreak + 'Exception class= ' + ExceptObject.ClassName;
      Mesg := Mesg + sLineBreak + 'ExceptAddr= ' + Format('%p', [ExceptAddr]);
    end;
    Mesg := Mesg + sLineBreak + 'Computer= ' + ComputerName;
    Mesg := Mesg + sLineBreak + 'User= ' + UserName;
    Mesg := Mesg + sLineBreak + 'Datetime= ' + DateToStr(Date) + '  ' +
      TimeToStr(Time);

    if ID <> '' then
      Mesg := Mesg + sLineBreak + 'ID= ' + ID;
    if ExceptionMessage <> '' then
      Mesg := Mesg + sLineBreak + 'ExceptionMessage= ' + ExceptionMessage;
    if Msg <> '' then
      Mesg := Mesg + sLineBreak + 'Message= ' + Msg;
    Mesg := Mesg + sLineBreak;

    if Assigned(MailSlot) then
      MailSlot.Send(Mesg);
    if (fehFileLogging in Options) and (Trim(LogFileName) <> '') then
    begin
      if FileExists(FileName) then
        fs := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
      else
        fs := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
      try
        fs.Seek(0, soFromEnd);
        fs.Write(PChar(Mesg)^, Length(Mesg));
      finally
        fs.Free;
      end;
    end;
  except
    on E: Exception do
      ShowMessage(E, 'An error occured while saving the log' + sLineBreak + sLineBreak + E.Message);
  end;

  if feScreenShots in Options then
    MakeScreenShot(ID);
end;

procedure TJvgExceptionHandler.MakeScreenShot(const ID: string);
var
  JpegImage: TJpegImage;
  BackImage: TBitmap;
  LogPath, ScreenShotName: string;
  SysTime: TSystemTime;
  DC: HDC;
begin
  GetLocalTime(SysTime);

  if FScreenShotList.IndexOf(ExceptAddr) <> -1 then
    Exit; //already done

  FScreenShotList.Add(ExceptAddr);

  JpegImage := TJpegImage.Create;
  BackImage := TBitmap.Create;
  try
    DC := GetDC(0);
    BackImage.Width := Screen.Width;
    BackImage.Height := Screen.Height;
    BitBlt(BackImage.Canvas.Handle, 0, 0, BackImage.Width, BackImage.Height,
      DC, 0, 0, SRCCOPY);
    ReleaseDC(0, DC);
    JpegImage.CompressionQuality := 50;
    JpegImage.Assign(BackImage);

    LogPath := ExtractFilePath(ParamStr(0)) +
      DeleteFileExt(ExtractFileName(ParamStr(0))) + '\';
    ScreenShotName := DeleteFileExt(ExtractFileName(ParamStr(0)) + ' ' +
      StringReplace(StringReplace(ID, '{', '', [rfReplaceAll]), '}', '', [rfReplaceAll])) +
      Format('  %2d-%2d-%4d %2d-%2d-%2d', [SysTime.wDay, SysTime.wMonth,
        SysTime.wYear, SysTime.wHour, SysTime.wMinute, SysTime.wSecond]);

    ForceDirectories(LogPath);

    JpegImage.SaveToFile(LogPath + ScreenShotName + '.jpg');
  finally
    JpegImage.Free;
    BackImage.Free;
  end;
end;

procedure TJvgExceptionHandler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = MailSlot) and (Operation = opRemove) then
    FMailSlot := nil;
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
    ShowMessage(E, E.Message + sLineBreak + sLineBreak + ID);
  raise EJvgHandlerException.Create('');
end;

procedure TJvgExceptionHandler.Raise_(E: Exception; const Msg, ID: string);
begin
  ProcessMessage(E.Message, Msg, ID);
  ShowMessage(E, Msg);
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

//{ Отображает сообщение, если исключение не внутреннее - EJvgHandlerException }
{ Shows message, if the Exception is not internal one. - EJvgHandlerException }

procedure TJvgExceptionHandler.ShowMessage(E: Exception; const Msg: string);
begin
  if not (E is EJvgHandlerException) and not (fehSupressExceptions in Options) then
    if Msg <> '' then
      Dialogs.ShowMessage(Msg)
    else
      Application.ShowException(E);
end;

procedure TJvgExceptionHandler.Refresh;
begin
  Application.OnException := OnException;
end;

//{ Вызывает исключение с заданным сообщением без протоколирования }
{ Fires exception with given message without logging it. }

procedure TJvgExceptionHandler.RaiseMessage(const Msg: string);
begin
  Dialogs.ShowMessage(Msg);
  raise EJvgHandlerException.Create('');
end;

end.


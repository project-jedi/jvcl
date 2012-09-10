{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBaseDBLogonDialog.pas, released on 2006-07-21

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTranslateString;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  JvComponentBase, JvResources;

type
  /// This component is for string-replacement. All replacements are based on
  /// delimiter-encapsulated words. The delimiters can be freely defined. The default
  /// is : "%"
  ///
  /// The following replacements are defined:
  /// APPL_NAME : Name of the application out of the File-Version-Information
  /// COMPANY_NAME : Name of the company of the application out of the File-Version-Information
  /// DATE : Current Date
  /// TIME : Current Time
  /// DATETIME : Current Date/Time
  /// EXENAME : Filename of the application
  /// FILENAME : Filename of the application without extention
  /// FULLDIREXE : Directory of the application exe file
  /// FORMNAME : Name of the current form
  /// FORMCAPTION : Caption of the current form
  /// FILEVERSION : Version of the application file out of the File-Version-Information
  /// PRODUCTVERSION : Product version of the application out of the File-Version-Information
  /// SCREENSIZE : Size of the screen in format widthxheight
  /// DESKTOPSIZE : Size of the desktop in format widthxheight
  TProcessCommandEvent = procedure(Sender: TObject; const Command: string;
    var CommandResult: string; var Changed: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvTranslateString = class(TJvComponent)
  private
    FAppNameHandled: Boolean;
    FAppName: string;
    FCompanyNameHandled: Boolean;
    FCompanyName: string;
    FFileVersionHandled: Boolean;
    FFileVersion: string;
    FProductVersionHandled: Boolean;
    FProductVersion: string;
    FDateFormat: string;
    FDateSeparator: Char;
    FTimeSeparator: Char;
    FDateTimeFormat: string;
    FLeftDelimiter: string;
    FRightDelimiter: string;
    FTimeFormat: string;
    FOnProcessCommand: TProcessCommandEvent;
    function GetFormName: string;
    function GetFormCaption: string;
    function GetVersionInfoAppName: string;
    function GetVersionInfoFileVersion: string;
    function GetVersionInfoProductVersion: string;
    function GetVersionInfoCompanyName: string;
    function ProcessCommand(const Command: string; var CommandResult: string): Boolean;
    procedure SetDateFormat(const Value: string);
    procedure SetDateTimeFormat(const Value: string);
    procedure SetTimeFormat(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function TranslateString(InString: string; var Changed: Boolean): string; overload;
    function TranslateString(InString: string): string; overload;
  published
    property DateFormat: string read FDateFormat write SetDateFormat;
    property DateSeparator: Char read FDateSeparator write FDateSeparator;
    property DateTimeFormat: string read FDateTimeFormat write SetDateTimeFormat;
    property LeftDelimiter: string read FLeftDelimiter write FLeftDelimiter;
    property RightDelimiter: string read FRightDelimiter write FRightDelimiter;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    property TimeSeparator: Char read FTimeSeparator write FTimeSeparator;
    property OnProcessCommand: TProcessCommandEvent read FOnProcessCommand write FOnProcessCommand;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF}
  SysUtils, Types, Forms, Dialogs,
  JclFileUtils,
  JvJCLUtils,
  JvJVCLUtils;

const
  cAppNameMask = 'APPL_NAME';
  cCompanyNameMask = 'COMPANY_NAME';
  cDateMask = 'DATE';
  cTimeMask = 'TIME';
  cDateTimeMask = 'DATETIME';
  cExeNameMask = 'EXENAME';
  cFileNameMask = 'FILENAME';
  cFullDirExeMask = 'FULLDIREXE';
  cFormNameMask = 'FORMNAME';
  cFormCaptionMask = 'FORMCAPTION';
  cFileVersionMask = 'FILEVERSION';
  cProductVersionMask = 'PRODUCTVERSION';
  cScreenSizeMask = 'SCREENSIZE';
  cDesktopSizeMask = 'DESKTOPSIZE';

  cDefaultAppName = 'MyJVCLApplication';
  cDefaultCompanyName = 'MyCompany';
  cDefaultVersion = '0.0.0.0';

constructor TJvTranslateString.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppNameHandled := False;
  FCompanyNameHandled := False;
  FLeftDelimiter := '%';
  FRightDelimiter := '%';
  FDateFormat := 'dd_mm_yyyy';
  FTimeFormat := 'hh_nn_ss';
  FDateTimeFormat := 'dd_mm_yyyy hh_nn_ss';
  FDateSeparator := chr(255);
  FTimeSeparator := chr(255);
  FProductVersionHandled := False;
  FFileVersionHandled := False;
end;

function TJvTranslateString.GetFormName: string;

  function GetName(Comp: TComponent): string;
  begin
    if Assigned(Owner) then
      if Comp is TCustomForm then
        Result := TCustomForm(Comp).Name
      else
        Result := GetName(Comp.Owner)
    else
      Result := '';
  end;

begin
  Result := GetName(Owner);
end;

function TJvTranslateString.GetFormCaption: string;

  function GetCaption(Comp: TComponent): string;
  begin
    if Assigned(Owner) then
      if Comp is TCustomForm then
        Result := TCustomForm(Comp).Caption
      else
        Result := GetCaption(Comp.Owner)
    else
      Result := '';
  end;

begin
  Result := GetCaption(Owner);
end;

function TJvTranslateString.GetVersionInfoAppName: string;
var
  VersionInfo: TJclFileVersionInfo;
begin
  if FAppNameHandled then
    Result := FAppName
  else
  try
    VersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
    try
      Result := VersionInfo.ProductName;
      FAppName := Result;
      FAppNameHandled := True;
    finally
      VersionInfo.Free;
    end;
  except
    on EJclFileVersionInfoError do
    begin
      MessageDlg(Format(RsRootValueReplaceFmt, [cAppNameMask, cDefaultAppName]), mtInformation, [mbOK], 0);
      Result := cDefaultAppName;
      FAppName := Result;
      FAppNameHandled := True;
    end
  else
    raise;
  end;
end;

function TJvTranslateString.GetVersionInfoFileVersion: string;
var
  VersionInfo: TJclFileVersionInfo;
begin
  if FFileVersionHandled then
    Result := FFileVersion
  else
  try
    VersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
    try
      Result := VersionInfo.FileVersion;
      FFileVersion := Result;
      FFileVersionHandled := True;
    finally
      VersionInfo.Free;
    end;
  except
    on EJclFileVersionInfoError do
    begin
      MessageDlg(Format(RsRootValueReplaceFmt, [cFileVersionMask, cDefaultVersion]), mtInformation, [mbOK], 0);
      Result := cDefaultVersion;
      FFileVersion := Result;
      FFileVersionHandled := True;
    end
  else
    raise;
  end;
end;

function TJvTranslateString.GetVersionInfoProductVersion: string;
var
  VersionInfo: TJclFileVersionInfo;
begin
  if FProductVersionHandled then
    Result := FProductVersion
  else
  try
    VersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
    try
      Result := VersionInfo.ProductVersion;
      FProductVersion := Result;
      FProductVersionHandled := True;
    finally
      VersionInfo.Free;
    end;
  except
    on EJclFileVersionInfoError do
    begin
      MessageDlg(Format(RsRootValueReplaceFmt, [cProductVersionMask, cDefaultVersion]), mtInformation, [mbOK], 0);
      Result := cDefaultVersion;
      FProductVersion := Result;
      FProductVersionHandled := True;
    end
  else
    raise;
  end;
end;

function TJvTranslateString.GetVersionInfoCompanyName: string;
var
  VersionInfo: TJclFileVersionInfo;
begin
  if FCompanyNameHandled then
    Result := FCompanyName
  else
  try
    VersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
    try
      Result := VersionInfo.CompanyName;
      FCompanyName := Result;
      FCompanyNameHandled := True;
    finally
      VersionInfo.Free;
    end;
  except
    on EJclFileVersionInfoError do
    begin
      MessageDlg(Format(RsRootValueReplaceFmt, [cCompanyNameMask, cDefaultCompanyName]), mtInformation, [mbOK], 0);
      Result := cDefaultCompanyName;
      FCompanyName := Result;
      FCompanyNameHandled := True;
    end
  else
    raise;
  end;
end;

function TJvTranslateString.ProcessCommand(const Command: string; var CommandResult: string): Boolean;
var
  UpperCommand: string;
begin
  Result := True;
  UpperCommand := Trim(UpperCase(Command));
  if UpperCommand = cAppNameMask then
  begin
    CommandResult := GetVersionInfoAppName;
    if CommandResult = '' then
      CommandResult := ExtractFileName(ChangeFileExt(Application.ExeName, ''));
  end
  else
  if UpperCommand = cCompanyNameMask then
  begin
    CommandResult := GetVersionInfoCompanyName;
    if CommandResult = '' then
      CommandResult := DefCompanyName;
  end
  else
  if UpperCommand = cFileVersionMask then
    CommandResult := GetVersionInfoFileVersion
  else
  if UpperCommand = cProductVersionMask then
    CommandResult := GetVersionInfoProductVersion
  else
  if UpperCommand = cDateMask then
    DateTimeToString(CommandResult, DateFormat, Now)
  else
  if UpperCommand = cTimeMask then
    DateTimeToString(CommandResult, TimeFormat, Now)
  else
  if UpperCommand = cDateTimeMask then
    DateTimeToString(CommandResult, DateTimeFormat, Now)
  else
  if UpperCommand = cExeNameMask then
    CommandResult := Application.ExeName
  else
  if UpperCommand = cFileNameMask then
    CommandResult := ExtractFileName(ChangeFileExt(Application.ExeName, ''))
  else
  if UpperCommand = cFullDirExeMask then
    CommandResult := ExtractFileDir(Application.ExeName)
  else
  if UpperCommand = cFormNameMask then
    CommandResult := GetFormName
  else
  if UpperCommand = cFormCaptionMask then
    CommandResult := GetFormCaption
  else
  if UpperCommand = cScreenSizeMask then
    CommandResult := Format('%dx%d', [Screen.Width, Screen.Height])
  else
  if UpperCommand = cDesktopSizeMask then
    CommandResult := Format('%dx%d', [Screen.DesktopWidth, Screen.DesktopHeight])
  else
    Result := False;
  if Assigned(FOnProcessCommand) then
    FOnProcessCommand(Self, UpperCommand, CommandResult, Result);
end;

procedure TJvTranslateString.SetDateFormat(const Value: string);
var i : Integer;
begin
  FDateFormat := Value;
  if DateSeparator = chr(255) then
    for i := 1 to Length(Value) do
      if not CharInSet(Value[i],['0'..'9']) then
      begin
        DateSeparator:= Value[i];
        Exit;
      end;
end;

procedure TJvTranslateString.SetDateTimeFormat(const Value: string);
begin
  FDateTimeFormat := Value;
end;

procedure TJvTranslateString.SetTimeFormat(const Value: string);
var i : Integer;
begin
  FTimeFormat := Value;
  if TimeSeparator = chr(255) then
    for i := 1 to Length(Value) do
      if not CharInSet(Value[i],['0'..'9']) then
      begin
        TimeSeparator:= Value[i];
        Exit;
      end;
end;

function TJvTranslateString.TranslateString(InString: string): string;
var
  I, J: Integer;
  Command: string;
  CommandResult: string;
begin
  Result := '';
  while InString <> '' do
  begin
    I := Pos(LeftDelimiter, InString);
    if I = 0 then
    begin
      Result := Result + InString;
      InString := '';
    end
    else
    begin
      Result := Result + Copy(InString, 1, I-1);
      Delete(InString, 1, i);
      J := Pos(RightDelimiter, InString);
      if J > 0 then
      begin
        Command := Copy(InString, 1, J-1);
        if ProcessCommand(Command, CommandResult) then
        begin
          Result := Result + CommandResult;
          Delete(InString, 1, J);
        end
        else
        begin
          Result := Result + Copy(InString, 1, J-1);
          Delete(InString, 1, J-1);
        end;
      end
      else
      begin
        Result := Result + LeftDelimiter + InString;
        InString := '';
      end
    end;
  end;
end;

function TJvTranslateString.TranslateString(InString: string; var Changed: Boolean): string;
var
  I, J: Integer;
  Command: string;
  CommandResult: string;
begin
  Result := '';
  Changed := False;
  while InString <> '' do
  begin
    I := Pos(LeftDelimiter, InString);
    if I = 0 then
    begin
      Result := Result + InString;
      InString := '';
    end
    else
    begin
      Result := Result + Copy(InString, 1, I-1);
      Delete(InString, 1, I);
      J := Pos(RightDelimiter, InString);
      Command := Copy(InString, 1, J-1);
      if ProcessCommand(Command, CommandResult) then
      begin
        Result := Result + CommandResult;
        Delete(InString, 1, J);
        Changed := True;
      end
      else
      begin
        Result := Result + Copy(InString, 1, J-1);
        Delete(InString, 1, J-1);
      end;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.


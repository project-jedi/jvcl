{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSysRequirements.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSysRequirements;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, Forms,
  JvComponentBase,
  JclSysInfo;

type
  TJvSystemFont = (fsfSmallFont, fsfBigFont);
  TJvSystemFontSet = set of TJvSystemFont;
  TWindowsVersionSet = set of TWindowsVersion;
  TJvSysReqBehavior = (fsbHalt, fsbWarning);

  TJvWarningEvent = procedure(Sender: TObject; var ReportMessage: string;
    var DoShowWarning, DoHalt: Boolean) of object;

const
  AllWindowsVersions = [wvUnknown];
  AllSystemFonts = [fsfSmallFont, fsfBigFont];

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvSysRequirements = class(TJvComponent)
  private
    FBehavior: TJvSysReqBehavior;
    FEnabled: Boolean;
    FMinColorDepth: Integer;
    FMaxColorDepth: Integer;
    FMinScreenX: Integer;
    FMaxScreenX: Integer;
    FMinScreenY: Integer;
    FMaxScreenY: Integer;
    FSystemFonts: TJvSystemFontSet;
    FWindowsVersions: TWindowsVersionSet;
    FMinVideoRefreshRate: Integer;
    FMaxVideoRefreshRate: Integer;
    FOnWarning: TJvWarningEvent;
    procedure SetMinColorDepth(Value: Integer);
    procedure SetMaxColorDepth(Value: Integer);
    procedure SetMinScreenX(Value: Integer);
    procedure SetMaxScreenX(Value: Integer);
    procedure SetMinScreenY(Value: Integer);
    procedure SetMaxScreenY(Value: Integer);
    procedure SetMinVideoRefreshRate(Value: Integer);
    procedure SetMaxVideoRefreshRate(Value: Integer);
    procedure SetSystemFonts(const Value: TJvSystemFontSet);
    procedure SetWindowsVersions(const Value: TWindowsVersionSet);
  protected
    procedure Loaded; override;
    function TestRequirements(var ReportMessage: string): Boolean; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Behavior: TJvSysReqBehavior read FBehavior write FBehavior default fsbHalt;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property MinColorDepth: Integer read FMinColorDepth write SetMinColorDepth default 0;
    property MaxColorDepth: Integer read FMaxColorDepth write SetMaxColorDepth default 0;
    property MinScreenX: Integer read FMinScreenX write SetMinScreenX default 0;
    property MaxScreenX: Integer read FMaxScreenX write SetMaxScreenX default 0;
    property MinScreenY: Integer read FMinScreenY write SetMinScreenY default 0;
    property MaxScreenY: Integer read FMaxScreenY write SetMaxScreenY default 0;
    property MinVideoRefreshRate: Integer read FMinVideoRefreshRate write SetMinVideoRefreshRate default 0;
    property MaxVideoRefreshRate: Integer read FMaxVideoRefreshRate write SetMaxVideoRefreshRate default 0;
    property WindowsVersions: TWindowsVersionSet read FWindowsVersions write SetWindowsVersions default
      AllWindowsVersions;
    property SystemFonts: TJvSystemFontSet read FSystemFonts write SetSystemFonts default AllSystemFonts;
    property OnWarning: TJvWarningEvent read FOnWarning write FOnWarning;
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
  JvResources;

constructor TJvSysRequirements.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBehavior := fsbHalt;
  FEnabled := True;
  FMinColorDepth := 0;
  FMaxColorDepth := 0;
  FMinScreenX := 0;
  FMaxScreenX := 0;
  FMinScreenY := 0;
  FMaxScreenY := 0;
  FMinVideoRefreshRate := 0;
  FMaxVideoRefreshRate := 0;
  FWindowsVersions := AllWindowsVersions;
  FSystemFonts := AllSystemFonts;
end;

procedure TJvSysRequirements.Loaded;
var
  ReportMessage: string;
  DoShowWarning, DoHalt: Boolean;
  Icon: Integer;
begin
  inherited Loaded;
  if Enabled and not (csDesigning in ComponentState) then
    if not TestRequirements(ReportMessage) then
    begin
      DoShowWarning := True;
      DoHalt := Behavior = fsbHalt;
      if Assigned(FOnWarning) then
        FOnWarning(Self, ReportMessage, DoShowWarning, DoHalt);
      if DoHalt then
        Icon := MB_ICONERROR
      else
        Icon := MB_ICONWARNING;
      if DoShowWarning then
        Application.MessageBox(PChar(ReportMessage),
          PChar(Format(RsSysRequirementsCaption, [ExtractFileName(ParamStr(0))])), MB_OK + Icon);
      if DoHalt then
        Application.Terminate;
    end;
end;

function TJvSysRequirements.TestRequirements(var ReportMessage: string): Boolean;
var
  DC: HDC;
  Refresh, ScreenX, ScreenY, BitsPerPixel, LogPixels: Integer;

  procedure Test(Value: Boolean; const ErrMsg: string);
  begin
    Result := Result and Value;
    if not Value then
    begin
      if ReportMessage <> '' then
        ReportMessage := ReportMessage + sLineBreak;
      ReportMessage := ReportMessage + ErrMsg;
    end;
  end;

  procedure TestMinMax(TestVal, MinVal, MaxVal: Integer; const MinMsg, MaxMsg, BetweenMsg: string);
  begin
    if (MinVal > 0) or (MaxVal > 0) then
      if (MinVal > 0) and (MaxVal > 0) then
        Test((TestVal >= MinVal) and (TestVal <= MaxVal), Format(BetweenMsg, [TestVal, MinVal, MaxVal]))
      else
      if MinVal > 0 then
        Test(TestVal >= MinVal, Format(MinMsg, [TestVal, MinVal]))
      else
        Test(TestVal <= MaxVal, Format(MaxMsg, [TestVal, MaxVal]));
  end;

begin
  ReportMessage := '';
  Result := True;

  DC := GetDC(HWND_DESKTOP);
  BitsPerPixel := GetDeviceCaps(DC, BITSPIXEL);
  ScreenX := GetDeviceCaps(DC, HORZRES);
  ScreenY := GetDeviceCaps(DC, VERTRES);
  Refresh := GetDeviceCaps(DC, VREFRESH);
  LogPixels := GetDeviceCaps(DC, LOGPIXELSX);
  ReleaseDC(HWND_DESKTOP, DC);

  TestMinMax(BitsPerPixel, MinColorDepth, MaxColorDepth, RsMinColorDepthReq, RsMaxColorDepthReq,
    RsBetweenColorDepthReq);
  TestMinMax(ScreenX, MinScreenX, MaxScreenX, RsMinScreenXReq, RsMaxScreenXReq, RsBetweenScreenXReq);
  TestMinMax(ScreenY, MinScreenY, MaxScreenY, RsMinScreenYReq, RsMaxScreenYReq, RsBetweenScreenYReq);
  TestMinMax(Refresh, MinVideoRefreshRate, MaxVideoRefreshRate, RsMinRefreshReq, RsMaxRefreshReq,
    RsBetweenRefreshReq);
  if not (wvUnknown in WindowsVersions) then
    Test(GetWindowsVersion in WindowsVersions, RsWindowsVersionReq);
  if SystemFonts = [fsfSmallFont] then
    Test(LogPixels = 96, RsSystemFontSmallReq);
  if SystemFonts = [fsfBigFont] then
    Test(LogPixels = 120, RsSystemFontBigReq);
end;

procedure TJvSysRequirements.SetMinColorDepth(Value: Integer);
begin
  Value := Abs(Value);
  FMinColorDepth := Value;
  if (Value > MaxColorDepth) and (MaxColorDepth <> 0) then
    MaxColorDepth := Value;
end;

procedure TJvSysRequirements.SetMaxColorDepth(Value: Integer);
begin
  Value := Abs(Value);
  FMaxColorDepth := Value;
  if (Value <> 0) and (Value < MinColorDepth) then
    MinColorDepth := Value;
end;

procedure TJvSysRequirements.SetMinScreenX(Value: Integer);
begin
  Value := Abs(Value);
  FMinScreenX := Value;
  if (Value > MaxScreenX) and (MaxScreenX <> 0) then
    MaxScreenX := Value;
end;

procedure TJvSysRequirements.SetMaxScreenX(Value: Integer);
begin
  Value := Abs(Value);
  FMaxScreenX := Value;
  if (Value <> 0) and (MaxScreenX <> 0) then
    MinScreenX := Value;
end;

procedure TJvSysRequirements.SetMinScreenY(Value: Integer);
begin
  Value := Abs(Value);
  FMinScreenY := Value;
  if (Value > MaxScreenY) and (MaxScreenY <> 0) then
    MaxScreenY := Value;
end;

procedure TJvSysRequirements.SetMaxScreenY(Value: Integer);
begin
  Value := Abs(Value);
  FMaxScreenY := Value;
  if (Value <> 0) and (Value < MinScreenY) then
    MinScreenY := Value;
end;

procedure TJvSysRequirements.SetMinVideoRefreshRate(Value: Integer);
begin
  Value := Abs(Value);
  FMinVideoRefreshRate := Value;
  if (Value > MaxVideoRefreshRate) and (MaxVideoRefreshRate <> 0) then
    MaxVideoRefreshRate := Value;
end;

procedure TJvSysRequirements.SetMaxVideoRefreshRate(Value: Integer);
begin
  Value := Abs(Value);
  FMaxVideoRefreshRate := Value;
  if (Value <> 0) and (Value < MinVideoRefreshRate) then
    MinVideoRefreshRate := Value;
end;

procedure TJvSysRequirements.SetSystemFonts(const Value: TJvSystemFontSet);
begin
  if Value = [] then
    FSystemFonts := [fsfSmallFont]
  else
    FSystemFonts := Value;
end;

procedure TJvSysRequirements.SetWindowsVersions(const Value: TWindowsVersionSet);
begin
  if ((wvUnknown in Value) and not (wvUnknown in FWindowsVersions)) or (Value = []) then
    FWindowsVersions := [wvUnknown]
  else
  if (wvUnknown in FWindowsVersions) and (Value <> [wvUnknown]) then
    FWindowsVersions := Value - [wvUnknown]
  else
    FWindowsVersions := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
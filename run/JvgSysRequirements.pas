{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSysRequirements.PAS, released on 2003-01-15.

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

unit JvgSysRequirements;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  JVComponent,
  Dialogs;

type
  TglMinVideoVRefreshRate = (frrIgnore, frr70Hertz, frr75Hertz, frr85Hertz);
  TglMinGraphicResolution = (fgrIgnore, fgr800x600, fgr1024x768);
  TglMinColorDepth = (fcdIgnore, fcd16BitColor, fcd32BitColor);
  TglSystemFont = (fsfIgnore, fsfSmallFont, fsfBigFont);
  //  TglProcessor                  = (fsrPentium, fsrMMX);
  TglOS_ = (fosWindowsNT, fosWindows95);
  TglOS = set of TglOS_;

  TglRequirements_ = (fsrVideoVRefreshRate, fsrGraphicResolution,
    fsrColorDepth, fsrSystemFont, {fsrProcessor, } fsrOSPlatform);
  TglRequirements = set of TglRequirements_;

  TglSysReqBehavior = (fsbHalt, fsbWarning);
  OnWarningEvent = procedure(Sender: TObject; var ReportMessage: string; var
    doShowWarning, doHalt: boolean) of object;

  TJvgSysRequirements = class(TJvComponent)
  private
    FEnabled: boolean;
    FMinColorDepth: TglMinColorDepth;
    FMinGraphicResolution: TglMinGraphicResolution;
    FOSPlatform: TglOS;
    FSystemFont: TglSystemFont;
    FMinVideoVRefreshRate: TglMinVideoVRefreshRate;
    FBehavior: TglSysReqBehavior;
    FOnWarning: OnWarningEvent;
    { Private declarations }
  protected

    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    function TestRequirements(var ReportMessage: string): boolean;
  published
    property Enabled: boolean read FEnabled write FEnabled default true;
    property MinVideoVRefreshRate: TglMinVideoVRefreshRate read
      FMinVideoVRefreshRate write FMinVideoVRefreshRate;
    property MinGraphicResolution: TglMinGraphicResolution read
      FMinGraphicResolution write FMinGraphicResolution;
    property MinColorDepth: TglMinColorDepth read FMinColorDepth write
      FMinColorDepth;
    property SystemFont: TglSystemFont read FSystemFont write FSystemFont;
    //    property Processor: TglProcessor;
    property OSPlatform: TglOS read FOSPlatform write FOSPlatform;
    property Behavior: TglSysReqBehavior read FBehavior write FBehavior;
    property OnWarning: OnWarningEvent read FOnWarning write FOnWarning;
  end;

procedure Register;

implementation
uses JvgConstSysRequirements;

procedure Register;
begin
  //  RegisterComponents('Gl Components', [TJvgSysRequirements]);
end;

{ TJvgSysRequirements }

constructor TJvgSysRequirements.Create(AOwner: TComponent);
begin
  inherited;
  //... defaults
  FEnabled := true;
end;

procedure TJvgSysRequirements.Loaded;
var
  ReportMessage: string;
  doShowWarning, doHalt: boolean;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    if not TestRequirements(ReportMessage) then
    begin

      doShowWarning := true;
      doHalt := Behavior = fsbHalt;

      if Assigned(OnWarning) then
        OnWarning(Self, ReportMessage, doShowWarning, doHalt);

      if doShowWarning then
        Application.MessageBox(PChar(ReportMessage),
          PChar(ExtractFilename(ParamStr(0))), MB_OK + MB_ICONINFORMATION);
      if doHalt then
        Application.Terminate;
    end;
end;

function TJvgSysRequirements.TestRequirements(var ReportMessage: string):
  boolean;
var
  DC: HDC;
  OSVersionInfo: TOSVersionInfo;

  procedure Test(Value: boolean; const ErrMsg: string);
  begin
    Result := Result and Value;
    if not Value then
    begin
      if ReportMessage <> '' then
        ReportMessage := ReportMessage + #13#10#13#10;
      ReportMessage := ReportMessage + ErrMsg;
    end;
  end;
begin
  ReportMessage := '';
  Result := true;
  DC := GetDC(0);

  case MinVideoVRefreshRate of
    frr70Hertz: Test(GetDeviceCaps(DC, VREFRESH) >= 70,
        Format(ERR_VideoVRefreshRate, [70]));
    frr75Hertz: Test(GetDeviceCaps(DC, VREFRESH) >= 75,
        Format(ERR_VideoVRefreshRate, [75]));
    frr85Hertz: Test(GetDeviceCaps(DC, VREFRESH) >= 85,
        Format(ERR_VideoVRefreshRate, [85]));
  end;

  case MinGraphicResolution of
    fgr800x600: Test((GetDeviceCaps(DC, HORZRES) >= 800) and
        (GetDeviceCaps(DC, VERTRES) >= 600), Format(ERR_GraphicResolution,
        ['800x600']));
    fgr1024x768: Test((GetDeviceCaps(DC, HORZRES) >= 1024) and
        (GetDeviceCaps(DC, VERTRES) >= 768), Format(ERR_GraphicResolution,
        ['1024x768']));
  end;

  case MinColorDepth of
    fcd16BitColor: Test(GetDeviceCaps(DC, BITSPIXEL) >= 16,
        Format(ERR_ColorDepth, ['65 536 (hi color)']));
    fcd32BitColor: Test(GetDeviceCaps(DC, BITSPIXEL) >= 32,
        Format(ERR_ColorDepth, ['4 294 967 296 (true color)']));
  end;

  case SystemFont of
    fsfSmallFont: Test(GetDeviceCaps(DC, LOGPIXELSX) = 96,
        Format(ERR_SystemFont, ['мелкий']));
    fsfBigFont: Test(GetDeviceCaps(DC, LOGPIXELSX) = 120,
        Format(ERR_SystemFont, ['крупный']));
  end;

  OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);

  GetVersionEx(OSVersionInfo);

  if OSPlatform = [fosWindowsNT] then
    Test(OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT,
      Format(ERR_OSPlatform, ['Windows NT/2000']));
  if OSPlatform = [fosWindows95] then
    Test(OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS,
      Format(ERR_OSPlatform, ['Windows 95/98']));

  {  fHalt := fHalt or fsrVideoVRefreshRate in HaltOptions;
    fHalt := fHalt or fsrGraphicResolution in HaltOptions;
    fHalt := fHalt or fsrColorDepth in HaltOptions;
    fHalt := fHalt or fsrSystemFont in HaltOptions;
    fHalt := fHalt or fsrOSPlatform in HaltOptions;
  }
  //  IsProcessorFeaturePresent(PF_MMX_INSTRUCTIONS_AVAILABLE	)

  ReleaseDC(0, DC);
end;

end.

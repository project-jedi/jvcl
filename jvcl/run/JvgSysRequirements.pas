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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgSysRequirements;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent;

type
  TglMinVideoVRefreshRate = (frrIgnore, frr70Hertz, frr75Hertz, frr85Hertz);
  TglMinGraphicResolution = (fgrIgnore, fgr800x600, fgr1024x768);
  TglMinColorDepth = (fcdIgnore, fcd16BitColor, fcd32BitColor);
  TglSystemFont = (fsfIgnore, fsfSmallFont, fsfBigFont);
  //  TglProcessor = (fsrPentium, fsrMMX);
  TglOS = (fosWindowsNT, fosWindows95);
  TglOSSet = set of TglOS;

  TglRequirement = (fsrVideoVRefreshRate, fsrGraphicResolution,
    fsrColorDepth, fsrSystemFont, {fsrProcessor, } fsrOSPlatform);
  TglRequirements = set of TglRequirement;

  TglSysReqBehavior = (fsbHalt, fsbWarning);
  TglWarningEvent = procedure(Sender: TObject; var ReportMessage: string;
    var DoShowWarning, DoHalt: Boolean) of object;

  TJvgSysRequirements = class(TJvComponent)
  private
    FEnabled: Boolean;
    FMinColorDepth: TglMinColorDepth;
    FMinGraphicResolution: TglMinGraphicResolution;
    FOSPlatform: TglOSSet;
    FSystemFont: TglSystemFont;
    FMinVideoVRefreshRate: TglMinVideoVRefreshRate;
    FBehavior: TglSysReqBehavior;
    FOnWarning: TglWarningEvent;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    function TestRequirements(var ReportMessage: string): Boolean;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property MinVideoVRefreshRate: TglMinVideoVRefreshRate read
      FMinVideoVRefreshRate write FMinVideoVRefreshRate;
    property MinGraphicResolution: TglMinGraphicResolution read
      FMinGraphicResolution write FMinGraphicResolution;
    property MinColorDepth: TglMinColorDepth read FMinColorDepth write
      FMinColorDepth;
    property SystemFont: TglSystemFont read FSystemFont write FSystemFont;
    //    property Processor: TglProcessor;
    property OSPlatform: TglOSSet read FOSPlatform write FOSPlatform;
    property Behavior: TglSysReqBehavior read FBehavior write FBehavior;
    property OnWarning: TglWarningEvent read FOnWarning write FOnWarning;
  end;

implementation

uses
  {$IFDEF USEJVCL}
  JvResources;
  {$ENDIF USEJVCL}

{$IFNDEF USEJVCL}
resourcestring
(* RUSSIAN
  RsVideoVRefreshRate = 'Частота обновления экрана должна быть %d герц или выше. Измените частоту обновления в свойствах экрана.';
  RsGraphicResolution = 'Разрешение экрана должно быть %s точек или выше. Измените разрешение в свойствах экрана.';
  RsColorDepth = 'Количество цветов экрана должно быть %s цветов или выше. Измените число цветов в свойствах экрана.';
  RsSystemFont = 'В системе должен быть установлен %s шрифт. Измените вид шрифта в свойствах экрана.';
  RsOSPlatform = 'Для работы программы необходима операционная система %s.';
*)
  RsVideoVRefreshRate = 'The monitor refresh rate should be %d Hertz or higher. Change monitor refresh rate in Monitor Control Panel.';
  RsGraphicResolution = 'The screen resolution should be equal %s pixels or higher. Change screen resolution in Monitor Control Panel.';
  RsColorDepth = 'The number of colors of the screen should be equal to %s colors or higher. Change screen colors in Monitor Control Panel.';
  RsSystemFont = 'In system the small font should be established. Change to small fonts in Monitor Control Panel.';
  RsOSPlatform = 'The program requires %s or better.';
{$ENDIF !USEJVCL}

constructor TJvgSysRequirements.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
end;

procedure TJvgSysRequirements.Loaded;
var
  ReportMessage: string;
  DoShowWarning, DoHalt: Boolean;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    if not TestRequirements(ReportMessage) then
    begin
      DoShowWarning := True;
      DoHalt := Behavior = fsbHalt;
      if Assigned(FOnWarning) then
        FOnWarning(Self, ReportMessage,DoShowWarning, DoHalt);
      if DoShowWarning then
        Application.MessageBox(PChar(ReportMessage),
          PChar(ExtractFilename(ParamStr(0))), MB_OK + MB_ICONINFORMATION);
      if DoHalt then
        Application.Terminate;
    end;
end;

function TJvgSysRequirements.TestRequirements(var ReportMessage: string): Boolean;
var
  DC: HDC;
  OSVersionInfo: TOSVersionInfo;

  procedure Test(Value: Boolean; const ErrMsg: string);
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
  Result := True;
  DC := GetDC(HWND_DESKTOP);

  case MinVideoVRefreshRate of
    frr70Hertz:
      Test(GetDeviceCaps(DC, VREFRESH) >= 70, Format(RsVideoVRefreshRate, [70]));
    frr75Hertz:
      Test(GetDeviceCaps(DC, VREFRESH) >= 75, Format(RsVideoVRefreshRate, [75]));
    frr85Hertz:
      Test(GetDeviceCaps(DC, VREFRESH) >= 85, Format(RsVideoVRefreshRate, [85]));
  end;

  case MinGraphicResolution of
    fgr800x600:
      Test((GetDeviceCaps(DC, HORZRES) >= 800) and
        (GetDeviceCaps(DC, VERTRES) >= 600), Format(RsGraphicResolution, ['800x600']));
    fgr1024x768:
      Test((GetDeviceCaps(DC, HORZRES) >= 1024) and
        (GetDeviceCaps(DC, VERTRES) >= 768), Format(RsGraphicResolution, ['1024x768']));
  end;

  case MinColorDepth of
    fcd16BitColor:
      Test(GetDeviceCaps(DC, BITSPIXEL) >= 16, Format(RsColorDepth, ['65.536 (hi color)']));
    fcd32BitColor: Test(GetDeviceCaps(DC, BITSPIXEL) >= 32,
        Format(RsColorDepth, ['4.294.967.296 (true color)']));
  end;

  case SystemFont of
    fsfSmallFont:
      Test(GetDeviceCaps(DC, LOGPIXELSX) = 96, Format(RsSystemFont, ['small']));
    fsfBigFont:
      Test(GetDeviceCaps(DC, LOGPIXELSX) = 120, Format(RsSystemFont, ['large']));
  end;

  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);

  GetVersionEx(OSVersionInfo);

  if OSPlatform = [fosWindowsNT] then
    Test(OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT,
      Format(RsOSPlatform, ['Windows NT/2000']));
  if OSPlatform = [fosWindows95] then
    Test(OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS,
      Format(RsOSPlatform, ['Windows 95/98']));

  {  fHalt := fHalt or fsrVideoVRefreshRate in HaltOptions;
    fHalt := fHalt or fsrGraphicResolution in HaltOptions;
    fHalt := fHalt or fsrColorDepth in HaltOptions;
    fHalt := fHalt or fsrSystemFont in HaltOptions;
    fHalt := fHalt or fsrOSPlatform in HaltOptions;
  }
  //  IsProcessorFeaturePresent(PF_MMX_INSTRUCTIONS_AVAILABLE)

  ReleaseDC(HWND_DESKTOP, DC);
end;

end.

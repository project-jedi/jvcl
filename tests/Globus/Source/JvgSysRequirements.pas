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

UNIT JvgSysRequirements;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   JVComponent,
   Dialogs;

TYPE
   TglMinVideoVRefreshRate = (frrIgnore, frr70Hertz, frr75Hertz, frr85Hertz);
   TglMinGraphicResolution = (fgrIgnore, fgr800x600, fgr1024x768);
   TglMinColorDepth = (fcdIgnore, fcd16BitColor, fcd32BitColor);
   TglSystemFont = (fsfIgnore, fsfSmallFont, fsfBigFont);
   //  TglProcessor                  = (fsrPentium, fsrMMX);
   TglOS_ = (fosWindowsNT, fosWindows95);
   TglOS = SET OF TglOS_;

   TglRequirements_ = (fsrVideoVRefreshRate, fsrGraphicResolution,
      fsrColorDepth, fsrSystemFont, {fsrProcessor, } fsrOSPlatform);
   TglRequirements = SET OF TglRequirements_;

   TglSysReqBehavior = (fsbHalt, fsbWarning);
   OnWarningEvent = PROCEDURE(Sender: TObject; VAR ReportMessage: STRING; VAR
      doShowWarning, doHalt: boolean) OF OBJECT;

   TJvgSysRequirements = CLASS(TJvComponent)
   PRIVATE
      FEnabled: boolean;
      FMinColorDepth: TglMinColorDepth;
      FMinGraphicResolution: TglMinGraphicResolution;
      FOSPlatform: TglOS;
      FSystemFont: TglSystemFont;
      FMinVideoVRefreshRate: TglMinVideoVRefreshRate;
      FBehavior: TglSysReqBehavior;
      FOnWarning: OnWarningEvent;
      { Private declarations }
   PROTECTED

      PROCEDURE Loaded; OVERRIDE;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      FUNCTION TestRequirements(VAR ReportMessage: STRING): boolean;
   PUBLISHED
      PROPERTY Enabled: boolean READ FEnabled WRITE FEnabled DEFAULT true;
      PROPERTY MinVideoVRefreshRate: TglMinVideoVRefreshRate READ
         FMinVideoVRefreshRate WRITE FMinVideoVRefreshRate;
      PROPERTY MinGraphicResolution: TglMinGraphicResolution READ
         FMinGraphicResolution WRITE FMinGraphicResolution;
      PROPERTY MinColorDepth: TglMinColorDepth READ FMinColorDepth WRITE
         FMinColorDepth;
      PROPERTY SystemFont: TglSystemFont READ FSystemFont WRITE FSystemFont;
      //    property Processor: TglProcessor;
      PROPERTY OSPlatform: TglOS READ FOSPlatform WRITE FOSPlatform;
      PROPERTY Behavior: TglSysReqBehavior READ FBehavior WRITE FBehavior;
      PROPERTY OnWarning: OnWarningEvent READ FOnWarning WRITE FOnWarning;
   END;

PROCEDURE Register;

IMPLEMENTATION
USES JvgConstSysRequirements;

PROCEDURE Register;
BEGIN
   //  RegisterComponents('Gl Components', [TJvgSysRequirements]);
END;

{ TJvgSysRequirements }

CONSTRUCTOR TJvgSysRequirements.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   //... defaults
   FEnabled := true;
END;

PROCEDURE TJvgSysRequirements.Loaded;
VAR
   ReportMessage              : STRING;
   doShowWarning, doHalt      : boolean;
BEGIN
   INHERITED;
   IF NOT (csDesigning IN ComponentState) THEN
      IF NOT TestRequirements(ReportMessage) THEN
      BEGIN

         doShowWarning := true;
         doHalt := Behavior = fsbHalt;

         IF Assigned(OnWarning) THEN
            OnWarning(Self, ReportMessage, doShowWarning, doHalt);

         IF doShowWarning THEN
            Application.MessageBox(PChar(ReportMessage),
               PChar(ExtractFilename(ParamStr(0))), MB_OK + MB_ICONINFORMATION);
         IF doHalt THEN
            Application.Terminate;
      END;
END;

FUNCTION TJvgSysRequirements.TestRequirements(VAR ReportMessage: STRING):
   boolean;
VAR
   DC                         : HDC;
   OSVersionInfo              : TOSVersionInfo;

   PROCEDURE Test(Value: boolean; CONST ErrMsg: STRING);
   BEGIN
      Result := Result AND Value;
      IF NOT Value THEN
      BEGIN
         IF ReportMessage <> '' THEN
            ReportMessage := ReportMessage + #13#10#13#10;
         ReportMessage := ReportMessage + ErrMsg;
      END;
   END;
BEGIN
   ReportMessage := '';
   Result := true;
   DC := GetDC(0);

   CASE MinVideoVRefreshRate OF
      frr70Hertz: Test(GetDeviceCaps(DC, VREFRESH) >= 70,
         Format(ERR_VideoVRefreshRate, [70]));
      frr75Hertz: Test(GetDeviceCaps(DC, VREFRESH) >= 75,
         Format(ERR_VideoVRefreshRate, [75]));
      frr85Hertz: Test(GetDeviceCaps(DC, VREFRESH) >= 85,
         Format(ERR_VideoVRefreshRate, [85]));
   END;

   CASE MinGraphicResolution OF
      fgr800x600: Test((GetDeviceCaps(DC, HORZRES) >= 800) AND
         (GetDeviceCaps(DC, VERTRES) >= 600), Format(ERR_GraphicResolution,
         ['800x600']));
      fgr1024x768: Test((GetDeviceCaps(DC, HORZRES) >= 1024) AND
         (GetDeviceCaps(DC, VERTRES) >= 768), Format(ERR_GraphicResolution,
         ['1024x768']));
   END;

   CASE MinColorDepth OF
      fcd16BitColor: Test(GetDeviceCaps(DC, BITSPIXEL) >= 16,
         Format(ERR_ColorDepth, ['65 536 (hi color)']));
      fcd32BitColor: Test(GetDeviceCaps(DC, BITSPIXEL) >= 32,
         Format(ERR_ColorDepth, ['4 294 967 296 (true color)']));
   END;

   CASE SystemFont OF
      fsfSmallFont: Test(GetDeviceCaps(DC, LOGPIXELSX) = 96,
         Format(ERR_SystemFont, ['мелкий']));
      fsfBigFont: Test(GetDeviceCaps(DC, LOGPIXELSX) = 120,
         Format(ERR_SystemFont, ['крупный']));
   END;

   OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);

   GetVersionEx(OSVersionInfo);

   IF OSPlatform = [fosWindowsNT] THEN
      Test(OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT,
         Format(ERR_OSPlatform, ['Windows NT/2000']));
   IF OSPlatform = [fosWindows95] THEN
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
END;

END.


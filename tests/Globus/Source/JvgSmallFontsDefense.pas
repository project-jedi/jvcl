{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSmallFontsDefense.PAS, released on 2003-01-15.

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

// Component prevents your apps from BIG fonts.

UNIT JvgSmallFontsDefense;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   jvComponent,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   grids;

TYPE
   TglSmallFontsDefenceOptions_ = (fdoExcludeGrids);
   TglSmallFontsDefenceOptions = SET OF TglSmallFontsDefenceOptions_;

   TJvgSmallFontsDefence = CLASS(TJvComponent)
   PRIVATE
      FOptions: TglSmallFontsDefenceOptions;
      PROCEDURE UpdateFonts(Control: TWinControl);
      PROCEDURE SetOptions(CONST Value: TglSmallFontsDefenceOptions);
      { Private declarations }
   PROTECTED
      PROCEDURE Loaded; OVERRIDE;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
   PUBLISHED
      PROPERTY Options: TglSmallFontsDefenceOptions READ FOptions WRITE
         SetOptions;
   END;

PROCEDURE Register;

IMPLEMENTATION
USES JvgUtils,
   JvgTypes;

PROCEDURE Register;
BEGIN
END;

{ TJvgSmallFontsDefence }

CONSTRUCTOR TJvgSmallFontsDefence.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   IF (Owner IS TForm) THEN
      (Owner AS TForm).Scaled := false;
END;

PROCEDURE TJvgSmallFontsDefence.Loaded;
BEGIN
   INHERITED;
   IF (Owner IS TForm) THEN
      (Owner AS TForm).Scaled := false;
   IF csDesigning IN ComponentState THEN
   BEGIN
      IF NOT IsSmallFonts THEN
         ShowMessage('Проектирование приложения в режиме крупных шрифтов недопустимо!'#13#10'Компонент TJvgSmallFontsDefence отказывается работать в таких условиях.');
   END
   ELSE
      UpdateFonts((Owner AS TForm));
END;

PROCEDURE TJvgSmallFontsDefence.SetOptions(CONST Value:
   TglSmallFontsDefenceOptions);
BEGIN
   FOptions := Value;
END;

PROCEDURE TJvgSmallFontsDefence.UpdateFonts(Control: TWinControl);
VAR
   i                          : integer;

   PROCEDURE UpdateFont(Font: TFont);
   BEGIN
      IF CompareText(Font.Name, 'MS Sans Serif') <> 0 THEN
         exit;
      Font.Name := 'Arial';
   END;
BEGIN
   IF IsSmallFonts THEN
      exit;
   IF (fdoExcludeGrids IN Options) AND (Control IS TCustomGrid) THEN
      exit;
   UpdateFont(TJvgShowFont(Control).Font);
   WITH Control DO
      FOR i := 0 TO ControlCount - 1 DO
      BEGIN
         UpdateFont(TJvgShowFont(Controls[i]).Font);
         IF Controls[i] IS TWinControl THEN
            UpdateFonts(Controls[i] AS TWinControl);
      END;

END;

END.


{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgFixFont.PAS, released on 2003-01-15.

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

UNIT JvgFixFont;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   JvComponent,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs;

TYPE
   TJvgPublicControlFont = CLASS(TControl)
   PUBLIC
      PROPERTY Font;
   END;

   TJvgFixFont = CLASS(TJvComponent)
   PRIVATE
    FFont: TFont;
      PROCEDURE FixFont(Window: TWinControl);
    procedure SetFont(const Value: TFont);
   PROTECTED
      { Protected declarations }
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      property Font : TFont read FFont write SetFont;
   PUBLISHED
      { Published declarations }
   END;

IMPLEMENTATION

//____________________________

CONSTRUCTOR TJvgFixFont.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FixFont(TWinControl(Owner));
END;

PROCEDURE TJvgFixFont.FixFont(Window: TWinControl);
VAR
   i                          : integer;
BEGIN
   WITH Window DO
   BEGIN
      TJvgPublicControlFont(Window).Font.Assign(fFont);
      FOR i := 0 TO ComponentCount - 1 DO
         IF Components[i] IS TWinControl THEN
            FixFont(TWinControl(Components[i]))
         ELSE IF Components[i] IS TControl THEN
            TJvgPublicControlFont(Components[i]).Font.Assign(FFont);
   END;
END;

procedure TJvgFixFont.SetFont(const Value: TFont);
begin
  FFont := Value;
end;

END.


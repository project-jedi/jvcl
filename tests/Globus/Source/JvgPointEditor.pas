{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgPointEditor.PAS, released on 2003-01-15.

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

UNIT JvgPointEditor;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   {$IFDEF COMPILER6_UP}
   DesignIntf,
   DesignEditors,
   PropertyCategories,
   {$ELSE}
   DsgnIntf,
   {$ENDIF COMPILER6_UP}

   TypInfo;                             //FrTypes;

TYPE
   TJvgPointProperty = CLASS(TPropertyEditor)

      FUNCTION GetAttributes: TPropertyAttributes; OVERRIDE;
      FUNCTION GetValue: STRING; OVERRIDE;
      //    procedure Edit; override;
   END;

PROCEDURE Register;

IMPLEMENTATION
VAR
   PPointTypeInfo             : PTypeInfo;
   PointTypeInfo              : TTypeInfo;

   {== TPointPropertyEditor Methods ==}

FUNCTION TJvgPointProperty.GetAttributes: TPropertyAttributes;
BEGIN
   Result := [];                        // paSubProperties, paReadOnly ];
END;

FUNCTION TJvgPointProperty.GetValue: STRING;
VAR
   pPT                        : PPoint;
BEGIN
   //  pPT := PPoint(GetOrdValue);
   //  Result := Format('(%d,%d)', [ ppt^.x, ppt^.y ]);
   Result := '[,]';
END;

PROCEDURE Register;
BEGIN
   PointTypeInfo.Name := 'TPoint';
   PointTypeInfo.Kind := tkFloat;
   PPointTypeInfo := @PointTypeInfo;
   RegisterPropertyEditor(TypeInfo(TPoint), NIL,
      '', TJvgPointProperty);

END;

END.


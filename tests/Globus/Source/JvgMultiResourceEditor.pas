{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgMultiResourceEditor.PAS, released on 2003-01-15.

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

UNIT JvgMultiResourceEditor;

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

   StdCtrls,
   JvgMultiResources,
   Grids;

TYPE

   TJvgResourcesProperty = CLASS(TPropertyEditor)
      FUNCTION GetAttributes: TPropertyAttributes; OVERRIDE;
      FUNCTION GetValue: STRING; OVERRIDE;
      PROCEDURE Edit; OVERRIDE;
   END;

   TJvgMultipleResourceEdit = CLASS(TForm)
      sg: TStringGrid;
      PROCEDURE FormShow(Sender: TObject);
      PROCEDURE sgSetEditText(Sender: TObject; ACol, ARow: Integer; CONST Value:
         STRING);
      PROCEDURE FormCreate(Sender: TObject);
      PROCEDURE FormDestroy(Sender: TObject);
   PRIVATE
      ControlsList: TList;
      PROCEDURE LoadDefaults;
   PUBLIC
      Component: TJvgMultipleResources;
      FUNCTION GetSub(SrcStr: STRING; No: integer; VAR ResStr: STRING): boolean;
   END;

VAR
   glMresEdit                 : TJvgMultipleResourceEdit;

IMPLEMENTATION
{$R *.DFM}

FUNCTION TJvgResourcesProperty.GetAttributes: TPropertyAttributes;
BEGIN
   Result := [paDialog];
END;

FUNCTION TJvgResourcesProperty.GetValue: STRING;
BEGIN
   Result := Format('(%s)', [GetPropType^.Name]);
END;

PROCEDURE TJvgResourcesProperty.Edit;
VAR
   Dialog                     : TJvgMultipleResourceEdit;
   I                          : Integer;
BEGIN
   TJvgMultipleResources(GetComponent(0)).Update;
   Dialog := glMresEdit.Create(Application);
   Dialog.Component := TJvgMultipleResources(GetComponent(0));
   Dialog.ShowModal;
   Dialog.free;
   //  GetComponent(0).Owner.Name
END;

PROCEDURE TJvgMultipleResourceEdit.LoadDefaults;
VAR
   ARow                       : integer;
BEGIN
   sg.RowCount := Component.Comps.Count + 2;
   FOR ARow := 1 TO Component.Comps.Count DO
      sg.Cells[1, ARow] := Component.Comps[ARow - 1];
END;

PROCEDURE TJvgMultipleResourceEdit.FormShow(Sender: TObject);
VAR
   Str, SubStr, ResStr        : STRING;
   i, uPos1, uPos2, ACol, ARow, SubStrNo: integer;
BEGIN

   sg.ColCount := 3;
   sg.RowCount := Component.Resources.Count + 2;
   sg.Cells[0, 0] := 'Control';
   sg.Cells[1, 0] := 'Default';
   LoadDefaults;
   WITH Component, sg DO
      FOR ARow := 1 TO Resources.Count DO
      BEGIN
         Str := Resources[ARow - 1];
         uPos1 := 1;
         uPos2 := uPos1 + 1;
         ACol := 0;
         SubStrNo := 1;
         WHILE GetSub(Str, SubStrNo, ResStr) DO
         BEGIN
            inc(ACol);
            inc(SubStrNo);
            IF sg.ColCount < ACol + 1 THEN
               sg.ColCount := ACol + 1;
            Cells[ACol, ARow] := ResStr;
         END;
         Cells[0, ARow] := Component.Comps[ARow - 1];
         {    repeat
               if Str[uPos2] = '#' then
               begin
          Cells[ ACol, ARow ] := copy(str, uPos1, uPos2-uPos1 );
          inc(uPos2);
          uPos1 := uPos2;
          inc(ACol);
               end;
               inc(uPos2);
             until uPos2 >= length(Str);}

         //    Cells[ ACol, ARow ] := copy(str, uPos1, uPos2-uPos1+1 );

      END;
   sg.FixedCols := 1;
   sg.FixedRows := 1;
END;

FUNCTION TJvgMultipleResourceEdit.GetSub(SrcStr: STRING; No: integer; VAR
   ResStr: STRING): boolean;
VAR
   Str, SubStr                : STRING;
   Counter, uPos1, uPos2, uPrevPos2, ACol, ARow: integer;
BEGIN
   uPos1 := 1;
   uPos2 := 1;
   uPrevPos2 := 1;
   Counter := 0;
   ResStr := '';
   IF SrcStr = '' THEN
      exit;
   REPEAT
      IF SrcStr[uPos2] = '#' THEN
      BEGIN
         inc(Counter);
         uPos1 := uPrevPos2;
         uPrevPos2 := uPos2;
      END;
      inc(uPos2);
   UNTIL (Counter = No) OR (uPos2 = length(SrcStr));
   Result := true;
   IF Counter = No THEN
      ResStr := copy(SrcStr, uPos1 - 1, uPos2 - uPos1 - 1)
   ELSE IF Counter + 1 = No THEN
      ResStr := copy(SrcStr, uPrevPos2, uPos2 - uPrevPos2 + 1)
   ELSE
      Result := false;

END;

PROCEDURE TJvgMultipleResourceEdit.sgSetEditText(Sender: TObject; ACol, ARow:
   Integer;
   CONST Value: STRING);
BEGIN
   IF ACol <> 1 THEN
      exit;
   //if Tlabel(ControlsList[ARow-1]).Caption <> Value then
 //    Tlabel(ControlsList[ARow-1]).Caption := Value;
END;

PROCEDURE TJvgMultipleResourceEdit.FormCreate(Sender: TObject);
BEGIN
   ControlsList := TList.create;
END;

PROCEDURE TJvgMultipleResourceEdit.FormDestroy(Sender: TObject);
BEGIN
   ControlsList.Free;
END;

END.


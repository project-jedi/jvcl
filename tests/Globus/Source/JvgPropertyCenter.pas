{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgPropertyCenter.PAS, released on 2003-01-15.

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

UNIT JvgPropertyCenter;

INTERFACE
USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   JVComponent,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   TypInfo;

TYPE

   TglProperties_ = (fupColor, fupFont, fupFontColor);
   TglProperties = SET OF TglProperties_;

   Tgl_Properties_ = (f_upColor, f_upFont, f_upFontColor);
   Tgl_Properties = SET OF Tgl_Properties_;

   TJvgPropertyCenter = CLASS(TJvComponent)
   PRIVATE
      FColorProperty: TColor;
      FFontColorProperty: TColor;
      FFontProperty: TFont;
      FComponentList: TStringList;
      FUseProperties: TglProperties;
      FAutoApdate: boolean;
      PROCEDURE SetColorProperty(Value: TColor);
      PROCEDURE SetFontColorProperty(Value: TColor);
      PROCEDURE SetFontProperty(Value: TFont);
      PROCEDURE UpdateProperties(Properties: Tgl_Properties);
   PROTECTED
      PROCEDURE Notification(Component: TComponent; Operation: TOperation);
      PROCEDURE Loaded; OVERRIDE;
   PUBLIC
      CompList: TList;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY ColorProperty: TColor READ FColorProperty WRITE SetColorProperty;
      PROPERTY FontColorProperty: TColor READ FFontColorProperty WRITE
         SetFontColorProperty;
      PROPERTY FontProperty: TFont READ FFontProperty WRITE SetFontProperty;
      PROPERTY ComponentList: TStringList READ FComponentList WRITE
         FComponentList;
      PROPERTY UseProperties: TglProperties READ FUseProperties WRITE
         FUseProperties;
      PROPERTY AutoApdate: boolean READ FAutoApdate WRITE FAutoApdate;
   END;

PROCEDURE Register;

IMPLEMENTATION
USES JvgUtils,
   JvgTypes,
   JvgComponentListEditor;

PROCEDURE Register;
BEGIN

END;

CONSTRUCTOR TJvgPropertyCenter.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   ComponentList := TStringList.Create;
   CompList := TList.Create;
END;

DESTRUCTOR TJvgPropertyCenter.Destroy;
BEGIN
   INHERITED;
   ComponentList.Free;
   CompList.Free;
END;

PROCEDURE TJvgPropertyCenter.Loaded;
VAR
   i                          : integer;
   Comp                       : TComponent;
BEGIN
   INHERITED;

   FOR i := 0 TO ComponentList.Count - 1 DO
   BEGIN
      Comp := Owner.FindComponent(ComponentList[i]);
      IF Comp = NIL THEN
         continue;
      CompList.Add(Comp);
      ComponentList.Add(Comp.Name);
   END;
END;

PROCEDURE TJvgPropertyCenter.Notification(Component: TComponent; Operation:
   TOperation);
BEGIN
   IF (Component <> Self) AND (Operation = opRemove) THEN
      IF CompList.IndexOf(Component) <> -1 THEN
         CompList.Delete(CompList.IndexOf(Component));
   INHERITED;
END;

PROCEDURE TJvgPropertyCenter.UpdateProperties(Properties: Tgl_Properties);
VAR
   i                          : integer;
   ColorPropInfo, FontPropInfo: PPropInfo;
BEGIN
   FOR i := 0 TO CompList.Count - 1 DO
   BEGIN

      IF f_upColor IN Properties THEN
      BEGIN
         ColorPropInfo := GetPropInfo(TComponent(CompList[i]).ClassInfo,
            'Color');
         IF ColorPropInfo <> NIL THEN
            SetOrdProp(TComponent(CompList[i]), ColorPropInfo, FColorProperty);
      END;
      {    if (fupFontColor in Properties)or(fupFont in Properties) then
          begin
            ColorPropInfo := GetPropInfo( TComponent(CompList[i]).ClassInfo, 'Font');
            if ColorPropInfo <> nil then
              if fupFontColor in Properties then
                SetOrdProp( TComponent(CompList[i]), PropInfo, FColorProperty );
          end;}
   END;

END;

PROCEDURE TJvgPropertyCenter.SetColorProperty(Value: TColor);
BEGIN
   IF FColorProperty = Value THEN
      exit;
   FColorProperty := Value;
   IF FAutoApdate THEN
      UpdateProperties([f_upColor]);
END;

PROCEDURE TJvgPropertyCenter.SetFontColorProperty(Value: TColor);
BEGIN
   IF FFontColorProperty = Value THEN
      exit;
   FFontColorProperty := Value;
   IF FAutoApdate THEN
      UpdateProperties([f_upFontColor]);
END;

PROCEDURE TJvgPropertyCenter.SetFontProperty(Value: TFont);
BEGIN
   IF FFontProperty = Value THEN
      exit;
   FFontProperty.Assign(Value);
   IF FAutoApdate THEN
      UpdateProperties([f_upFont]);
END;

END.


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

unit JvgPropertyCenter;

interface
uses
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

type

  TglProperties_ = (fupColor, fupFont, fupFontColor);
  TglProperties = set of TglProperties_;

  Tgl_Properties_ = (f_upColor, f_upFont, f_upFontColor);
  Tgl_Properties = set of Tgl_Properties_;

  TJvgPropertyCenter = class(TJvComponent)
  private
    FColorProperty: TColor;
    FFontColorProperty: TColor;
    FFontProperty: TFont;
    FComponentList: TStringList;
    FUseProperties: TglProperties;
    FAutoApdate: boolean;
    procedure SetColorProperty(Value: TColor);
    procedure SetFontColorProperty(Value: TColor);
    procedure SetFontProperty(Value: TFont);
    procedure UpdateProperties(Properties: Tgl_Properties);
  protected
    procedure Notification(Component: TComponent; Operation: TOperation);
    procedure Loaded; override;
  public
    CompList: TList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorProperty: TColor read FColorProperty write SetColorProperty;
    property FontColorProperty: TColor read FFontColorProperty write
      SetFontColorProperty;
    property FontProperty: TFont read FFontProperty write SetFontProperty;
    property ComponentList: TStringList read FComponentList write
      FComponentList;
    property UseProperties: TglProperties read FUseProperties write
      FUseProperties;
    property AutoApdate: boolean read FAutoApdate write FAutoApdate;
  end;


implementation
uses JvgUtils,
  JvgTypes,
  JvgComponentListEditorForm;

constructor TJvgPropertyCenter.Create(AOwner: TComponent);
begin
  inherited;
  ComponentList := TStringList.Create;
  CompList := TList.Create;
end;

destructor TJvgPropertyCenter.Destroy;
begin
  inherited;
  ComponentList.Free;
  CompList.Free;
end;

procedure TJvgPropertyCenter.Loaded;
var
  i: integer;
  Comp: TComponent;
begin
  inherited;

  for i := 0 to ComponentList.Count - 1 do
  begin
    Comp := Owner.FindComponent(ComponentList[i]);
    if Comp = nil then
      continue;
    CompList.Add(Comp);
    ComponentList.Add(Comp.Name);
  end;
end;

procedure TJvgPropertyCenter.Notification(Component: TComponent; Operation:
  TOperation);
begin
  if (Component <> Self) and (Operation = opRemove) then
    if CompList.IndexOf(Component) <> -1 then
      CompList.Delete(CompList.IndexOf(Component));
  inherited;
end;

procedure TJvgPropertyCenter.UpdateProperties(Properties: Tgl_Properties);
var
  i: integer;
  ColorPropInfo, FontPropInfo: PPropInfo;
begin
  for i := 0 to CompList.Count - 1 do
  begin

    if f_upColor in Properties then
    begin
      ColorPropInfo := GetPropInfo(TComponent(CompList[i]).ClassInfo,
        'Color');
      if ColorPropInfo <> nil then
        SetOrdProp(TComponent(CompList[i]), ColorPropInfo, FColorProperty);
    end;
    {    if (fupFontColor in Properties)or(fupFont in Properties) then
        begin
          ColorPropInfo := GetPropInfo( TComponent(CompList[i]).ClassInfo, 'Font');
          if ColorPropInfo <> nil then
            if fupFontColor in Properties then
              SetOrdProp( TComponent(CompList[i]), PropInfo, FColorProperty );
        end;}
  end;

end;

procedure TJvgPropertyCenter.SetColorProperty(Value: TColor);
begin
  if FColorProperty = Value then
    exit;
  FColorProperty := Value;
  if FAutoApdate then
    UpdateProperties([f_upColor]);
end;

procedure TJvgPropertyCenter.SetFontColorProperty(Value: TColor);
begin
  if FFontColorProperty = Value then
    exit;
  FFontColorProperty := Value;
  if FAutoApdate then
    UpdateProperties([f_upFontColor]);
end;

procedure TJvgPropertyCenter.SetFontProperty(Value: TFont);
begin
  if FFontProperty = Value then
    exit;
  FFontProperty.Assign(Value);
  if FAutoApdate then
    UpdateProperties([f_upFont]);
end;

end.

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgComponentListEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgComponentListEditorForm;

interface

uses
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, TypInfo, Buttons, ImgList,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvgPropertyCenter, JvComponent;

type
  TJvgComponentListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TJvgComponentListEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvgCompListEditor = class(TJvForm)
    lvAll: TListView;
    Label1: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label2: TLabel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    lvSel: TListView;
    pbAdd: TBitBtn;
    pbRemove: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvAllDblClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure lvSelDblClick(Sender: TObject);
    procedure lvSelChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvAllChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
//    ComponentList: TStringList;
  public
    Component: TJvgPropertyCenter;
  end;

implementation

uses
  JvDsgnConsts, JvDsgnTypes;

{$R *.dfm}

//=== common proc ============================================================

procedure ShowCompListEditor(Designer: IJvDesigner; glPropertyCenter:
  TJvgPropertyCenter);
var
  Dialog: TJvgCompListEditor;
begin
  Dialog := TJvgCompListEditor.Create(Application);
  Dialog.Component := glPropertyCenter; //TJvgPropertyCenter(GetComponent(0));
  Dialog.ShowModal;
  Dialog.Free;
end;

//=== TJvgComponentListProperty ==============================================

function TJvgComponentListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvgComponentListProperty.GetValue: string;
begin
  Result := Format('(%s)', [GetPropType^.Name]);
end;

procedure TJvgComponentListProperty.Edit;
begin
  ShowCompListEditor(Designer, TJvgPropertyCenter(GetComponent(0)));
  //  GetComponent(0).Owner.Name
end;

//=== TJvgComponentListEditor ================================================

procedure TJvgComponentListEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowCompListEditor(Designer, TJvgPropertyCenter(Component));
  end;
end;

function TJvgComponentListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsEditComponentListEllipsis;
  end;
end;

function TJvgComponentListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== TJvgCompListEditor =====================================================

procedure TJvgCompListEditor.FormCreate(Sender: TObject);
begin
  //  ControlsList := TList.Create;
end;

procedure TJvgCompListEditor.FormDestroy(Sender: TObject);
begin
  //  ControlsList.Free;
end;

procedure TJvgCompListEditor.FormShow(Sender: TObject);
const
  cSigns: array [Boolean] of PChar = ('-', '+');
  cFont = 'Font';
  cColor = 'Color';
var
  I, J: Integer;
  ListItem: TListItem;
  Comp: TComponent;
  ColorPropInfo, FontPropInfo: PPropInfo;
begin
  lvAll.Items.Clear;
  lvSel.Items.Clear;

  for I := 0 to Component.ComponentList.Count - 1 do
  begin
    ListItem := lvSel.Items.Add;
    ListItem.Caption := Component.ComponentList[I];
    Comp := Component.Owner.FindComponent(Component.ComponentList[I]);
    if Comp = nil then
      Continue;
    ColorPropInfo := GetPropInfo(Comp.ClassInfo, cColor);
    FontPropInfo := GetPropInfo(Comp.ClassInfo, cFont);
    ListItem.SubItems.Add(cSigns[Assigned(ColorPropInfo)]);
    ListItem.SubItems.Add(cSigns[Assigned(FontPropInfo)]);
    ListItem.ImageIndex := 1;
  end;

  with Component.Owner do
    for I := 0 to ComponentCount - 1 do
    begin
      ColorPropInfo := GetPropInfo(Components[I].ClassInfo, cColor);
      FontPropInfo := GetPropInfo(Components[I].ClassInfo, cFont);
      if (ColorPropInfo <> nil) or (FontPropInfo <> nil) then
      begin
        ListItem := lvAll.Items.Add;
        ListItem.Caption := Components[I].Name;
        ListItem.SubItems.Add(cSigns[Assigned(ColorPropInfo)]);
        ListItem.SubItems.Add(cSigns[Assigned(FontPropInfo)]);

        for J := 0 to lvSel.Items.Count - 1 do
          if lvSel.Items[J].Caption = ListItem.Caption then
          begin
            ListItem.ImageIndex := 1;
            Break;
          end;
      end;
      //  SetOrdProp(FormX.Components[I], PropInfo, clGreen);
    end;
end;

procedure TJvgCompListEditor.lvAllDblClick(Sender: TObject);
var
  ListItem: TListItem;
begin
  if (lvAll.Selected = nil) or (lvAll.Selected.ImageIndex = 1) then
    Exit;

  ListItem := lvSel.Items.Add;
  ListItem.Caption := lvAll.Selected.Caption;
  ListItem.SubItems.Add(lvAll.Selected.SubItems[0]);
  ListItem.SubItems.Add(lvAll.Selected.SubItems[1]);
  lvAll.Selected.ImageIndex := 1;
  ListItem.ImageIndex := 1;
end;

procedure TJvgCompListEditor.BitBtn4Click(Sender: TObject);
begin
  Close;
end;

procedure TJvgCompListEditor.BitBtn3Click(Sender: TObject);
var
  I: Integer;
  Comp: TComponent;
begin
  Component.ComponentList.Clear;
  Component.CompList.Clear;
  for I := 0 to lvSel.Items.Count - 1 do
  begin
    Comp := Component.Owner.FindComponent(lvSel.Items[I].Caption);
    if Comp = nil then
      Continue;
    Component.CompList.Add(Comp);
    Component.ComponentList.Add(lvSel.Items[I].Caption);
  end;
  Close;
end;

procedure TJvgCompListEditor.lvSelDblClick(Sender: TObject);
var
  I: Integer;
begin
  if not Assigned(lvSel.Selected) then
    Exit;
  for I := 0 to lvAll.Items.Count - 1 do
    if lvAll.Items[I].Caption = lvSel.Selected.Caption then
    begin
      lvAll.Items[I].ImageIndex := 0;
      lvSel.Items.Delete(lvSel.Selected.Index);
      Break;
    end;
end;

procedure TJvgCompListEditor.lvSelChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  pbRemove.Enabled := Assigned(lvSel.Selected);
end;

procedure TJvgCompListEditor.lvAllChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  pbAdd.Enabled := Assigned(lvAll.Selected) and (lvAll.Selected.ImageIndex = 0);
end;

end.

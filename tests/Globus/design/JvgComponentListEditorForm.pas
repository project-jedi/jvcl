{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgComponentListEditor.PAS, released on 2003-01-15.

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

unit JvgComponentListEditorForm;

interface

uses
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
  JvgPropertyCenter,
  ComCtrls,
  ExtCtrls,
  TypInfo,
  Buttons{$IFDEF COMPILER5_UP},
  ImgList{$ENDIF};

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

  TJvgCompListEditor = class(TForm)
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
    ComponentList: TStringList;
  public
    Component: TJvgPropertyCenter;
  end;

  {$IFDEF COMPILER6_UP}
type
  TDesigner = DesignIntf.IDesigner;
  TFormDesigner = DesignIntf.IDesigner;
  {$ELSE}
  {$IFDEF COMPILER4_UP}
type
  TDesigner = IDesigner;
  TFormDesigner = IFormDesigner;
  {$ENDIF}
  {$ENDIF}

var
  glCompListEditor: TJvgCompListEditor;

implementation
{$R *.DFM}
//----------- common proc

procedure ShowCompListEditor(Designer: TDesigner; glPropertyCenter:
  TJvgPropertyCenter);
var
  Dialog: TJvgCompListEditor;
  I: Integer;
begin
  Dialog := TJvgCompListEditor.Create(Application);
  Dialog.Component := glPropertyCenter; //TJvgPropertyCenter(GetComponent(0));
  Dialog.ShowModal;
  Dialog.free;
end;

//----------- TJvgComponentListProperty

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
//----------- TJvgComponentListEditor

procedure TJvgComponentListEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowCompListEditor(Designer, TJvgPropertyCenter(Component));
  end;
end;

function TJvgComponentListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit component list...';
  end;
end;

function TJvgComponentListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//----------- TJvgCompListEditor

procedure TJvgCompListEditor.FormCreate(Sender: TObject);
begin
  //  ControlsList := TList.create;
end;

procedure TJvgCompListEditor.FormDestroy(Sender: TObject);
begin
  //  ControlsList.Free;
end;

procedure TJvgCompListEditor.FormShow(Sender: TObject);
var
  i, j: integer;
  ListItem: TListItem;
  Comp: TComponent;
  ColorPropInfo, FontPropInfo: PPropInfo;
const
  aSigns: array[boolean] of string = ('-', '+');
begin
  lvAll.Items.Clear;
  lvSel.Items.Clear;

  for i := 0 to Component.ComponentList.Count - 1 do
  begin
    ListItem := lvSel.Items.Add;
    ListItem.Caption := Component.ComponentList[i];
    Comp := Component.Owner.FindComponent(Component.ComponentList[i]);
    if Comp = nil then
      continue;
    ColorPropInfo := GetPropInfo(Comp.ClassInfo, 'Color');
    FontPropInfo := GetPropInfo(Comp.ClassInfo, 'Font');
    ListItem.SubItems.Add(aSigns[Assigned(ColorPropInfo)]);
    ListItem.SubItems.Add(aSigns[Assigned(FontPropInfo)]);
    ListItem.ImageIndex := 1;
  end;

  with Component.Owner do
    for i := 0 to ComponentCount - 1 do
    begin
      ColorPropInfo := GetPropInfo(Components[i].ClassInfo, 'Color');
      FontPropInfo := GetPropInfo(Components[i].ClassInfo, 'Font');
      if (ColorPropInfo <> nil) or (FontPropInfo <> nil) then
      begin
        ListItem := lvAll.Items.Add;
        ListItem.Caption := Components[i].Name;
        ListItem.SubItems.Add(aSigns[Assigned(ColorPropInfo)]);
        ListItem.SubItems.Add(aSigns[Assigned(FontPropInfo)]);

        for j := 0 to lvSel.Items.Count - 1 do
          if lvSel.Items[j].Caption = ListItem.Caption then
          begin
            ListItem.ImageIndex := 1;
            break;
          end;

      end;
      //  SetOrdProp( FormX.Components[i], PropInfo, clGreen );
    end;
end;

procedure TJvgCompListEditor.lvAllDblClick(Sender: TObject);
var
  ListItem: TListItem;
begin
  if (lvAll.Selected = nil) or (lvAll.Selected.ImageIndex = 1) then
    exit;

  ListItem := lvSel.Items.Add;
  ListItem.Caption := lvAll.Selected.Caption;
  ListItem.SubItems.Add(lvAll.Selected.SubItems[0]);
  ListItem.SubItems.Add(lvAll.Selected.SubItems[1]);
  lvAll.Selected.ImageIndex := 1;
  ListItem.ImageIndex := 1;
end;

procedure TJvgCompListEditor.BitBtn4Click(Sender: TObject);
begin
  close;
end;

procedure TJvgCompListEditor.BitBtn3Click(Sender: TObject);
var
  i: integer;
  Comp: TComponent;
begin
  Component.ComponentList.Clear;
  Component.CompList.Clear;
  for i := 0 to lvSel.Items.Count - 1 do
  begin
    Comp := Component.Owner.FindComponent(lvSel.Items[i].Caption);
    if Comp = nil then
      continue;
    Component.CompList.Add(Comp);
    Component.ComponentList.Add(lvSel.Items[i].Caption);
  end;
  close;
end;

procedure TJvgCompListEditor.lvSelDblClick(Sender: TObject);
var
  i: integer;
begin
  if not Assigned(lvSel.Selected) then
    exit;
  for i := 0 to lvAll.Items.Count - 1 do
    if lvAll.Items[i].Caption = lvSel.Selected.Caption then
      break;

  lvAll.Items[i].ImageIndex := 0;
  lvSel.Items.Delete(lvSel.Selected.Index);
end;

procedure TJvgCompListEditor.lvSelChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  pbRemove.Enabled := Assigned(lvSel.Selected);
end;

procedure TJvgCompListEditor.lvAllChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  pbAdd.Enabled := Assigned(lvAll.Selected) and (lvAll.Selected.ImageIndex =
    0);
end;

end.

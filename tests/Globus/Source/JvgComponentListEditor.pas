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

UNIT JvgComponentListEditor;

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
   JvgPropertyCenter,
   ComCtrls,
   ExtCtrls,
   TypInfo,
   Buttons{$IFDEF COMPILER5_UP},
   ImgList{$ENDIF};

TYPE

   TJvgComponentListProperty = CLASS(TPropertyEditor)
      FUNCTION GetAttributes: TPropertyAttributes; OVERRIDE;
      FUNCTION GetValue: STRING; OVERRIDE;
      PROCEDURE Edit; OVERRIDE;
   END;

   TJvgComponentListEditor = CLASS(TComponentEditor)
      PROCEDURE ExecuteVerb(Index: Integer); OVERRIDE;
      FUNCTION GetVerb(Index: Integer): STRING; OVERRIDE;
      FUNCTION GetVerbCount: Integer; OVERRIDE;
   END;

   TJvgCompListEditor = CLASS(TForm)
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
      PROCEDURE FormCreate(Sender: TObject);
      PROCEDURE FormDestroy(Sender: TObject);
      PROCEDURE FormShow(Sender: TObject);
      PROCEDURE lvAllDblClick(Sender: TObject);
      PROCEDURE BitBtn4Click(Sender: TObject);
      PROCEDURE BitBtn3Click(Sender: TObject);
      PROCEDURE lvSelDblClick(Sender: TObject);
      PROCEDURE lvSelChange(Sender: TObject; Item: TListItem;
         Change: TItemChange);
      PROCEDURE lvAllChange(Sender: TObject; Item: TListItem;
         Change: TItemChange);
   PRIVATE
      ComponentList: TStringList;
   PUBLIC
      Component: TJvgPropertyCenter;
   END;

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

VAR
   glCompListEditor           : TJvgCompListEditor;

IMPLEMENTATION
{$R *.DFM}
//----------- common proc

PROCEDURE ShowCompListEditor(Designer: TDesigner; glPropertyCenter:
   TJvgPropertyCenter);
VAR
   Dialog                     : TJvgCompListEditor;
   I                          : Integer;
BEGIN
   Dialog := TJvgCompListEditor.Create(Application);
   Dialog.Component := glPropertyCenter; //TJvgPropertyCenter(GetComponent(0));
   Dialog.ShowModal;
   Dialog.free;
END;

//----------- TJvgComponentListProperty

FUNCTION TJvgComponentListProperty.GetAttributes: TPropertyAttributes;
BEGIN
   Result := [paDialog];
END;

FUNCTION TJvgComponentListProperty.GetValue: STRING;
BEGIN
   Result := Format('(%s)', [GetPropType^.Name]);
END;

PROCEDURE TJvgComponentListProperty.Edit;
BEGIN
   ShowCompListEditor(Designer, TJvgPropertyCenter(GetComponent(0)));
   //  GetComponent(0).Owner.Name
END;
//----------- TJvgComponentListEditor

PROCEDURE TJvgComponentListEditor.ExecuteVerb(Index: Integer);
BEGIN
   CASE Index OF
      0: ShowCompListEditor(Designer, TJvgPropertyCenter(Component));
   END;
END;

FUNCTION TJvgComponentListEditor.GetVerb(Index: Integer): STRING;
BEGIN
   CASE Index OF
      0: Result := 'Edit component list...';
   END;
END;

FUNCTION TJvgComponentListEditor.GetVerbCount: Integer;
BEGIN
   Result := 1;
END;

//----------- TJvgCompListEditor

PROCEDURE TJvgCompListEditor.FormCreate(Sender: TObject);
BEGIN
   //  ControlsList := TList.create;
END;

PROCEDURE TJvgCompListEditor.FormDestroy(Sender: TObject);
BEGIN
   //  ControlsList.Free;
END;

PROCEDURE TJvgCompListEditor.FormShow(Sender: TObject);
VAR
   i, j                       : integer;
   ListItem                   : TListItem;
   Comp                       : TComponent;
   ColorPropInfo, FontPropInfo: PPropInfo;
CONST
   aSigns                     : ARRAY[boolean] OF STRING = ('-', '+');
BEGIN
   lvAll.Items.Clear;
   lvSel.Items.Clear;

   FOR i := 0 TO Component.ComponentList.Count - 1 DO
   BEGIN
      ListItem := lvSel.Items.Add;
      ListItem.Caption := Component.ComponentList[i];
      Comp := Component.Owner.FindComponent(Component.ComponentList[i]);
      IF Comp = NIL THEN
         continue;
      ColorPropInfo := GetPropInfo(Comp.ClassInfo, 'Color');
      FontPropInfo := GetPropInfo(Comp.ClassInfo, 'Font');
      ListItem.SubItems.Add(aSigns[Assigned(ColorPropInfo)]);
      ListItem.SubItems.Add(aSigns[Assigned(FontPropInfo)]);
      ListItem.ImageIndex := 1;
   END;

   WITH Component.Owner DO
      FOR i := 0 TO ComponentCount - 1 DO
      BEGIN
         ColorPropInfo := GetPropInfo(Components[i].ClassInfo, 'Color');
         FontPropInfo := GetPropInfo(Components[i].ClassInfo, 'Font');
         IF (ColorPropInfo <> NIL) OR (FontPropInfo <> NIL) THEN
         BEGIN
            ListItem := lvAll.Items.Add;
            ListItem.Caption := Components[i].Name;
            ListItem.SubItems.Add(aSigns[Assigned(ColorPropInfo)]);
            ListItem.SubItems.Add(aSigns[Assigned(FontPropInfo)]);

            FOR j := 0 TO lvSel.Items.Count - 1 DO
               IF lvSel.Items[j].Caption = ListItem.Caption THEN
               BEGIN
                  ListItem.ImageIndex := 1;
                  break;
               END;

         END;
         //  SetOrdProp( FormX.Components[i], PropInfo, clGreen );
      END;
END;

PROCEDURE TJvgCompListEditor.lvAllDblClick(Sender: TObject);
VAR
   ListItem                   : TListItem;
BEGIN
   IF (lvAll.Selected = NIL) OR (lvAll.Selected.ImageIndex = 1) THEN
      exit;

   ListItem := lvSel.Items.Add;
   ListItem.Caption := lvAll.Selected.Caption;
   ListItem.SubItems.Add(lvAll.Selected.SubItems[0]);
   ListItem.SubItems.Add(lvAll.Selected.SubItems[1]);
   lvAll.Selected.ImageIndex := 1;
   ListItem.ImageIndex := 1;
END;

PROCEDURE TJvgCompListEditor.BitBtn4Click(Sender: TObject);
BEGIN
   close;
END;

PROCEDURE TJvgCompListEditor.BitBtn3Click(Sender: TObject);
VAR
   i                          : integer;
   Comp                       : TComponent;
BEGIN
   Component.ComponentList.Clear;
   Component.CompList.Clear;
   FOR i := 0 TO lvSel.Items.Count - 1 DO
   BEGIN
      Comp := Component.Owner.FindComponent(lvSel.Items[i].Caption);
      IF Comp = NIL THEN
         continue;
      Component.CompList.Add(Comp);
      Component.ComponentList.Add(lvSel.Items[i].Caption);
   END;
   close;
END;

PROCEDURE TJvgCompListEditor.lvSelDblClick(Sender: TObject);
VAR
   i                          : integer;
BEGIN
   IF NOT Assigned(lvSel.Selected) THEN
      exit;
   FOR i := 0 TO lvAll.Items.Count - 1 DO
      IF lvAll.Items[i].Caption = lvSel.Selected.Caption THEN
         break;

   lvAll.Items[i].ImageIndex := 0;
   lvSel.Items.Delete(lvSel.Selected.Index);
END;

PROCEDURE TJvgCompListEditor.lvSelChange(Sender: TObject; Item: TListItem;
   Change: TItemChange);
BEGIN
   pbRemove.Enabled := Assigned(lvSel.Selected);
END;

PROCEDURE TJvgCompListEditor.lvAllChange(Sender: TObject; Item: TListItem;
   Change: TItemChange);
BEGIN
   pbAdd.Enabled := Assigned(lvAll.Selected) AND (lvAll.Selected.ImageIndex =
      0);
END;

END.


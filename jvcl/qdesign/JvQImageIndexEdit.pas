{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQImageIndexEdit.pas, released on 2004-07-08.

The Initial Developer of the Original Code is: André Snepvangers [asn att xs4all dott nl]
Copyright (c) 2004 André Snepvangers
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQImageIndexEdit;

interface

uses
  SysUtils, QGraphics, QForms, QStdCtrls, QExtCtrls,
  QControls, Classes, QDialogs, QComCtrls, QImgList, Qt, Types, QTypes;

function EditImageIndex(AImageList: TImageList; var SelectedIndex: integer): Boolean;

implementation

uses
  TypInfo, ClxImgEdit;

procedure GetImages(ImageList: TImageList; Index: Integer; Image, Mask: TBitmap);
var
  R: TRect;
begin
  with ImageList do
  begin
    R := Rect(0, 0, Width, Height);
    Image.Width := Width;
    Image.Height := Height;
    Mask.Width := Width;
    Mask.Height := Height;
  end;
  with Image.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(R);
    ImageList.Draw(Image.Canvas, 0, 0, Index);
  end;
  with Mask.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(R);
    ImageList.Draw(Mask.Canvas, 0, 0, Index, itMask);
  end;
end;

procedure UpdateImageView(ImageListEditor: TClxImageListEditor);
const
  NewState: array[Boolean] of Integer = (-1, 0);
var
  I: Integer;
  S: string;
  Items: TIconViewItems;
begin
    try
      Items := ImageListEditor.ImageView.Items;
      Items.BeginUpdate;
      try
        for I := 0 to ImageListEditor.InfoList.Count - 1 do
        begin
          S := IntToStr(I);
          if Items.Count <= I then
          begin
            { Add }
            with Items.Add do
            begin
              Caption := S;
              ImageIndex := I;
            end;
          end
          else
          begin
            { Update }
            Items[I].Caption := S;
            Items[I].ImageIndex := I;
          end;
        end;
        while Items.Count > ImageListEditor.InfoList.Count do
          Items[Items.Count - 1].Free;
      finally
        Items.EndUpdate;
      end;
    finally
      ;
    end;
end;

procedure FocusImage(ImageView: TIconView; Index, Count: Integer);
var
  Item: TIconViewItem;
  I: Integer;
begin
  with ImageView do
  begin
    Selected := nil;
    Item := nil;
    for I := Items.Count - 1 downto Items.Count - Count do
      Items[I].Selected := True;
    if Items.Count > 0 then
      Item := Items[Index];
    Selected := Item;
    if Assigned(Item) then
      Item.MakeVisible;
  end;
end;

function EditImageIndex(AImageList: TImageList; var SelectedIndex: integer): Boolean;
var
  I: Integer;
  ListItems: TClxImageListEditor;
  OnApplyClick: TNotifyEvent;

  procedure ApplyClick(Sender: TObject);
  begin
    ListItems.ApplyClick(nil);
    AImageList.assign(ListItems.ImageList);
  end;


begin
  ListItems := TClxImageListEditor.CreateImgListEditor(Application, AImageList);
  with ListItems do
    try
      Screen.Cursor := crHourglass;
      try
        ImageList.Assign(ListItems.ComponentList);
        with ImageListView do
        begin
          Assign(ImageList);
        end;
        ImageList.Clear;
        with ImageBitmap do
        begin
          Height := ImageList.Height;
          Width := ImageList.Width;
        end;
        for I := 0 to ListItems.ComponentList.Count - 1 do
          with TImageInfo.Create(InfoList, ListItems) do
          begin
            GetImages(ComponentList, I, Bitmap, Mask);
            TransparentColor := clDefault;
            Change;
          end;
        ActiveControl := ImageView;
        ImageView.MultiSelect := false;
        ImageView.ReadOnly := true;
        UpdateImageView(ListItems);
        SelectImage(SelectedIndex);
        FocusImage(ImageView, SelectedIndex, 0);
        OnApplyClick := ApplyClick;
        Apply.Enabled := False;
        Apply.OnClick := OnApplyClick;
      finally
        Screen.Cursor := crDefault;
      end;
      Caption := Format('Select index from %s%s%s', [AImageList.Owner.Name, DotSep,
        AImageList.Name]);
      Result := ShowModal = mrOk;
      if Result then
      begin
        SelectedIndex := Index;
        if Apply.Enabled then
          ApplyClick(nil);
      end;
    finally
      Free;
    end;
end;

end.


{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvReorderListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JVCLVer;

type
  TJvReorderListBox = class(TListBox)
  private
    FDragIndex: Integer;
    FDragImage: TDragImagelist;
    FAboutJVCL: TJVCLAboutInfo;
  protected
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
  public
    procedure DefaultDragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); virtual;
    procedure DefaultStartDrag(var DragObject: TDragObject); virtual;
    procedure DefaultDragDrop(Source: TObject; X, Y: Integer); virtual;

    procedure CreateDragImage(const S: string);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function GetDragImages: TDragImagelist; override;
    property DragIndex: Integer read FDragIndex;
    property DragImages: TDragImageList read GetDragImages;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

procedure TJvReorderListBox.CreateDragImage(const S: string);
var
  size: TSize;
  bmp: TBitmap;
begin
  if not Assigned(FDragImage) then
    FDragImage := TDragImagelist.Create(self)
  else
    FDragImage.Clear;
  Canvas.Font := Font;
  size := Canvas.TextExtent(S);
  FDragImage.Width := size.cx;
  FDragImage.Height := size.cy;
  bmp := TBitmap.Create;
  try
    bmp.Width := size.cx;
    bmp.Height := size.cy;
    bmp.Canvas.Font := Font;
    bmp.Canvas.Font.Color := clBlack;
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.Brush.Style := bsSolid;
    bmp.Canvas.TextOut(0, 0, S);
    FDragImage.AddMasked(bmp, clWhite);
  finally
    bmp.free
  end;
  ControlStyle := ControlStyle + [csDisplayDragImage];
end;

procedure TJvReorderListBox.DefaultDragDrop(Source: TObject; X,
  Y: Integer);
var
  dropindex, ti: Integer;
  S: string;
  obj: TObject;
begin
  if Source = Self then
  begin
    S := Items[FDragIndex];
    obj := Items.Objects[FDragIndex];
    dropIndex := ItemAtPos(Point(X, Y), true);
    ti := TopIndex;
    if dropIndex > FDragIndex then
      Dec(dropIndex);
    Items.Delete(FDragIndex);
    if dropIndex < 0 then
      items.AddObject(S, obj)
    else
      items.InsertObject(dropIndex, S, obj);
    TopIndex := ti;
  end;
end;

procedure TJvReorderListBox.DefaultDragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
// var i:integer;
begin
  Accept := Source = Self;
  if Accept then
  begin
    // Handle autoscroll in the "hot zone" 5 pixels from top or bottom of
    // client area
    if (Y < 5) or ((ClientHeight - Y) <= 5) then
    begin
      FDragImage.HideDragImage;
      try
        if Y < 5 then
        begin
          Perform(WM_VSCROLL, SB_LINEUP, 0);
          Perform(WM_VSCROLL, SB_ENDSCROLL, 0);
        end
        else if (ClientHeight - Y) <= 5 then
        begin
          Perform(WM_VSCROLL, SB_LINEDOWN, 0);
          Perform(WM_VSCROLL, SB_ENDSCROLL, 0);
        end
      finally
        FDragImage.ShowDragImage;
      end;
    end;
//    i := ItemAtPos(Point(X,Y),true);
//    if i > -1 then ItemIndex := i;
  end;
end;

procedure TJvReorderListBox.DefaultStartDrag(var DragObject: TDragObject);
begin
  FDragIndex := ItemIndex;
  if FDragIndex >= 0 then
    CreateDragImage(Items[FDragIndex])
  else
    CancelDrag;
end;

procedure TJvReorderListBox.DoStartDrag(var DragObject: TDragObject);
begin
  if Assigned(OnStartDrag) then
    inherited
  else
    DefaultStartDrag(DragObject);
end;

procedure TJvReorderListBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited
  else
    DefaultDragDrop(Source, X, Y);
end;

procedure TJvReorderListBox.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver) then
    inherited
  else
    DefaultDragOver(Source, X, Y, State, Accept);
end;

function TJvReorderListBox.GetDragImages: TDragImagelist;
begin
  Result := FDragImage;
end;

end.

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvItemsPanel.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A Panel that is divided into items defined by the Items property }
unit JvItemsPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvComponent;

type
  TJvPanelItemClickEvent = procedure(Sender: TObject; ItemIndex: integer) of object;
  TJvPanelOrientation = (poHorizontal, poVertical);

  TJvItemsPanel = class(TJvCustomPanel)
  private
    FItems: TStrings;
    FItemHeight: integer;
    FAutoSize: boolean;
    FAutoGrow: boolean;
    FDown: boolean;
    FClickable: boolean;
    FDownRect: TRect;
    FHotTrack: boolean;
    FHotTrackColor: TColor;
    FOnItemClick: TJvPanelItemClickEvent;
    FOrientation: TJvPanelOrientation;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    function GetCaption: TCaption;
    procedure SetItems(const Value: TStrings);
    procedure SetItemHeight(const Value: integer);
    procedure SetAutoGrow(const Value: boolean);
    procedure SetHotTrack(const Value: boolean);
    procedure SetHotTrackColor(const Value: TColor);
    procedure SetClickable(const Value: boolean);
    procedure SetOrientation(const Value: TJvPanelOrientation);

    { Private declarations }
  protected
    { Protected declarations }
{$IFDEF COMPILER6_UP}
    procedure SetAutoSize(Value: boolean); override;
{$ENDIF}
    procedure Grow;
    procedure PaintDown;
    procedure PaintUp;
    procedure PaintHi;
    procedure DrawItemText(Index: integer; R: TRect; HighLight: boolean);

    procedure Paint; override;
    procedure DoItemClick(ItemIndex: integer); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetItemAt(X, Y: integer): integer;
    function GetItemRect(Index: integer): Trect;
    property Canvas;
  published
    { Published declarations }
    property AutoGrow: boolean read FAutoGrow write SetAutoGrow;
{$IFDEF COMPILER6_UP}
    property AutoSize: boolean read FAutoSize write SetAutoSize;
{$ENDIF}
    property Items: TStrings read FItems write SetItems;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 16;
    property HotTrack: boolean read FHotTrack write SetHotTrack;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default clHighLight;
    property Caption: TCaption read GetCaption; // hide
    property Clickable: boolean read FClickable write SetClickable default true;
    property Orientation: TJvPanelOrientation read FOrientation write SetOrientation default poVertical;
    //    property Images:TImageList;
    //    property ImageIndex[ItemIndex:integer]:integer;
    property OnItemClick: TJvPanelItemClickEvent read FOnItemClick write FOnItemClick;

    // redeclared
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property Color;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    //    property FullRepaint;
    property Font;
    //    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

function GetFontHeight(Canvas: TCanvas): integer;
begin
  Result := Canvas.TextHeight('Wq');
end;

{ TJvItemsPanel }

constructor TJvItemsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FItemHeight := 16;
  FItems := TStringlist.Create;
  FHotTrackColor := clHighLight;
  FClickable := true;
  FOrientation := poVertical;
  inherited Caption := '';
end;

destructor TJvItemsPanel.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJvItemsPanel.GetCaption: TCaption;
begin
  Result := '';
  inherited Caption := '';
end;

procedure TJvItemsPanel.Paint;
var i, Rest: integer; R: TRect;
begin
  inherited Paint;
  Canvas.FillRect(ClientRect);
  if FItems.Count = 0 then
    Exit;
  rest := 0;
  if AutoSize then
  begin
    if Orientation = poVertical then
    begin
      ItemHeight := Height div FItems.Count;
      Rest := Height - ItemHeight * FItems.Count
    end
    else
    begin
      ItemHeight := Width div FItems.Count;
      Rest := Width - ItemHeight * FItems.Count
    end;
  end;

  for i := 0 to FItems.Count - 1 do
  begin
    R := GetItemRect(i);
    if (i = FItems.Count - 1) then
    begin
      if Orientation = poVertical then
        R.Bottom := R.Bottom + Rest
      else
        R.Right := R.Right + Rest;
    end;
    Frame3d(Canvas, R, clBtnHighLight, clBtnShadow, 1);
    InflateRect(R, 1, 1);
    DrawItemText(i, R, false);
  end;
end;

procedure TJvItemsPanel.DrawItemText(Index: integer; R: TRect; HighLight: boolean);
var flags: integer;
begin
  if (Index < 0) or (Index >= FItems.Count) then
    Exit;
  flags := DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_END_ELLIPSIS or DT_EDITCONTROL;
  case Alignment of
    taLeftJustify: flags := flags or DT_LEFT;
    taCenter: flags := flags or DT_CENTER;
    taRightJustify: flags := flags or DT_RIGHT;
  end;
  R.Left := R.Left + Canvas.TextWidth(' ');
  R.Right := R.Right - Canvas.TextWidth('  ');
  Canvas.Font := Font;
  if FHotTrack and HighLight then
    Canvas.Font.Color := FHotTrackColor;
  DrawText(Canvas.Handle, PChar(FItems[Index]), -1, R, flags);
end;

procedure TJvItemsPanel.SetAutoGrow(const Value: boolean);
begin
  if FAutoGrow <> Value then
  begin
    FAutoGrow := Value;
    if FAutoGrow then
    begin
      AutoSize := false;
      Align := alNone;
    end;
    Grow;
  end;
end;

{$IFDEF COMPILER6_UP}
procedure TJvItemsPanel.SetAutoSize(Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if AutoSize then
    begin
      if Orientation = poVertical then
        ItemHeight := Height div (FItems.Count + 1)
      else
        ItemHeight := Width div (FItems.Count + 1);
    end;
    Grow;
  end;
end;
{$ENDIF}
procedure TJvItemsPanel.SetItemHeight(const Value: integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Grow;
  end;
end;

procedure TJvItemsPanel.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
  Grow;
end;

procedure TJvItemsPanel.WMSize(var Message: TWMSize);
begin
  inherited;
  Grow;
end;

procedure TJvItemsPanel.Grow;
begin
  if AutoGrow and (Items.Count > 0) then
    Height := Items.Count * ItemHeight
  else
    Invalidate;
end;

function TJvItemsPanel.GetItemAt(X, Y: integer): integer;
begin
  if Orientation = poVertical then
  begin
    if (Y < 0) or (Y > Items.Count * ItemHeight) or (X < 0) or (X > Width) then
      Result := -1
    else
      Result := Y div ItemHeight;
  end
  else
  begin
    if (X < 0) or (X > Items.Count * ItemHeight) or (Y < 0) or (Y > Height) then
      Result := -1
    else
      Result := X div ItemHeight;
  end;
end;

function TJvItemsPanel.GetItemRect(Index: integer): Trect;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < 0) or (Index >= Items.Count) then
    Exit;
  if Orientation = poVertical then
    Result := Rect(0, Index * ItemHeight, Width, Index * ItemHeight + ItemHeight)
  else
    Result := Rect(Index * ItemHeight, 0, Index * ItemHeight + ItemHeight, Height);
end;

procedure TJvItemsPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  FDown := true;
  FDownRect := GetItemRect(GetItemAt(X, Y));
  PaintDown;
end;

procedure TJvItemsPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDown then
  begin
    PaintUp;
    FDownRect := GetItemRect(GetItemAt(X, Y));
    PaintDown;
  end
  else if FHotTrack then
  begin
    PaintUp;
    FDownRect := GetItemRect(GetItemAt(X, Y));
    PaintHi;
  end;
end;

procedure TJvItemsPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var i: integer;
begin
  inherited;
  PaintUp;
  i := GetItemAt(X, Y);
  if (i <> -1) and FDown then
    DoItemClick(i);
  FDown := false;
  FDownRect := Rect(0, 0, 0, 0);
end;

procedure TJvItemsPanel.PaintDown;
begin
  if not FClickable then
    Exit;
  Frame3d(Canvas, FDownRect, clBtnShadow, clBtnHighLight, 1);
  InflateRect(FDownRect, 1, 1);
  if Orientation = poVertical then
    DrawItemText(GetItemAt(1, FDownRect.Top + 1), FDownRect, true)
  else
    DrawItemText(GetItemAt(FDownRect.Left + 1, 1), FDownRect, true);
end;

procedure TJvItemsPanel.PaintUp;
begin
  Frame3d(Canvas, FDownRect, clBtnHighLight, clBtnShadow, 1);
  InflateRect(FDownRect, 1, 1);
  if Orientation = poVertical then
    DrawItemText(GetItemAt(1, FDownRect.Top + 1), FDownRect, true)
  else
    DrawItemText(GetItemAt(FDownRect.Left + 1, 1), FDownRect, true);
end;

procedure TJvItemsPanel.PaintHi;
begin
  Frame3d(Canvas, FDownRect, clWhite, clBlack, 1);
  InflateRect(FDownRect, 1, 1);
  if Orientation = poVertical then
    DrawItemText(GetItemAt(1, FDownRect.Top + 1), FDownRect, true)
  else
    DrawItemText(GetItemAt(FDownRect.Left + 1, 1), FDownRect, true);
end;

procedure TJvItemsPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  PaintUp;
end;

procedure TJvItemsPanel.SetHotTrack(const Value: boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    Invalidate;
  end;
end;

procedure TJvItemsPanel.SetHotTrackColor(const Value: TColor);
begin
  if FHotTrackColor <> Value then
  begin
    FHotTrackColor := Value;
    Invalidate;
  end;
end;

procedure TJvItemsPanel.SetClickable(const Value: boolean);
begin
  if FClickable <> Value then
  begin
    FClickable := Value;
    Invalidate;
  end;
end;

procedure TJvItemsPanel.DoItemClick(ItemIndex: integer);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(self, ItemIndex);
end;

procedure TJvItemsPanel.SetOrientation(const Value: TJvPanelOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Invalidate;
  end;
end;

end.


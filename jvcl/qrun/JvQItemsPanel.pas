{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvItemsPanel.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A Panel that is divided into items defined by the Items property

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQItemsPanel;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  QWindows, QMessages, QGraphics, QControls, QExtCtrls,
  JvQComponent, JvQThemes, JvQJCLUtils, JvQExControls;

type
  TJvPanelItemClickEvent = procedure(Sender: TObject; ItemIndex: Integer) of object;
  TJvPanelOrientation = (poHorizontal, poVertical);

  TJvItemsPanel = class(TJvCustomPanel, IJvDenySubClassing)
  private
    FItems: TStringList;
    FItemHeight: Integer;
    FAutoSize: Boolean;
    FAutoGrow: Boolean;
    FDown: Boolean;
    FClickable: Boolean;
    FDownRect: TRect;
    FHotTrack: Boolean;
    FHotTrackColor: TColor;
    FOnItemClick: TJvPanelItemClickEvent;
    FOrientation: TJvPanelOrientation;
    function GetCaption: TCaption;
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    procedure SetItemHeight(const Value: Integer);
    procedure SetAutoGrow(const Value: Boolean);
    procedure SetHotTrack(const Value: Boolean);
    procedure SetHotTrackColor(const Value: TColor);
    procedure SetClickable(const Value: Boolean);
    procedure SetOrientation(const Value: TJvPanelOrientation); 
  protected
    procedure SetAutoSize(Value: Boolean);  
    procedure AdjustSize; override; 
    procedure Grow;
    procedure PaintDown;
    procedure PaintUp;
    procedure PaintHi;
    procedure DrawItemText(Index: Integer; R: TRect; HighLight: Boolean);
    procedure Paint; override;
    procedure DoItemClick(ItemIndex: Integer); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave(Control: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetItemAt(X, Y: Integer): Integer;
    function GetItemRect(Index: Integer): TRect;
    property Canvas;
  published
    property AutoGrow: Boolean read FAutoGrow write SetAutoGrow;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Items: TStrings read GetItems write SetItems;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 16;
    property HotTrack: Boolean read FHotTrack write SetHotTrack;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default clHighLight;
    property Caption: TCaption read GetCaption; // hide
    property Clickable: Boolean read FClickable write SetClickable default True;
    property Orientation: TJvPanelOrientation read FOrientation write SetOrientation default poVertical;
    //    property Images: TImageList;
    //    property ImageIndex[ItemIndex: Integer]: Integer;
    property OnItemClick: TJvPanelItemClickEvent read FOnItemClick write FOnItemClick;
    property Align;
    property Alignment;
    property Anchors; 
    property BorderWidth;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    //    property FullRepaint;
    property Font;
    //    property Locked;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup; 
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag; 
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

constructor TJvItemsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption]; 
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FItemHeight := 16;
  FItems := TStringList.Create;
  FHotTrackColor := clHighLight;
  FClickable := True;
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
var
  I, Rest: Integer;
  R: TRect;
begin
//  inherited Paint;
  Canvas.Brush.Color := Self.Color;
  DrawThemedBackground(Self, Canvas, ClientRect);
  if Items.Count = 0 then
    Exit;
  Rest := 0;
  if AutoSize then
  begin
    if Orientation = poVertical then
    begin
      ItemHeight := Height div Items.Count;
      Rest := Height - ItemHeight * Items.Count
    end
    else
    begin
      ItemHeight := Width div Items.Count;
      Rest := Width - ItemHeight * Items.Count
    end;
  end;

  for I := 0 to Items.Count - 1 do
  begin
    R := GetItemRect(I);
    if I = Items.Count - 1 then
    begin
      if Orientation = poVertical then
        R.Bottom := R.Bottom + Rest
      else
        R.Right := R.Right + Rest;
    end;
    Frame3D(Canvas, R, clBtnHighLight, clBtnShadow, 1);
    InflateRect(R, 1, 1);
    DrawItemText(I, R, False);
  end;
end;

procedure TJvItemsPanel.DrawItemText(Index: Integer; R: TRect; HighLight: Boolean);
var
  Flags: Integer;
begin
  if (Index < 0) or (Index >= Items.Count) then
    Exit;
  Flags := DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_END_ELLIPSIS or DT_EDITCONTROL;
  case Alignment of
    taLeftJustify:
      Flags := Flags or DT_LEFT;
    taCenter:
      Flags := Flags or DT_CENTER;
    taRightJustify:
      Flags := Flags or DT_RIGHT;
  end;
  R.Left := R.Left + Canvas.TextWidth(' ');
  R.Right := R.Right - Canvas.TextWidth('  ');
  Canvas.Font := Font;
  if FHotTrack and HighLight then
    Canvas.Font.Color := FHotTrackColor; 
  DrawText(Canvas, Items[Index], -1, R, Flags);
end;

procedure TJvItemsPanel.SetAutoGrow(const Value: Boolean);
begin
  if FAutoGrow <> Value then
  begin
    FAutoGrow := Value;
    if FAutoGrow then
    begin
      AutoSize := False;
      Align := alNone;
    end;
    Grow;
  end;
end;

procedure TJvItemsPanel.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if AutoSize then
    begin
      if Orientation = poVertical then
        ItemHeight := Height div (Items.Count + 1)
      else
        ItemHeight := Width div (Items.Count + 1);
    end;
    Grow;
  end;
end;

procedure TJvItemsPanel.SetItemHeight(const Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Grow;
  end;
end;

function TJvItemsPanel.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TJvItemsPanel.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
  Grow;
end;




procedure TJvItemsPanel.AdjustSize;
begin
  inherited AdjustSize;
  Grow;
end;


procedure TJvItemsPanel.Grow;
begin
  if AutoGrow and (Items.Count > 0) then
  begin
    if Orientation = poVertical then
      Height := Items.Count * ItemHeight
    else
      Width := Items.Count * ItemHeight;
  end
  else
    Invalidate;
end;

function TJvItemsPanel.GetItemAt(X, Y: Integer): Integer;
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

function TJvItemsPanel.GetItemRect(Index: Integer): TRect;
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
  inherited MouseDown(Button, Shift, X, Y);
  if Button <> mbLeft then
    Exit;
  FDown := True;
  FDownRect := GetItemRect(GetItemAt(X, Y));
  PaintDown;
end;

procedure TJvItemsPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FDown then
  begin
    PaintUp;
    FDownRect := GetItemRect(GetItemAt(X, Y));
    PaintDown;
  end
  else
  if FHotTrack then
  begin
    PaintUp;
    FDownRect := GetItemRect(GetItemAt(X, Y));
    PaintHi;
  end;
end;

procedure TJvItemsPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  PaintUp;
  I := GetItemAt(X, Y);
  if (I <> -1) and FDown then
    DoItemClick(I);
  FDown := False;
  FDownRect := Rect(0, 0, 0, 0);
end;

procedure TJvItemsPanel.PaintDown;
begin
  if not FClickable then
    Exit;
  Frame3D(Canvas, FDownRect, clBtnShadow, clBtnHighLight, 1);
  InflateRect(FDownRect, 1, 1);
  if Orientation = poVertical then
    DrawItemText(GetItemAt(1, FDownRect.Top + 1), FDownRect, True)
  else
    DrawItemText(GetItemAt(FDownRect.Left + 1, 1), FDownRect, True);
end;

procedure TJvItemsPanel.PaintUp;
begin
  Frame3D(Canvas, FDownRect, clBtnHighLight, clBtnShadow, 1);
  InflateRect(FDownRect, 1, 1);
  if Orientation = poVertical then
    DrawItemText(GetItemAt(1, FDownRect.Top + 1), FDownRect, False)
  else
    DrawItemText(GetItemAt(FDownRect.Left + 1, 1), FDownRect, False);
end;

procedure TJvItemsPanel.PaintHi;
begin
  Frame3D(Canvas, FDownRect, HotTrackColor, HotTrackColor, 1);
  InflateRect(FDownRect, 1, 1);
  if Orientation = poVertical then
    DrawItemText(GetItemAt(1, FDownRect.Top + 1), FDownRect, True)
  else
    DrawItemText(GetItemAt(FDownRect.Left + 1, 1), FDownRect, True);
end;

procedure TJvItemsPanel.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseLeave(Control);
  PaintUp;
end;

procedure TJvItemsPanel.SetHotTrack(const Value: Boolean);
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

procedure TJvItemsPanel.SetClickable(const Value: Boolean);
begin
  if FClickable <> Value then
  begin
    FClickable := Value;
    Invalidate;
  end;
end;

procedure TJvItemsPanel.DoItemClick(ItemIndex: Integer);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, ItemIndex);
end;

procedure TJvItemsPanel.SetOrientation(const Value: TJvPanelOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Invalidate;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.


{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBMPListBox.PAS, released on 2000-11-22.

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

unit JvBMPListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  JVCLVer;

type
  TBackgroundFillmode = (bfmTile, bfmStretch);

  TJvBMPListBox = class(TCustomListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FBackground: TBitmap;
    FFillmode: TBackgroundFillmode;
    { Private declarations }
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;

    procedure SetBackground(const Value: TBitmap);
    procedure SetFillmode(const Value: TBackgroundFillmode);
    procedure DrawBackGround(ADC: HDC);

  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Background: TBitmap read FBackground write SetBackground;
    property BackgroundFillmode: TBackgroundFillmode
      read FFillmode write SetFillmode default bfmTile;

    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

constructor TJvBMPListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
  FBackground := TBitmap.Create;
end;

destructor TJvBMPListBox.Destroy;
begin
  FBackground.Free;
  inherited Destroy;
end;

procedure TJvBMPListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  // draw text transparently
  Canvas.Brush.Style := bsClear;
  // always use font color, CNDrawItem sets it to clHighlitetext for
  // selected items.
  Canvas.Font.Color := Font.Color;

  // The listbox does not erase the background for the item before
  // sending the WM_DRAWITEM message! We have to do that here manually.
  SaveDC(Canvas.Handle);
  IntersectClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  Perform(WM_ERASEBKGND, Canvas.Handle, 0);
  RestoreDC(Canvas.Handle, -1);

  if (Index >= 0) and (Index < Items.Count) then
    Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);

  // invert the item if selected
  if odSelected in State then
    InvertRect(Canvas.Handle, Rect);
  // no need to draw focus rect, CNDrawItem does that for us
end;

procedure TJvBMPListBox.SetBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
  Invalidate;
end;

procedure TJvBMPListBox.SetFillmode(const Value: TBackgroundFillmode);
begin
  if FFillMode <> Value then
  begin
    FFillmode := Value;
    Invalidate;
  end;
end;

procedure TJvBMPListBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if FBackground.Empty then
    inherited
  else
  begin
//    Msg.Result := 1;
    DrawBackGround(Msg.DC);
  end;
end;

procedure TJvBMPListBox.WMHScroll(var Msg: TWMHScroll);
begin
  Items.BeginUpdate;
  inherited;
  Invalidate;
  Items.EndUpdate;
end;

procedure TJvBMPListBox.DrawBackGround(ADC: HDC);
var
  ImageRect, ClipBox, ClientRect, Temp: TRect;
  Cv: TCanvas;
  ClipComplexity: Integer;
begin
  if ADC = 0 then
    Exit;
  ClientRect := Self.ClientRect;
  ClipComplexity := GetClipBox(ADC, ClipBox);
  if ClipComplexity = NULLREGION then
    Exit; // nothing to paint
  if ClipComplexity = ERROR then
    ClipBox := ClientRect;

  Cv := TCanvas.Create;
  try
    Cv.Handle := ADC;
    if Cv.Handle = 0 then
      Exit;
    if FFillmode = bfmStretch then
      Cv.StretchDraw(ClientRect, FBackground)
    else
    begin
      ImageRect := FBackground.Canvas.ClipRect;
      while ImageRect.top < ClientRect.bottom do
      begin
        while ImageRect.Left < ClientRect.Right do
        begin
          if IntersectRect(Temp, ClipBox, ImageRect) then
            Cv.Draw(ImageRect.Left, ImageRect.Top, FBackground);
          OffsetRect(ImageRect, ImageRect.Right - ImageRect.Left, 0);
        end;
        OffsetRect(ImageRect, -ImageRect.Left,
          ImageRect.Bottom - ImageRect.Top);
      end;
    end;
  finally
    Cv.Handle := 0;
    Cv.Free;
  end;
end;

procedure TJvBMPListBox.WMVScroll(var Msg: TWMVScroll);
begin
  Items.BeginUpdate;
  inherited;
  Invalidate;
  Items.EndUpdate;
end;

end.


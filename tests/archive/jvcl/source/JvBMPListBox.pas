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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JVCLVer;

type
  TBackgroundFillmode = (bfm_Tile, bfm_Stretch);
  TJvBMPListBox = class(TCustomListBox)
  private
    FBackground: TBitmap;
    FFillmode: TBackgroundFillmode;
    FAboutJVCL: TJVCLAboutInfo;
    { Private declarations }
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMVScroll(var msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var msg: TWMHScroll); message WM_HSCROLL;

    procedure SetBackground(const Value: TBitmap);
    procedure SetFillmode(const Value: TBackgroundFillmode);
    procedure DrawBackGround(aDC: HDC);

  protected
    { Protected declarations }
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Background: TBitmap read FBackground write SetBackground;
    property BackgroundFillmode: TBackgroundFillmode
      read FFillmode write SetFillmode default bfm_Tile;

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

{ TJvBMPListBox }

constructor TJvBMPListBox.Create(aOwner: TComponent);
begin
  inherited;
  Style := lbOwnerdrawFixed;
  FBackground := Tbitmap.Create;
end;

destructor TJvBMPListBox.Destroy;
begin
  FBackground.Free;
  inherited;
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
  SaveDC(Canvas.handle);
  IntersectClipRect(canvas.handle, rect.left, rect.top, rect.right, rect.bottom);
  perform(WM_ERASEBKGND, canvas.handle, 0);
  RestoreDC(Canvas.Handle, -1);

  if (Index >= 0) and (Index < Items.Count) then
    Canvas.TextOut(Rect.left + 2, Rect.top, Items[Index]);

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

procedure TJvBMPListBox.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  if FBackground.Empty then
    inherited
  else
  begin
//    msg.result := 1;
    DrawBackGround(Msg.DC);
  end;
end;

procedure TJvBMPListBox.WMHScroll(var msg: TWMHScroll);
begin
  items.BeginUpdate;
  inherited;
  Invalidate;
  items.EndUpdate;
end;

procedure TJvBMPListBox.DrawBackGround(aDC: HDC);
var
  imagerect, clipbox, clientrect, temp: TRect;
  cv: TCanvas;
  clipComplexity: Integer;
begin
  if aDC = 0 then Exit;
  clientrect := Self.Clientrect;
  clipComplexity := GetClipBox(aDC, clipbox);
  if clipComplexity = NULLREGION then
    Exit; // nothing to paint
  if ClipComplexity = ERROR then
    clipbox := clientRect;

  cv := TCanvas.Create;
  try
    cv.Handle := aDC;
    if cv.Handle = 0 then Exit;
    if FFillmode = bfm_Stretch then
      cv.StretchDraw(ClientRect, FBackground)
    else
    begin
      imagerect := FBackground.canvas.Cliprect;
      while imagerect.top < clientrect.bottom do
      begin
        while imagerect.left < clientrect.right do
        begin
          if IntersectRect(temp, clipbox, imagerect) then
            cv.Draw(imagerect.left, imagerect.top, FBackground);
          OffsetRect(imagerect, imagerect.right - imagerect.left, 0);
        end; { While }
        OffsetRect(imagerect, -imagerect.left,
          imagerect.bottom - imagerect.top);
      end; { while }
    end; { else }
  finally
    cv.Handle := 0;
    cv.free;
  end; { finally }
end; { else }

procedure TJvBMPListBox.WMVScroll(var msg: TWMVScroll);
begin
  items.BeginUpdate;
  inherited;
  Invalidate;
  items.EndUpdate;
end;

end.


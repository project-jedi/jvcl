{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvListbox2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This unit is a merging of the original TJvListBox, JvListBox2, TJvExListBox.
Merging done 2002-06-15 by Peter Thornqvist [peter3@peter3.com]

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Petr Vones (petr.v@mujmail.cz)
Peter Below <100113.1101@compuserve.com>

MERGE NOTES:
  * The Alignment property might mess things up depending on other property settings
  * not very extensively tested
  * TJvListBox in JvCtrls inherits from TJvCustomListbox in this unit.
    Maybe TJvListBox should be moved here instead (or this code into JvCtrls)?
  * TJvPlaylist now inherits from JvListBox

Last Modified: 2002-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
Known Issues:

Merge notes (2002-03-21):
* (p3) merged JvMultilineListBox, JvReorderListBox, JvTextListBox, JvBMPListBox
Notes (2003-05-21) // Remko Bonte
* Removed OwnerData
* Some bug-fixes. Combinations of Multiline, Alignment, Scrollbars seem to work now.
* Did some rewrite of background-drawing. Most of it seems to work, but a lot
  of flickering, best avoid it or set ScrollBars to ssNone.
* Updated drag image to use with MultiLine.

-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, StdCtrls, Controls, Forms,
  JvItemsSearchs, JVCLVer;

type
  TJvListboxFillMode = (bfmTile, bfmStretch);
  TJvListBoxDataEvent = procedure(Sender: TWinControl; Index: Integer; var Text: string) of object;
  TJvListboxChange = procedure(Sender: TObject; Item: string) of object;
  TJvScrollEvent = procedure(Sender: TObject; const Msg: TWMScroll; var dontScroll: Boolean) of object;

  TJvListBoxBackground = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FImage: TBitmap;
    FFillmode: TJvListboxFillMode;
    FVisible: Boolean;
    procedure SetFillMode(const Value: TJvListboxFillMode);
    procedure SetImage(const Value: TBitmap);
    procedure SetVisible(const Value: Boolean);
    function GetDoDraw: Boolean;
  protected
    procedure Change;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property DoDraw: Boolean read GetDoDraw;
    property Image: TBitmap read FImage write SetImage;
    property FillMode: TJvListboxFillMode read FFillMode write SetFillMode;
    property Visible: Boolean read FVisible write SetVisible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvCustomListBox = class(TCustomListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FAlignment: TAlignment;
    FHotTrack: Boolean;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FMaxWidth: Integer;
    FItemSearchs: TJvItemsSearchs;
    FScrollBars: TScrollStyle;
    FSorted: Boolean;
    FOnGetText: TJvListBoxDataEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnSelectCancel: TNotifyEvent;
    FOnDeleteString: TJvListboxChange;
    FOnAddString: TJvListboxChange;
    FOnChange: TNotifyEvent;
    FOnHorizontalScroll: TJvScrollEvent;
    FOnVerticalScroll: TJvScrollEvent;
    FDragIndex: Integer;
    FDragImage: TDragImageList;
    FMultiline: Boolean;
    FShowFocusRect: Boolean;
    FSelectedTextColor: TColor;
    FSelectedColor: TColor;
    FDisabledTextColor: TColor;
    FBackground: TJvListBoxBackground;
    FLeftPosition: Integer;

    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure CNKeyDown(var Msg: TWMKeyDown); message CN_KEYDOWN;

    procedure DrawBackGround(ADC: HDC; const DoOffSet: Boolean);

    procedure UpdateStyle;

    { Handle messages that insert or delete strings from the listbox to
      manage the horizontal scrollbar if FMutliline is false. }
    procedure LBAddString(var Msg: TMessage); message LB_ADDSTRING;
    procedure LBInsertString(var Msg: TMessage); message LB_INSERTSTRING;
    procedure LBDeleteString(var Msg: TMessage); message LB_DELETESTRING;
    { Override CN_DRAWITEM handling to be able to switch off focus rect. }
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetMultiline(const Value: Boolean);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedTextColor(const Value: TColor);
    procedure SetShowFocusRect(const Value: Boolean);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetMaxWidth(const Value: Integer);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetSorted(const Value: Boolean);
    procedure SetHotTrack(const Value: Boolean);
    procedure SetBackground(const Value: TJvListBoxBackground);
    function GetLimitToClientWidth: Boolean;
  protected
    procedure BeginRedraw;
    procedure EndRedraw;

    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SelectCancel(var Msg: TMessage); message LBN_SELCANCEL;
    procedure Changed; virtual;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure RemeasureAll;
    procedure DoBackgroundChange(Sender: TObject);

    procedure Loaded; override;

    property LimitToClientWidth: Boolean read GetLimitToClientWidth;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property OnGetText: TJvListBoxDataEvent read FOnGetText write FOnGetText;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnSelectCancel: TNotifyEvent read FOnSelectCancel write FOnSelectCancel;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDeleteString: TJvListboxChange read FOnDeleteString write FOnDeleteString;
    property OnAddString: TJvListboxChange read FOnAddString write FOnAddString;
    property OnVerticalScroll: TJvScrollEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TJvScrollEvent read FOnHorizontalScroll write FOnHorizontalScroll;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MeasureString(const S: string; WidthAvail: Integer; var ASize: TSize);

    procedure DefaultDrawItem(Index: Integer; ARect: TRect;
      State: TOwnerDrawState); virtual;
    procedure DefaultDragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); virtual;
    procedure DefaultStartDrag(var DragObject: TDragObject); virtual;
    procedure DefaultDragDrop(Source: TObject; X, Y: Integer); virtual;
    procedure CreateDragImage(const S: string);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function GetDragImages: TDragImageList; override;
    property DragIndex: Integer read FDragIndex;
    property DragImages: TDragImageList read GetDragImages;
    procedure WndProc(var Msg: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure UpdateHorizontalExtent;
    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
    procedure SelectAll; {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure UnselectAll;
    procedure InvertSelection;
    procedure MoveSelectedUp; virtual;
    procedure MoveSelectedDown; virtual;
    procedure DeleteSelected; {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure DeleteAllButSelected;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  protected
    property MultiLine: Boolean read FMultiline write SetMultiline default False;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clHighlight;
    property SelectedTextColor: TColor read FSelectedTextColor write SetSelectedTextColor default clHighlightText;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default True;
    property Background: TJvListBoxBackground read FBackground write SetBackground;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

uses
  JclBase;

resourcestring
  RsLBVirtualCantBeSorted = 'ListBox doesn''t allow sorting in virtual mode';

const
  AlignFlags: array [TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);

  { TJvCustomListBox }

procedure TJvCustomListBox.BeginRedraw;
begin
  SendMessage(Handle, WM_SETREDRAW, Ord(False), 0);
end;

procedure TJvCustomListBox.Changed;
begin
  // (rom) TODO?
end;

procedure TJvCustomListBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomListBox.CMFontChanged(var Msg: TMessage);
const
  CShowFocusRect: array [Boolean] of Integer = (0, 2);
begin
  inherited;
  Canvas.Font := Font;
  if Style <> lbStandard then
    ItemHeight := Canvas.TextHeight('Äy') + CShowFocusRect[ShowFocusRect];
  RemeasureAll;
end;

procedure TJvCustomListBox.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
    if FHotTrack then
      Ctl3D := True;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomListBox.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Ctl3D := False;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomListBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{ This routine is copied mostly from TCustomListbox.CNDRawItem.
  The setting of colors is modified.
  Drawing of the focus rectangle is delegated to DrawItem.}

procedure TJvCustomListBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(itemID) >= 0 then
    begin
      if odSelected in State then
      begin
        Canvas.Brush.Color := FSelectedColor;
        Canvas.Font.Color := FSelectedTextColor;
      end;
      if (([odDisabled, odGrayed] * State) <> []) or not Enabled then
        Canvas.Font.Color := FDisabledTextColor;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
    begin
      if Background.DoDraw then
      begin
        Perform(WM_ERASEBKGND, Canvas.Handle, 0);
        if odFocused in State then
          DrawFocusRect(hDC, rcItem);
      end
      else
      begin
        Canvas.FillRect(rcItem);
        if odFocused in State then
          DrawFocusRect(hDC, rcItem);
      end;
    end;
    Canvas.Handle := 0;
  end;
end;

procedure TJvCustomListBox.CNKeyDown(var Msg: TWMKeyDown);
begin
  if Background.DoDraw and
    (Msg.Result = 0) and (Msg.CharCode in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN]) then
  begin
    BeginRedraw;
    try
      inherited;
    finally
      EndRedraw;
    end;
  end
  else
    inherited;
end;

constructor TJvCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // JvBMPListBox:
  //  Style := lbOwnerDrawFixed;
  FBackground := TJvListBoxBackground.Create;
  FBackground.OnChange := DoBackgroundChange;
  FScrollBars := ssBoth;
  FAlignment := taLeftJustify;
  FMultiline := False;
  FSelectedColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FDisabledTextColor := clGrayText;
  FShowFocusRect := True;
  //  Style := lbOwnerDrawVariable;

  FMaxWidth := 0;
  FHotTrack := False;
  FHintColor := clInfoBk;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FItemSearchs := TJvItemsSearchs.Create;
end;

procedure TJvCustomListBox.CreateDragImage(const S: string);
const
  CLeftMargin = 15;
var
  Size: TSize;
  Bmp: TBitmap;
  SizeRect: TRect;
begin
  if not Assigned(FDragImage) then
    FDragImage := TDragImageList.Create(Self)
  else
    FDragImage.Clear;
  Canvas.Font := Font;
  if MultiLine then
  begin
    SizeRect := Rect(0, 0, MaxInt, 0);
    DrawText(Canvas.Handle, PChar(S), -1, SizeRect, DT_CALCRECT or
      DrawTextBiDiModeFlags(DT_WORDBREAK or DT_NOPREFIX or AlignFlags[FAlignment]));
    Size.cx := SizeRect.Right;
    Size.cy := SizeRect.Bottom;
  end
  else
    Size := Canvas.TextExtent(S);
  Inc(Size.cx, CLeftMargin);

  FDragImage.Width := Size.cx;
  FDragImage.Height := Size.cy;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Size.cx;
    Bmp.Height := Size.cy;
    Bmp.Canvas.Font := Font;
    Bmp.Canvas.Font.Color := clBlack;
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.Brush.Style := bsSolid;
    if MultiLine then
    begin
      Inc(SizeRect.Right, CLeftMargin);
      Bmp.Canvas.FillRect(SizeRect);
      Inc(SizeRect.Left, CLeftMargin);
      DrawText(Bmp.Canvas.Handle, PChar(S), -1, SizeRect,
        DrawTextBiDiModeFlags(DT_WORDBREAK or DT_NOPREFIX or AlignFlags[FAlignment]));
    end
    else
      Bmp.Canvas.TextOut(CLeftMargin, 0, S);
    FDragImage.AddMasked(Bmp, clWhite);
  finally
    Bmp.Free;
  end;
  ControlStyle := ControlStyle + [csDisplayDragImage];
end;

procedure TJvCustomListBox.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array [TScrollStyle] of DWORD =
    (0, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
  Sorted: array [Boolean] of DWORD =
    (0, LBS_SORT);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and not (WS_HSCROLL or WS_VSCROLL) or ScrollBar[FScrollBars] or
      Sorted[FSorted];
  end;
end;

procedure TJvCustomListBox.CreateWnd;
begin
  if not (csLoading in ComponentState) then
  begin
    FMultiLine := MultiLine and (Style = lbOwnerDrawVariable);

    if not (Style in [lbOwnerDrawVariable, lbOwnerDrawFixed]) then
      FAlignment := taLeftJustify;
  end;
  FLeftPosition := 0;
  inherited CreateWnd;
  UpdateHorizontalExtent;
end;

procedure TJvCustomListBox.DefaultDragDrop(Source: TObject;
  X, Y: Integer);
var
  DropIndex, Ti: Integer;
  S: string;
  Obj: TObject;
begin
  if Source = Self then
  begin
    S := Items[FDragIndex];
    Obj := Items.Objects[FDragIndex];
    DropIndex := ItemAtPos(Point(X, Y), True);
    Ti := TopIndex;
    if DropIndex > FDragIndex then
      Dec(DropIndex);
    Items.Delete(FDragIndex);
    if DropIndex < 0 then
      Items.AddObject(S, Obj)
    else
      Items.InsertObject(DropIndex, S, Obj);
    TopIndex := Ti;
  end;
end;

procedure TJvCustomListBox.DefaultDragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
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
        else
        if (ClientHeight - Y) <= 5 then
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

{ This procedure is a slightly modified version of TCustomListbox.DrawItem! }

procedure TJvCustomListBox.DefaultDrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
const
  AlignFlags: array [TAlignment] of DWORD =
    (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Flags: Longint;
  ActualRect: TRect;
begin
  // JvBMPListBox:
  // draw text transparently
  if ScrollBars in [ssHorizontal, ssBoth] then
  begin
    if FMaxWidth < ClientWidth then
      ActualRect := Rect(0, ARect.Top, ClientWidth, ARect.Bottom)
    else
      ActualRect := Rect(0, ARect.Top, FMaxWidth, ARect.Bottom);
  end
  else
    ActualRect := ARect;

  if Background.DoDraw then
  begin
    Canvas.Brush.Style := bsClear;
    // always use font color, CNDrawItem sets it to clHighlitetext for
    // selected items.
    Canvas.Font.Color := Font.Color;

    // The listbox does not erase the background for the item before
    // sending the WM_DRAWITEM message! We have to do that here manually.
    SaveDC(Canvas.Handle);
    IntersectClipRect(Canvas.Handle, ActualRect.Left, ActualRect.Top, ActualRect.Right, ActualRect.Bottom);
    DrawBackGround(Canvas.Handle, True);
    RestoreDC(Canvas.Handle, -1);
  end;

  if Index < Items.Count then
  begin
    if not Background.DoDraw then
      Canvas.FillRect(ActualRect);

    if FMultiline then
      Flags := DrawTextBiDiModeFlags(DT_WORDBREAK or DT_NOPREFIX or
        AlignFlags[FAlignment])
    else
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or
        AlignFlags[FAlignment]);
    if not UseRightToLeftAlignment then
      Inc(ActualRect.Left, 2)
    else
      Dec(ActualRect.Right, 2);
    DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), ActualRect, Flags);

    //if (Index >= 0) and (Index < Items.Count) then
    //  Canvas.TextOut(ActualRect.Left + 2, ActualRect.Top, Items[Index]);

    // invert the item if selected
    if Background.DoDraw and (odSelected in State) then
      InvertRect(Canvas.Handle, ActualRect);
    // no need to draw focus rect, CNDrawItem does that for us
  end;
end;

procedure TJvCustomListBox.DefaultStartDrag(var DragObject: TDragObject);
begin
  FDragIndex := ItemIndex;
  if FDragIndex >= 0 then
    CreateDragImage(Items[FDragIndex])
  else
    CancelDrag;
end;

procedure TJvCustomListBox.DeleteAllButSelected;
var
  I: Integer;
begin
  if not MultiSelect then
    Exit;
  I := 0;
  while I < Items.Count do
    if not Selected[I] then
      Items.Delete(I)
    else
      Inc(I);
  Changed;
end;

function TJvCustomListBox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
  Changed;
end;

procedure TJvCustomListBox.DeleteSelected;
var
  I: Integer;
begin
  if MultiSelect then
  begin
    for I := Items.Count - 1 downto 0 do
      if Selected[I] then
        Items.Delete(I);
  end
  else
  if ItemIndex <> -1 then
  begin
    I := ItemIndex;
    Items.Delete(I);
    if I > 0 then
      Dec(I);
    if Items.Count > 0 then
      ItemIndex := I;
  end;
  Changed;
end;

destructor TJvCustomListBox.Destroy;
begin
  FItemSearchs.Free;
  FBackground.Free;
  inherited Destroy;
end;

procedure TJvCustomListBox.DoBackgroundChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomListBox.DoStartDrag(var DragObject: TDragObject);
begin
  if Assigned(OnStartDrag) then
    inherited DoStartDrag(DragObject)
  else
    DefaultStartDrag(DragObject);
end;

procedure TJvCustomListBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    DefaultDragDrop(Source, X, Y);
end;

procedure TJvCustomListBox.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    DefaultDragOver(Source, X, Y, State, Accept);
end;

procedure TJvCustomListBox.DrawBackGround(ADC: HDC; const DoOffSet: Boolean);
var
  ImageRect, ClipBox, ClientRect, Temp: TRect;
  Cv: TCanvas;
  ClipComplexity: Integer;
begin
  if (ADC = 0) or not Background.DoDraw then
    Exit;
  ClientRect := Self.ClientRect;
  ClipComplexity := GetClipBox(ADC, ClipBox);
  if ClipComplexity = NULLREGION then
    Exit; // nothing to paint
  if ClipComplexity = ERROR then
    ClipBox := ClientRect;

  if DoOffSet then
    OffsetRect(ClientRect, FLeftPosition, 0);

  Cv := TCanvas.Create;
  try
    Cv.Handle := ADC;
    if Cv.Handle = 0 then
      Exit;
    if Background.Fillmode = bfmStretch then
      Cv.StretchDraw(ClientRect, Background.Image)
    else
    begin
      ImageRect := Background.Image.Canvas.ClipRect;
      while ImageRect.Top < ClientRect.Bottom do
      begin
        while ImageRect.Left < ClientRect.Right do
        begin
          if IntersectRect(Temp, ClipBox, ImageRect) then
            Cv.Draw(ImageRect.Left, ImageRect.Top, Background.Image);
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

procedure TJvCustomListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if Assigned(OnDrawItem) then
    inherited DrawItem(Index, Rect, State)
  else
  begin
    { Call the drawing code. This is isolated in its own public routine
      so a OnDrawItem handler can use it, too. }
    DefaultDrawItem(Index, Rect, State);
    if FShowFocusRect and (odFocused in State) then
      Canvas.DrawFocusRect(Rect);
  end;
end;

procedure TJvCustomListBox.EndRedraw;
var
  R: TRect;
begin
  SendMessage(Handle, WM_SETREDRAW, Ord(True), 0);
  R := Rect(0, 0, Width, Height);
  InvalidateRect(Handle, @R, True);
end;

function TJvCustomListBox.GetDragImages: TDragImageList;
begin
  Result := FDragImage;
end;

function TJvCustomListBox.GetLimitToClientWidth: Boolean;
begin
  Result := FMultiline and (ScrollBars in [ssNone, ssVertical]);
end;

procedure TJvCustomListBox.InvertSelection;
var
  I: Integer;
begin
  if MultiSelect then
  begin
    Items.BeginUpdate;
    for I := 0 to Items.Count - 1 do
      Selected[I] := not Selected[I];
    Items.EndUpdate;
  end;
end;

procedure TJvCustomListBox.LBAddString(var Msg: TMessage);
var
  LSize: TSize;
begin
  { (rb) Because TJvDirectoryListBox displays shorter strings than it stores in
         it's Items property - ie it stores the complete path, displays only
         the last part of a directory - the following code will cause the
         TJvCustomListBox think that the size of the strings are bigger than
         they really are (thus you probably will see a horizontal scroll bar)
  }
  if not LimitToClientWidth then
  begin
    MeasureString(PChar(Msg.LParam), 0, LSize);
    if LSize.cx > FMaxWidth then
      SetMaxWidth(LSize.cx);
  end;
  inherited;
  if Assigned(FOnAddString) then
    FOnAddString(Self, StrPas(PChar(Msg.LParam)));
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCustomListBox.LBDeleteString(var Msg: TMessage);
var
  LSize: TSize;
  InheritedCalled: Boolean;
begin
  InheritedCalled := False;
  if not LimitToClientWidth then
  begin
    MeasureString(Items[Msg.WParam], 0, LSize);
    InheritedCalled := LSize.cy = FMaxWidth;
    if InheritedCalled then
    begin
      inherited;
      RemeasureAll;
    end;
  end;
  if Assigned(FOnDeleteString) then
    FOnDeleteString(Self, Items.Strings[Longint(Msg.WParam)]);
  if not InheritedCalled then
    inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCustomListBox.LBInsertString(var Msg: TMessage);
var
  LSize: TSize;
begin
  if not LimitToClientWidth then
  begin
    MeasureString(PChar(Msg.LParam), 0, LSize);
    if LSize.cx > FMaxWidth then
      SetMaxWidth(LSize.cx);
  end;
  inherited;
end;

procedure TJvCustomListBox.Loaded;
begin
  inherited Loaded;

  UpdateStyle;
end;

procedure TJvCustomListBox.MeasureItem(Index: Integer;
  var Height: Integer);
var
  LSize: TSize;
begin
  if Assigned(OnMeasureItem) or not MultiLine or
    (Index < 0) or (Index >= Items.Count) then
    inherited MeasureItem(Index, Height)
  else
  begin
    if LimitToClientWidth then
      MeasureString(Items[Index], ClientWidth, LSize)
    else
      MeasureString(Items[Index], 0, LSize);
    Height := LSize.cy;
  end;
end;

procedure TJvCustomListBox.MeasureString(const S: string;
  WidthAvail: Integer; var ASize: TSize);
var
  Flags: Longint;
  R: TRect;
begin
  Canvas.Font := Font;
  { Note: doing the TextHeight unconditionally makes sure the font is properly
    selected into the device context. }
  ASize.cx := Canvas.TextHeight(S);

  Flags := DrawTextBiDiModeFlags(
    DT_WORDBREAK or DT_NOPREFIX or DT_CALCRECT or AlignFlags[FAlignment]);
  if WidthAvail = 0 then
    WidthAvail := MaxInt
  else
    Dec(WidthAvail, 2);
  R := Rect(0, 0, WidthAvail, 1);
  DrawText(Canvas.Handle, PChar(S), Length(S), R, Flags);
  ASize.cx := R.Right + 4;
  ASize.cy := R.Bottom;

  { Note: item height in a listbox is limited to 255 pixels since Windows
    stores the height in a single byte.}
  if ASize.cy > 255 then
    ASize.cy := 255;
  if ASize.cy < ItemHeight then
    ASize.cy := ItemHeight;
end;

procedure TJvCustomListBox.MoveSelectedDown;
var
  I: Integer;
begin
  if not MultiSelect then
  begin
    if (ItemIndex <> -1) and (ItemIndex < Items.Count - 1) then
    begin
      Items.Exchange(ItemIndex, ItemIndex + 1);
      ItemIndex := ItemIndex + 1;
    end;
    Exit;
  end;
  if (Items.Count > 0) and (SelCount > 0) and (not Selected[Items.Count - 1]) then
  begin
    I := Items.Count - 2;
    while I >= 0 do
    begin
      if Selected[I] then
      begin
        Items.Exchange(I, I + 1);
        Selected[I + 1] := True;
      end;
      Dec(I);
    end;
  end;
end;

procedure TJvCustomListBox.MoveSelectedUp;
var
  I: Integer;
begin
  if not MultiSelect then
  begin
    if ItemIndex > 1 then
    begin
      Items.Exchange(ItemIndex, ItemIndex - 1);
      ItemIndex := ItemIndex - 1;
    end;
    Exit;
  end;
  if (Items.Count > 0) and (SelCount > 0) and not Selected[0] then
  begin
    I := 1;
    while I < Items.Count do
    begin
      if Selected[I] then
      begin
        Items.Exchange(I, I - 1);
        Selected[I - 1] := True;
      end;
      Inc(I);
    end;
  end;
end;

procedure TJvCustomListBox.RemeasureAll;
var
  I: Integer;
  LMaxWidth, cx: Integer;
  LItemSize: TSize;
begin
  LMaxWidth := 0;
  if LimitToClientWidth then
    cx := ClientWidth
  else
    cx := 0;

  for I := 0 to Items.Count - 1 do
  begin
    MeasureString(Items[I], cx, LItemSize);
    if MultiLine then
      Perform(LB_SETITEMHEIGHT, I, LItemSize.cy);

    if not LimitToClientWidth and (LItemSize.cx > LMaxWidth) then
      LMaxWidth := LItemSize.cx;
  end;
  if not LimitToClientWidth then
    MaxWidth := LMaxWidth;
end;

function TJvCustomListBox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

function TJvCustomListBox.SearchPrefix(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

function TJvCustomListBox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

procedure TJvCustomListBox.SelectAll;
var
  I: Integer;
begin
  if MultiSelect then
  begin
    Items.BeginUpdate;
    for I := 0 to Items.Count - 1 do
      Selected[I] := True;
    Items.EndUpdate;
  end;
end;

procedure TJvCustomListBox.SelectCancel(var Msg: TMessage);
begin
  if Assigned(FOnSelectCancel) then
    FOnSelectCancel(Self);
end;

procedure TJvCustomListBox.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;

    UpdateStyle;
    Invalidate;
  end;
end;

procedure TJvCustomListBox.SetBackground(
  const Value: TJvListBoxBackground);
begin
  FBackground.Assign(Value);
end;

procedure TJvCustomListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Alignment <> taLeftJustify then
    Repaint;
end;

procedure TJvCustomListBox.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomListBox.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    Ctl3D := not FHotTrack;
  end;
end;

procedure TJvCustomListBox.SetMaxWidth(const Value: Integer);
begin
  if not LimitToClientWidth and (FMaxWidth <> Value) then
  begin
    FMaxWidth := Value;
    Perform(LB_SETHORIZONTALEXTENT, Value, 0);
  end;
end;

procedure TJvCustomListBox.SetMultiline(const Value: Boolean);
begin
  if FMultiline <> Value then
  begin
    FMultiline := Value;

    UpdateStyle;
    if FMultiline then
    begin
      // make sure scrollbars matches
      if ScrollBars = ssBoth then
        ScrollBars := ssVertical;
      if ScrollBars = ssHorizontal then
        ScrollBars := ssNone;
      FMaxWidth := 0;
      Perform(LB_SETHORIZONTALEXTENT, 0, 0);
    end
    else
      RemeasureAll;
  end;
end;

procedure TJvCustomListBox.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomListBox.SetSelectedColor(const Value: TColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomListBox.SetSelectedTextColor(const Value: TColor);
begin
  if FSelectedTextColor <> Value then
  begin
    FSelectedTextColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomListBox.SetShowFocusRect(const Value: Boolean);
const
  CShowFocusRect: array [Boolean] of Integer = (0, 2);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;

    ItemHeight := Canvas.TextHeight('Äy') + CShowFocusRect[ShowFocusRect];
    RemeasureAll;
    if Focused then
      Invalidate;
  end;
end;

procedure TJvCustomListBox.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomListBox.UnselectAll;
var
  I: Integer;
begin
  if MultiSelect then
  begin
    Items.BeginUpdate;
    for I := 0 to Items.Count - 1 do
      Selected[I] := False;
    Items.EndUpdate;
  end
  else
    ItemIndex := -1;
end;

procedure TJvCustomListBox.UpdateHorizontalExtent;
begin
  if HandleAllocated and (FScrollBars in [ssHorizontal, ssBoth]) then
    RemeasureAll;
  //    SendMessage(Handle, LB_SETHORIZONTALEXTENT, FHorizontalExtent, 0);
end;

procedure TJvCustomListBox.UpdateStyle;
const
  CShowFocusRect: array [Boolean] of Integer = (0, 2);
var
  PreviousStyle: TListBoxStyle;
begin
  if csLoading in ComponentState then
    Exit;

  PreviousStyle := Style;

  if MultiLine then
    Style := lbOwnerDrawVariable
  else
  if Alignment <> taLeftJustify then
    Style := lbOwnerDrawFixed;

  if (PreviousStyle = lbStandard) and (Style <> lbStandard) then
  begin
    ItemHeight := Canvas.TextHeight('Äy') + CShowFocusRect[ShowFocusRect];
    RemeasureAll;
  end;
end;

procedure TJvCustomListBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if not Background.DoDraw then
    inherited
  else
  begin
    //    Msg.Result := 1;
    DrawBackGround(Msg.DC, False);
  end;
end;

procedure TJvCustomListBox.WMHScroll(var Msg: TWMHScroll);
var
  DontScroll: Boolean;
  DoUpdate: Boolean;
  ScrollInfo: TScrollInfo;
begin
  DoUpdate := Background.DoDraw;

  if DoUpdate then
    BeginRedraw;
  try
    if Assigned(FOnHorizontalScroll) then
    begin
      DontScroll := False;
      FOnHorizontalScroll(Self, Msg, DontScroll);
      if DontScroll then
        Exit;
    end;
    inherited;

    if DoUpdate and (FMaxWidth > 0) then
    begin
      with ScrollInfo do
      begin
        cbSize := SizeOf(ScrollInfo);
        fMask := SIF_ALL;
        if GetScrollInfo(Handle, SB_HORZ, ScrollInfo) then
          FLeftPosition := Round((FMaxWidth / nMax) * nPos);
      end;
    end
    else
      FLeftPosition := 0;

    //if DoUpdate then
    //  Invalidate;
  finally
    if DoUpdate then
      EndRedraw;
  end;
end;

procedure TJvCustomListBox.WMVScroll(var Msg: TWMVScroll);
var
  DontScroll: Boolean;
  DoUpdate: Boolean;
begin
  DoUpdate := Background.DoDraw;

  if DoUpdate then
    BeginRedraw;
  try
    if Assigned(FOnVerticalScroll) then
    begin
      DontScroll := False;
      FOnVerticalScroll(Self, Msg, DontScroll);
      if DontScroll then
        Exit;
    end;
    inherited;

    //if DoUpdate then
    //  Invalidate;
  finally
    if DoUpdate then
      EndRedraw;
  end;
end;

procedure TJvCustomListBox.WndProc(var Msg: TMessage);
var
  ItemWidth: Word;
begin
  case Msg.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        ItemWidth := Canvas.TextWidth(StrPas(PChar(Msg.LParam)) + ' ');
        if FMaxWidth < ItemWidth then
          FMaxWidth := ItemWidth;
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
      end;
    LB_DELETESTRING:
      begin
        ItemWidth := Canvas.TextWidth(Items[Msg.WParam] + ' ');
        if ItemWidth = FMaxWidth then
        begin
          inherited WndProc(Msg);
          UpdateHorizontalExtent;
          Exit;
        end;
      end;
    LB_RESETCONTENT:
      SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    WM_SETFONT:
      begin
        inherited WndProc(Msg);
        Canvas.Font.Assign(Font);
        UpdateHorizontalExtent;
        Exit;
      end;
  end;
  inherited WndProc(Msg);
end;

{ TJvListBoxBackground }

procedure TJvListBoxBackground.Assign(Source: TPersistent);
begin
  if Source is TJvListBoxBackground then
  begin
    FImage.Assign(TJvListBoxBackground(Source).Image);
    FFillmode := TJvListBoxBackground(Source).FillMode;
    FVisible := TJvListBoxBackground(Source).Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TJvListBoxBackground.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TJvListBoxBackground.Create;
begin
  inherited Create;
  FImage := TBitmap.Create;
end;

destructor TJvListBoxBackground.Destroy;
begin
  FImage.Free;
  inherited;
end;

function TJvListBoxBackground.GetDoDraw: Boolean;
begin
  Result := Visible and not Image.Empty;
end;

procedure TJvListBoxBackground.SetFillMode(
  const Value: TJvListboxFillMode);
begin
  if FFillMode <> Value then
  begin
    FFillMode := Value;
    Change;
  end;
end;

procedure TJvListBoxBackground.SetImage(const Value: TBitmap);
begin
  FImage.Assign(Value);
  Change;
end;

procedure TJvListBoxBackground.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Change;
  end;
end;

end.


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
    FVisible: boolean;
    procedure SetFillMode(const Value: TJvListboxFillMode);
    procedure SetImage(const Value: TBitmap);
    procedure SetVisible(const Value: boolean);
  protected
    procedure Change;
  public
    constructor Create;
    destructor Destroy;override;
  published
    property Image:TBitmap read FImage write SetImage;
    property FillMode:TJvListboxFillMode read FFillMode write SetFillMode;
    property Visible:boolean read FVisible write SetVisible;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvCustomListBox = class(TListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FAlignment: TAlignment;
    FHotTrack: Boolean;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FMaxWidth: Integer;
    FItemSearchs: TJvItemsSearchs;
    FCount: Integer;
    FScrollBars: TScrollStyle;
    FSorted: Boolean;
    FOwnerData: Boolean;
    FOnGetText: TJvListBoxDataEvent;
    FOldStyle: TListBoxStyle;
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
    FDragImage: TDragImagelist;
    FMultiline: Boolean;
    FShowFocusRect: Boolean;
    FSelectedTextColor: TColor;
    FSelectedColor: TColor;
    FDisabledTextColor: TColor;
    FBackground: TJvListBoxBackground;
    { Private declarations }
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;

    procedure DrawBackGround(ADC: HDC);

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
    procedure SetOwnerData(const Value: Boolean);
    procedure SetCount(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetSorted(const Value: Boolean);
    procedure SetHotTrack(const Value: Boolean);
    function GetItems: TStrings;
  protected
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
    procedure DoBackgroundChange(Sender:TObject);
    property MaxWidth: Integer  read FMaxWidth write SetMaxWidth;

    property Count: Integer read FCount write SetCount;
    property Items: TStrings read GetItems write SetItems;
    property OwnerData: Boolean read FOwnerData write SetOwnerData default False;
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
    function MeasureString(const S: string; WidthAvail: Integer): Integer;
    procedure DefaultDrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); virtual;
    procedure DefaultDragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); virtual;
    procedure DefaultStartDrag(var DragObject: TDragObject); virtual;
    procedure DefaultDragDrop(Source: TObject; X, Y: Integer); virtual;
    procedure CreateDragImage(const S: string);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function GetDragImages: TDragImagelist; override;
    property DragIndex: Integer read FDragIndex;
    property DragImages: TDragImageList read GetDragImages;
    procedure WndProc(var Msg: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure UpdateCount;
    procedure UpdateHorizontalExtent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property Background: TJvListBoxBackground read FBackground write FBackground;
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


constructor TJvCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
// JvBMPListBox:
//  Style := lbOwnerDrawFixed;
  FBackground := TJvListBoxBackground.Create;
  FBackground.OnChange := DoBackgroundChange;
  FScrollBars := ssBoth;
  FAlignment := taLeftJustify;
  FMultiline := false;
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

destructor TJvCustomListBox.Destroy;
begin
  FItemSearchs.Free;
  FBackground.Free;
  inherited Destroy;
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
    if FOwnerData then
    begin
      Style := Style and not (LBS_SORT or LBS_HASSTRINGS) or LBS_NODATA;
      if Self.Style = lbStandard then
        Style := Style or LBS_OWNERDRAWFIXED;
    end;
  end;
end;

procedure TJvCustomListBox.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    Ctl3d := not FHotTrack;
  end;
end;

procedure TJvCustomListBox.LBAddString(var Msg: TMessage);
var
  W: Integer;
begin
  if not FMultiline then
  begin
    W := MeasureString(PChar(Msg.LParam), 0);
    if W > FMaxWidth then
      SetMaxWidth(W);
  end;
  inherited;
  if Assigned(FOnAddString) then
    FOnAddString(Self, StrPas(PChar(Msg.lParam)));
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCustomListBox.LBDeleteString(var Msg: TMessage);
var
  W: Integer;
  InheritedCalled:boolean;
begin
  InheritedCalled := false;
  if not FMultiline then
  begin
    W := MeasureString(Items[Msg.WParam], 0);
    InheritedCalled := W = FMaxWidth;
    if InheritedCalled then
    begin
      inherited;
      RemeasureAll;
    end;
  end;
  if Assigned(FOnDeleteString) then
    FOnDeleteString(Self, Items.Strings[LongInt(Msg.wParam)]);
  if not InheritedCalled then
    inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCustomListBox.WndProc(var Msg: TMessage);
var
  Text: string;
  ItemWidth: Word;
begin
  if FOwnerData then
  begin
    with Msg do
      case Msg of
        LB_GETTEXT:
          if Assigned(FOnGetText) and (wParam > -1) and (wParam < Count) then
          begin
            Text := '';
            OnGetText(Self, WParam, Text);
            Result := Length(Text);
            StrPCopy(PChar(LParam), Text);
          end
          else
            Result := LB_ERR;
        LB_GETTEXTLEN:
          if Assigned(FOnGetText) and (wParam > -1) and (wParam < Count) then
          begin
            Text := '';
            OnGetText(Self, WParam, Text);
            Result := Length(Text);
          end
          else
            Result := LB_ERR;
        LB_ADDSTRING, LB_INSERTSTRING, LB_SETITEMDATA:
          Result := LB_ERR;
      else
        inherited;
      end;
    Exit;
  end
  else
    case Msg.Msg of
      LB_ADDSTRING, LB_INSERTSTRING:
        begin
          ItemWidth := Canvas.TextWidth(StrPas(PChar(Msg.lParam)) + ' ');
          if FMaxWidth < ItemWidth then
            FMaxWidth := ItemWidth;
          SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
        end;
      LB_DELETESTRING:
        begin
          ItemWidth := Canvas.TextWidth(Items[Msg.wParam] + ' ');
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

procedure TJvCustomListBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomListBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
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
      Ctl3d := True;
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
      Ctl3d := False;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
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

procedure TJvCustomListBox.WMHScroll(var Msg: TWMHScroll);
var
  DontScroll: Boolean;
begin
  Items.BeginUpdate;
  if Assigned(FOnHorizontalScroll) then
  begin
    DontScroll := False;
    FOnHorizontalScroll(Self, Msg, DontScroll);
    if DontScroll then
      Exit;
  end;
  inherited;
  Invalidate;
  Items.EndUpdate;
end;

procedure TJvCustomListBox.WMVScroll(var Msg: TWMVScroll);
var
  DontScroll: Boolean;
begin
  Items.BeginUpdate;
  if Assigned(FOnVerticalScroll) then
  begin
    DontScroll := False;
    FOnVerticalScroll(Self, Msg, DontScroll);
    if DontScroll then
      Exit;
  end;
  inherited;
  Invalidate;
  Items.EndUpdate;
end;

procedure TJvCustomListBox.SelectCancel(var Msg: TMessage);
begin
  if Assigned(FOnSelectCancel) then
    FOnSelectCancel(Self);
end;

function TJvCustomListBox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

function TJvCustomListBox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
  Changed;
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

procedure TJvCustomListBox.Changed;
begin
  // (rom) TODO?
end;

procedure TJvCustomListBox.CreateWnd;
begin
  inherited CreateWnd;
  UpdateHorizontalExtent;
  UpdateCount;
end;

procedure TJvCustomListBox.SetCount(const Value: Integer);
begin
  if FCount <> Value then
  begin
    FCount := Value;
    UpdateCount;
  end;
end;

procedure TJvCustomListBox.SetItems(const Value: TStrings);
begin
  if FOwnerData then
    inherited Items.Clear
  else
    inherited Items := Value;
end;

procedure TJvCustomListBox.SetOwnerData(const Value: Boolean);
begin
  if FOwnerData <> Value then
  begin
    FOwnerData := Value;
    Items.Clear;
    FSorted := False;
    RecreateWnd;
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

procedure TJvCustomListBox.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if FOwnerData and Value then
      {$TYPEDADDRESS OFF}
      raise EJclError.CreateResRec(@RsLBVirtualCantBeSorted);
      {$TYPEDADDRESS ON}
    FSorted := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomListBox.UpdateCount;
begin
  if FOwnerData and HandleAllocated then
    Perform(LB_SETCOUNT, FCount, 0);
end;

procedure TJvCustomListBox.UpdateHorizontalExtent;
begin
  if HandleAllocated and (FScrollBars in [ssHorizontal, ssBoth]) then
    RemeasureAll;
//    SendMessage(Handle, LB_SETHORIZONTALEXTENT, FHorizontalExtent, 0);
end;

function TJvCustomListBox.GetItems: TStrings;
begin
  Result := inherited Items;
end;

procedure TJvCustomListBox.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if FAlignment <> taLeftJustify then
    begin
      FOldStyle := Style;
      Style := lbOwnerDrawFixed;
    end
    else
      Style := FoldStyle;
    Invalidate;
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

procedure TJvCustomListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Alignment <> taLeftJustify then
    Repaint;
end;

procedure TJvCustomListBox.CreateDragImage(const S: string);
var
  Size: TSize;
  Bmp: TBitmap;
begin
  if not Assigned(FDragImage) then
    FDragImage := TDragImagelist.Create(self)
  else
    FDragImage.Clear;
  Canvas.Font := Font;
  Size := Canvas.TextExtent(S);
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
    Bmp.Canvas.TextOut(0, 0, S);
    FDragImage.AddMasked(Bmp, clWhite);
  finally
    Bmp.Free;
  end;
  ControlStyle := ControlStyle + [csDisplayDragImage];
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
    DropIndex := ItemAtPos(Point(X, Y), true);
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

procedure TJvCustomListBox.DefaultStartDrag(var DragObject: TDragObject);
begin
  FDragIndex := ItemIndex;
  if FDragIndex >= 0 then
    CreateDragImage(Items[FDragIndex])
  else
    CancelDrag;
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

function TJvCustomListBox.GetDragImages: TDragImagelist;
begin
  Result := FDragImage;
end;

{ This routine is copied mostly from TCustomListbox.CNDRawItem.
  The setting of colors is modified.
  Drawing of the focus rectangle is delegated to DrawItem.}

procedure TJvCustomListBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  Itemheight := Canvas.TextHeight('Äy') + 2;
  RemeasureAll;
end;

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
      Canvas.FillRect(rcItem);
      if odFocused in State then
        DrawFocusRect(hDC, rcItem);
    end;
    Canvas.Handle := 0;
  end;
end;

{ This procedure is a slightly modified version of TCustomListbox.DrawItem! }

procedure TJvCustomListBox.DefaultDrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
const
  AlignFlags: array [TAlignment] of DWORD =
    (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Flags: Longint;
begin
  // JvBMPListBox:
  // draw text transparently
  if Background.Visible and not Background.Image.Empty then
  begin
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
  end
  else if Index < Items.Count then
  begin
    Canvas.FillRect(Rect);
    if FMultiline then
      Flags := DrawTextBiDiModeFlags(DT_WORDBREAK or DT_NOPREFIX or
        AlignFlags[FAlignment])
    else
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    if not UseRightToLeftAlignment then
      Inc(Rect.Left, 2)
    else
      Dec(Rect.Right, 2);
    DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), Rect, Flags);
  end;
end;

procedure TJvCustomListBox.LBInsertString(var Msg: TMessage);
var
  W: Integer;
begin
  if not FMultiline then
  begin
    W := MeasureString(PChar(Msg.LParam), 0);
    if W > FMaxWidth then
      SetMaxWidth(W);
  end;
  inherited;
end;

procedure TJvCustomListBox.MeasureItem(Index: Integer;
  var Height: Integer);
begin
  if Assigned(OnMeasureItem) or (not Multiline) or
    (Index < 0) or (Index >= items.count) then
    inherited MeasureItem(Index, Height)
  else
    Height := MeasureString(Items[index], ClientWidth);
end;

function TJvCustomListBox.MeasureString(const S: string;
  WidthAvail: Integer): Integer;
var
  Flags: Longint;
  R: TRect;
begin
  Canvas.Font := Font;
  Result := Canvas.TextWidth(S);
  { Note: doing the TextWidth unconditionally makes sure the font is properly
    selected into the device context. }
  if WidthAvail > 0 then
  begin
    Flags := DrawTextBiDiModeFlags(
      DT_WORDBREAK or DT_NOPREFIX or DT_CALCRECT or AlignFlags[FAlignment]);
    R := Rect(0, 0, WidthAvail - 2, 1);
    DrawText(canvas.handle, Pchar(S), Length(S), R, Flags);
    Result := R.Bottom;
    if Result > 255 then
      Result := 255;
    if Result < ItemHeight then
      Result := ItemHeight;
    { Note: item height in a listbox is limited to 255 pixels since Windows
      stores the height in a single byte.}
  end;
end;

procedure TJvCustomListBox.RemeasureAll;
var
  i: Integer;
  max, cx, w: Integer;
begin
  max := 0;
  w := 0;
  if FMultiline then
    cx := ClientWidth
  else
    cx := 0;

  for i := 0 to Items.Count - 1 do
  begin
    w := MeasureString(Items[i], cx);
    if FMultiline then
      Perform(LB_SETITEMHEIGHT, i, w)
    else
    if w > max then
      max := w;
  end;

  if not FMultiline then
    MaxWidth := w;
end;

procedure TJvCustomListBox.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomListBox.SetMaxWidth(const Value: Integer);
begin
  if not FMultiline and (FMaxWidth <> Value) then
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
    if Value then
    begin
      Style := lbOwnerDrawVariable;
      // make sure scrollbars matches
      if (ScrollBars = ssBoth) then
        ScrollBars := ssVertical;
      if ScrollBars = ssHorizontal then
        ScrollBars := ssNone;
      FMaxWidth := 0;
      Perform(LB_SETHORIZONTALEXTENT, 0, 0);
    end
    else
    begin
      Style := lbOwnerDrawFixed;
      RemeasureAll;
    end;
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
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    if Focused then
      Invalidate;
  end;
end;

procedure TJvCustomListBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if Background.Image.Empty or not Background.Visible then
    inherited
  else
  begin
//    Msg.Result := 1;
    DrawBackGround(Msg.DC);
  end;
end;

procedure TJvCustomListBox.DrawBackGround(ADC: HDC);
var
  ImageRect, ClipBox, ClientRect, Temp: TRect;
  Cv: TCanvas;
  ClipComplexity: Integer;
begin
  if (ADC = 0) or not Background.Visible then
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
    if Background.Fillmode = bfmStretch then
      Cv.StretchDraw(ClientRect, Background.Image)
    else
    begin
      ImageRect := Background.Image.Canvas.ClipRect;
      while ImageRect.top < ClientRect.bottom do
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

{ TJvListBoxBackground }

procedure TJvListBoxBackground.Change;
begin
  if Assigned(FOnChange) then FOnChange(self);
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

procedure TJvListBoxBackground.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Change;
  end;
end;

procedure TJvCustomListBox.DoBackgroundChange(Sender: TObject);
begin
  Invalidate;
end;

end.


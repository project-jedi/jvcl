{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvListComb.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

This is a merging of the original TJvListBox3 and TJvImageListBox
TJvListBox3 has been renamed TJvImageListBox and the original TJvImageListBox has been moved to \archive

Contributor(s):
Sébastien Buysse [sbuysse att buypin dott com]
Michael Beck [mbeck att bigfoot dott com]
Olivier Sannier [obones att altern dott org]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  A unit to allow display of bitmaps in TComboboxes and TListboxes

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvListComb;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, ExtCtrls, StdCtrls, ImgList,
  JvJCLUtils, JvCombobox,
  JvExStdCtrls;

type
  TJvButtonColors = (fsLighter, fsLight, fsMedium, fsDark, fsDarker);
  TJvListPropertiesUsed = set of (puFont, puColorHighlight, puColorHighlightText);

const
  AllListPropertiesUsed = [puFont, puColorHighlight, puColorHighlightText];

type
  IJvResetItemHeight = interface
  ['{29F7C34D-F03C-41FE-8423-0388289A505B}']
    procedure ResetItemHeight;
  end;

  TJvImageItems = class;

  TJvImageItem = class(TCollectionItem)
  private
    FOwner: TJvImageItems;
    FImageIndex: Integer;
    FIndent: Integer;
    FListPropertiesUsed: TJvListPropertiesUsed;
    FFont: TFont;
    FColorHighlight: TColor;
    FColorHighlightText: TColor;
    FGlyph: TBitmap;
    FLinkedObject: TObject;
    FNoTextAssign: Boolean;
    FBrush: TBrush;
    FStringsIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetIndent(const Value: Integer);
    function GetWinControl: TWinControl;
    procedure Change;
    function GetText: string;
    function GetOwnerStrings: TStrings;
    function GetFont: TFont;
    function GetGlyph: TBitmap;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetFont(const Value: TFont);
    function GetColorHighlight: TColor;
    function GetColorHighlightText: TColor;
    procedure SetColorHighlight(const Value: TColor);
    procedure SetColorHighlightText(const Value: TColor);
    function IsColorHighlightTextStored: Boolean;
    function IsColorHighlightStored: Boolean;
    procedure SetBrush(const Value: TBrush);
    procedure SyncControlItemIndex(ControlItemIndex: Integer; UpdateIndex: Boolean);
  protected
    procedure SetIndex(Value: Integer); override;
    function GetStringsIndex: Integer;
    function GetDisplayName: string; override;

    function IsFontStored: Boolean;
    procedure FontChange(Sender: TObject);
  public
    constructor Create(Collection: Classes.TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property LinkedObject: TObject read FLinkedObject write FLinkedObject;
  published
    // ListPropertiesUsed must come before properties named the same
    // as in the list or the component will not be created
    // correctly when restored from a DFM stream.
    property ListPropertiesUsed: TJvListPropertiesUsed read FListPropertiesUsed
      write FListPropertiesUsed default AllListPropertiesUsed;
    property ColorHighlight: TColor read GetColorHighlight
      write SetColorHighlight stored IsColorHighlightStored default clHighlight;
    property ColorHighlightText: TColor read GetColorHighlightText
      write SetColorHighlightText stored IsColorHighlightTextStored default clHighlightText;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property Brush: TBrush read FBrush write SetBrush;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored True;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Indent: Integer read FIndent write SetIndent default 2;
    property Text: string read GetText write SetText;
  end;

  TJvImageItems = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TJvImageItem;
    procedure SetItems(Index: Integer; const Value: TJvImageItem);
    function GetObjects(Index: Integer): TObject;
    procedure SetObjects(Index: Integer; const Value: TObject);
  protected
    FStrings: TStrings;  // Protected to allow to use it in derived classes
    FDestroying: Boolean; // True when our Destroy has been called.
    FClearing: Boolean; // True when our Clear is active

    procedure Update(Item: TCollectionItem); override;
    procedure FillItems;
    procedure UpdateItemIndices;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    function IsStringsSorted: Boolean;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add: TJvImageItem;
    function AddTextItem(const Text: string): TJvImageItem;
    function AddText(const Text: string): Integer;
    function AddObject(const Text: string; ALinkedObject: TObject): Integer;
    function Insert(Index: Integer): TJvImageItem;
    function InsertTextItem(Index: Integer; const Text: string): TJvImageItem;
    procedure InsertText(Index: Integer; const Text: string);
    procedure InsertObject(Index: Integer; const Text: string; ALinkedObject: TObject);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Sort(SortProc: TCollectionSortProc);
    procedure Clear; // unfortunately not virtual

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    function IndexOfLinkedObject(ALinkedObject: TObject): Integer;

    property Items[Index: Integer]: TJvImageItem read GetItems write SetItems; default;
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvImageComboBox = class(TJvCustomComboBox, IUnknown, IJvResetItemHeight)
  private
    FItems: TJvImageItems;
    FImageList: TCustomImageList;
    FDefaultIndent: Integer;
    FChangeLink: TChangeLink;
    FMouseInControl: Boolean;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FColorHighlight: TColor;
    FColorHighlightText: TColor;
    FOnChange: TNotifyEvent;
    FButtonFrame: Boolean;
    FButtonStyle: TJvButtonColors;
    FIndentSelected: Boolean;
    FDroppedWidth: Integer;
    FFullWidthItemDraw: Boolean;
    FCanvas: TControlCanvas;
    function GetCanvas: TCanvas;
    function GetDroppedWidth: Integer;
    procedure SetDroppedWidth(Value: Integer);
    procedure SetColorHighlight(Value: TColor);
    procedure SetColorHighlightText(Value: TColor);
    procedure SetImageList(Value: TCustomImageList);

    procedure ImageListChange(Sender: TObject);
    procedure SetDefaultIndent(const Value: Integer);
    procedure SetItems(const Value: TJvImageItems); reintroduce;
    procedure SetIndentSelected(const Value: Boolean);
    procedure SetFullWidthItemDraw(const Value: Boolean);
    { IJvResetItemHeight }
    procedure ResetItemHeight;
    function GetSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
  protected
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure FontChanged; override;
    procedure EnabledChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RecreateWnd;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;

    procedure Change; override;

    function GetImageWidth(Index: Integer): Integer;
    function GetImageHeight(Index: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
    property Text;
  published
    property Style; {Must be published before Items}

    property Align;
    property Anchors;
    property AutoComplete default True;
    property AutoDropDown default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property ButtonFrame: Boolean read FButtonFrame write FButtonFrame default False;
    property ButtonStyle: TJvButtonColors read FButtonStyle write FButtonStyle;
    property CharCase;
    property Color;
    property ColorHighlight: TColor read FColorHighlight write SetColorHighlight default clHighlight;
    property ColorHighlightText: TColor read FColorHighlightText write SetColorHighlightText default clHighlightText;
    property Constraints;
    property DefaultIndent: Integer read FDefaultIndent write SetDefaultIndent default 0;
    property DragCursor;
    property DragKind;
    property DroppedWidth: Integer read GetDroppedWidth write SetDroppedWidth;
    property DragMode;
    property DropDownCount;
    property EmptyValue;
    property EmptyFontColor;
    property Enabled;
    property Font;
    property FullWidthItemDraw: Boolean read FFullWidthItemDraw write SetFullWidthItemDraw default False;
    property HintColor;
    property ImageHeight: Integer read FImageHeight write FImageHeight;
    property ImageWidth: Integer read FImageWidth write FImageWidth;
    property Images: TCustomImageList read FImageList write SetImageList;
    property ImeMode;
    property ImeName;
    property IndentSelected: Boolean read FIndentSelected write SetIndentSelected default False;
    property ItemHeight;
    property ItemIndex;
    property MaxLength;
    property MaxPixel;
    property MeasureStyle;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Provider;
    property ReadOnly;
    property ShowHint;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property Tag;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;

    property Items: TJvImageItems read FItems write SetItems; // must be declared after OnMeasureItem
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvImageListBox = class(TJvExCustomListBox, IUnknown, IJvResetItemHeight)
  private
    FImageList: TCustomImageList;
    FItems: TJvImageItems;
    FChangeLink: TChangeLink;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FAlignment: TAlignment;
    FColorHighlight: TColor;
    FColorHighlightText: TColor;
    FButtonFrame: Boolean;
    FButtonStyle: TJvButtonColors;
    FFullWidthItemDraw: Boolean;
    FCanvas: TControlCanvas;
    function GetCanvas: TCanvas;
    procedure SetColorHighlight(Value: TColor);
    procedure SetColorHighlightText(Value: TColor);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetAlignment(Value: TAlignment);
    procedure DrawLeftGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure DrawRightGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure DrawCenteredGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure SetItems(const Value: TJvImageItems);
    procedure SetFullWidthItemDraw(const Value: Boolean);
    { IJvResetItemHeight }
    procedure ResetItemHeight;
  protected
    procedure FontChanged; override;
    procedure ImageListChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure Resize; override;

    function GetImageWidth(Index: Integer): Integer;
    function GetImageHeight(Index: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
  published
    property Anchors;
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property BorderStyle;
    property Color;
    property DragMode;
    property DragCursor;
    property IntegralHeight;
    property Enabled;
    property Font;
    property Items: TJvImageItems read FItems write SetItems;
    property ImageHeight: Integer read FImageHeight write FImageHeight;
    property ImageWidth: Integer read FImageWidth write FImageWidth;
    property ButtonFrame: Boolean read FButtonFrame write FButtonFrame default False;
    property ButtonStyle: TJvButtonColors read FButtonStyle write FButtonStyle;
    property ColorHighlight: TColor read FColorHighlight write SetColorHighlight default clHighlight;
    property ColorHighlightText: TColor read FColorHighlightText write SetColorHighlightText default clHighlightText;
    property Images: TCustomImageList read FImageList write SetImageList;
    property FullWidthItemDraw: Boolean read FFullWidthItemDraw write SetFullWidthItemDraw default False;
    property MultiSelect;
    property ItemHeight;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Sorted;
    property Tag;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF RTL330_UP}
  System.Generics.Collections, // for TCollectionNotification items
  {$ENDIF RTL330_UP}
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF}
  Math, JvJVCLUtils;

type
  TWinControlAccessProtected = class(TWinControl);

{ utility }

{
function DropArrowWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
end;
}


function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(HWND_DESKTOP);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(HWND_DESKTOP, DC);
  Result := Metrics.tmHeight;
end;

procedure DrawBtnFrame(Canvas: TCanvas; ButtonStyle: TJvButtonColors; DefColor: TColor; Default: Boolean; R: TRect);
const
  TopStyles: array [TJvButtonColors] of TColor =
    (clBtnHighLight, clBtnHighLight, clBtnHighLight, clBtnFace, clBtnShadow);
  BottomStyles: array [TJvButtonColors] of TColor =
    (clBtnFace, clBtnShadow, cl3DDkShadow, cl3DDkShadow, cl3DDkShadow);
begin
  if Default then
    Frame3D(Canvas, R, DefColor, DefColor, 1)
  else
    Frame3D(Canvas, R, TopStyles[ButtonStyle], BottomStyles[ButtonStyle], 1);
end;

procedure TJvImageItem.Assign(Source: TPersistent);
begin
  if Source is TJvImageItem then
  begin
    if not FNoTextAssign then
      Text := TJvImageItem(Source).Text;
    FImageIndex := TJvImageItem(Source).ImageIndex;
    FIndent := TJvImageItem(Source).Indent;
    FLinkedObject := TJvImageItem(Source).LinkedObject;
    Glyph := TJvImageItem(Source).Glyph;
    FColorHighlight := TJvImageItem(Source).ColorHighlight;
    FColorHighlightText := TJvImageItem(Source).ColorHighlightText;
    if TJvImageItem(Source).FFont <> nil then
      Font := TJvImageItem(Source).Font
    else
      FreeAndNil(FFont);
    ListPropertiesUsed := TJvImageItem(Source).ListPropertiesUsed;
    Change;
  end
  else
    inherited Assign(Source);
end;

//=== { TJvImageItem } =======================================================

constructor TJvImageItem.Create(Collection: Classes.TCollection);
begin
  // FGlyph and FBrush MUST be created before calling inherited or the
  // creation of the item from a stream (DFM for instance)
  // will not work correctly.
  FGlyph := TBitmap.Create;
  FBrush := TBrush.Create;
  FBrush.Style := bsClear;

  inherited Create(Collection);
  FImageIndex := -1;
  FOwner := Collection as TJvImageItems;
  FListPropertiesUsed := AllListPropertiesUsed;
  FFont := nil;
  FColorHighlight := clHighlight;
  FColorHighlightText := clHighlightText;
  FStringsIndex := -1;
end;

destructor TJvImageItem.Destroy;
var
  S: TStrings;
  I, Idx: Integer;
begin
  S := GetOwnerStrings;
  FOwner := nil; // indicate that the item is in the destructor

  if (Collection <> nil) and not (TJvImageItems(Collection).FDestroying or TJvImageItems(Collection).FClearing) then
  begin
    //if (S <> nil) and not (csDestroying in TComponent(FOwner.GetWinControl).ComponentState) then
    if (S <> nil) and (GetWinControl <> nil) and not (csDestroying in GetWinControl.ComponentState) then
    begin
      S.BeginUpdate;
      try
        Idx := Index;
        S.Delete(Idx);
        for I := Idx to S.Count - 1 do
          TJvImageItem(S.Objects[I]).SyncControlItemIndex(I, False); // we don't need to set Item.Index because we remove it from the collection
      finally
        S.EndUpdate;
      end;
    end;
  end;
  FFont.Free;
  FGlyph.Free;
  FBrush.Free;
  inherited Destroy;
end;

function TJvImageItem.GetDisplayName: string;
begin
  if Text = '' then
    Result := inherited GetDisplayName
  else
    Result := Text;
end;

procedure TJvImageItem.Change;
begin
  if Assigned(FOwner) then
    FOwner.Update(Self);
end;

procedure TJvImageItem.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TJvImageItem.SetIndent(const Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    Change;
  end;
end;

function TJvImageItem.GetOwnerStrings: TStrings;
begin
  Result := nil;
  if Assigned(FOwner) then
    Result := FOwner.FStrings;
end;

procedure TJvImageItem.SetText(const Value: string);
var
  S: TStrings;
  SavedOwner: TJvImageItems;
  Idx: Integer;
begin
  S := GetOwnerStrings;
  if Assigned(FOwner) and (FOwner.FStrings.Count <> FOwner.Count) then
    FOwner.FillItems;
  if S <> nil then
  begin
    Idx := GetStringsIndex;
    if S[Idx] <> Value then
    begin
      // do not add the item in FillItems which might be called by the draw message handler while deleting the string
      SavedOwner := FOwner;
      try
        FOwner := nil;
        S.BeginUpdate;
        try
          S.Delete(Idx);
          if SavedOwner.IsStringsSorted then
            SyncControlItemIndex(S.AddObject(Value, Self), True) // AddObject moved the item (sorted)
          else
          begin
            S.InsertObject(Idx, Value, Self);
            SyncControlItemIndex(Idx, False);
          end;
        finally
          S.EndUpdate;
        end;
      finally
        FOwner := SavedOwner;
      end;
      Change;
    end;
  end;
end;

function TJvImageItem.GetText: string;
var
  S: TStrings;
begin
  Result := '';
  if Assigned(FOwner) and (FOwner.FStrings.Count <> FOwner.Count) then
    FOwner.FillItems;
  S := GetOwnerStrings;
  if S <> nil then
    Result := S[Index];
end;

procedure TJvImageItem.SetIndex(Value: Integer);
var
  OldIndex, TmpIndex: Integer;
  S: TStrings;
begin
  OldIndex := Index;
  if Value <> OldIndex then
  begin
    inherited SetIndex(Value);
    S := GetOwnerStrings;
    TmpIndex := GetStringsIndex;
    if (TmpIndex > -1) and (TmpIndex <> Value) then
    begin
      S.Move(OldIndex, Value);
      FStringsIndex := Value;
    end;
  end;
end;

procedure TJvImageItem.SyncControlItemIndex(ControlItemIndex: Integer; UpdateIndex: Boolean);
begin
  // We don't need to update the control's string list because the index comes from it so we
  // can set the index much faster.
  FStringsIndex := ControlItemIndex;
  if UpdateIndex then
    inherited SetIndex(ControlItemIndex);
end;

function TJvImageItem.GetStringsIndex: Integer;
var
  S: TStrings;
begin
  S := GetOwnerStrings;
  if S <> nil then
  begin
    Result := FStringsIndex;
    if Result = -1 then // we don't have that information yet
    begin
      FStringsIndex := S.IndexOfObject(Self);
      Result := FStringsIndex;
    end
    else
    begin
      if (Result >= 0) and (Result < S.Count) then
      begin
        if S.Objects[Result] <> Self then // we have the wrong item
        begin
          FStringsIndex := S.IndexOfObject(Self);
          Result := FStringsIndex;
        end;
      end
      else
        Result := -1;
    end;
  end
  else
    Result := -1;
end;

//=== { TJvImageItems } ======================================================

constructor TJvImageItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvImageItem);
  FDestroying := False;
end;

destructor TJvImageItems.Destroy;
begin
  FDestroying := True;
  inherited Destroy;
end;

procedure TJvImageItems.Clear;
begin
  FClearing := True; // prevent the item by item deletion from FStrings
  try
    if (GetOwner is TWinControl) and TWinControl(GetOwner).HandleAllocated then
      FStrings.Clear;
    inherited Clear;
  finally
    FClearing := False;
  end;
end;

function TJvImageItems.Add: TJvImageItem;
begin
  Result := Items[AddText('')];
end;

function TJvImageItems.AddTextItem(const Text: string): TJvImageItem;
begin
  Result := Items[AddText(Text)];
end;

function TJvImageItems.AddText(const Text: string): Integer;
var
  Index: Integer;
  MinIndex, Idx: Integer;
  I: Integer;
  Item: TJvImageItem;
begin
  MinIndex := Count + 1;

  // Add missing items to FStrings
  Index := FStrings.Count;
  while Index < Count do
  begin
    Idx := FStrings.AddObject('', Items[Index]);
    if Idx <> Count - 1 then
      Items[Index].SyncControlItemIndex(Idx, True); // don't use SetIndex here because it will move the FStrings element
    if Idx < MinIndex then
      MinIndex := Idx;
  end;

  // Add new item
  Item := TJvImageItem(inherited Add);
  Result := FStrings.AddObject(Text, Item);
  if Result <> Count - 1 then
    Item.SyncControlItemIndex(Result, True); // don't use SetIndex here because it will move the FStrings element
  if Result < MinIndex then
    MinIndex := Result;

  // If we were sorted all indices of the items from the first inserted must be sync'ed.
  // Include the added item because the SyncControlItemIndex call above may not have been called.
  for I := MinIndex to Count - 1 do
    Items[Index].SyncControlItemIndex(I, False);
end;

function TJvImageItems.Insert(Index: Integer): TJvImageItem;
begin
  Result := InsertTextItem(Index, '');
end;

function TJvImageItems.InsertTextItem(Index: Integer; const Text: string): TJvImageItem;
begin
  InsertText(Index, Text);
  Result := Items[Index];
end;

procedure TJvImageItems.InsertText(Index: Integer; const Text: string);
var
  Item: TJvImageItem;
begin
  // InsertString ignores the SORT flag of the control
  //   https://docs.microsoft.com/en-us/windows/win32/controls/cb-insertstring
  //   https://docs.microsoft.com/en-us/windows/win32/controls/lb-insertstring

  //Item := TJvImageItem(inherited Insert(Index)); => Add+SetIndex
  // Don't move the not yet existing FStrings element what SetIndex in inherited Insert would do
  Item := TJvImageItem(inherited Add);
  if Index < Count - 1 then // Don't move at all if we just append
    Item.SyncControlItemIndex(Index, True);
  FStrings.InsertObject(Index, Text, Item);

  // The StringsIndex of the affected items must be sync'ed
  while Index < Count do
  begin
    Items[Index].SyncControlItemIndex(Index, False);
    Inc(Index);
  end;
end;

procedure TJvImageItems.Move(CurIndex, NewIndex: Integer);
var
  Item: TJvImageItem;
  ItemText: string;
begin
  if NewIndex < 0 then
    NewIndex := 0;
  if NewIndex > Count then
    NewIndex := Count;

  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      Item := TJvImageItem(FStrings.Objects[CurIndex]);
      ItemText := Item.Text;
      FStrings.Delete(CurIndex);
      FStrings.InsertObject(NewIndex, ItemText, Item);
      Item.SyncControlItemIndex(NewIndex, True); // move the item without moving the FStrings element

      // update FStringsIndex for all affected items
      if NewIndex > CurIndex then
      begin
        while CurIndex < NewIndex do
        begin
          Items[CurIndex].SyncControlItemIndex(CurIndex, False);
          Inc(CurIndex);
        end;
      end
      else
      begin
        Inc(NewIndex);
        while NewIndex <= CurIndex do
        begin
          Items[NewIndex].SyncControlItemIndex(NewIndex, False);
          Inc(NewIndex);
        end;
      end;
    finally
      EndUpdate;
    end;
    Changed;
  end;
end;

function TJvImageItems.IsStringsSorted: Boolean;
begin
  if GetOwner is TJvImageListBox then
    Result := TJvImageListBox(GetOwner).Sorted
  else if GetOwner is TJvImageComboBox then
    Result := TJvImageComboBox(GetOwner).Sorted
  else
    Result := False;
end;

function TJvImageItems.IndexOfLinkedObject(ALinkedObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].LinkedObject = ALinkedObject then
      Exit;
  Result := -1;
end;

procedure TJvImageItems.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvImageItems then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvImageItems(Source).Count - 1 do
        Add.Assign(TJvImageItems(Source)[I]);
    finally
      EndUpdate;
    end;
  end
  else
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TStrings(Source).Count - 1 do
        Add.Text := TStrings(Source)[I];
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvImageItems.GetItems(Index: Integer): TJvImageItem;
begin
  Result := TJvImageItem(inherited Items[Index]);
end;

procedure TJvImageItems.SetItems(Index: Integer; const Value: TJvImageItem);
begin
  inherited Items[Index] := Value;
end;

function TJvImageItems.GetObjects(Index: Integer): TObject;
begin
  Result := Items[Index].LinkedObject;
end;

procedure TJvImageItems.SetObjects(Index: Integer; const Value: TObject);
begin
  Items[Index].LinkedObject := Value;
end;

procedure TJvImageItems.Update(Item: TCollectionItem);
var
  W: TPersistent;
  Obj: IJvResetItemHeight;
begin
  if UpdateCount <> 0 then
    Exit;
  inherited Update(Item);
  W := GetOwner;
  if Supports(W, IJvResetItemHeight, Obj) then
    Obj.ResetItemHeight
  else
  if W is TWinControl then
    TWinControl(W).Invalidate;
end;

procedure TJvImageItems.BeginUpdate;
begin
  inherited BeginUpdate;
  if not FDestroying then
    FStrings.BeginUpdate;
end;

procedure TJvImageItems.EndUpdate;
begin
  if not FDestroying then
    FStrings.EndUpdate;
  inherited EndUpdate;
end;

procedure TJvImageItems.FillItems;
var
  Index: Integer;
  List: TList;
  ControlInUpdate: Boolean;
begin
  if Count > 0 then
  begin
    // Create a list with all objects from FStrings, so that we don't need to communicate with the
    // control via (slow) SendMessage calls for item and every IndexOfObject call.
    List := TList.Create;
    try
      for Index := 0 to FStrings.Count - 1 do
        List.Add(FStrings.Objects[Index]);

      ControlInUpdate := False;
      BeginUpdate;
      try
        for Index := 0 to Count - 1 do
        begin
          if Items[Index].FOwner = Self then // not in destructor
          begin
            if List.IndexOf(Items[Index]) = -1 then
            begin
              if not ControlInUpdate then
              begin
                ControlInUpdate := True;
                FStrings.BeginUpdate;
              end;
              // Keep List and FStrings in sync
              List.Insert(Index, Items[Index]);
              FStrings.InsertObject(Index, '', Items[Index]);
            end;
          end;
        end;

        // Slow if many items exist because of the GetIndex in SetIndex
        for Index := 0 to List.Count - 1 do
          TJvImageItem(List[Index]).SyncControlItemIndex(Index, True);
      finally
        if ControlInUpdate then
          FStrings.EndUpdate;
        EndUpdate;
      end;
    finally
      List.Free;
    end;
  end;
end;

procedure TJvImageItems.UpdateItemIndices;
var
  Index: Integer;
begin
  // Move all items to the indices from FStrings
  if Count > 0 then
  begin
    BeginUpdate;
    try
      for Index := 0 to FStrings.Count - 1 do
        TJvImageItem(FStrings.Objects[Index]).SyncControlItemIndex(Index, True);
    finally
      EndUpdate;
    end;
  end;
end;

function TJvImageItems.AddObject(const Text: string; ALinkedObject: TObject): Integer;
var
  Item: TJvImageItem;
begin
  Result := AddText(Text);
  Item := Items[Result];
  Item.LinkedObject := ALinkedObject;
end;

procedure TJvImageItems.InsertObject(Index: Integer; const Text: string; ALinkedObject: TObject);
begin
  InsertTextItem(Index, Text).LinkedObject := ALinkedObject;
end;

procedure TJvImageItems.Sort(SortProc: TCollectionSortProc);
begin
  CollectionSort(Self, SortProc);
end;

procedure TJvImageItems.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
var
  Idx: Integer;
begin
  inherited Notify(Item, Action);

  if FDestroying then
    Exit;

  // For Added and Deleting, the TListBoxStrings class will deal with
  // notifying the list box. In the case of Extracting though, we must
  // remove the item ourselves or the count in FStrings will be out of
  // sync with the count in this class.
  case Action of
    cnAdded: ;
    cnExtracting:
      begin
        if not FClearing and not FDestroying then
        begin
          Idx := TJvImageItem(Item).GetStringsIndex;
          if Idx <> -1 then
            FStrings.Delete(Idx);
        end;
      end;
    cnDeleting: ;
  end;
end;

//=== { TJvImageComboBox } ===================================================

constructor TJvImageComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TJvImageItems.Create(Self);
  FItems.FStrings := inherited Items;
  FImageWidth := 0;
  FImageHeight := 0;
  FImageList := nil;
  FDefaultIndent := 0;
  FButtonFrame := False;
  Style := csOwnerDrawVariable;
  Color := clWindow;
  FColorHighlight := clHighlight;
  FColorHighlightText := clHighlightText;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  ResetItemHeight;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImageListChange;

  FFullWidthItemDraw := False;
end;

destructor TJvImageComboBox.Destroy;
begin
  FItems.Free;
  FChangeLink.Free;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

function TJvImageComboBox.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvImageComboBox.ImageListChange(Sender: TObject);
begin
  //  Invalidate;
end;

procedure TJvImageComboBox.SetImageList(Value: TCustomImageList);
begin
  if FImageList <> Value then
  begin
    ReplaceImageListReference(Self, Value, FImageList, FChangeLink);
    ResetItemHeight;
    RecreateWnd;
  end;
end;

procedure TJvImageComboBox.SetFullWidthItemDraw(const Value: Boolean);
begin
  if Value <> FFullWidthItemDraw then
  begin
    FFullWidthItemDraw := Value;
    Invalidate
  end;
end;

procedure TJvImageComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetDroppedWidth(FDroppedWidth);
end;

procedure TJvImageComboBox.RecreateWnd;
begin
  inherited RecreateWnd;
  SetDroppedWidth(FDroppedWidth);
end;

procedure TJvImageComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;
end;

procedure TJvImageComboBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  if csDestroying in ComponentState then
    Exit;
  with Msg.DrawItemStruct^ do
  begin
    State := [];
    if (itemState and ODS_CHECKED) <> 0 then
      Include(State, odChecked);
    if (itemState and ODS_COMBOBOXEDIT) <> 0 then
      Include(State, odComboBoxEdit);
    if (itemState and ODS_DEFAULT) <> 0 then
      Include(State, odDefault);
    if (itemState and ODS_DISABLED) <> 0 then
      Include(State, odDisabled);
    if (itemState and ODS_FOCUS) <> 0 then
      Include(State, odFocused);
    if (itemState and ODS_GRAYED) <> 0 then
      Include(State, odGrayed);
    if (itemState and ODS_SELECTED) <> 0 then
      Include(State, odSelected);

    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;

    if (Integer(itemID) >= 0) then
    begin
      if Items[itemID].Brush.Style <> bsClear then
        FCanvas.Brush := Items[itemID].Brush;
      if (odSelected in State) then
      begin
        FCanvas.Brush.Color := FColorHighlight;
        FCanvas.Font.Color := FColorHighlightText;
      end;

      DrawItem(itemID, rcItem, State)
    end
    else
    begin
      FCanvas.FillRect(rcItem);
    end;

    FCanvas.Handle := 0;
  end;
end;

procedure TJvImageComboBox.DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState);
var
  Offset, Tmp: Integer;
  TmpCol: TColor;
  TmpR, OrigR: TRect;
  SavedColor: TColor;
begin
  if csDestroying in ComponentState then
    Exit;
  SavedColor := Canvas.Font.Color;

  if odSelected in State then
  begin
    Canvas.Brush.Color := FColorHighlight;
    Canvas.Font.Color := FColorHighlightText;
  end;

  if State <> [] then
    Canvas.Font.Color := SavedColor;
  OrigR := R;
  with Canvas do
  begin
    if not FullWidthItemDraw then
    begin
      TmpCol := Brush.Color;
      Brush.Color := Color;
      FillRect(R);
      Brush.Color := TmpCol;
    end;

    // (p3) don't draw indentation for edit item unless explicitly told to do so
    if not (odComboBoxEdit in State) or IndentSelected then
      R.Left := R.Left + Items[Index].Indent;

    if not Items[Index].Glyph.Empty then
    begin
      Offset := ((R.Bottom - R.Top) - GetImageHeight(Index)) div 2;

      Canvas.Draw(R.Left + 2, R.Top + Offset, Items[Index].Glyph);

      if FButtonFrame then
      begin
        TmpR := Rect(R.Left, R.Top, R.Left + FImageList.Width + 4, R.Top + FImageList.Height + 4);
        DrawBtnFrame(Canvas, FButtonStyle, Color,
          not ((odFocused in State) and not (odComboBoxEdit in State)), TmpR);
      end;

      Inc(R.Left, GetImageWidth(Index) + 8);
      OrigR.Left := R.Left;
    end
    else
    if Assigned(FImageList) then
    begin
      Tmp := Items[Index].ImageIndex;
      //      R.Left := R.Left + Items[Index].Indent;
      Offset := ((R.Bottom - R.Top) - GetImageHeight(Index)) div 2;
      // PRY 2002.06.04
      //FImageList.Draw(FCanvas, R.Left + 2, R.Top + Offset, Tmp, dsTransparent, itImage);
      FImageList.Draw(Canvas, R.Left + 2, R.Top + Offset, Tmp, dsTransparent, itImage);
      // PRY END
      if FButtonFrame then
      begin
        TmpR := Rect(R.Left, R.Top, R.Left + FImageList.Width + 4, R.Top + FImageList.Height + 4);
        DrawBtnFrame(Canvas, FButtonStyle, Color,
          not ((Tmp in [0..FImageList.Count - 1]) and
          (odFocused in State) and not (odComboBoxEdit in State)), TmpR);
      end;
      Inc(R.Left, GetImageWidth(Index) + 8);
      OrigR.Left := R.Left;
    end;

    R.Right := R.Left + TextWidth(Items[Index].Text);
    InflateRect(R, 2, -1);
    if Length(Items[Index].Text) > 0 then
    begin
      Inc(R.Right,2);
      if FullWidthItemDraw then
        FillRect(OrigR)
      else
        FillRect(R);
      Inc(R.Left, 2);
      DrawText(Canvas, Items[Index].Text, Length(Items[Index].Text), R,
        DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
      Dec(R.Left, 2);
      if (odSelected in State) and (Color <> FColorHighlight) then
      begin
        if FullWidthItemDraw then
          DrawFocusRect(OrigR)
        else
          DrawFocusRect(R);
      end;
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvImageComboBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := Max(GetItemHeight(Font) + 4, GetImageHeight(Index) + (Ord(ButtonFrame) * 4));
//  if Assigned(FImageList) then
//    Height := Max(Height,FImageList.Height);
end;

procedure TJvImageComboBox.SetColorHighlight(Value: TColor);
begin
  if FColorHighlight <> Value then
  begin
    FColorHighlight := Value;
    Invalidate;
  end;
end;

procedure TJvImageComboBox.SetColorHighlightText(Value: TColor);
begin
  if FColorHighlightText <> Value then
  begin
    FColorHighlightText := Value;
    Invalidate;
  end;
end;

function TJvImageComboBox.GetDroppedWidth: Integer;
begin
  HandleNeeded;
  Result := SendMessage(Handle, CB_GETDROPPEDWIDTH, 0, 0);
end;

procedure TJvImageComboBox.SetDroppedWidth(Value: Integer);
begin
  HandleNeeded;
  FDroppedWidth := SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
end;

procedure TJvImageComboBox.FontChanged;
begin
  inherited FontChanged;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvImageComboBox.ResetItemHeight;
var
  MaxImageHeight, H: Integer;
  I: Integer;
begin
  MaxImageHeight := GetImageHeight(-1);
  for I := 0 to FItems.Count - 1 do
  begin
    H := GetImageHeight(I);
    if H > MaxImageHeight then
      MaxImageHeight := H;
  end;
  H := Max(GetItemHeight(Font) + 4, MaxImageHeight + Ord(ButtonFrame) * 4);
  if ItemHeight <> H then
    ItemHeight := H; // SetItemHeight does a RecreateWnd without checking if the value has changed
end;

procedure TJvImageComboBox.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvImageComboBox.CNCommand(var Msg: TWMCommand);
begin
  inherited;
  // If OnSelect is Assigned, OnChange is not triggered
  // so we do it ourselves. But to avoid triggering OnChange twice (Mantis 3175)
  // for the same change of Item, we only do it if OnSelect is Assigned.
  case Msg.NotifyCode of
    CBN_SELCHANGE:
      if Assigned(OnSelect) then
        Change;
  end;
end;

procedure TJvImageComboBox.EnabledChanged;
const
  EnableColors: array [Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited EnabledChanged;
  Color := EnableColors[Enabled];
end;

procedure TJvImageComboBox.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  FMouseInControl := True;
end;

procedure TJvImageComboBox.MouseLeave(AControl: TControl);
begin
  FMouseInControl := False;
  inherited MouseLeave(AControl);
end;

procedure TJvImageComboBox.SetDefaultIndent(const Value: Integer);
begin
  if FDefaultIndent <> Value then
  begin
    FDefaultIndent := Value;
    Invalidate;
  end;
end;

procedure TJvImageComboBox.SetItems(const Value: TJvImageItems);
begin
  FItems.Assign(Value);
  FItems.Update(nil);
end;

procedure TJvImageComboBox.SetSorted(const Value: Boolean);
begin
  if Sorted <> Value then
  begin
    inherited Sorted := Value;
    if Value then
      FItems.UpdateItemIndices; // all indices have changed, we must apply the changes to the Items
  end;
end;

procedure TJvImageComboBox.SetIndentSelected(const Value: Boolean);
begin
  if FIndentSelected <> Value then
  begin
    FIndentSelected := Value;
    Invalidate;
  end;
end;

function TJvImageComboBox.GetImageWidth(Index: Integer): Integer;
begin
  if (Index > -1) and not Items[Index].Glyph.Empty then
    Result := Items[Index].Glyph.Width
  else
  if Assigned(FImageList) then
    Result := FImageList.Width
  else
    Result := FImageWidth;
end;

function TJvImageComboBox.GetSorted: Boolean;
begin
  Result := inherited Sorted;
end;

function TJvImageComboBox.GetImageHeight(Index: Integer): Integer;
begin
  if (Index > -1) and not Items[Index].Glyph.Empty then
    Result := Items[Index].Glyph.Height
  else
  if Assigned(FImageList) then
    Result := FImageList.Height
  else
    Result := FImageHeight;
end;

//=== { TJvImageListBox } ====================================================

constructor TJvImageListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // ControlStyle := ControlStyle + [csAcceptsControls];
  SetBounds(0, 0, 121, 97);
  FItems := TJvImageItems.Create(Self);
  FItems.FStrings := inherited Items;
  Color := clWindow;
  FColorHighlight := clHighlight;
  FColorHighlightText := clHighlightText;
  FImageWidth := 0;
  FImageHeight := 0;
  FAlignment := taLeftJustify;

  FButtonFrame := False;
  Style := lbOwnerDrawFixed;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  ResetItemHeight;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImageListChange;

  FFullWidthItemDraw := False;
end;

destructor TJvImageListBox.Destroy;
begin
  FItems.Free;
  FChangeLink.Free;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

function TJvImageListBox.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvImageListBox.ImageListChange;
begin
  //  Invalidate;
end;

procedure TJvImageListBox.SetImageList(Value: TCustomImageList);
begin
  if ReplaceImageListReference(Self, Value, FImageList, FChangeLink) then
  begin
    ResetItemHeight;
    RecreateWnd;
  end;
end;

procedure TJvImageListBox.SetFullWidthItemDraw(const Value: Boolean);
begin
  if Value <> FFullWidthItemDraw then
  begin
    FFullWidthItemDraw := Value;
    Invalidate;
  end;
end;

procedure TJvImageListBox.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    ResetItemHeight;
  end;
end;

procedure TJvImageListBox.CreateWnd;
begin
  inherited CreateWnd;
  Items.FillItems;
  SetBkMode(FCanvas.Handle, TRANSPARENT);
end;

procedure TJvImageListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;
end;

procedure TJvImageListBox.SetColorHighlight(Value: TColor);
begin
  if FColorHighlight <> Value then
  begin
    FColorHighlight := Value;
    Invalidate;
  end;
end;

procedure TJvImageListBox.SetColorHighlightText(Value: TColor);
begin
  if FColorHighlightText <> Value then
  begin
    FColorHighlightText := Value;
    Invalidate;
  end;
end;

procedure TJvImageListBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  if csDestroying in ComponentState then
    Exit;
  with Msg.DrawItemStruct^ do
  begin
    State := [];
    if (itemState and ODS_CHECKED) <> 0 then
      Include(State, odChecked);
    if (itemState and ODS_COMBOBOXEDIT) <> 0 then
      Include(State, odComboBoxEdit);
    if (itemState and ODS_DEFAULT) <> 0 then
      Include(State, odDefault);
    if (itemState and ODS_DISABLED) <> 0 then
      Include(State, odDisabled);
    if (itemState and ODS_FOCUS) <> 0 then
      Include(State, odFocused);
    if (itemState and ODS_GRAYED) <> 0 then
      Include(State, odGrayed);
    if (itemState and ODS_SELECTED) <> 0 then
      Include(State, odSelected);
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;

    if (Integer(itemID) >= 0) then
    begin
      if Items[Integer(itemID)].Brush.Style <> bsClear then
      begin
        FCanvas.Brush := Items[Integer(itemID)].Brush;
        FCanvas.FillRect(rcItem);
      end;

      if (odSelected in State) then
      begin
        FCanvas.Brush.Color := Items[Integer(itemID)].ColorHighlight;
        FCanvas.Font.Color := Items[Integer(itemID)].ColorHighlightText;
      end;

      DrawItem(itemID, rcItem, State)
    end
    else
    begin
      FCanvas.FillRect(rcItem);
    end;

    FCanvas.Handle := 0;
  end;
end;

procedure TJvImageListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  SavedColor: TColor;
begin
  if csDestroying in ComponentState then
    Exit;
  SavedColor := Canvas.Font.Color;
  Canvas.Font.Assign(Items[Index].Font);

  if State <> [] then
    Canvas.Font.Color := SavedColor;
  case FAlignment of
    taLeftJustify:
      DrawLeftGlyph(Index, Rect, State);
    taRightJustify:
      DrawRightGlyph(Index, Rect, State);
    taCenter:
      DrawCenteredGlyph(Index, Rect, State);
  end;
end;

procedure TJvImageListBox.DrawCenteredGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
var
  Tmp, Tmp2: Integer;
  TmpCol: TColor;
  TmpR, OrigR: TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  OrigR := R;
  with Canvas do
  begin
    if not FullWidthItemDraw then
    begin
      TmpCol := Brush.Color;
      Brush.Color := Color;
      FillRect(R);
      Brush.Color := TmpCol;
    end;

    if not Items[Index].Glyph.Empty then
    begin
      Tmp := ((R.Right - R.Left) - GetImageWidth(Index)) div 2;

      Draw(R.Left + Tmp, R.Top + 2, Items[Index].Glyph);

      if FButtonFrame then
      begin
        TmpR := Rect(R.Left + Tmp - 2, R.Top + 2,
          R.Left + Tmp + FImageList.Width + 2, R.Top + FImageList.Height + 2);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not (odSelected in State), TmpR);
      end;
      InflateRect(R, 1, -4);
    end
    else
    if Assigned(FImageList) then
    begin
      Tmp := ((R.Right - R.Left) - GetImageWidth(Index)) div 2;
      Tmp2 := Items[Index].ImageIndex;
      // PRY 2002.06.04
      //FImageList.Draw(FCanvas, R.Left + Tmp, R.Top + 2, Tmp2, dsTransparent, itImage);
      FImageList.Draw(Canvas, R.Left + Tmp, R.Top + 2, Tmp2, dsTransparent, itImage);
      // PRY END
      if FButtonFrame then
      begin
        TmpR := Rect(R.Left + Tmp - 2, R.Top + 2,
          R.Left + Tmp + FImageList.Width + 2, R.Top + FImageList.Height + 2);
        DrawBtnFrame(Canvas, FButtonStyle, Color,
          not ((Tmp2 in [0..FImageList.Count - 1]) and (odSelected in State)), TmpR);
      end;
      InflateRect(R, 1, -4);
    end;
    R.Left := ((R.Right - R.Left) - TextWidth(Items[Index].Text)) div 2 - 1;
    R.Right := R.Left + TextWidth(Items[Index].Text) + 1;
    R.Top := R.Bottom - TextHeight(Items[Index].Text) - 1;
    if Length(Items[Index].Text) > 0 then
    begin
      if FullWidthItemDraw then
        FillRect(OrigR)
      else
        FillRect(R);

      DrawText(Canvas, Items[Index].Text, Length(Items[Index].Text), R,
        DT_SINGLELINE or DT_NOPREFIX or DT_CENTER or DT_BOTTOM);

      if (odSelected in State) and (Color <> FColorHighlight) then
      begin
        if FullWidthItemDraw then
          DrawFocusRect(OrigR)
        else
          DrawFocusRect(R);
      end;
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvImageListBox.DrawLeftGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
var
  Offset, Tmp: Integer;
  TmpCol: TColor;
  TmpR, OrigR: TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  OrigR := R;
  with Canvas do
  begin
    if not FullWidthItemDraw then
    begin
      TmpCol := Brush.Color;
      Brush.Color := Color;
      FillRect(R);
      Brush.Color := TmpCol;
    end;

    if not Items[Index].Glyph.Empty then
    begin
      Offset := ((R.Bottom - R.Top) - GetImageHeight(Index)) div 2;

      Draw(R.Left + 2, R.Top + Offset, Items[Index].Glyph);

      if FButtonFrame then
      begin
        TmpR := Rect(R.Left, R.Top, R.Left + FImageList.Width + 4, R.Top + FImageList.Height + 4);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not (odSelected in State), TmpR);
      end;

      Inc(R.Left, GetImageWidth(Index) + 8);
      OrigR.Left := R.Left;
    end
    else
    if Assigned(FImageList) then
    begin
      Offset := ((R.Bottom - R.Top) - GetImageHeight(Index)) div 2;
      Tmp := Items[Index].ImageIndex;
      // PRY 2002.06.04
      //FImageList.Draw(FCanvas, R.Left + 2, R.Top + Offset, Tmp, dsTransparent, itImage);
      FImageList.Draw(Canvas, R.Left + 2, R.Top + Offset, Tmp, dsTransparent, itImage);
      // PRY END
      if FButtonFrame then
      begin
        TmpR := Rect(R.Left, R.Top, R.Left + FImageList.Width + 4, R.Top + FImageList.Height + 4);
        DrawBtnFrame(Canvas, FButtonStyle, Color,
          not ((Tmp in [0..FImageList.Count - 1]) and (odSelected in State)), TmpR);
      end;
      Inc(R.Left, GetImageWidth(Index) + 8);
      OrigR.Left := R.Left;
    end;

    R.Right := R.Left + TextWidth(Items[Index].Text);
    InflateRect(R, 2, -1);
    if Length(Items[Index].Text) > 0 then
    begin
      Inc(R.Right, 2);
      if FullWidthItemDraw then
        FillRect(OrigR)
      else
        FillRect(R);
      Inc(R.Left, 2);
      DrawText(Canvas, Items[Index].Text, Length(Items[Index].Text), R,
        DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
      Dec(R.Left, 2);
      if (odSelected in State) and (Color <> FColorHighlight) then
      begin
        if FullWidthItemDraw then
          DrawFocusRect(OrigR)
        else
          DrawFocusRect(R);
      end;
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvImageListBox.DrawRightGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
var
  Offset, Tmp: Integer;
  TmpCol: TColor;
  TmpR, OrigR: TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  OrigR := R;
  with Canvas do
  begin
    if not FullWidthItemDraw then
    begin
      TmpCol := Brush.Color;
      Brush.Color := Color;
      FillRect(R);
      Brush.Color := TmpCol;
    end;

    if not Items[Index].Glyph.Empty then
    begin
      Offset := ((R.Bottom - R.Top) - GetImageHeight(Index)) div 2;

      Draw(R.Right - (GetImageWidth(Index) + 2), R.Top + Offset, Items[Index].Glyph);

      if FButtonFrame then
      begin
        TmpR := Rect(R.Right - (FImageList.Width + 2) - 2,
          R.Top + Offset - 2, R.Right - 2, R.Top + Offset + FImageList.Height + 2);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not (odSelected in State), TmpR);
      end;

      Dec(R.Right, FImageList.Width + 4);
      OrigR.Right := R.Right;
    end
    else
    if Assigned(FImageList) then
    begin
      Tmp := Items[Index].ImageIndex;

      Offset := ((R.Bottom - R.Top) - GetImageHeight(Index)) div 2;
      // PRY 2002.06.04
      //FImageList.Draw(FCanvas, R.Right - (FWidth + 2), R.Top + Offset, Tmp, dsTransparent, itImage);
      FImageList.Draw(Canvas, R.Right - (GetImageWidth(Index) + 2), R.Top + Offset, Tmp, dsTransparent, itImage);
      // PRY END
      if FButtonFrame then
      begin
        TmpR := Rect(R.Right - (FImageList.Width + 2) - 2,
          R.Top + Offset - 2, R.Right - 2, R.Top + Offset + FImageList.Height + 2);
        DrawBtnFrame(Canvas, FButtonStyle, Color,
          not ((Tmp in [0..FImageList.Count - 1]) and (odSelected in State)), TmpR);
      end;
      Dec(R.Right, FImageList.Width + 4);
      OrigR.Right := R.Right;
    end;

    R.Left := R.Right - TextWidth(Items[Index].Text);
    //    R.Right := R.Left + TextWidth(Items[Index].Text);
    InflateRect(R, 2, -1);
    if Length(Items[Index].Text) > 0 then
    begin
      Dec(R.Right, 2);
      if FullWidthItemDraw then
        FillRect(OrigR)
      else
        FillRect(R);
      DrawText(Canvas, Items[Index].Text, Length(Items[Index].Text), R,
        DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or DT_RIGHT);
      Inc(R.Right, 2);
      if (odSelected in State) and (Color <> FColorHighlight) then
      begin
        if FullWidthItemDraw then
          DrawFocusRect(OrigR)
        else
          DrawFocusRect(R);
      end;
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvImageListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := Max(GetItemHeight(Font) + 4, GetImageHeight(Index) + Ord(ButtonFrame) * 4);
end;

procedure TJvImageListBox.FontChanged;
begin
  inherited FontChanged;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvImageListBox.ResetItemHeight;
var
  MaxImageHeight: Integer;
  I: Integer;
begin
  MaxImageHeight := GetImageHeight(-1);
  for I := 0 to FItems.Count - 1 do
  begin
    if GetImageHeight(I) > MaxImageHeight then
      MaxImageHeight := GetImageHeight(I);
  end;
  case FAlignment of
    taLeftJustify, taRightJustify:
      ItemHeight := Max(ItemHeight, Max(GetItemHeight(Font) + 4, MaxImageHeight + Ord(ButtonFrame) * 4));
    taCenter:
      ItemHeight := Max(ItemHeight, Max(GetItemHeight(Font) + 4, MaxImageHeight + Ord(ButtonFrame) * 8));
  end;
  Invalidate;
end;

procedure TJvImageListBox.Resize;
begin
  inherited Resize;
  Invalidate;
end;

procedure TJvImageListBox.SetItems(const Value: TJvImageItems);
begin
  FItems.Assign(Value);
  FItems.Update(nil);
end;

function TJvImageListBox.GetImageWidth(Index: Integer): Integer;
begin
  if (Index > -1) and not Items[Index].Glyph.Empty then
    Result := Items[Index].Glyph.Width
  else
  if Assigned(FImageList) then
    Result := FImageList.Width
  else
    Result := FImageWidth;
end;

function TJvImageListBox.GetImageHeight(Index: Integer): Integer;
begin
  if (Index > -1) and not Items[Index].Glyph.Empty then
    Result := Items[Index].Glyph.Height
  else
  if Assigned(FImageList) then
    Result := FImageList.Height
  else
    Result := FImageHeight;
end;

procedure TJvImageItem.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TJvImageItem.SetFont(const Value: TFont);
begin
  if not (puFont in FListPropertiesUsed) then
    Font.Assign(Value);
end;

function TJvImageItem.GetFont: TFont;
begin
  if puFont in FListPropertiesUsed then
  begin
    Result := TWinControlAccessProtected(GetWinControl).Font
  end
  else
  begin
    if not Assigned(FFont) then
    begin
      FFont := TFont.Create;
      FFont.OnChange := FontChange;
      FFont.Assign(TWinControlAccessProtected(GetWinControl).Font);
    end;
    Result := FFont;
  end;
end;

function TJvImageItem.GetGlyph: TBitmap;
begin
  Result := FGlyph;
end;

procedure TJvImageItem.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
  if GetWinControl <> nil then
    GetWinControl.Invalidate;
end;

procedure TJvImageItem.FontChange(Sender: TObject);
begin
  if not (puFont in FListPropertiesUsed) then
    if GetWinControl <> nil then
      GetWinControl.Invalidate;
end;

function TJvImageItem.GetWinControl: TWinControl;
begin
  Result := nil;
  if Assigned(Collection) then
    Result := TWinControl(TJvImageItems(Collection).GetOwner);
end;

function TJvImageItem.GetColorHighlight: TColor;
begin
  if (puColorHighlight in FListPropertiesUsed) and (GetWinControl <> nil) then
  begin
    if GetWinControl is TJvImageListBox then
      Result := TJvImageListBox(GetWinControl).ColorHighlight
    else
      Result := TJvImageComboBox(GetWinControl).ColorHighlight;
  end
  else
    Result := FColorHighlight;
end;

function TJvImageItem.GetColorHighlightText: TColor;
begin
  if (puColorHighlightText in FListPropertiesUsed) and (GetWinControl <> nil) then
  begin
    if GetWinControl is TJvImageListBox then
      Result := TJvImageListBox(GetWinControl).ColorHighlightText
    else
      Result := TJvImageComboBox(GetWinControl).ColorHighlightText;
  end
  else
    Result := FColorHighlightText;
end;

procedure TJvImageItem.SetColorHighlight(const Value: TColor);
begin
  if (puColorHighlight in FListPropertiesUsed) and (GetWinControl <> nil) then
  begin
    if GetWinControl is TJvImageListBox then
      TJvImageListBox(GetWinControl).ColorHighlight := Value
    else
      TJvImageComboBox(GetWinControl).ColorHighlight := Value;
  end
  else
    FColorHighlight := Value;
end;

procedure TJvImageItem.SetColorHighlightText(const Value: TColor);
begin
  if (puColorHighlightText in FListPropertiesUsed) and (GetWinControl <> nil) then
  begin
    if GetWinControl is TJvImageListBox then
      TJvImageListBox(GetWinControl).ColorHighlightText := Value
    else
      TJvImageComboBox(GetWinControl).ColorHighlightText := Value
  end
  else
    FColorHighlightText := Value;
end;

function TJvImageItem.IsColorHighlightTextStored: Boolean;
begin
  Result := not (puColorHighlightText in FListPropertiesUsed);
end;

function TJvImageItem.IsFontStored: Boolean;
begin
  Result := not (puFont in FListPropertiesUsed);
end;

function TJvImageItem.IsColorHighlightStored: Boolean;
begin
  Result := not (puColorHighlight in FListPropertiesUsed);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

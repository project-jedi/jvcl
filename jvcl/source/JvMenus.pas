{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMenus.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2003-04-21

Contributors: Olivier Sannier (2003-04-21) - Added DisabledImages
                                             Added HotImages
                                             Added StandardIsOffice property
                                               to allow using Office style
                                               buttons when specifying
                                               msStandard style

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMenus;

interface

uses{$IFDEF WIN32}Windows, {$ELSE}WinTypes, WinProcs, {$ENDIF}SysUtils,
  Classes, Controls, Messages, Graphics, {$IFDEF COMPILER4_UP}ImgList, {$ENDIF}
  Menus, JvHook, JvWndProcHook, JVCLVer;

type
  TJvMenuStyle = (msStandard, msOwnerDraw{$IFDEF WIN32}, msBtnLowered,
    msBtnRaised{$ENDIF});
  TMenuOwnerDrawState = set of (mdSelected, mdGrayed, mdDisabled, mdChecked,
    mdFocused{$IFDEF WIN32}, mdDefault{$ENDIF});

  TDrawMenuItemEvent = procedure(Sender: TMenu; Item: TMenuItem; Rect: TRect;
    State: TMenuOwnerDrawState) of object;
  TMeasureMenuItemEvent = procedure(Sender: TMenu; Item: TMenuItem; var Width,
    Height: Integer) of object;
  TDrawMarginEvent = procedure(Sender: TMenu; Rect: TRect) of object;
  TItemParamsEvent = procedure(Sender: TMenu; Item: TMenuItem;
    State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
    var Graphic: TGraphic; var NumGlyphs: Integer) of object;
  {$IFDEF WIN32}
  TItemImageEvent = procedure(Sender: TMenu; Item: TMenuItem;
    State: TMenuOwnerDrawState; var ImageIndex: Integer) of object;
  {$ENDIF}

  { TJvMainMenu }

  TJvMainMenu = class(TMainMenu)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FStyle: TJvMenuStyle;
    FCanvas: TCanvas;
    FHook: TJvWindowHook;
    FShowCheckMarks: Boolean;
    FMinTextOffset: Cardinal;
    FCursor: TCursor;
    FOnDrawItem: TDrawMenuItemEvent;
    FOnMeasureItem: TMeasureMenuItemEvent;
    FOnGetItemParams: TItemParamsEvent;
    {$IFDEF WIN32}
    FStandardIsOffice: Boolean;

    FImages: TImageList;
    FImageChangeLink: TChangeLink;
    FOnGetImageIndex: TItemImageEvent;

    FDisabledImages: TImageList;
    FDisabledImageChangeLink: TChangeLink;
    FOnGetDisabledImageIndex: TItemImageEvent;

    FHotImages: TImageList;
    FHotImageChangeLink: TChangeLink;
    FOnGetHotImageIndex: TItemImageEvent;

    procedure SetStandardIsOffice(Value: Boolean);

    procedure SetImages(Value: TImageList);
    procedure ImageListChange(Sender: TObject);

    procedure SetDisabledImages(Value: TImageList);
    procedure DisabledImageListChange(Sender: TObject);

    procedure SetHotImages(Value: TImageList);
    procedure HotImageListChange(Sender: TObject);
    {$ENDIF}
    procedure SetStyle(Value: TJvMenuStyle);
    function FindForm: TWinControl;
    procedure WndMessage(Sender: TObject; var AMsg: TMessage;
      var Handled: Boolean);
    procedure CMMenuChanged(var Message: TMessage); message CM_MENUCHANGED;
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMMeasureItem(var Message: TWMMeasureItem); message WM_MEASUREITEM;
    procedure WMMenuSelect(var Message: TWMMenuSelect); message WM_MENUSELECT;
  protected
    procedure Loaded; override;
    {$IFDEF WIN32}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState;
      var ImageIndex: Integer); dynamic;
    {$ENDIF}
    procedure DrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState); virtual;
    procedure GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState;
      AFont: TFont; var Color: TColor; var Graphic: TGraphic;
      var NumGlyphs: Integer); dynamic;
    procedure MeasureItem(Item: TMenuItem; var Width, Height: Integer); dynamic;
    procedure RefreshMenu(AOwnerDraw: Boolean); virtual;
    function IsOwnerDrawMenu: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure DefaultDrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState);
    property Canvas: TCanvas read FCanvas;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property MinTextOffset: Cardinal read FMinTextOffset write FMinTextOffset default 0;
    property Style: TJvMenuStyle read FStyle write SetStyle default msStandard;
    property ShowCheckMarks: Boolean read FShowCheckMarks write FShowCheckMarks default True;
    {$IFDEF COMPILER4_UP}
    property OwnerDraw stored False;
    {$ENDIF}
    {$IFDEF WIN32}
    property StandardIsOffice: Boolean read FStandardIsOffice write SetStandardIsOffice default true;

    property Images: TImageList read FImages write SetImages;
    property OnGetImageIndex: TItemImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property OnGetDisabledImageIndex: TItemImageEvent read FOnGetDisabledImageIndex write FOnGetDisabledImageIndex;
    property HotImages: TImageList read FHotImages write SetHotImages;
    property OnGetHotImageIndex: TItemImageEvent read FOnGetHotImageIndex write FOnGetHotImageIndex;
    {$ENDIF}
    property OnDrawItem: TDrawMenuItemEvent read FOnDrawItem write FOnDrawItem;
    property OnGetItemParams: TItemParamsEvent read FOnGetItemParams write FOnGetItemParams;
    property OnMeasureItem: TMeasureMenuItemEvent read FOnMeasureItem write FOnMeasureItem;
  end;

  { TJvPopupMenu }

  TJvPopupMenu = class(TPopupMenu)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FStyle: TJvMenuStyle;
    FCanvas: TCanvas;
    FShowCheckMarks: Boolean;
    FMinTextOffset: Cardinal;
    FLeftMargin: Cardinal;
    FCursor: TCursor;
    FOnDrawItem: TDrawMenuItemEvent;
    FOnMeasureItem: TMeasureMenuItemEvent;
    FOnDrawMargin: TDrawMarginEvent;
    FOnGetItemParams: TItemParamsEvent;
    {$IFDEF COMPILER4_UP}
    FPopupPoint: TPoint;
    FParentBiDiMode: Boolean;
    {$ENDIF}
    {$IFDEF WIN32}
    FStandardIsOffice: Boolean;

    FImages: TImageList;
    FImageChangeLink: TChangeLink;
    FOnGetImageIndex: TItemImageEvent;

    FDisabledImages: TImageList;
    FDisabledImageChangeLink: TChangeLink;
    FOnGetDisabledImageIndex: TItemImageEvent;

    FHotImages: TImageList;
    FHotImageChangeLink: TChangeLink;
    FOnGetHotImageIndex: TItemImageEvent;

    procedure SetStandardIsOffice(Value: Boolean);

    procedure SetImages(Value: TImageList);
    procedure ImageListChange(Sender: TObject);

    procedure SetDisabledImages(Value: TImageList);
    procedure DisabledImageListChange(Sender: TObject);

    procedure SetHotImages(Value: TImageList);
    procedure HotImageListChange(Sender: TObject);
    {$ENDIF}
    procedure SetStyle(Value: TJvMenuStyle);
    procedure WndMessage(Sender: TObject; var AMsg: TMessage;
      var Handled: Boolean);
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMMeasureItem(var Message: TWMMeasureItem); message WM_MEASUREITEM;
    {$IFDEF COMPILER4_UP}
    procedure SetBiDiModeFromPopupControl;
    {$ENDIF}
  protected
    procedure Loaded; override;
    {$IFDEF COMPILER4_UP}
    function UseRightToLeftAlignment: Boolean;
    {$ENDIF}
    {$IFDEF WIN32}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState;
      var ImageIndex: Integer); dynamic;
    {$ENDIF}
    procedure DrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState); virtual;
    procedure DrawMargin(ARect: TRect); virtual;
    procedure GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState;
      AFont: TFont; var Color: TColor; var Graphic: TGraphic;
      var NumGlyphs: Integer); dynamic;
    procedure MeasureItem(Item: TMenuItem; var Width, Height: Integer); dynamic;
    procedure RefreshMenu(AOwnerDraw: Boolean); virtual;
    function IsOwnerDrawMenu: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure Popup(X, Y: Integer); override;
    procedure DefaultDrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState);
    procedure DefaultDrawMargin(ARect: TRect; StartColor, EndColor: TColor);
    property Canvas: TCanvas read FCanvas;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property LeftMargin: Cardinal read FLeftMargin write FLeftMargin default 0;
    property MinTextOffset: Cardinal read FMinTextOffset write FMinTextOffset default 0;
    property Style: TJvMenuStyle read FStyle write SetStyle default msStandard;
    property ShowCheckMarks: Boolean read FShowCheckMarks write FShowCheckMarks default True;
    {$IFDEF COMPILER4_UP}
    property OwnerDraw stored False;
    {$ENDIF}
    {$IFDEF WIN32}
    property StandardIsOffice: Boolean read FStandardIsOffice write SetStandardIsOffice default true;

    property Images: TImageList read FImages write SetImages;
    property OnGetImageIndex: TItemImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property OnGetDisabledImageIndex: TItemImageEvent read FOnGetDisabledImageIndex write FOnGetDisabledImageIndex;
    property HotImages: TImageList read FHotImages write SetHotImages;
    property OnGetHotImageIndex: TItemImageEvent read FOnGetHotImageIndex write FOnGetHotImageIndex;
    {$ENDIF}
    property OnDrawItem: TDrawMenuItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDrawMargin: TDrawMarginEvent read FOnDrawMargin write FOnDrawMargin;
    property OnGetItemParams: TItemParamsEvent read FOnGetItemParams write FOnGetItemParams;
    property OnMeasureItem: TMeasureMenuItemEvent read FOnMeasureItem write FOnMeasureItem;
  end;

  { Utility routines }

procedure SetDefaultMenuFont(AFont: TFont);
function IsItemPopup(Item: TMenuItem): Boolean;

implementation

uses{$IFDEF WIN32}CommCtrl, {$ENDIF}Forms, ExtCtrls, Consts, JvConst,
  JvVCLUtils, JvClipIcon, JvStrUtils, Math;

const
  DefMarginColor: TColor = clBlue;
  AddWidth = 2;
  AddHeight = 4;
  Tab = #9#9;
  Separator = '-';

type
  TBtnStyle = (bsNone, bsLowered, bsRaised, bsOffice);

function BtnStyle(MenuStyle: TJvMenuStyle; StandardIsOffice: boolean): TBtnStyle;
begin
  {$IFDEF WIN32}
  case MenuStyle of
    msBtnLowered: Result := bsLowered;
    msBtnRaised: Result := bsRaised;
  else
    begin
      if StandardIsOffice then
      begin
        Result := bsOffice;
      end
      else
      begin
        Result := bsNone;
      end;
    end;
  end;
  {$ELSE}
  Result := bsNone;
  {$ENDIF}
end;

function IsItemPopup(Item: TMenuItem): Boolean;
begin
  Result := (Item.Parent = nil) or (Item.Parent.Parent <> nil) or
    not (Item.Parent.Owner is TMainMenu);
end;

{$IFNDEF WIN32}
const
  { return codes for WM_MENUCHAR (not defined in Delphi 1.0) }
  MNC_IGNORE = 0;
  MNC_CLOSE = 1;
  MNC_EXECUTE = 2;
  MNC_SELECT = 3;
  {$ENDIF}

  {$IFNDEF COMPILER4_UP}

procedure ProcessMenuChar(AMenu: TMenu; var Message: TWMMenuChar);
var
  C, I, First, Hilite, Next: Integer;
  State: Word;

  function IsAccelChar(Menu: HMENU; State: Word; I: Integer; C: Char): Boolean;
  var
    Item: TMenuItem;
    Id: Cardinal;
  begin
    Item := nil;
    if State and MF_POPUP <> 0 then
    begin
      Menu := GetSubMenu(Menu, I);
      Item := AMenu.FindItem(Menu, fkHandle);
    end
    else
    begin
      Id := GetMenuItemID(Menu, I);
      if Id <> {$IFDEF WIN32}$FFFFFFFF{$ELSE}$FFFF{$ENDIF} then
        Item := AMenu.FindItem(Id, fkCommand);
    end;
    if Item <> nil then
      Result := IsAccel(Ord(C), Item.Caption)
    else
      Result := False;
  end;

  function IsInitialChar(Menu: HMENU; State: Word; I: Integer; C: Char): Boolean;
  var
    Item: TMenuItem;
  begin
    if State and MF_POPUP <> 0 then
    begin
      Menu := GetSubMenu(Menu, I);
      Item := AMenu.FindItem(Menu, fkHandle);
    end
    else
    begin
      Item := AMenu.FindItem(Menu, fkHandle);
      if Item <> nil then Item := Item.Items[I];
    end;
    if (Item <> nil) and (Item.Caption <> '') then
      Result := AnsiCompareText(Item.Caption[1], C) = 0
    else
      Result := False;
  end;

begin
  with Message do
  begin
    Result := MNC_IGNORE; { No item found: beep }
    First := -1;
    Hilite := -1;
    Next := -1;
    C := GetMenuItemCount(Menu);
    for I := 0 to C - 1 do
    begin
      State := GetMenuState(Menu, I, MF_BYPOSITION);
      if IsAccelChar(Menu, State, I, User) then
      begin
        if State and MF_DISABLED <> 0 then
        begin
          { Close the menu if this is the only disabled item to choose from.
            Otherwise, ignore the item. }
          if First < 0 then First := -2;
          Continue;
        end;
        if First < 0 then
        begin
          First := I;
          Result := MNC_EXECUTE;
        end
        else
          Result := MNC_SELECT;
        if State and MF_HILITE <> 0 then
          Hilite := I
        else if Hilite >= 0 then
          Next := I;
      end;
    end;
    { We found a single disabled item. End the selection. }
    if First < -1 then
    begin
      Result := MNC_CLOSE shl 16;
      Exit;
    end;

    { If we can't find accelerators, then look for initial letters }
    if First < 0 then
      for I := 0 to C - 1 do
      begin
        State := GetMenuState(Menu, I, MF_BYPOSITION);
        if IsInitialChar(Menu, State, I, User) then
        begin
          if State and MF_DISABLED <> 0 then
          begin
            Result := MNC_CLOSE shl 16;
            Exit;
          end;
          if First < 0 then
          begin
            First := I;
            Result := MNC_EXECUTE;
          end
          else
            Result := MNC_SELECT;
          if State and MF_HILITE <> 0 then
            Hilite := I
          else if Hilite >= 0 then
            Next := I;
        end;
      end;

    if (Result = MNC_EXECUTE) then
      Result := Result shl 16 or First
    else if Result = MNC_SELECT then
    begin
      if Next < 0 then Next := First;
      Result := Result shl 16 or Next;
    end;
  end;
end;
{$ENDIF COMPILER4_UP}

procedure MenuWndMessage(Menu: TMenu; var AMsg: TMessage; var Handled: Boolean);
var
  Message: TMessage;
  Item: Pointer;
begin
  with AMsg do
    case Msg of
      WM_MEASUREITEM:
        if (TWMMeasureItem(AMsg).MeasureItemStruct^.CtlType = ODT_MENU) then
        begin
          Item := Menu.FindItem(TWMMeasureItem(AMsg).MeasureItemStruct^.itemID, fkCommand);
          if Item <> nil then
          begin
            Message := AMsg;
            TWMMeasureItem(Message).MeasureItemStruct^.ItemData := Longint(Item);
            Menu.Dispatch(Message);
            Result := 1;
            Handled := True;
          end;
        end;
      WM_DRAWITEM:
        if (TWMDrawItem(AMsg).DrawItemStruct^.CtlType = ODT_MENU) then
        begin
          Item := Menu.FindItem(TWMDrawItem(AMsg).DrawItemStruct^.itemID, fkCommand);
          if Item <> nil then
          begin
            Message := AMsg;
            TWMDrawItem(Message).DrawItemStruct^.ItemData := Longint(Item);
            Menu.Dispatch(Message);
            Result := 1;
            Handled := True;
          end;
        end;
      WM_MENUSELECT: Menu.Dispatch(AMsg);
      CM_MENUCHANGED: Menu.Dispatch(AMsg);
      WM_MENUCHAR:
        begin
          {$IFDEF COMPILER4_UP}
          Menu.ProcessMenuChar(TWMMenuChar(AMsg));
          {$ELSE}
          ProcessMenuChar(Menu, TWMMenuChar(AMsg));
          {$ENDIF}
        end;
    end;
end;

{$IFNDEF COMPILER4_UP}

procedure RefreshMenuItem(MenuItem: TMenuItem; OwnerDraw: Boolean);
const
  Breaks: array[TMenuBreak] of Longint = (0, MF_MENUBREAK, MF_MENUBARBREAK);
  Checks: array[Boolean] of LongInt = (MF_UNCHECKED, MF_CHECKED);
  Enables: array[Boolean] of LongInt = (MF_DISABLED or MF_GRAYED, MF_ENABLED);
  Separators: array[Boolean] of LongInt = (MF_STRING, MF_SEPARATOR);
  {$IFDEF WIN32}
  IBreaks: array[TMenuBreak] of DWORD = (MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK);
  IRadios: array[Boolean] of DWORD = (MFT_STRING, MFT_RADIOCHECK);
  ISeparators: array[Boolean] of DWORD = (MFT_STRING, MFT_SEPARATOR);
  IOwnerDraw: array[Boolean] of DWORD = (MFT_STRING, MFT_OWNERDRAW);
  {$ENDIF}
var
  {$IFDEF WIN32}
  MenuItemInfo: TMenuItemInfo;
  {$ENDIF}
  CCaption: array[0..255] of Char;
  NewFlags: Integer;
  ItemID, I, C: Integer;
  MenuHandle: THandle;
  Item: TMenuItem;

  {$IFDEF WIN32}

  procedure PrepareItemInfo;
  begin
    FillChar(MenuItemInfo, SizeOf(TMenuItemInfo), 0);
    with MenuItemInfo do
    begin
      cbSize := SizeOf(TMenuItemInfo);
      fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or
        MIIM_SUBMENU or MIIM_TYPE;
      cch := SizeOf(CCaption) - 1;
    end;
  end;
  {$ENDIF}

begin
  if (MenuItem <> nil) then
  begin
    StrPCopy(CCaption, MenuItem.Caption);
    NewFlags := Breaks[MenuItem.Break] or Checks[MenuItem.Checked] or
      Enables[MenuItem.Enabled] or Separators[MenuItem.Caption = Separator] or
      MF_BYCOMMAND;
    ItemID := MenuItem.Command;
    if MenuItem.Count > 0 then
    begin
      NewFlags := NewFlags or MF_POPUP;
      ItemID := MenuItem.Handle;
    end
    else
    begin
      if (MenuItem.ShortCut <> scNone) and ((MenuItem.Parent = nil) or
        (MenuItem.Parent.Parent <> nil) or
        not (MenuItem.Parent.Owner is TMainMenu)) then
        StrPCopy(StrECopy(StrEnd(CCaption), Tab),
          ShortCutToText(MenuItem.ShortCut));
    end;
    Item := MenuItem;
    while Item.Parent <> nil do
      Item := Item.Parent;
    if (Item.Owner <> nil) and (Item.Owner is TMenu) then
      MenuHandle := TMenu(Item.Owner).Handle
    else
      MenuHandle := Item.Handle;
    {$IFDEF WIN32}
    if Lo(GetVersion) >= 4 then
    begin
      FillChar(MenuItemInfo, SizeOf(TMenuItemInfo), 0);
      MenuItemInfo.cbSize := SizeOf(TMenuItemInfo);
      if MenuItem.Count > 0 then
      begin
        MenuItemInfo.fMask := MIIM_DATA or MIIM_TYPE;
        with MenuItem do
          MenuItemInfo.fType := IRadios[RadioItem] or IBreaks[Break] or
            ISeparators[Caption = Separator] or IOwnerDraw[OwnerDraw];
        MenuItemInfo.dwTypeData := CCaption;
        SetMenuItemInfo(MenuHandle, MenuItem.Command, False, MenuItemInfo);
      end
      else
      begin
        C := GetMenuItemCount(MenuHandle);
        ItemID := -1;
        for I := 0 to C - 1 do
        begin
          PrepareItemInfo;
          MenuItemInfo.dwTypeData := CCaption;
          GetMenuItemInfo(MenuHandle, I, True, MenuItemInfo);
          if MenuItemInfo.wID = MenuItem.Command then
          begin
            ItemID := I;
            Break;
          end;
        end;
        if (ItemID < 0) and (MenuItem.Parent <> nil) then
        begin
          MenuHandle := MenuItem.Parent.Handle;
          C := GetMenuItemCount(MenuHandle);
          for I := 0 to C - 1 do
          begin
            PrepareItemInfo;
            MenuItemInfo.dwTypeData := CCaption;
            GetMenuItemInfo(MenuHandle, I, True, MenuItemInfo);
            if MenuItemInfo.wID = MenuItem.Command then
            begin
              ItemID := I;
              Break;
            end;
          end;
        end;
        if ItemID < 0 then Exit;
        with MenuItem do
          MenuItemInfo.fType := IRadios[RadioItem] or IBreaks[Break] or
            ISeparators[Caption = Separator] or IOwnerDraw[OwnerDraw];
        MenuItemInfo.dwTypeData := CCaption;
        DeleteMenu(MenuHandle, MenuItem.Command, MF_BYCOMMAND);
        InsertMenuItem(MenuHandle, ItemID, True, MenuItemInfo);
      end;
    end
    else
      {$ENDIF WIN32}
    begin
      if OwnerDraw then
      begin
        ModifyMenu(MenuHandle, MenuItem.Command, NewFlags or MF_OWNERDRAW and
          not MF_STRING, ItemID, PChar(MenuItem));
      end
      else
      begin
        ModifyMenu(MenuHandle, MenuItem.Command, NewFlags, ItemID, CCaption);
      end;
    end;
    for I := 0 to MenuItem.Count - 1 do
      RefreshMenuItem(MenuItem.Items[I], OwnerDraw);
  end;
end;
{$ENDIF COMPILER4_UP}

procedure SetDefaultMenuFont(AFont: TFont);
{$IFDEF WIN32}
var
  NCMetrics: TNonCLientMetrics;
  {$ENDIF}
begin
  {$IFDEF WIN32}
  if NewStyleControls then
  begin
    NCMetrics.cbSize := SizeOf(TNonCLientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCMetrics, 0) then
    begin
      AFont.Handle := CreateFontIndirect(NCMetrics.lfMenuFont);
      Exit;
    end;
  end;
  {$ENDIF}
  with AFont do
  begin
    if NewStyleControls then
      Name := 'MS Sans Serif'
    else
      Name := 'System';
    Size := 8;
    Color := clMenuText;
    Style := [];
  end;
  AFont.Color := clMenuText;
end;

function GetDefItemHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYMENU);
  if NewStyleControls then Dec(Result, 2);
end;

function GetMarginOffset: Integer;
begin
  Result := Round(LoWord(GetMenuCheckMarkDimensions) * 0.3);
end;

procedure MenuLine(Canvas: TCanvas; C: TColor; X1, Y1, X2, Y2: Integer);
begin
  with Canvas do
  begin
    Pen.Color := C;
    MoveTo(X1, Y1);
    LineTo(X2, Y2);
  end;
end;

procedure DrawDisabledBitmap(Canvas: TCanvas; X, Y: Integer; Bitmap: TBitmap;
  State: TMenuOwnerDrawState);
const
  ROP_DSPDxax = $00E20746;
var
  Bmp: TBitmap;
  GrayColor, SaveColor: TColor;
  IsHighlight: Boolean;
begin
  if (mdSelected in State) then
    GrayColor := clGrayText
  else
    GrayColor := clBtnShadow;
  IsHighlight := NewStyleControls and ((not (mdSelected in State)) or
    (GetNearestColor(Canvas.Handle, ColorToRGB(clGrayText)) =
    GetNearestColor(Canvas.Handle, ColorToRGB(clHighlight))));
  if Bitmap.Monochrome then
  begin
    SaveColor := Canvas.Brush.Color;
    try
      if IsHighlight then
      begin
        Canvas.Brush.Color := clBtnHighlight;
        SetTextColor(Canvas.Handle, clWhite);
        SetBkColor(Canvas.Handle, clBlack);
        BitBlt(Canvas.Handle, X + 1, Y + 1, Bitmap.Width, Bitmap.Height,
          Bitmap.Canvas.Handle, 0, 0, ROP_DSPDxax);
      end;
      Canvas.Brush.Color := GrayColor;
      SetTextColor(Canvas.Handle, clWhite);
      SetBkColor(Canvas.Handle, clBlack);
      BitBlt(Canvas.Handle, X, Y, Bitmap.Width, Bitmap.Height,
        Bitmap.Canvas.Handle, 0, 0, ROP_DSPDxax);
    finally
      Canvas.Brush.Color := SaveColor;
    end;
  end
  else
  begin
    Bmp := CreateDisabledBitmapEx(Bitmap, clBlack, clMenu,
      clBtnHighlight, GrayColor, IsHighlight);
    try
      DrawBitmapTransparent(Canvas, X, Y, Bmp, clMenu);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure DrawMenuBitmap(Canvas: TCanvas; X, Y: Integer; Bitmap: TBitmap;
  IsColor: Boolean; State: TMenuOwnerDrawState);
begin
  if (mdDisabled in State) then
    DrawDisabledBitmap(Canvas, X, Y, Bitmap, State)
  else
  begin
    if Bitmap.Monochrome and not IsColor then
      BitBlt(Canvas.Handle, X, Y, Bitmap.Width, Bitmap.Height,
        Bitmap.Canvas.Handle, 0, 0, SRCCOPY)
    else
      DrawBitmapTransparent(Canvas, X, Y, Bitmap, Bitmap.TransparentColor
        and not PaletteMask);
  end;
end;

procedure DrawMenuItem(AMenu: TMenu; Item: TMenuItem; Glyph: TGraphic;
  NumGlyphs: Integer; Canvas: TCanvas; ShowCheck: Boolean; Buttons: TBtnStyle;
  Rect: TRect; MinOffset: {$IFDEF COMPILER4_UP}Integer{$ELSE}Cardinal{$ENDIF};
  State: TMenuOwnerDrawState{$IFDEF WIN32}; Images: TImageList;
  DisabledImages: TImageList; HotImages: TImageList;
  ImageIndex: Integer{$ENDIF});
var
  Left, LineTop, MaxWidth, I, W: Integer;
  CheckSize: Longint;
  BtnRect: TRect;
  IsPopup, DrawHighlight, DrawLowered: Boolean;
  GrayColor: TColor;
  Bmp: TBitmap;
  SaveBrush: TBrush; // to save brush when drawing an Office button
  {$IFDEF WIN32}
  Ico: HIcon;
  H: Integer;
  {$ENDIF}
  {$IFDEF COMPILER4_UP}
  ParentMenu: TMenu;
  {$ENDIF}

  procedure MenuTextOut(X, Y: Integer; const Text: string; Flags: Longint);
  var
    R: TRect;
  begin
    if Length(Text) = 0 then Exit;
    {$IFDEF COMPILER4_UP}
    if (ParentMenu <> nil) and (ParentMenu.IsRightToLeft) then
    begin
      if Flags and DT_LEFT = DT_LEFT then
        Flags := Flags and (not DT_LEFT) or DT_RIGHT
      else if Flags and DT_RIGHT = DT_RIGHT then
        Flags := Flags and (not DT_RIGHT) or DT_LEFT;
      Flags := Flags or DT_RTLREADING;
    end;
    {$ENDIF}
    R := Rect;
    R.Left := X;
    R.Top := Y;
    if (mdDisabled in State) then
    begin
      if DrawHighlight then
      begin
        Canvas.Font.Color := clBtnHighlight;
        OffsetRect(R, 1, 1);
        DrawText(Canvas.Handle, @Text[1], Length(Text), R, Flags);
        OffsetRect(R, -1, -1);
      end;
      Canvas.Font.Color := GrayColor;
    end;
    DrawText(Canvas.Handle, @Text[1], Length(Text), R, Flags)
  end;

  procedure DrawCheckImage(X, Y: Integer);
  begin
    Bmp := TBitmap.Create;
    try
      {$IFDEF WIN32}
      with Bmp do
      begin
        Width := LoWord(CheckSize);
        Height := HiWord(CheckSize);
      end;
      if Item.RadioItem then
      begin
        with Bmp do
        begin
          DrawFrameControl(Canvas.Handle, Bounds(0, 0, Width, Height),
            DFC_MENU, DFCS_MENUBULLET);
          Monochrome := True;
        end;
      end
      else
      begin
        with Bmp do
        begin
          DrawFrameControl(Canvas.Handle, Bounds(0, 0, Width, Height),
            DFC_MENU, DFCS_MENUCHECK);
          Monochrome := True;
        end;
      end;
      {$ELSE}
      Bmp.Handle := LoadBitmap(0, PChar(32760));
      {$ENDIF}
      DrawMenuBitmap(Canvas, X, Y, Bmp, DrawLowered, State);
    finally
      Bmp.Free;
    end;
  end;

  procedure DrawGlyphCheck(ARect: TRect);
  var
    SaveColor: TColor;
    Bmp: TBitmap;
  begin
    InflateRect(ARect, 0, -1);
    SaveColor := Canvas.Brush.Color;
    try
      if not (mdSelected in State) then
        {$IFDEF COMPILER4_UP}
        Bmp := AllocPatternBitmap(clMenu, clBtnHighlight)
          {$ELSE}
        Bmp := CreateTwoColorsBrushPattern(clMenu, clBtnHighlight)
          {$ENDIF}
      else
        Bmp := nil;
      try
        if Bmp <> nil then
          Canvas.Brush.Bitmap := Bmp
        else
          Canvas.Brush.Color := clMenu;
        Canvas.FillRect(ARect);
      finally
        Canvas.Brush.Bitmap := nil;
        {$IFNDEF COMPILER4_UP}
        Bmp.Free;
        {$ENDIF}
      end;
    finally
      Canvas.Brush.Color := SaveColor;
    end;
    Frame3D(Canvas, ARect, GrayColor, clBtnHighlight, 1);
  end;

  {$IFDEF WIN32}

  function UseImages: Boolean;
  begin
    Result := Assigned(Images) and (ImageIndex >= 0) and
      (ImageIndex < Images.Count) and Images.HandleAllocated;
  end;

  function UseHotImages: Boolean;
  begin
    Result := Assigned(HotImages) and (ImageIndex >= 0) and
      (ImageIndex < HotImages.Count) and HotImages.HandleAllocated;
  end;

  function UseDisabledImages: Boolean;
  begin
    Result := Assigned(DisabledImages) and (ImageIndex >= 0) and
      (ImageIndex < DisabledImages.Count) and DisabledImages.HandleAllocated;
  end;
  {$ENDIF}

begin
  IsPopup := IsItemPopup(Item);

  DrawLowered := Item.Checked and IsPopup and not (ShowCheck or
    (Buttons in [bsLowered, bsRaised]));
  DrawHighlight := NewStyleControls and (not (mdSelected in State) or
    (Buttons in [bsLowered, bsRaised]) or (not IsPopup and
    (Buttons = bsOffice)) or
    (GetNearestColor(Canvas.Handle, ColorToRGB(clGrayText)) =
    GetNearestColor(Canvas.Handle, ColorToRGB(clHighlight))));
  if (mdSelected in State) and not (Buttons in [bsLowered, bsRaised]) then
    GrayColor := clGrayText
  else
    GrayColor := clBtnShadow;
  if IsPopup then
  begin
    if ShowCheck then
      CheckSize := GetMenuCheckMarkDimensions
    else
      CheckSize := 2;
    Left := 2 * GetMarginOffset + LoWord(CheckSize);
  end
  else
  begin
    MinOffset := 0;
    CheckSize := 0;
    Left := GetMarginOffset + 2;
  end;
  if (Buttons <> bsNone) and (mdSelected in State) then
  begin
    case Buttons of
      bsLowered: Frame3D(Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
      bsRaised: Frame3D(Canvas, Rect, clBtnHighlight, clBtnShadow, 1);
      bsOffice:
        if not IsPopup then
        begin
          SaveBrush := Canvas.Brush;
          Canvas.Brush.Color := clBtnFace;
          inc(Rect.Right);
          Canvas.FillRect(Rect);
          dec(Rect.Right);
          Canvas.Brush := SaveBrush;
          Frame3D(Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
        end;
    end;
  end;
  if Assigned(Item) then
  begin
    {$IFDEF COMPILER4_UP}
    ParentMenu := Item.GetParentMenu;
    {$ENDIF}
    if Item.Checked and ShowCheck and IsPopup then
    begin
      DrawCheckImage(Rect.Left + (Left - LoWord(CheckSize)) div 2,
        (Rect.Bottom + Rect.Top - HiWord(CheckSize)) div 2);
    end;
    {$IFDEF WIN32}
    if Assigned(Images) and IsPopup then
      MinOffset := Max(MinOffset, Images.Width + AddWidth);
    {$ENDIF}
    if not ShowCheck and (Assigned(Glyph) or (MinOffset > 0)) then
      if Buttons = bsOffice then
        Left := 1
      else
        Left := GetMarginOffset;
    {$IFDEF WIN32}
    if UseImages then
    begin
      W := Images.Width + AddWidth;
      if W < Integer(MinOffset) then W := MinOffset;
      BtnRect := Bounds(Rect.Left + Left - 1, Rect.Top, W + 2,
        Rect.Bottom - Rect.Top);
      if DrawLowered then
        DrawGlyphCheck(BtnRect)
      else if (mdSelected in State) and IsPopup and (Buttons = bsOffice) and
        not ShowCheck then
      begin
        SaveBrush := Canvas.Brush;
        Canvas.Brush.Color := clBtnFace;
        inc(BtnRect.Right);
        Canvas.FillRect(BtnRect);
        dec(BtnRect.Right);
        Canvas.Brush := SaveBrush;
        Frame3D(Canvas, BtnRect, clBtnHighlight, GrayColor, 1);
      end;
      if (mdDisabled in State) then
      begin
        if UseDisabledImages then
        begin
          ImageList_Draw(DisabledImages.Handle, ImageIndex, Canvas.Handle,
            Rect.Left + Left + (W - DisabledImages.Width) div 2, (Rect.Bottom +
            Rect.Top - DisabledImages.Height) div 2, ILD_NORMAL);
        end
        else
        begin
          ImageListDrawDisabled(Images, Canvas, Rect.Left + Left +
            (W - Images.Width) div 2, (Rect.Bottom + Rect.Top -
            Images.Height) div 2, ImageIndex, clBtnHighlight, GrayColor,
            DrawHighlight)
        end;
      end
      else
      begin
        if UseHotImages and (mdSelected in State) then
        begin
          ImageList_Draw(HotImages.Handle, ImageIndex, Canvas.Handle,
            Rect.Left + Left + (W - HotImages.Width) div 2, (Rect.Bottom +
            Rect.Top - HotImages.Height) div 2, ILD_NORMAL);
        end
        else
        begin
          ImageList_Draw(Images.Handle, ImageIndex, Canvas.Handle,
            Rect.Left + Left + (W - Images.Width) div 2, (Rect.Bottom +
            Rect.Top - Images.Height) div 2, ILD_NORMAL);
        end;
      end;
      Inc(Left, W + GetMarginOffset);
    end
    else
      {$ENDIF}if Assigned(Glyph) and not Glyph.Empty and (Item.Caption <> Separator) then
    begin
      W := Glyph.Width;
      if (Glyph is TBitmap) and (NumGlyphs in [2..5]) then
        W := W div NumGlyphs;
      W := Max(W + AddWidth, MinOffset);
      {$IFDEF WIN32}
      if not (Glyph is TIcon) then
        {$ENDIF}
      begin
        BtnRect := Bounds(Rect.Left + Left - 1, Rect.Top, W + 2,
          Rect.Bottom - Rect.Top);
        if DrawLowered then
          DrawGlyphCheck(BtnRect)
        else if (mdSelected in State) and IsPopup and (Buttons = bsOffice) and
          not ShowCheck then
        begin
          Frame3D(Canvas, BtnRect, clBtnHighlight, GrayColor, 1);
        end;
      end;
      if Glyph is TBitmap then
      begin
        if (NumGlyphs in [2..5]) then
        begin
          I := 0;
          if (mdDisabled in State) then
            I := 1
          else if (mdChecked in State) then
            I := 3
          else if (mdSelected in State) then
            I := 2;
          if I > NumGlyphs - 1 then I := 0;
          Bmp := TBitmap.Create;
          try
            AssignBitmapCell(Glyph, Bmp, NumGlyphs, 1, I);
            DrawMenuBitmap(Canvas, Rect.Left + Left + (W - Bmp.Width) div 2,
              (Rect.Bottom + Rect.Top - Bmp.Height) div 2, Bmp, DrawLowered,
              State - [mdDisabled]);
          finally
            Bmp.Free;
          end;
        end
        else
          DrawMenuBitmap(Canvas, Rect.Left + Left + (W - Glyph.Width) div 2,
            (Rect.Bottom + Rect.Top - Glyph.Height) div 2, TBitmap(Glyph),
            DrawLowered, State);
        Inc(Left, W + GetMarginOffset);
      end
        {$IFDEF WIN32}
      else if Glyph is TIcon then
      begin
        Ico := CreateRealSizeIcon(TIcon(Glyph));
        try
          GetIconSize(Ico, W, H);
          I := Max(W + AddWidth, MinOffset);
          BtnRect := Bounds(Rect.Left + Left - 1, Rect.Top, I + 2,
            Rect.Bottom - Rect.Top);
          if DrawLowered then
            DrawGlyphCheck(BtnRect)
          else if (mdSelected in State) and IsPopup and (Buttons = bsOffice) and
            not ShowCheck then
          begin
            Frame3D(Canvas, BtnRect, clBtnHighlight, GrayColor, 1);
          end;
          DrawIconEx(Canvas.Handle, Rect.Left + Left + (I - W) div 2,
            (Rect.Top + Rect.Bottom - H) div 2, Ico, W, H, 0, 0, DI_NORMAL);
          Inc(Left, I + GetMarginOffset);
        finally
          DestroyIcon(Ico);
        end;
      end
        {$ENDIF}
      else
      begin
        Canvas.Draw(Rect.Left + Left + (W - Glyph.Width) div 2,
          (Rect.Bottom + Rect.Top - Glyph.Height) div 2, Glyph);
        Inc(Left, W + GetMarginOffset);
      end;
    end
    else if (MinOffset > 0) then
    begin
      BtnRect := Bounds(Rect.Left + Left - 1, Rect.Top, MinOffset + 2,
        Rect.Bottom - Rect.Top);
      if DrawLowered then
      begin
        DrawGlyphCheck(BtnRect);
        CheckSize := GetMenuCheckMarkDimensions;
        DrawCheckImage(BtnRect.Left + 2 + (MinOffset - LoWord(CheckSize)) div 2,
          (Rect.Bottom + Rect.Top - HiWord(CheckSize)) div 2 + 1);
      end
      else if (mdSelected in State) and IsPopup and (Buttons = bsOffice) and
        not ShowCheck then
      begin
        Frame3D(Canvas, BtnRect, clBtnHighlight, GrayColor, 1);
      end;
      Inc(Left, MinOffset + GetMarginOffset);
    end;
    if Item.Caption = Separator then
    begin
      LineTop := (Rect.Top + Rect.Bottom) div 2 - 1;
      if NewStyleControls then
      begin
        Canvas.Pen.Width := 1;
        MenuLine(Canvas, clBtnShadow, Rect.Left, LineTop, Rect.Right, LineTop);
        MenuLine(Canvas, clBtnHighlight, Rect.Left, LineTop + 1, Rect.Right, LineTop + 1);
      end
      else
      begin
        Canvas.Pen.Width := 2;
        MenuLine(Canvas, clMenuText, Rect.Left, LineTop + 1, Rect.Right, LineTop + 1);
      end;
    end
    else
    begin
      MaxWidth := Canvas.TextWidth(DelChars(Item.Caption, '&') + Tab);
      if (Item.Parent <> nil) and (Item.ShortCut <> scNone) then
      begin
        for I := 0 to Item.Parent.Count - 1 do
          MaxWidth := Max(Canvas.TextWidth(DelChars(Item.Parent.Items[I].Caption,
            '&') + Tab), MaxWidth);
      end;
      Canvas.Brush.Style := bsClear;
      LineTop := (Rect.Bottom + Rect.Top - Canvas.TextHeight('Ay')) div 2;
      MenuTextOut(Rect.Left + Left, LineTop, Item.Caption, DT_EXPANDTABS or
        DT_LEFT or DT_SINGLELINE);
      if (Item.ShortCut <> scNone) and (Item.Count = 0) and IsPopup then
      begin
        MenuTextOut(Rect.Left + Left + MaxWidth, LineTop,
          ShortCutToText(Item.ShortCut), DT_EXPANDTABS or DT_LEFT or
          DT_SINGLELINE);
      end;
    end;
  end;
end;

procedure MenuMeasureItem(AMenu: TMenu; Item: TMenuItem; Canvas: TCanvas;
  ShowCheck: Boolean; Glyph: TGraphic; NumGlyphs: Integer; var ItemWidth,
  ItemHeight: Integer; MinOffset: Cardinal{$IFDEF WIN32}; Images: TImageList;
  ImageIndex: Integer{$ENDIF});
var
  IsPopup: Boolean;
  W, H: Integer;
  {$IFDEF WIN32}
  Ico: HIcon;
  {$ENDIF}

  function GetTextWidth(Item: TMenuItem): Integer;
  var
    I, MaxW: Integer;
  begin
    if IsPopup then
    begin
      Result := Canvas.TextWidth(DelChars(Item.Caption, '&') + Tab);
      MaxW := Canvas.TextWidth(ShortCutToText(Item.ShortCut) + ' ');
      if (Item.Parent <> nil) and (Item.ShortCut <> scNone) then
      begin
        for I := 0 to Item.Parent.Count - 1 do
          with Item.Parent.Items[I] do
          begin
            Result := Max(Result, Canvas.TextWidth(DelChars(Caption, '&') + Tab));
            MaxW := Max(MaxW, Canvas.TextWidth(ShortCutToText(ShortCut) + ' '));
          end;
      end;
      Result := Result + MaxW;
      if Item.Count > 0 then Inc(Result, Canvas.TextWidth(Tab));
    end
    else
      Result := Canvas.TextWidth(DelChars(Item.Caption, '&'));
  end;

begin
  IsPopup := IsItemPopup(Item);
  ItemHeight := GetDefItemHeight;
  if IsPopup then
  begin
    ItemWidth := GetMarginOffset * 2;
    {$IFDEF WIN32}
    if Assigned(Images) then
      MinOffset := Max(MinOffset, Images.Width + AddWidth);
    {$ENDIF}
  end
  else
  begin
    ItemWidth := 0;
    MinOffset := 0;
  end;
  Inc(ItemWidth, GetTextWidth(Item));
  if IsPopup and ShowCheck then
    Inc(ItemWidth, LoWord(GetMenuCheckMarkDimensions));
  if Item.Caption = Separator then
  begin
    ItemHeight := Max(Canvas.TextHeight(Separator) div 2, 9);
  end
  else
  begin
    ItemHeight := Max(ItemHeight, Canvas.TextHeight(Item.Caption));
    {$IFDEF WIN32}
    if Assigned(Images) and (IsPopup or ((ImageIndex >= 0) and
      (ImageIndex < Images.Count))) then
    begin
      Inc(ItemWidth, Max(Images.Width + AddWidth, MinOffset));
      if not IsPopup then Inc(ItemWidth, GetMarginOffset);
      if (ImageIndex >= 0) and (ImageIndex < Images.Count) then
        ItemHeight := Max(ItemHeight, Images.Height + AddHeight);
    end
    else
      {$ENDIF}if Assigned(Glyph) and not Glyph.Empty then
    begin
      W := Glyph.Width;
      if (Glyph is TBitmap) and (NumGlyphs in [2..5]) then
        W := W div NumGlyphs;
      H := Glyph.Height;
      {$IFDEF WIN32}
      if Glyph is TIcon then
      begin
        Ico := CreateRealSizeIcon(TIcon(Glyph));
        try
          GetIconSize(Ico, W, H);
        finally
          DestroyIcon(Ico);
        end;
      end;
      {$ENDIF}
      W := Max(W + AddWidth, MinOffset);
      Inc(ItemWidth, W);
      if not IsPopup then Inc(ItemWidth, GetMarginOffset);
      ItemHeight := Max(ItemHeight, H + AddHeight);
    end
    else if MinOffset > 0 then
    begin
      Inc(ItemWidth, MinOffset);
      if not IsPopup then Inc(ItemWidth, GetMarginOffset);
    end;
  end;
end;

{ TJvMainMenu }

constructor TJvMainMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FShowCheckMarks := True;
  FHook := TJvWindowHook.Create(Self);
  FHook.AfterMessage := WndMessage;
  {$IFDEF WIN32}
  FStandardIsOffice := true;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImageListChange;
  FHotImageChangeLink := TChangeLink.Create;
  FHotImageChangeLink.OnChange := HotImageListChange;
  {$ENDIF}
end;

destructor TJvMainMenu.Destroy;
begin
  {$IFDEF WIN32}
  FImageChangeLink.Free;
  FHotImageChangeLink.Free;
  FDisabledImageChangeLink.Free;
  {$ENDIF}
  SetStyle(msStandard);
  FHook.Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvMainMenu.Loaded;
begin
  inherited Loaded;
  if IsOwnerDrawMenu then RefreshMenu(True);
end;

function TJvMainMenu.IsOwnerDrawMenu: Boolean;
begin
  Result := (FStyle <> msStandard)
    {$IFDEF WIN32} or (Assigned(FImages) and (FImages.Count > 0)){$ENDIF};
end;

{$IFDEF WIN32}

procedure TJvMainMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then SetImages(nil);
    if AComponent = FDisabledImages then SetDisabledImages(nil);
  end;
end;

procedure TJvMainMenu.SetStandardIsOffice(Value: Boolean);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if Value <> FStandardIsOffice then
  begin
    FStandardIsOffice := Value;
    // force redrawing
    if IsOwnerDrawMenu then
      FHook.Control := FindForm
    else
      FHook.Control := nil;
    if IsOwnerDrawMenu <> OldOwnerDraw then RefreshMenu(not OldOwnerDraw);
  end;
end;

procedure TJvMainMenu.ImageListChange(Sender: TObject);
begin
  if Sender = FImages then RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvMainMenu.SetImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FImages <> nil then FImages.UnregisterChanges(FImageChangeLink);
  FImages := Value;
  if Value <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu then
    FHook.Control := FindForm
  else
    FHook.Control := nil;
  if IsOwnerDrawMenu <> OldOwnerDraw then RefreshMenu(not OldOwnerDraw);
end;

procedure TJvMainMenu.DisabledImageListChange(Sender: TObject);
begin
  if Sender = FDisabledImages then RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvMainMenu.SetDisabledImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FDisabledImages <> nil then FDisabledImages.UnregisterChanges(FDisabledImageChangeLink);
  FDisabledImages := Value;
  if Value <> nil then
  begin
    FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
    FDisabledImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu then
    FHook.Control := FindForm
  else
    FHook.Control := nil;
  if IsOwnerDrawMenu <> OldOwnerDraw then RefreshMenu(not OldOwnerDraw);
end;

procedure TJvMainMenu.HotImageListChange(Sender: TObject);
begin
  if Sender = FHotImages then RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvMainMenu.SetHotImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FHotImages <> nil then FHotImages.UnregisterChanges(FHotImageChangeLink);
  FHotImages := Value;
  if Value <> nil then
  begin
    FHotImages.RegisterChanges(FHotImageChangeLink);
    FHotImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu then
    FHook.Control := FindForm
  else
    FHook.Control := nil;
  if IsOwnerDrawMenu <> OldOwnerDraw then RefreshMenu(not OldOwnerDraw);
end;
{$ENDIF}

procedure TJvMainMenu.SetStyle(Value: TJvMenuStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if IsOwnerDrawMenu then
      FHook.Control := FindForm
    else
      FHook.Control := nil;
    RefreshMenu(IsOwnerDrawMenu);
  end;
end;

function TJvMainMenu.FindForm: TWinControl;
begin
  Result := FindControl(WindowHandle);
  if (Result = nil) and (Owner is TWinControl) then
    Result := TWinControl(Owner);
end;

procedure TJvMainMenu.Refresh;
begin
  RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvMainMenu.RefreshMenu(AOwnerDraw: Boolean);
{$IFDEF COMPILER4_UP}
begin
  Self.OwnerDraw := AOwnerDraw and (FHook.Control <> nil) and
    not (csDesigning in ComponentState);
  {$ELSE}
var
  I: Integer;
begin
  if AOwnerDraw and (FHook.Control = nil) then Exit;
  if not (csDesigning in ComponentState) then
    for I := 0 to Items.Count - 1 do
      RefreshMenuItem(Items[I], AOwnerDraw);
  {$ENDIF}
end;

procedure TJvMainMenu.DefaultDrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
var
  Graphic: TGraphic;
  BackColor: TColor;
  NumGlyphs{$IFDEF WIN32}, ImageIndex{$ENDIF}: Integer;
begin
  if Canvas.Handle <> 0 then
  begin
    Graphic := nil;
    BackColor := Canvas.Brush.Color;
    NumGlyphs := 1;
    GetItemParams(Item, State, Canvas.Font, BackColor, Graphic, NumGlyphs);
    {$IFDEF WIN32}
    {$IFDEF COMPILER4_UP}
    ImageIndex := Item.ImageIndex;
    {$ELSE}
    ImageIndex := -1;
    {$ENDIF}
    GetImageIndex(Item, State, ImageIndex);
    {$ENDIF}
    DrawMenuItem(Self, Item, Graphic, NumGlyphs, Canvas, FShowCheckMarks,
      BtnStyle(Style, FStandardIsOffice), Rect, FMinTextOffset, State
      {$IFDEF WIN32}, FImages, FDisabledImages, FHotImages, ImageIndex{$ENDIF});
  end;
end;

procedure TJvMainMenu.DrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
var
  Graphic: TGraphic;
  BackColor: TColor;
  NumGlyphs{$IFDEF WIN32}, ImageIndex{$ENDIF}: Integer;
begin
  if Canvas.Handle <> 0 then
  begin
    Graphic := nil;
    BackColor := Canvas.Brush.Color;
    NumGlyphs := 1;
    GetItemParams(Item, State, Canvas.Font, BackColor, Graphic, NumGlyphs);
    if BackColor <> clNone then
    begin
      Canvas.Brush.Color := BackColor;
      Canvas.FillRect(Rect);
    end;
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, Item, Rect, State)
    else
    begin
      {$IFDEF WIN32}
      {$IFDEF COMPILER4_UP}
      ImageIndex := Item.ImageIndex;
      {$ELSE}
      ImageIndex := -1;
      {$ENDIF}
      GetImageIndex(Item, State, ImageIndex);
      {$ENDIF}
      DrawMenuItem(Self, Item, Graphic, NumGlyphs, Canvas, FShowCheckMarks,
        BtnStyle(Style, FStandardIsOffice), Rect, FMinTextOffset, State
        {$IFDEF WIN32}, FImages, FDisabledImages, FHotImages, ImageIndex{$ENDIF});
    end;
  end;
end;

procedure TJvMainMenu.MeasureItem(Item: TMenuItem; var Width, Height: Integer);
begin
  if Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, Item, Width, Height)
end;

procedure TJvMainMenu.WndMessage(Sender: TObject; var AMsg: TMessage;
  var Handled: Boolean);
begin
  if IsOwnerDrawMenu then MenuWndMessage(Self, AMsg, Handled);
end;

procedure TJvMainMenu.GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState;
  AFont: TFont; var Color: TColor; var Graphic: TGraphic; var NumGlyphs: Integer);
begin
  if Assigned(FOnGetItemParams) then
    FOnGetItemParams(Self, Item, State, AFont, Color, Graphic, NumGlyphs);
  if (Item <> nil) and (Item.Caption = Separator) then Graphic := nil;
end;

{$IFDEF WIN32}

procedure TJvMainMenu.GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState;
  var ImageIndex: Integer);
begin
  if Assigned(FImages) and (Item <> nil) and (Item.Caption <> Separator) and
    Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, Item, State, ImageIndex);
end;
{$ENDIF}

procedure TJvMainMenu.CMMenuChanged(var Message: TMessage);
begin
  {$IFNDEF COMPILER4_UP}
  if IsOwnerDrawMenu then RefreshMenu(True);
  {$ENDIF}
end;

procedure TJvMainMenu.WMDrawItem(var Message: TWMDrawItem);
var
  State: TMenuOwnerDrawState;
  SaveIndex: Integer;
  Item: TMenuItem;
begin
  with Message.DrawItemStruct^ do
  begin
    {$IFDEF WIN32}
    State := TMenuOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
    {$ELSE}
    State := TMenuOwnerDrawState(WordRec(itemState).Lo);
    {$ENDIF}
    {if (mdDisabled in State) then State := State - [mdSelected];}
    Item := TMenuItem(Pointer(itemData));
    if Assigned(Item) and
      (FindItem(Item.Command, fkCommand) = Item) then
    begin
      SaveIndex := SaveDC(hDC);
      try
        FCanvas.Handle := hDC;
        SetDefaultMenuFont(FCanvas.Font);
        FCanvas.Font.Color := clMenuText;
        FCanvas.Brush.Color := clMenu;
        {$IFDEF WIN32}
        if mdDefault in State then
          FCanvas.Font.Style := FCanvas.Font.Style + [fsBold];
        {$ENDIF}
        if (mdSelected in State){$IFDEF WIN32} and not
        (Style in [msBtnLowered, msBtnRaised]){$ENDIF} then
        begin
          FCanvas.Brush.Color := clHighlight;
          FCanvas.Font.Color := clHighlightText;
        end;
        with rcItem do
          IntersectClipRect(FCanvas.Handle, Left, Top, Right, Bottom);
        DrawItem(Item, rcItem, State);
        FCanvas.Handle := 0;
      finally
        RestoreDC(hDC, SaveIndex);
      end;
    end;
  end;
end;

procedure TJvMainMenu.WMMeasureItem(var Message: TWMMeasureItem);
var
  Item: TMenuItem;
  Graphic: TGraphic;
  BackColor: TColor;
  DC: HDC;
  NumGlyphs{$IFDEF WIN32}, ImageIndex{$ENDIF}: Integer;
begin
  with Message.MeasureItemStruct^ do
  begin
    Item := TMenuItem(Pointer(itemData));
    if Assigned(Item) and (FindItem(Item.Command, fkCommand) = Item) then
    begin
      DC := GetDC(0);
      try
        FCanvas.Handle := DC;
        SetDefaultMenuFont(FCanvas.Font);
        {$IFDEF WIN32}
        if Item.Default then
          FCanvas.Font.Style := FCanvas.Font.Style + [fsBold];
        {$ENDIF}
        Graphic := nil;
        BackColor := FCanvas.Brush.Color;
        NumGlyphs := 1;
        GetItemParams(Item, [], FCanvas.Font, BackColor, Graphic, NumGlyphs);
        {$IFDEF WIN32}
        {$IFDEF COMPILER4_UP}
        ImageIndex := Item.ImageIndex;
        {$ELSE}
        ImageIndex := -1;
        {$ENDIF}
        GetImageIndex(Item, [], ImageIndex);
        {$ENDIF}
        MenuMeasureItem(Self, Item, FCanvas, FShowCheckMarks, Graphic,
          NumGlyphs, Integer(itemWidth), Integer(itemHeight), FMinTextOffset
          {$IFDEF WIN32}, FImages, ImageIndex{$ENDIF});
        MeasureItem(Item, Integer(itemWidth), Integer(itemHeight));
      finally
        FCanvas.Handle := 0;
        ReleaseDC(0, DC);
      end;
    end;
  end;
end;

procedure TJvMainMenu.WMMenuSelect(var Message: TWMMenuSelect);
var
  MenuItem: TMenuItem;
  FindKind: TFindItemKind;
  MenuID: Integer;
begin
  if FCursor <> crDefault then
    with Message do
    begin
      FindKind := fkCommand;
      if MenuFlag and MF_POPUP <> 0 then
      begin
        FindKind := fkHandle;
        MenuId := GetSubMenu(Menu, IDItem);
      end
      else
        MenuId := IDItem;
      MenuItem := FindItem(MenuId, FindKind);
      if (MenuItem <> nil) and (IsItemPopup(MenuItem) or (MenuItem.Count = 0))
        and (MenuFlag and MF_HILITE <> 0) then
        SetCursor(Screen.Cursors[FCursor])
      else
        SetCursor(Screen.Cursors[crDefault]);
    end;
end;

{ TJvPopupList }

type
  TJvPopupList = class(TList)
  private
    {$IFNDEF WIN32}
    FMenuHelp: THelpContext;
    {$ENDIF}
    procedure WndProc(var Message: TMessage);
  public
    Window: HWND;
    procedure Add(Popup: TPopupMenu);
    procedure Remove(Popup: TPopupMenu);
  end;

const
  PopupList: TJvPopupList = nil;

procedure TJvPopupList.WndProc(var Message: TMessage);
var
  I: Integer;
  MenuItem: TMenuItem;
  FindKind: TFindItemKind;
  ContextID: Integer;
  Handled: Boolean;
begin
  try
    case Message.Msg of
      WM_MEASUREITEM, WM_DRAWITEM:
        for I := 0 to Count - 1 do
        begin
          Handled := False;
          TJvPopupMenu(Items[I]).WndMessage(nil, Message, Handled);
          if Handled then Exit;
        end;
      WM_COMMAND:
        for I := 0 to Count - 1 do
          if TJvPopupMenu(Items[I]).DispatchCommand(Message.wParam) then Exit;
      WM_INITMENUPOPUP:
        for I := 0 to Count - 1 do
          with TWMInitMenuPopup(Message) do
            if TJvPopupMenu(Items[I]).DispatchPopup(MenuPopup) then Exit;
      WM_MENUSELECT:
        with TWMMenuSelect(Message) do
        begin
          FindKind := fkCommand;
          if MenuFlag and MF_POPUP <> 0 then
          begin
            FindKind := fkHandle;
            ContextId := GetSubMenu(Menu, IDItem);
          end
          else
            ContextId := IDItem;
          for I := 0 to Count - 1 do
          begin
            MenuItem := TJvPopupMenu(Items[I]).FindItem(ContextId, FindKind);
            if MenuItem <> nil then
            begin
              {$IFNDEF WIN32}
              FMenuHelp := MenuItem.HelpContext;
              {$ENDIF}
              Application.Hint := MenuItem.Hint;
              with TJvPopupMenu(Items[I]) do
                if FCursor <> crDefault then
                begin
                  if (MenuFlag and MF_HILITE <> 0) then
                    SetCursor(Screen.Cursors[FCursor])
                  else
                    SetCursor(Screen.Cursors[crDefault]);
                end;
              Exit;
            end;
          end;
          {$IFNDEF WIN32}
          FMenuHelp := 0;
          {$ENDIF}
          Application.Hint := '';
        end;
      WM_MENUCHAR:
        for I := 0 to Count - 1 do
          with TJvPopupMenu(Items[I]) do
            if (Handle = HMenu(Message.LParam)) or
              (FindItem(Message.LParam, fkHandle) <> nil) then
            begin
              {$IFDEF COMPILER4_UP}
              ProcessMenuChar(TWMMenuChar(Message));
              {$ELSE}
              ProcessMenuChar(TJvPopupMenu(Items[I]), TWMMenuChar(Message));
              {$ENDIF}
              Exit;
            end;
      {$IFDEF WIN32}
      WM_HELP:
        with PHelpInfo(Message.LParam)^ do
        begin
          for I := 0 to Count - 1 do
            if TJvPopupMenu(Items[I]).Handle = hItemHandle then
            begin
              ContextID := TMenu(Items[I]).GetHelpContext(iCtrlID, True);
              if ContextID = 0 then
                ContextID := TMenu(Items[I]).GetHelpContext(hItemHandle, False);
              if Screen.ActiveForm = nil then Exit;
              if (biHelp in Screen.ActiveForm.BorderIcons) then
                Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID)
              else
                Application.HelpContext(ContextID);
              Exit;
            end;
        end;
      {$ELSE}
      WM_ENTERIDLE:
        if (TWMEnterIdle(Message).Source = MSGF_MENU) and
          (GetKeyState(VK_F1) < 0) and (FMenuHelp <> 0) then
        begin
          Application.HelpContext(FMenuHelp);
          FMenuHelp := 0;
          Exit;
        end;
      {$ENDIF WIN32}
    end;
    with Message do
      Result := DefWindowProc(Window, Msg, wParam, lParam);
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvPopupList.Add(Popup: TPopupMenu);
begin
  if Count = 0 then Window := {$IFDEF COMPILER6_UP}Classes.{$ENDIF}AllocateHWnd(WndProc);
  inherited Add(Popup);
end;

procedure TJvPopupList.Remove(Popup: TPopupMenu);
begin
  inherited Remove(Popup);
  if Count = 0 then {$IFDEF COMPILER6_UP}Classes.{$ENDIF}DeallocateHWnd(Window);
end;

{ TJvPopupMenu }

constructor TJvPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if PopupList = nil then
    PopupList := TJvPopupList.Create;
  FShowCheckMarks := True;
  FCanvas := TControlCanvas.Create;
  FCursor := crDefault;
  PopupList.Add(Self);
  {$IFDEF WIN32}
  FStandardIsOffice := true;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImageListChange;
  FHotImageChangeLink := TChangeLink.Create;
  FHotImageChangeLink.OnChange := HotImageListChange;
  {$ENDIF}
  {$IFDEF COMPILER4_UP}
  FPopupPoint := Point(-1, -1);
  {$ENDIF}
end;

destructor TJvPopupMenu.Destroy;
begin
  {$IFDEF WIN32}
  FImageChangeLink.Free;
  FDisabledImageChangeLink.Free;
  FHotImageChangeLink.Free;
  {$ENDIF}
  SetStyle(msStandard);
  PopupList.Remove(Self);
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvPopupMenu.Loaded;
begin
  inherited Loaded;
  if IsOwnerDrawMenu then RefreshMenu(True);
end;

{$IFDEF WIN32}

procedure TJvPopupMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then SetImages(nil);
  end;
end;

procedure TJvPopupMenu.SetStandardIsOffice(Value: Boolean);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if Value <> FStandardIsOffice then
  begin
    FStandardIsOffice := Value;
    // force redrawing
    if IsOwnerDrawMenu <> OldOwnerDraw then RefreshMenu(not OldOwnerDraw);
  end;
end;

procedure TJvPopupMenu.ImageListChange(Sender: TObject);
begin
  if Sender = FImages then RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvPopupMenu.SetImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FImages <> nil then FImages.UnregisterChanges(FImageChangeLink);
  FImages := Value;
  if Value <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then RefreshMenu(not OldOwnerDraw);
end;

procedure TJvPopupMenu.DisabledImageListChange(Sender: TObject);
begin
  if Sender = FDisabledImages then RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvPopupMenu.SetDisabledImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FDisabledImages <> nil then FDisabledImages.UnregisterChanges(FDisabledImageChangeLink);
  FDisabledImages := Value;
  if Value <> nil then
  begin
    FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
    FDisabledImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then RefreshMenu(not OldOwnerDraw);
end;

procedure TJvPopupMenu.HotImageListChange(Sender: TObject);
begin
  if Sender = FHotImages then RefreshMenu(IsOwnerDrawMenu);
end;

procedure TJvPopupMenu.SetHotImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FHotImages <> nil then FImages.UnregisterChanges(FHotImageChangeLink);
  FHotImages := Value;
  if Value <> nil then
  begin
    FHotImages.RegisterChanges(FHotImageChangeLink);
    FHotImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then RefreshMenu(not OldOwnerDraw);
end;
{$ENDIF}

{$IFDEF COMPILER4_UP}

function FindPopupControl(const Pos: TPoint): TControl;
var
  Window: TWinControl;
begin
  Result := nil;
  Window := FindVCLWindow(Pos);
  if Window <> nil then
  begin
    Result := Window.ControlAtPos(Pos, False);
    if Result = nil then Result := Window;
  end;
end;

procedure TJvPopupMenu.SetBiDiModeFromPopupControl;
var
  AControl: TControl;
begin
  if not SysLocale.MiddleEast then Exit;
  if FParentBiDiMode then
  begin
    AControl := FindPopupControl(FPopupPoint);
    if AControl <> nil then
      BiDiMode := AControl.BiDiMode
    else
      BiDiMode := Application.BiDiMode;
  end;
end;

function TJvPopupMenu.UseRightToLeftAlignment: Boolean;
var
  AControl: TControl;
begin
  Result := False;
  if not SysLocale.MiddleEast then Exit;
  if FParentBiDiMode then
  begin
    AControl := FindPopupControl(FPopupPoint);
    if AControl <> nil then
      Result := AControl.UseRightToLeftAlignment
    else
      Result := Application.UseRightToLeftAlignment;
  end
  else
    Result := (BiDiMode = bdRightToLeft);
end;
{$ENDIF COMPILER4_UP}

procedure TJvPopupMenu.Popup(X, Y: Integer);
const
  {$IFDEF COMPILER4_UP}
  Flags: array[Boolean, TPopupAlignment] of Word =
  ((TPM_LEFTALIGN, TPM_RIGHTALIGN, TPM_CENTERALIGN),
    (TPM_RIGHTALIGN, TPM_LEFTALIGN, TPM_CENTERALIGN));
  Buttons: array[TTrackButton] of Word = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
  {$ELSE}
  Flags: array[TPopupAlignment] of Word = (TPM_LEFTALIGN, TPM_RIGHTALIGN,
    TPM_CENTERALIGN);
  {$ENDIF}
var
  FOnPopup: TNotifyEvent;
begin
  {$IFDEF COMPILER4_UP}
  FPopupPoint := Point(X, Y);
  FParentBiDiMode := ParentBiDiMode;
  try
    SetBiDiModeFromPopupControl;
    {$ENDIF}
    FOnPopup := OnPopup;
    if Assigned(FOnPopup) then FOnPopup(Self);
    if IsOwnerDrawMenu then RefreshMenu(True);
    {$IFNDEF WIN32}
    PopupList.FMenuHelp := HelpContext;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    AdjustBiDiBehavior;
    TrackPopupMenu(Items.Handle,
      Flags[UseRightToLeftAlignment, Alignment] or Buttons[TrackButton], X, Y,
      0 { reserved }, PopupList.Window, nil);
  finally
    ParentBiDiMode := FParentBiDiMode;
  end;
  {$ELSE}
    TrackPopupMenu(Items.Handle, Flags[Alignment] or TPM_RIGHTBUTTON, X, Y,
      0 { reserved }, PopupList.Window, nil);
    {$ENDIF}
  end;

procedure TJvPopupMenu.Refresh;
begin
  RefreshMenu(IsOwnerDrawMenu);
end;

function TJvPopupMenu.IsOwnerDrawMenu: Boolean;
begin
  Result := (FStyle <> msStandard)
    {$IFDEF WIN32} or (Assigned(FImages) and (FImages.Count > 0)){$ENDIF};
end;

procedure TJvPopupMenu.RefreshMenu(AOwnerDraw: Boolean);
{$IFDEF COMPILER4_UP}
begin
  Self.OwnerDraw := AOwnerDraw and not (csDesigning in ComponentState);
  {$ELSE}
var
  I: Integer;
begin
  if not (csDesigning in ComponentState) then
    for I := 0 to Items.Count - 1 do
      RefreshMenuItem(Items[I], AOwnerDraw);
  {$ENDIF}
end;

procedure TJvPopupMenu.SetStyle(Value: TJvMenuStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    RefreshMenu(IsOwnerDrawMenu);
  end;
end;

procedure TJvPopupMenu.DefaultDrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
var
  Graphic: TGraphic;
  BackColor: TColor;
  NumGlyphs{$IFDEF WIN32}, ImageIndex{$ENDIF}: Integer;
begin
  if Canvas.Handle <> 0 then
  begin
    Graphic := nil;
    BackColor := Canvas.Brush.Color;
    NumGlyphs := 1;
    GetItemParams(Item, State, Canvas.Font, BackColor, Graphic, NumGlyphs);
    {$IFDEF WIN32}
    {$IFDEF COMPILER4_UP}
    ImageIndex := Item.ImageIndex;
    {$ELSE}
    ImageIndex := -1;
    {$ENDIF}
    GetImageIndex(Item, State, ImageIndex);
    {$ENDIF}
    DrawMenuItem(Self, Item, Graphic, NumGlyphs, Canvas, FShowCheckMarks,
      BtnStyle(Style, FStandardIsOffice), Rect, FMinTextOffset, State
      {$IFDEF WIN32}, FImages, FDisabledImages, FHotImages, ImageIndex{$ENDIF});
  end;
end;

procedure TJvPopupMenu.DrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
var
  Graphic: TGraphic;
  BackColor: TColor;
  NumGlyphs{$IFDEF WIN32}, ImageIndex{$ENDIF}: Integer;
begin
  if Canvas.Handle <> 0 then
  begin
    Graphic := nil;
    BackColor := Canvas.Brush.Color;
    NumGlyphs := 1;
    GetItemParams(Item, State, Canvas.Font, BackColor, Graphic, NumGlyphs);
    if BackColor <> clNone then
    begin
      Canvas.Brush.Color := BackColor;
      Canvas.FillRect(Rect);
    end;
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, Item, Rect, State)
    else
    begin
      {$IFDEF WIN32}
      {$IFDEF COMPILER4_UP}
      ImageIndex := Item.ImageIndex;
      {$ELSE}
      ImageIndex := -1;
      {$ENDIF}
      GetImageIndex(Item, State, ImageIndex);
      {$ENDIF}
      DrawMenuItem(Self, Item, Graphic, NumGlyphs, Canvas, FShowCheckMarks,
        BtnStyle(Style, FStandardIsOffice), Rect, FMinTextOffset, State
        {$IFDEF WIN32}, FImages, FDisabledImages, FHotImages, ImageIndex{$ENDIF});
    end;
  end;
end;

procedure TJvPopupMenu.MeasureItem(Item: TMenuItem; var Width, Height: Integer);
begin
  if Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, Item, Width, Height)
end;

procedure TJvPopupMenu.WndMessage(Sender: TObject; var AMsg: TMessage;
  var Handled: Boolean);
begin
  if IsOwnerDrawMenu then MenuWndMessage(Self, AMsg, Handled);
end;

procedure TJvPopupMenu.GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState;
  AFont: TFont; var Color: TColor; var Graphic: TGraphic; var NumGlyphs: Integer);
begin
  if Assigned(FOnGetItemParams) then
    FOnGetItemParams(Self, Item, State, AFont, Color, Graphic, NumGlyphs);
  if (Item <> nil) and (Item.Caption = Separator) then Graphic := nil;
end;

{$IFDEF WIN32}

procedure TJvPopupMenu.GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState;
  var ImageIndex: Integer);
begin
  if Assigned(FImages) and (Item <> nil) and (Item.Caption <> Separator) and
    Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, Item, State, ImageIndex);
end;
{$ENDIF}

procedure TJvPopupMenu.DefaultDrawMargin(ARect: TRect; StartColor,
  EndColor: TColor);
var
  R: Integer;
begin
  with ARect do
  begin
    if NewStyleControls then
      R := Right - 3
    else
      R := Right;
    GradientFillRect(Canvas, Rect(Left, Top, R, Bottom), StartColor,
      EndColor, fdTopToBottom, 32);
    if NewStyleControls then
    begin
      MenuLine(Canvas, clBtnShadow, Right - 2, Top, Right - 2, Bottom);
      MenuLine(Canvas, clBtnHighlight, Right - 1, Top, Right - 1, Bottom);
    end;
  end;
end;

procedure TJvPopupMenu.DrawMargin(ARect: TRect);
begin
  if Assigned(FOnDrawMargin) then
    FOnDrawMargin(Self, ARect)
  else
  begin
    DefaultDrawMargin(ARect, DefMarginColor, RGB(
      GetRValue(DefMarginColor) div 4,
      GetGValue(DefMarginColor) div 4,
      GetBValue(DefMarginColor) div 4));
  end;
end;

procedure TJvPopupMenu.WMDrawItem(var Message: TWMDrawItem);
var
  State: TMenuOwnerDrawState;
  SaveIndex: Integer;
  Item: TMenuItem;
  MarginRect: TRect;
begin
  with Message.DrawItemStruct^ do
  begin
    {$IFDEF WIN32}
    State := TMenuOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
    {$ELSE}
    State := TMenuOwnerDrawState(WordRec(itemState).Lo);
    {$ENDIF}
    Item := TMenuItem(Pointer(itemData));
    if Assigned(Item) and
      (FindItem(Item.Command, fkCommand) = Item) then
    begin
      SaveIndex := SaveDC(hDC);
      try
        FCanvas.Handle := hDC;
        if (Item.Parent = Self.Items) and (FLeftMargin > 0) then
          if (itemAction = ODA_DRAWENTIRE) then
          begin
            MarginRect := FCanvas.ClipRect;
            MarginRect.Left := 0;
            MarginRect.Right := FLeftMargin;
            DrawMargin(MarginRect);
          end;
        SetDefaultMenuFont(FCanvas.Font);
        FCanvas.Font.Color := clMenuText;
        FCanvas.Brush.Color := clMenu;
        {$IFDEF WIN32}
        if mdDefault in State then
          FCanvas.Font.Style := FCanvas.Font.Style + [fsBold];
        {$ENDIF}
        if (mdSelected in State){$IFDEF WIN32} and
        not (Style in [msBtnLowered, msBtnRaised]){$ENDIF} then
        begin
          FCanvas.Brush.Color := clHighlight;
          FCanvas.Font.Color := clHighlightText;
        end;
        if (Item.Parent = Self.Items) then
          Inc(rcItem.Left, LeftMargin + 1);
        with rcItem do
          IntersectClipRect(FCanvas.Handle, Left, Top, Right, Bottom);
        DrawItem(Item, rcItem, State);
        FCanvas.Handle := 0;
      finally
        RestoreDC(hDC, SaveIndex);
      end;
    end;
  end;
end;

procedure TJvPopupMenu.WMMeasureItem(var Message: TWMMeasureItem);
var
  Item: TMenuItem;
  Graphic: TGraphic;
  BackColor: TColor;
  NumGlyphs{$IFDEF WIN32}, ImageIndex{$ENDIF}: Integer;
begin
  with Message.MeasureItemStruct^ do
  begin
    Item := TMenuItem(Pointer(itemData));
    if Assigned(Item) and (FindItem(Item.Command, fkCommand) = Item) then
    begin
      FCanvas.Handle := GetDC(0);
      try
        SetDefaultMenuFont(FCanvas.Font);
        {$IFDEF WIN32}
        if Item.Default then
          FCanvas.Font.Style := FCanvas.Font.Style + [fsBold];
        {$ENDIF}
        Graphic := nil;
        BackColor := Canvas.Brush.Color;
        NumGlyphs := 1;
        GetItemParams(Item, [], FCanvas.Font, BackColor, Graphic, NumGlyphs);
        {$IFDEF WIN32}
        {$IFDEF COMPILER4_UP}
        ImageIndex := Item.ImageIndex;
        {$ELSE}
        ImageIndex := -1;
        {$ENDIF}
        GetImageIndex(Item, [], ImageIndex);
        {$ENDIF}
        MenuMeasureItem(Self, Item, FCanvas, FShowCheckMarks, Graphic,
          NumGlyphs, Integer(itemWidth), Integer(itemHeight), FMinTextOffset
          {$IFDEF WIN32}, FImages, ImageIndex{$ENDIF});
        MeasureItem(Item, Integer(itemWidth), Integer(itemHeight));
        if (Item.Parent = Self.Items) then
          Inc(itemWidth, LeftMargin + 1);
      finally
        ReleaseDC(0, FCanvas.Handle);
        FCanvas.Handle := 0;
      end;
    end;
  end;
end;

{$IFNDEF WIN32}

procedure FreePopupList; far;
begin
  if PopupList <> nil then
  begin
    PopupList.Free;
    PopupList := nil;
  end;
end;
{$ENDIF}

initialization
  PopupList := nil;
  {$IFDEF WIN32}
finalization
  if PopupList <> nil then PopupList.Free;
  {$ELSE}
  AddExitProc(FreePopupList);
  {$ENDIF}
end.


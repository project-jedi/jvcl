{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOLBar.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Outlook style control. Simpler than TJvLookout) }
unit JvOLBar;

{  Hierarcy:
    TJvCustomOutlookBar
      Pages:TJvOutlookBarPages
        Page:TJvOutlookBarPage
          Buttons:TJvOutlookBarButtons
            Button:TJvOutlookbarButton
}

interface
uses
  Windows, Messages, SysUtils, Classes, Controls,
  Buttons, Graphics, ImgList, Forms, StdCtrls,JvComponent;

const
  CM_CAPTION_EDITING = CM_BASE + 756;
  CM_CAPTION_EDIT_ACCEPT = CM_CAPTION_EDITING + 1;
  CM_CAPTION_EDIT_CANCEL = CM_CAPTION_EDITING + 2;

type
  TJvBarButtonSize = (olbsLarge,olbsSmall);
  TJvOutlookbarButton = class(TCollectionItem)
  private
    FImageIndex: integer;
    FCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    procedure SetImageIndex(const Value: integer);
    procedure Change;
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(Collection:TCollection);override;
    procedure Assign(Source:TPersistent);override;
    procedure EditCaption;
  published
    property Caption:TCaption read FCaption write SetCaption;
    property ImageIndex:integer read FImageIndex write SetImageIndex;
  end;

  TJvOutlookBarButtons = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvOutlookbarButton;
    procedure SetItem(Index: Integer; const Value: TJvOutlookbarButton);
  protected
    function GetOwner:TPersistent;override;
    procedure Update(Item: TCollectionItem);override;
  public
    constructor Create(AOwner:TPersistent);
    function Add:TJvOutlookbarButton;
    procedure Assign(Source:TPersistent);override;
    function Insert(Index: Integer): TJvOutlookbarButton;
    property Items[Index: Integer]: TJvOutlookbarButton read GetItem write SetItem;default;
  end;

  TJvOutlookBarPage = class(TCollectionItem)
  private
    FImage: TBitmap;
    FCaption: TCaption;
    FColor: TColor;
    FButtonSize: TJvBarButtonSize;
    FParentButtonSize,FParentFont: boolean;

    FParentColor: boolean;
    FTopButtonIndex: integer;
    FButtons: TJvOutlookBarButtons;
    FFont: TFont;
    FImageIndex: integer;
    FAlignment: TAlignment;

    procedure SetButtonSize(const Value: TJvBarButtonSize);
    procedure SetCaption(const Value: TCaption);
    procedure SetColor(const Value: TColor);
    procedure SetImage(const Value: TBitmap);
    procedure Change;
    procedure SetParentButtonSize(const Value: boolean);
    procedure SetParentColor(const Value: boolean);
    procedure SetTopButtonIndex(const Value: integer);
    procedure SetButtons(const Value: TJvOutlookBarButtons);
    procedure SetParentFont(const Value: boolean);
    procedure SetFont(const Value: TFont);
    procedure SetImageIndex(const Value: integer);
    procedure SetAlignment(const Value: TAlignment);
  protected
    function GetDisplayName: String; override;
    { TODO: implement ImageIndex and Alignment }
    property ImageIndex:integer read FImageIndex write SetImageIndex default -1;
    property Alignment:TAlignment read FAlignment write SetAlignment default taCenter;
  public
    constructor Create(Collection:TCollection);override;
    destructor Destroy;override;
    procedure Assign(Source:TPersistent);override;
    procedure EditCaption;
  published
    property Buttons:TJvOutlookBarButtons read FButtons write SetButtons;
    property Caption:TCaption read FCaption write SetCaption;
    property Image:TBitmap read FImage write SetImage;

    property Color:TColor read FColor write SetColor;
    property Font:TFont read FFont write SetFont;
    property ButtonSize:TJvBarButtonSize read FButtonSize write SetButtonSize;
    property ParentButtonSize:boolean read FParentButtonSize write SetParentButtonSize default true;
    property ParentFont:boolean read FParentFont write SetParentFont default false;
    property ParentColor:boolean read FParentColor write SetParentColor;
    property TopButtonIndex:integer read FTopButtonIndex write SetTopButtonIndex;
  end;

  TJvOutlookBarPages = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvOutlookBarPage;
    procedure SetItem(Index: Integer; const Value: TJvOutlookBarPage);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner:TPersistent);
    function Add:TJvOutlookBarPage;
    function Insert(Index: Integer): TJvOutlookBarPage;
    procedure Assign(Source:TPersistent);override;
    property Items[Index: Integer]: TJvOutlookBarPage read GetItem write SetItem;default;
  end;

  TOutlookBarPageChanging = procedure (Sender:TObject;Index:integer;var AllowChange:boolean) of object;
  TOutlookBarPageChange   = procedure (Sender:TObject;Index:integer) of object;
  TOutlookBarButtonClick  = procedure (Sender:TObject; Index:integer) of object;
  TOutlookBarEditCaption  = procedure (Sender:TObject;var NewText:string;Index:integer;var Allow:boolean) of object;

  TJvCustomOutlookBar = class(TJvCustomControl)
  private
    FTopButton,FBtmButton:TSpeedButton;
    FPages: TJvOutlookBarPages;
    FLargeChangeLink, FSmallChangeLink:TChangeLink;
    FActivePageIndex: integer;
    FButtonSize: TJvBarButtonSize;
    FSmallImages: TImageList;
    FLargeImages: TImageList;
    FPageButtonHeight: integer;
    FBorderStyle: TBorderStyle;
    FNextActivePage,FPressedPageBtn:integer;
    FOnPageChange: TOutlookBarPageChange;
    FOnPageChanging: TOutlookBarPageChanging;
    FButtonRect:TRect;
    FLastButtonIndex,FPressedButtonIndex:integer;
    FOnButtonClick: TOutlookBarButtonClick;
    FPopUpObject: TObject;
    FEdit:TCustomEdit;
    FOnEditButton: TOutlookBarEditCaption;
    FOnEditPage: TOutlookBarEditCaption;
    procedure SetPages(const Value: TJvOutlookBarPages);
    procedure DoChangeLinkChange(Sender: TObject);
    procedure SetActivePageIndex(const Value: integer);
    procedure SetButtonSize(const Value: TJvBarButtonSize);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetLargeImages(const Value: TImageList);
    procedure SetSmallImages(const Value: TImageList);
    procedure SetPageButtonHeight(const Value: integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    function DrawTopPages: integer;
    procedure DrawCurrentPage(PageIndex:integer);
    procedure DrawPageButton(R:TRect; Pressed:boolean);
    procedure DrawBottomPages(StartIndex: integer);
    procedure DrawButtons(Index: integer);
    procedure DrawArrowButtons(Index: integer);
    procedure DrawButtonFrame(PageIndex, ButtonIndex, PressedIndex: integer);
    function DrawBitmap(R: TRect; Bmp: TBitmap): boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure DoDwnClick(Sender: TObject);
    procedure DoUpClick(Sender: TObject);
    procedure RedrawRect(R:TRect;Erase:boolean=false);
    procedure CMCaptionEditing(var Message:TMessage);message CM_CAPTION_EDITING;
    procedure CMCaptionEditAccept(var Message:TMessage);message CM_CAPTION_EDIT_ACCEPT;
    procedure CMCaptionEditCancel(var Message:TMessage);message CM_CAPTION_EDIT_CANCEL;
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure DoButtonEdit(NewText: string; B: TJvOutlookbarButton);
    procedure DoPageEdit(NewText: string; P: TJvOutlookBarPage);
    function getActivePage: TJvOutlookBarPage;
    function GetActivePageIndex: integer;
  protected
    procedure CreateParams(var Params: TCreateParams);override;
    function getButtonHeight(PageIndex:integer):integer;
    function getButtonFrameRect(PageIndex, ButtonIndex: integer): TRect;
    function getButtonTextRect(PageIndex, ButtonIndex: integer): TRect;
    function getButtonRect(PageIndex,ButtonIndex:integer):TRect;
    function getPageButtonRect(Index:integer):TRect;
    function getPageTextRect(Index:integer):TRect;
    function getPageRect(Index:integer):TRect;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Paint;override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;

    function DoPageChanging(Index: integer): boolean;virtual;
    procedure DoPageChange(Index: integer);virtual;
    procedure DoButtonClick(Index:integer);virtual;
    procedure DoContextPopup(MousePos: TPoint; var Handled: boolean); override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    function getButtonAtPos(P:TPoint):TJvOutlookbarButton;
    function getPageButtonAtPos(P: TPoint): TJvOutlookBarPage;
  protected
    property PopUpObject:TObject read FPopUpObject write FPopUpObject;
    property Width default 100;
    property Height default 220;
    property TopButton:TSpeedButton read FTopButton;
    property BtmButton:TSpeedButton read FBtmButton;
    property BorderStyle:TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Font:TFont read GetFont write SetFont;
    property Color:TColor read GetColor write SetColor default clBtnShadow;
    property Pages:TJvOutlookBarPages read FPages write SetPages;
    property LargeImages:TImageList read FLargeImages write SetLargeImages;
    property SmallImages:TImageList read FSmallImages write SetSmallImages;
    property ButtonSize:TJvBarButtonSize read FButtonSize write SetButtonSize default olbsLarge;
    property PageButtonHeight:integer read FPageButtonHeight write SetPageButtonHeight default 19;
    property ActivePageIndex:integer read GetActivePageIndex write SetActivePageIndex default 0;
    property OnPageChanging:TOutlookBarPageChanging read FOnPageChanging write FOnPageChanging;
    property OnPageChange:TOutlookBarPageChange read FOnPageChange write FOnPageChange;
    property OnButtonClick:TOutlookBarButtonClick read FOnButtonClick write FOnButtonClick;
    property OnEditButton:TOutlookBarEditCaption read FOnEditButton write FOnEditButton;
    property OnEditPage:TOutlookBarEditCaption read FOnEditPage write FOnEditPage;
  public
    property ActivePage:TJvOutlookBarPage read getActivePage;
  end;

  TJvOutlookBar = class(TJvCustomOutlookBar)
  public
    property PopupObject;
  published
    property Align;
    property Pages;
    property LargeImages;
    property SmallImages;
    property ButtonSize;
    property PageButtonHeight;
    property ActivePageIndex;
    property OnButtonClick;
    property OnEditButton;
    property OnPageChange;
    property OnPageChanging;
    property OnEditPage;

    { republished properties }
    property Action;
    property Anchors;
    property BiDiMode;
    property ParentBiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Height;
    property HelpContext;
    //PRY 2002.06.04
    {$IFDEF COMPILER6_UP}
    property HelpKeyword;
    property HelpType;
    {$ENDIF COMPILER6_UP}
    // PRY END
    property Hint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Width;

    property OnClick;
    property OnDblClick;
    property OnContextPopUp;
  end;

implementation
uses
  ExtCtrls;

{$R JvOUTLOOKBARRES.RES}

const
  cButtonLeftOffset = 4;
  cButtonTopOffset = 4;


function Max(Val1,Val2:integer):integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;


{ TJvOutlookBarEdit }

type
  TJvOutlookBarEdit = class(TCustomEdit)
  private
    FCanvas:TControlCanvas;
    procedure WMNCPaint(var Message:TMessage);message WM_NCPAINT;
    procedure EditAccept;
    procedure EditCancel;
    function GetCanvas: TCanvas;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState);override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
       X, Y: Integer);override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor CreateInternal(AOwner:TComponent; AParent:TWinControl;AObject:TObject);
    destructor Destroy;override;
    procedure ShowEdit(const AText:string;R:TRect);
    property Canvas:TCanvas read GetCanvas;
  end;

{ TJvOutlookbarButton }

procedure TJvOutlookbarButton.Assign(Source: TPersistent);
begin
  if Source is TJvOutlookbarButton then
  begin
    Caption := TJvOutlookbarButton(Source).Caption;
    ImageIndex := TJvOutlookbarButton(Source).ImageIndex;
    Change;
    Exit;
  end;
  inherited;
end;

procedure TJvOutlookbarButton.Change;
begin
// PRY 2002.06.04
  //if (Collection <> nil) and (Collection.Owner <> nil) and
  //  (TCollectionItem(Collection.Owner).Collection <> nil)
  //   and (TCustomControl(TCollectionItem(Collection.Owner).Collection.Owner) <> nil) then
  //     TCustomControl(TCollectionItem(Collection.Owner).Collection.Owner).Invalidate;
  if (Collection <> nil) and (TJvOutlookbarButtons(Collection).GetOwner <> nil) and
    (TCollectionItem(TJvOutlookbarButtons(Collection).GetOwner).Collection <> nil)
     and (TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookbarButtons(Collection).GetOwner).Collection).GetOwner) <> nil) then
       TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookbarButtons(Collection).GetOwner).Collection).GetOwner).Invalidate;
// PRY END
end;

constructor TJvOutlookbarButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

procedure TJvOutlookbarButton.EditCaption;
begin
  // PRY 2002.06.04
  //SendMessage(TCustomControl(TCollectionItem(Collection.Owner).Collection.Owner).Handle,
  SendMessage(TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookbarButtons(Collection).GetOwner).Collection).GetOwner).Handle,
     CM_CAPTION_EDITING,integer(self),0);
end;

function TJvOutlookbarButton.GetDisplayName: String;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

procedure TJvOutlookbarButton.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Change;
  end;
end;

procedure TJvOutlookbarButton.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

{ TJvOutlookBarButtons }

function TJvOutlookBarButtons.Add: TJvOutlookbarButton;
begin
  Result := TJvOutlookbarButton(inherited Add);
end;

procedure TJvOutlookBarButtons.Assign(Source: TPersistent);
var i:integer;
begin
  if Source is TJvOutlookBarButtons then
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to TJvOutlookBarButtons(Source).Count - 1 do
        Add.Assign(TJvOutlookBarButtons(Source)[i]);
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited;
end;

constructor TJvOutlookBarButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TJvOutlookbarButton);
end;

function TJvOutlookBarButtons.GetItem(Index: Integer): TJvOutlookbarButton;
begin
  Result := TJvOutlookbarButton(inherited Items[Index]);
end;

function TJvOutlookBarButtons.GetOwner: TPersistent;
begin
  Result := inherited getOwner;
end;

function TJvOutlookBarButtons.Insert(Index: Integer): TJvOutlookbarButton;
begin
  Result := TJvOutlookbarButton(inherited Insert(Index));
end;

procedure TJvOutlookBarButtons.SetItem(Index: Integer;
  const Value: TJvOutlookbarButton);
begin
  inherited Items[Index] := Value;
end;

procedure TJvOutlookBarButtons.Update(Item: TCollectionItem);
begin
  inherited;
  if (GetOwner <> nil) then
    TJvOutlookBarPage(GetOwner).Changed(false);
end;

{ TJvOutlookBarPage }

procedure TJvOutlookBarPage.Assign(Source: TPersistent);
begin
  if Source is TJvOutlookBarPage then
  begin
    Caption           := TJvOutlookBarPage(Source).Caption;
    Image             := TJvOutlookBarPage(Source).Image;
    Color             := TJvOutlookBarPage(Source).Color;
    ButtonSize        := TJvOutlookBarPage(Source).ButtonSize;
    ParentButtonSize  := TJvOutlookBarPage(Source).ParentButtonSize;
    ParentColor       := TJvOutlookBarPage(Source).ParentColor;
    Change;
    Exit;
  end;
  inherited;
end;

procedure TJvOutlookBarPage.Change;
begin
  if (Collection <> nil) and (TJvOutlookBarPages(Collection).UpdateCount = 0) then
    TJvOutlookBarPages(Collection).Update(self);
end;

constructor TJvOutlookBarPage.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FButtons := TJvOutlookBarButtons.Create(self);
  FFont := TFont.Create;
  FParentColor := true;
  FParentFont := true;
  FImage := TBitmap.Create;
  FAlignment := taCenter;
  FImageIndex := -1;
  //PRY 2002.06.04
  //if (Collection <> nil) and (Collection.Owner <> nil) then
  if (Collection <> nil) and (TJvOutlookBarPages(Collection).GetOwner <> nil) then
  begin
    //PRY 2002.06.04
    //FButtonSize := TJvCustomOutlookBar(Collection.Owner).ButtonSize;
    FButtonSize := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).GetOwner).ButtonSize;
    //PRY 2002.06.04
    //FColor      := TJvCustomOutlookBar(Collection.Owner).Color;
    FColor      := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).GetOwner).Color;
    //PRY 2002.06.04
    //Font        := TJvCustomOutlookBar(Collection.Owner).Font;
    Font        := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).GetOwner).Font;
  end
  else
  begin
    FButtonSize := olbsLarge;
    FColor      := clGray;
  end;
  FFont.Color   := clWhite;
  FParentButtonSize := true;
end;

destructor TJvOutlookBarPage.Destroy;
begin
  FButtons.Free;
  FImage.Free;
  FFont.Free;
  inherited;
end;

procedure TJvOutlookBarPage.SetTopButtonIndex(const Value: integer);
begin
  if (FTopButtonIndex <> Value) and (FTopButtonIndex >= 0) and (FTopButtonIndex < Buttons.Count) then
  begin
    FTopButtonIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetButtons(const Value: TJvOutlookBarButtons);
begin
  FButtons.Assign(Value);
  Change;
end;

procedure TJvOutlookBarPage.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetButtonSize(const Value: TJvBarButtonSize);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    //PRY 2002.06.04
    //if not (csReading in TComponent(Collection.Owner).ComponentState) then
    if not (csReading in TComponent(TJvOutlookBarPages(Collection).GetOwner).ComponentState) then
      FParentButtonSize := false;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FParentColor := false;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FParentFont := false;
  Change;
end;

procedure TJvOutlookBarPage.SetImage(const Value: TBitmap);
begin
  FImage.Assign(Value);
  Change;
end;

procedure TJvOutlookBarPage.SetParentButtonSize(const Value: boolean);
begin
  if FParentButtonSize <> Value then
  begin
    FParentButtonSize := Value;
    if Value then
    begin
      //PRY 2002.06.04
      //FButtonSize := (Collection.Owner as TJvCustomOutlookBar).ButtonSize;
      FButtonSize := (TJvOutlookBarPages(Collection).GetOwner as TJvCustomOutlookBar).ButtonSize;
      Change;
    end;
  end;
end;

procedure TJvOutlookBarPage.SetParentColor(const Value: boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then
    begin
      //PRY 2002.06.04
      //FColor := (Collection.Owner as TJvCustomOutlookBar).Color;
      FColor := (TJvOutlookBarPages(Collection).GetOwner as TJvCustomOutlookBar).Color;
      Change;
    end;
  end;
end;

procedure TJvOutlookBarPage.SetParentFont(const Value: boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    if Value then
    begin
      //PRY 2002.06.04
      //FFont.Assign((Collection.Owner as TJvCustomOutlookBar).Font);
      FFont.Assign((TJvOutlookBarPages(Collection).GetOwner as TJvCustomOutlookBar).Font);
      Change;
    end;
  end;
end;

procedure TJvOutlookBarPage.EditCaption;
begin
  //PRY 2002.06.04
  //SendMessage(TCustomControl(Collection.Owner).Handle, CM_CAPTION_EDITING,integer(self),1);
  SendMessage(TCustomControl(TJvOutlookBarPages(Collection).GetOwner).Handle, CM_CAPTION_EDITING,integer(self),1);
end;

function TJvOutlookBarPage.GetDisplayName: String;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

procedure TJvOutlookBarPage.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Change;
  end;
end;

{ TJvOutlookBarPages }

function TJvOutlookBarPages.Add: TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Add);
end;

procedure TJvOutlookBarPages.Assign(Source: TPersistent);
var i:integer;
begin
  if Source is TJvOutlookBarPages then
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to TJvOutlookBarPages(Source).Count - 1 do
        Add.Assign(TJvOutlookBarPages(Source)[i]);
    finally
      EndUpdate
    end;
    Exit;
  end;
  inherited;
end;

constructor TJvOutlookBarPages.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TJvOutlookBarPage);
end;

function TJvOutlookBarPages.GetItem(Index: Integer): TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Items[Index]);
end;

function TJvOutlookBarPages.Insert(Index: Integer): TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Insert(Index));
end;

procedure TJvOutlookBarPages.SetItem(Index: Integer;
  const Value: TJvOutlookBarPage);
begin
  inherited Items[Index] := Value;
end;

procedure TJvOutlookBarPages.Update(Item: TCollectionItem);
begin
  inherited;
  //PRY 2002.06.04
  //if (Owner <> nil) then
  if (GetOwner <> nil) then
  //PRY END
//    TJvCustomOutlookBar(Owner).RedrawPage(TJvOutlookBarPage(Item));
  //PRY 2002.06.04
    //TJvCustomOutlookBar(Owner).Repaint;
    TJvCustomOutlookBar(GetOwner).Repaint;
end;

{ TJvCustomOutlookBar }

procedure TJvCustomOutlookBar.DoDwnClick(Sender:TObject);
begin
  if Pages[ActivePageIndex].TopButtonIndex < Pages[ActivePageIndex].Buttons.Count then
    Pages[ActivePageIndex].TopButtonIndex := Pages[ActivePageIndex].TopButtonIndex + 1;
end;

procedure TJvCustomOutlookBar.DoUpClick(Sender:TObject);
begin
  if Pages[ActivePageIndex].TopButtonIndex > 0 then
    Pages[ActivePageIndex].TopButtonIndex := Pages[ActivePageIndex].TopButtonIndex - 1;
end;

constructor TJvCustomOutlookBar.Create(AOwner: TComponent);
var bmp:TBitmap;
begin
  inherited;
  DoubleBuffered := true;
  ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
  bmp := Tbitmap.Create;
  try
    FTopButton := TSpeedButton.Create(self);
    with FTopButton do
    begin
      Parent := self;
      Visible := false;
      bmp.LoadFromResourceName(hInstance,'UPARROW');
      Glyph := bmp;
      OnClick := DoUpClick;
      if csDesigning in ComponentState then
        Top := -1000;
    end;

    FBtmButton := TSpeedButton.Create(self);
    with FBtmButton do
    begin
      Parent := self;
      Visible := false;
      bmp.LoadFromResourceName(hInstance,'DWNARROW');
      Glyph := bmp;
      OnClick := DoDwnClick;
      if csDesigning in ComponentState then
        Top := -1000;
    end;
  finally
    bmp.Free;
  end;

  FPages := TJvOutlookBarPages.Create(self);
  FLargeChangeLink := TChangeLink.Create;
  FLargeChangeLink.OnChange := DoChangeLinkChange;
  FSmallChangeLink := TChangeLink.Create;
  FSmallChangeLink.OnChange := DoChangeLinkChange;
  FEdit := TJvOutlookBarEdit.CreateInternal(self,self,nil);
  FEdit.Top := -1000;
  //set up defaults
  Width := 100;
  Height := 220;
  Color := clBtnShadow;
  BorderStyle := bsSingle;
  ButtonSize := olbsLarge;
  PageButtonHeight := 19;

  FPressedPageBtn := -1;
  FNextActivePage  := -1;
  FLastButtonIndex  := -1;
  FPressedButtonIndex := -1;
  ActivePageIndex := 0;
end;

procedure TJvCustomOutlookBar.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end; 
end;

destructor TJvCustomOutlookBar.Destroy;
begin
  FEdit.Free;
  FLargeChangeLink.Free;
  FSmallChangeLink.Free;
  FPages.Free;
  inherited;
end;

procedure TJvCustomOutlookBar.DoChangeLinkChange(Sender:TObject);
begin
  Invalidate;
end;

function TJvCustomOutlookBar.GetColor: TColor;
begin
  Result := inherited Color;
end;

procedure TJvCustomOutlookBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if AComponent = FLargeImages then
      LargeImages := nil;
    if AComponent = FSmallImages then
      SmallImages := nil;
  end;
end;

procedure TJvCustomOutlookBar.DrawPageButton(R:TRect; Pressed:boolean);
begin
  if Pressed then
  begin
    if BorderStyle = bsNone then
    begin
      Frame3D(Canvas,R,clBtnShadow,clBtnHighlight,1);
    end
    else
    begin
      Frame3D(Canvas,R,cl3DDkShadow,clBtnHighlight,1);
      Frame3D(Canvas,R,clBtnShadow,clBtnFace,1);
    end;
  end
  else
  begin
    if BorderStyle = bsNone then
    begin
      Frame3D(Canvas,R,clBtnHighlight,clBtnShadow,1);
    end
    else
    begin
      Frame3D(Canvas,R,clBtnHighlight,cl3DDkShadow,1);
      Frame3D(Canvas,R,clBtnFace,clBtnShadow,1);
    end;
  end;
end;

function TJvCustomOutlookBar.DrawTopPages:integer;
var R:TRect;i:integer;
begin
  R := getPageButtonRect(0);
  for i := 0 to Pages.Count - 1 do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(R);
    DrawPageButton(R,FPressedPageBtn = i);
    OffsetRect(R,0,-1);
    SetBkMode(Canvas.Handle,TRANSPARENT);
    // TODO: add Pages[i].ImageIndex and Pages[i].Alignment to the equation
    DrawText(Canvas.Handle,PChar(Pages[i].Caption),-1,R,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    OffsetRect(R,0,PageButtonHeight + 1);
    if i >= ActivePageIndex then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := Pages.Count - 1;
end;

procedure TJvCustomOutlookBar.DrawButtons(Index:integer);
var i,H:integer;R,R2,R3:TRect;C:TColor;
begin
  if Pages[Index].Buttons.Count <= 0 then Exit;
  R2 := getPageRect(Index);
  R := getButtonRect(Index,Pages[Index].TopButtonIndex);
  H := getButtonHeight(Index);
  C := Canvas.Pen.Color;
  try
    Canvas.Font := Pages[Index].Font;
    Canvas.Brush.Style := bsClear;
    for i := Pages[Index].TopButtonIndex to Pages[Index].Buttons.Count - 1 do
    begin
//      Canvas.Rectangle(R);  // DEBUG
      case Pages[Index].ButtonSize of
        olbsLarge:
        begin
          if LargeImages <> nil then
            LargeImages.Draw(Canvas,R.Left + ((R.Right - R.Left)-LargeImages.Width) div 2,R.Top + 4,Pages[Index].Buttons[i].ImageIndex);
          R3 := getButtonTextRect(ActivePageIndex,i);
//          Canvas.Rectangle(R3);  // DEBUG
          DrawText(Canvas.Handle,PChar(Pages[Index].Buttons[i].Caption),-1,R3,
            DT_EXPANDTABS or DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
        end;
        olbsSmall:
        begin
          if SmallImages <> nil then
            SmallImages.Draw(Canvas,R.Left + 2,R.Top + 2,Pages[Index].Buttons[i].ImageIndex);
          R3 := getButtonTextRect(ActivePageIndex,i);
//          Canvas.Rectangle(R3);  // DEBUG
          DrawText(Canvas.Handle,PChar(Pages[Index].Buttons[i].Caption),-1,R3,
            DT_EXPANDTABS or DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_NOCLIP or DT_NOPREFIX);
        end;
      end;
      OffsetRect(R,0,H);
      if R.Top >= R2.Bottom then Break;
    end;
  finally
    Canvas.Font := Font;
    Canvas.Pen.Color := C;
  end;
end;

procedure TJvCustomOutlookBar.DrawArrowButtons(Index:integer);
var R:TRect;H:integer;
begin
  R := getPageRect(Index);
  H := getButtonHeight(Index);
  TopButton.Visible := (Pages.Count > 0) and (R.Top < R.Bottom - 20) and (Pages[Index].TopButtonIndex > 0);
  BtmButton.Visible := (Pages.Count > 0) and (R.Top < R.Bottom - 20) and (Pages[Index].Buttons.Count > 0) and
    (R.Bottom - R.Top < (Pages[Index].Buttons.Count - Pages[Index].TopButtonIndex) * H); // remove the last - H to show arrow
                                                                                             // button when the bottom of the last button is beneath the edge
  if TopButton.Visible then
    TopButton.SetBounds(ClientWidth - 20,R.Top + 4,16,16)
  else if csDesigning in ComponentState then
    TopButton.Top := -1000;
  if BtmButton.Visible then
    BtmButton.SetBounds(ClientWidth - 20,R.Bottom - 20,16,16)
  else if csDesigning in ComponentState then
    BtmButton.Top := -1000;
end;

function TJvCustomOutlookBar.DrawBitmap(R:TRect;Bmp:TBitmap):boolean;
begin
  Result := Assigned(Bmp) and not Bmp.Empty;
  if Result then
  begin
    Canvas.Brush.Bitmap := Bmp;
    Canvas.FillRect(R);
    Canvas.Brush.Bitmap := nil;
  end;
end;

procedure TJvCustomOutlookBar.DrawCurrentPage(PageIndex:integer);
var R:TRect;AColor:TColor;
begin
  if (PageIndex < 0) or (PageIndex >= Pages.Count) then Exit;
  R := getPageRect(PageIndex);
  AColor := Canvas.Brush.Color;
  try
    Canvas.Brush.Color := Pages[PageIndex].Color;
    if not DrawBitmap(R,Pages[PageIndex].Image) then
      Canvas.FillRect(R);
    DrawButtons(PageIndex);
  finally
    Canvas.Brush.Color := AColor;
    Canvas.Brush.Style := bsClear;
    SetBkMode(Canvas.Handle,TRANSPARENT);
  end;
  DrawArrowButtons(PageIndex);
  DrawButtonFrame(ActivePageIndex,FLastButtonIndex,FPressedButtonIndex);
end;

procedure TJvCustomOutlookBar.DrawBottomPages(StartIndex:integer);
var R:TRect;i:integer;
begin
  R := getPageButtonRect(Pages.Count - 1);
  for i := Pages.Count - 1 downto StartIndex do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(R);
    DrawPageButton(R,FPressedPageBtn = i);
    OffsetRect(R,0,-1);
    SetBkMode(Canvas.Handle,TRANSPARENT);
    DrawText(Canvas.Handle,PChar(Pages[i].Caption),-1,R,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    OffsetRect(R,0,-PageButtonHeight+1);
  end;
end;

function TJvCustomOutlookBar.getPageButtonAtPos(P:TPoint):TJvOutlookBarPage;
var i:integer;
begin
  // TODO: rewrite more optimal (no loop)
  for i := 0 to Pages.Count - 1 do
  begin
    if PtInRect(getPageButtonRect(i),P) then
    begin
      Result := Pages[i];
      Exit;
    end;
  end;
  Result := nil;
end;

function TJvCustomOutlookBar.getPageButtonRect(Index: integer): TRect;
begin
  Result := Rect(0,0,0,0);
  if (Index < 0) or (Index >= Pages.Count) then Exit;
  Result := Rect(0,0,ClientWidth,PageButtonHeight);
  if Index <= ActivePageIndex then
    OffsetRect(Result,0,PageButtonHeight * Index)
  else
    OffsetRect(Result,0,(ClientHeight - PageButtonHeight * (Pages.Count - Index)));
end;

function TJvCustomOutlookBar.getPageTextRect(Index: integer): TRect;
begin
  Result := getPageButtonRect(Index);
  InflateRect(Result,-2,-2);
end;

function TJvCustomOutlookBar.getPageRect(Index: integer): TRect;
begin
  Result := Rect(0,0,0,0);
  if (Index < 0) or (Index >= Pages.Count) then Exit;
  Result := Rect(0,PageButtonHeight * Index + PageButtonHeight,ClientWidth,ClientHeight - (Pages.Count - Index) * PageButtonHeight + PageButtonHeight);
end;

function TJvCustomOutlookBar.getButtonAtPos(P:TPoint): TJvOutlookbarButton;
var i,H:integer;R,B:TRect;
begin
  // this always returns the button in the visible part of the active page (if any)
  Result := nil;
  if (ActivePageIndex < 0) or (ActivePageIndex >= Pages.Count) then Exit;
  B := getButtonRect(ActivePageIndex,0);
  H := B.Bottom - B.Top;
  R := getPageRect(ActivePageIndex);
  for i := 0 to Pages[ActivePageIndex].Buttons.Count - 1 do
  begin
    if PtInRect(B,P) then
    begin
      Result := Pages[ActivePageIndex].Buttons[i];
      Exit;
    end;
    OffsetRect(B,0,H);
    if B.Top >= R.Bottom then Break;
  end;
end;

function TJvCustomOutlookBar.getButtonRect(PageIndex, ButtonIndex: integer): TRect;
var H:integer;
begin
  Result := Rect(0,0,0,0);
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < 0) or (ButtonIndex >= Pages[PageIndex].Buttons.Count) then Exit;
  H := getButtonHeight(PageIndex);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if LargeImages <> nil then
      begin
        Result := Rect(0,0,Max(LargeImages.Width,Canvas.TextWidth(Pages[PageIndex].Buttons[ButtonIndex].Caption)) + 4,H);
        OffsetRect(Result,(ClientWidth - (Result.Right - Result.Left)) div 2,cButtonTopOffset);
      end
      else
        Result := Rect(0,0,ClientWidth,cButtonTopOffset+H);
    olbsSmall:
      if SmallImages <> nil then
      begin
        Result := Rect(0,0,SmallImages.Width + Canvas.TextWidth(Pages[PageIndex].Buttons[ButtonIndex].Caption) + 8,H);
        OffsetRect(Result,cButtonLeftOffset,cButtonTopOffset);
      end
      else
        Result := Rect(0,0,ClientWidth,cButtonTopOffset+H);
  end;
  OffsetRect(Result,0,(ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + getPageRect(PageIndex).Top);
end;

function TJvCustomOutlookBar.getButtonFrameRect(PageIndex, ButtonIndex: integer): TRect;
var H:integer;
begin
  Result := Rect(0,0,0,0);
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < 0) or (ButtonIndex >= Pages[PageIndex].Buttons.Count) then Exit;
  H := getButtonHeight(PageIndex);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if LargeImages <> nil then
      begin
        Result := Rect(0,0,LargeImages.Width + 6,LargeImages.Height + 6);
        OffsetRect(Result,(ClientWidth - (Result.Right - Result.Left)) div 2,
          cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + getPageRect(PageIndex).Top + 1);
      end
      else
      begin
        Result := Rect(0,0,ClientWidth,H);
        OffsetRect(Result,0,
          cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + getPageRect(PageIndex).Top + 1);
      end;
    olbsSmall:
      if SmallImages <> nil then
      begin
        Result := Rect(0,0,SmallImages.Width + 4,SmallImages.Height + 4);
        OffsetRect(Result,cButtonLeftOffset,cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + getPageRect(PageIndex).Top);
      end
      else
      begin
        Result := Rect(0,0,ClientWidth,H);
        OffsetRect(Result,0,cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + getPageRect(PageIndex).Top);
      end;
  end;
end;

function TJvCustomOutlookBar.getButtonTextRect(PageIndex,
  ButtonIndex: integer): TRect;
var H:integer;
begin
  Result := Rect(0,0,0,0);
  if Pages[PageIndex].Buttons.Count <= ButtonIndex then Exit;
  Result := getButtonRect(PageIndex,ButtonIndex);
  H := getButtonHeight(PageIndex);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if LargeImages <> nil then
      begin
        Result.Top := Result.Bottom + Pages[PageIndex].Font.Height;
        OffsetRect(Result,0,-4);
      end;
    olbsSmall:
      if SmallImages <> nil then
      begin
        Result.Left := SmallImages.Width + 10;
        Result.Top := Result.Top + (getButtonHeight(PageIndex) + Pages[PageIndex].Font.Height) div 2;
        Result.Bottom := Result.Top - Pages[PageIndex].Font.Height + 2;
        Result.Right := Result.Left + Canvas.TextWidth(Pages[PageIndex].Buttons[ButtonIndex].Caption) + 4;
        OffsetRect(Result,0,-(H - (Result.Bottom - Result.Top)) div 4);
      end;
  end;
end;

procedure TJvCustomOutlookBar.Paint;
var i:integer;
begin
  inherited Paint;
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  SetBkMode(Canvas.Handle,TRANSPARENT);
  i := DrawTopPages;
  if i >= 0 then
    DrawCurrentPage(i);
  DrawBottomPages(i+1);
end;

function TJvCustomOutlookBar.DoPageChanging(Index:integer):boolean;
begin
  Result := true;
  if (Index > -1) and Assigned(FOnPageChanging) then
    FOnPageChanging(self,Index,Result);
end;

procedure TJvCustomOutlookBar.DoPageChange(Index:integer);
begin
  if (Index > -1) and Assigned(FOnPageChange) then
    FOnPageChange(self,Index);
end;

procedure TJvCustomOutlookBar.DoButtonClick(Index:integer);
begin
  if (Index > -1) and Assigned(FOnButtonClick) then
    FOnButtonClick(self,Index);
end;


procedure TJvCustomOutlookBar.SetActivePageIndex(const Value: integer);
begin
  if (Value >= 0) and (Value < FPages.Count)  then
  begin
    FPressedPageBtn := -1; // reset cache
    // remove old button info
    FLastButtonIndex := -1;
    FPressedButtonIndex := -1;
    FButtonRect := Rect(0,0,0,0);
    if (FActivePageIndex <> Value) then
    begin
      if not DoPageChanging(Value) then Exit;
      FActivePageIndex := Value;
      DoPageChange(Value);
    end;
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomOutlookBar.SetButtonSize(const Value: TJvBarButtonSize);
var i:integer;  
begin
  FButtonSize := Value;
  Pages.BeginUpdate;
  try
    for i := 0 to Pages.Count - 1 do
      if Pages[i].ParentButtonSize then
      begin
        Pages[i].ParentButtonSize := false;
        Pages[i].ParentButtonSize := true; // reset flag
      end;
  finally
    Pages.EndUpdate; // calls invalidate
  end;
end;

procedure TJvCustomOutlookBar.SetColor(const Value: TColor);
var i:integer;
begin
  if inherited Color <> Value then
  begin
    inherited Color := Value;
    for i := 0 to Pages.Count - 1 do
      if Pages[i].ParentColor then
      begin
        Pages[i].ParentColor := false;
        Pages[i].ParentColor := true; // reset flag
      end;
  end;
end;

function TJvCustomOutlookBar.GetFont: TFont;
begin
  Result := inherited Font;
end;

procedure TJvCustomOutlookBar.SetFont(const Value: TFont);
var i:integer;
begin
  inherited Font := Value;
  for i := 0 to Pages.Count - 1 do
    if Pages[i].ParentFont then
    begin
      Pages[i].ParentFont := false;
      Pages[i].ParentFont := true; // reset flag
    end;
end;

procedure TJvCustomOutlookBar.SetLargeImages(const Value: TImageList);
begin
  if FLargeImages <> Value then
  begin
    if Assigned(FLargeImages) then
      FLargeImages.UnRegisterChanges(FLargeChangeLink);
    FLargeImages := Value;
    if Assigned(FLargeImages) then
      FLargeImages.RegisterChanges(FLargeChangeLink);
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetPageButtonHeight(const Value: integer);
begin
  if FPageButtonHeight <> Value then
  begin
    FPageButtonHeight := Value;
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetPages(const Value: TJvOutlookBarPages);
begin
  FPages.Assign(Value); // Assign calls Invalidate
end;

procedure TJvCustomOutlookBar.SetSmallImages(const Value: TImageList);
begin
  if FSmallImages <> Value then
  begin
    if Assigned(FSmallImages) then
      FSmallImages.UnRegisterChanges(FSmallChangeLink);
    FSmallImages := Value;
    if Assigned(FSmallImages) then
      FSmallImages.RegisterChanges(FSmallChangeLink);
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.DrawButtonFrame(PageIndex, ButtonIndex, PressedIndex: integer);
var R:TRect;
begin
  if (ButtonIndex < 0) or (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < Pages[PageIndex].TopButtonIndex) then Exit;
  R := getButtonFrameRect(PageIndex,ButtonIndex);
  if PressedIndex = ButtonIndex then
    Frame3D(Canvas,R,clBlack,clWhite,1)
  else
    Frame3D(Canvas,R,clWhite,clBlack,1);
end;

procedure TJvCustomOutlookBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var P:TJvOutlookBarPage;B:TJvOutlookbarButton;
begin
  inherited;
  if Button = mbRight then Exit;
  P := getPageButtonAtPos(Point(X,Y));
  if (P <> nil) and (P.Index <> FNextActivePage) then
  begin
    FNextActivePage := P.Index;
    if FNextActivePage <> ActivePageIndex then
    begin // draw button pressed
      FPressedPageBtn := FNextActivePage;
      RedrawRect(getPageButtonRect(FNextActivePage));
    end;
    Exit;
  end
  else
  begin
    if FNextActivePage > -1 then
      RedrawRect(getPageButtonRect(FNextActivePage));
    FNextActivePage := -1;
    FPressedPageBtn := -1;
  end;
  B := getButtonAtPos(Point(X,Y));
  if B <> nil then
  begin
    FLastButtonIndex := B.Index;
    FPressedButtonIndex   := B.Index;
    FButtonRect     := getButtonFrameRect(ActivePageIndex,B.Index);
    RedrawRect(FButtonRect);
  end;
end;

procedure TJvCustomOutlookBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var P:TJvOutlookBarPage;B:TJvOutlookbarButton;R:TRect;
begin
  inherited;
  { TODO -oJv : 
1. check whether the mouse is down on a page button and whether the mouse has moved from
    the currently pressed page button }
  P := getPageButtonAtPos(Point(X,Y));
  if (FPressedPageBtn > -1) then
  begin
    if (P = nil) or (P.Index <> FPressedPageBtn) then
    begin
      R := getPageButtonRect(FPressedPageBtn);
      RedrawRect(R);
      FPressedPageBtn := -1;
    end;
  end
  else if (P <> nil) and (P.Index <> ActivePageIndex) then
  begin
    if (P.Index = FNextActivePage) then
    begin
      FPressedPageBtn := FNextActivePage;
      RedrawRect(getPageButtonRect(FPressedPageBtn));
      Exit;
    end;
  end;
  // TODO: check for button highlight
  B := getButtonAtPos(Point(X,Y));
  if (B <> nil) then
  begin
    if (B.Index <> FLastButtonIndex) then
    begin
      RedrawRect(FButtonRect,true);
      FButtonRect := getButtonFrameRect(ActivePageIndex,B.Index);
      RedrawRect(FButtonRect);
      FLastButtonIndex := B.Index;
    end;
  end
  else if (B = nil) then
  begin
    if (FLastButtonIndex > -1) then
      RedrawRect(FButtonRect);
    FLastButtonIndex := -1;
    FButtonRect := Rect(0,0,0,0);
  end;
end;

procedure TJvCustomOutlookBar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var P:TJvOutlookBarPage;B:TJvOutlookbarButton;
begin
  inherited;
  if Button = mbRight then Exit;
  if (FNextActivePage > -1) and (FNextActivePage <> ActivePageIndex) then
  begin
    P := getPageButtonAtPos(Point(X,Y));
    if (P <> nil) and (P.Index = FNextActivePage) then
      ActivePageIndex := FNextActivePage;
  end;
  FNextActivePage := -1;

  B := getButtonAtPos(Point(X,Y));
  if B <> nil then
  begin
    if B.Index = FPressedButtonIndex then
      DoButtonClick(FPressedButtonIndex);
    FLastButtonIndex := B.Index;
    FPressedButtonIndex   := -1;
    FButtonRect     := getButtonFrameRect(ActivePageIndex,FLastButtonIndex);
    RedrawRect(FButtonRect);
  end
  else
  begin
    FButtonRect     := getButtonFrameRect(ActivePageIndex,FLastButtonIndex);
    FLastButtonIndex := -1;
    FPressedButtonIndex   := -1;
    RedrawRect(FButtonRect);
  end;
end;

procedure TJvCustomOutlookBar.CMMouseEnter(var Message: TMessage);
begin
  RedrawRect(FButtonRect);
  inherited;
end;

procedure TJvCustomOutlookBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  RedrawRect(FButtonRect);
  FPressedPageBtn := -1;
  FLastButtonIndex := -1;
end;

function TJvCustomOutlookBar.getButtonHeight(PageIndex: integer): integer;
const
  cLargeOffset = 8;
  cSmallOffset = 4;
var  
  TM:TTextMetric;
begin
  GetTextMetrics(Canvas.Handle,TM);
  Result := TM.tmHeight + TM.tmExternalLeading;
  if (PageIndex >= 0) and (PageIndex < Pages.Count) then
  begin
    case Pages[PageIndex].ButtonSize of
      olbsLarge:
        if LargeImages <> nil then
          Result := Max(Result,LargeImages.Height - Pages[PageIndex].Font.Height + cLargeOffset)
        else
          Result := -Pages[PageIndex].Font.Height + cLargeOffset;
      olbsSmall:
        if SmallImages <> nil then
          Result := Max(SmallImages.Height,-Pages[PageIndex].Font.Height) + cSmallOffset
        else
          Result := -Pages[PageIndex].Font.Height + cSmallOffset;
    end;
  end;
  Inc(Result,4);
end;

procedure TJvCustomOutlookBar.WMEraseBkGnd(var Message: TMessage);
begin
  // don't redraw background: we always fill it anyway
  Message.Result := Ord(true);
end;

procedure TJvCustomOutlookBar.RedrawRect(R: TRect; Erase:boolean=false);
begin
  InvalidateRect(Handle,@R,Erase);
end;

procedure TJvCustomOutlookBar.CMCaptionEditing(var Message: TMessage);
function Max(Val1,Val2:integer):integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

function Min(Val1,Val2:integer):integer;
begin
  Result := Val1;
  if Val2 < Val1 then
    Result := Val2;
end;
var R:TRect;B:TJvOutlookbarButton;P:TJvOutlookBarPage;
begin
  TJvOutlookBarEdit(FEdit).Tag := Message.wParam;
//  TJvOutlookBarEdit(FEdit).Font.Name := Pages[ActivePageIndex].Font.Name;
//  TJvOutlookBarEdit(FEdit).Font.Size := Pages[ActivePageIndex].Font.Size;
  case Message.lParam of
    0: // button
    begin
      B := TJvOutlookbarButton(Message.wParam);
      R := getButtonTextRect(ActivePageIndex,B.Index);
      R.Left  := Max(R.Left,0);
      R.Right := Min(R.Right,ClientWidth);
      TJvOutlookBarEdit(FEdit).ShowEdit(B.Caption,R);
    end;
    1: // page
    begin
      P := TJvOutlookBarPage(Message.wParam);
      R := getPageTextRect(P.Index);
      TJvOutlookBarEdit(FEdit).ShowEdit(P.Caption,R);
    end;
  end;
end;

procedure TJvCustomOutlookBar.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var P:TPersistent;
begin
  P := getPageButtonAtPos(MousePos);
  if Assigned(P) then
    PopUpObject := P
  else
  begin
    P := getButtonAtPos(MousePos);
    if Assigned(P) then
      PopUpObject := P;
  end;
  if P = nil then
    PopUpObject := self;
  inherited;
end;


procedure TJvCustomOutlookBar.DoButtonEdit(NewText:string;B:TJvOutlookbarButton);
var Allow:boolean;
begin
  Allow := true;
  if Assigned(FOnEditButton) then
    FOnEditButton(self,NewText,B.Index,Allow);
  if Allow then
    B.Caption := NewText;
end;

procedure TJvCustomOutlookBar.DoPageEdit(NewText:string;P:TJvOutlookBarPage);
var Allow:boolean;
begin
  Allow := true;
  if Assigned(FOnEditPage) then
    FOnEditPage(self,NewText,P.Index,Allow);
  if Allow then
    P.Caption := NewText;
end;

procedure TJvCustomOutlookBar.CMCaptionEditAccept(var Message: TMessage);
begin
  with Message do
  begin
    if TObject(LParam) is TJvOutlookbarButton then
      DoButtonEdit(TJvOutlookBarEdit(WParam).Text,TJvOutlookbarButton(LParam))
    else if TObject(LParam) is TJvOutlookBarPage then
      DoPageEdit(TJvOutlookBarEdit(WParam).Text,TJvOutlookBarPage(LParam));
  end;
end;

procedure TJvCustomOutlookBar.CMCaptionEditCancel(var Message: TMessage);
begin
{  with Message do
  begin
    if TObject(LParam) is TJvOutlookbarButton then
      DoButtonEditCancel(TJvOutlookbarButton(LParam))
    else TObject(LParam) is TJvOutlookBarPage then
      DoPageEditCancel(TJvOutlookBarPage(LParam));
  end;
  }
end;


function TJvCustomOutlookBar.getActivePage: TJvOutlookBarPage;
begin
  if (ActivePageIndex > -1) and (ActivePageIndex < Pages.Count) then
    Result := Pages[ActivePageIndex]
  else 
    Result := nil;
end;

function TJvCustomOutlookBar.GetActivePageIndex: integer;
begin
  if (FActivePageIndex < 0) or (FActivePageIndex >= FPages.Count) then
    FActivePageIndex := 0;
  Result := FActivePageIndex;
end;

{ TJvOutlookBarEdit }

constructor TJvOutlookBarEdit.CreateInternal(AOwner: TComponent;
  AParent: TWinControl; AObject: TObject);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := self;
  AutoSize := true;
  Visible := false;
  Parent := AParent;
  BorderStyle := bsNone;
  ParentFont := false;
  Tag := integer(AObject);
end;

destructor TJvOutlookBarEdit.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TJvOutlookBarEdit.EditAccept;
begin
  Parent.Perform(CM_CAPTION_EDIT_ACCEPT,integer(self),Tag);
  Hide;
end;

procedure TJvOutlookBarEdit.EditCancel;
begin
  Parent.Perform(CM_CAPTION_EDIT_CANCEL,integer(self),Tag);
  Hide;
end;

function TJvOutlookBarEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvOutlookBarEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
    begin
      Key := 0;
      EditAccept;
      if Handle = GetCapture then
        ReleaseCapture;
//      Hide;
//      Free;
//      Screen.Cursor := crDefault;
    end;
    VK_ESCAPE:
    begin
      Key := 0;
      if Handle = GetCapture then
        ReleaseCapture;
      EditCancel;
//      Hide;
//      Free;
//      Screen.Cursor := crDefault;
    end;
  end; { case }
  inherited;
end;

procedure TJvOutlookBarEdit.KeyPress(var Key: Char);
begin
  if Key = #13 then Key := #0; // remove beep
  inherited;
end;

procedure TJvOutlookBarEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not PtInRect(ClientRect,Point(X,Y)) or ((Button = mbRight) and Visible) then
  begin
    if Handle = GetCapture then
      ReleaseCapture;
    EditCancel;
//    Screen.Cursor := crDefault;
//    FEdit.Hide;
//    FEdit.Free;
//    FEdit := nil;
  end
  else
  begin
    ReleaseCapture;
//    Screen.Cursor := crIBeam;
    SetCapture(Handle);
  end;
end;

procedure TJvOutlookBarEdit.ShowEdit(const AText: string; R: TRect);
begin
  Hide;
  Text := AText;
  SetBounds(R.Left,R.Top,R.Right-R.Left,R.Bottom-R.Top);
  Show;
  SetCapture(Handle);
  SelStart := 0;
  SelLength := Length(Text);
  SetFocus;
end;

procedure TJvOutlookBarEdit.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  RC, RW: TRect;
begin
  inherited;
  Exit;
  DC := GetWindowDC(Handle);
  try
    Canvas.Handle := DC;
    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);

    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);

    Canvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,Canvas.Brush.Handle);
    InflateRect(RW,-1,-1);

{    Canvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,Canvas.Brush.Handle);
    InflateRect(RW,-1,-1);

    Canvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,Canvas.Brush.Handle);
    InflateRect(RW,-1,-1); }

    { Erase parts not drawn }
    IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

end.

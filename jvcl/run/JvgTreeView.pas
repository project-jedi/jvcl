{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTreeView.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgTreeView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls, CommCtrl, Imglist, FlatSB,
  JvComponent,
  JvgTypes, JvgCommClasses;

const
  ITEM_ENABLED = integer(true);
  ITEM_DISABLED = integer(false);
  ncsUndefined = -1;
  ncsUnChecked = 1;
  ncsChecked = 2;
  ncsPartChecked = 3;

  TVS_NOTOOLTIPS = $0080;
  {$EXTERNALSYM TVS_NOTOOLTIPS}
  TVS_CHECKBOXES = $0100;
  {$EXTERNALSYM TVS_CHECKBOXES}
  TVS_TRACKSELECT = $0200;
  {$EXTERNALSYM TVS_TRACKSELECT}

type
  FOnTNDrawEvent = procedure(Sender: TObject; Message: TWMPaint) of object;

  TJvgCustomTreeView = class(TJvCustomTreeView)
  private
    FCanvas: TControlCanvas;
    FWallpaper: TBitmap;
    FBoldSelection: boolean;
    FBevel: TJvgBevelOptions;
    FHotTrack: Boolean;
    FCheckboxes: Boolean;
    FToolTips: Boolean;
    FMask: TBitmap;
    FGradient: TJvgGradient;
    FOnDraw: FOnTNDrawEvent;
    FOnEndEdit: TNotifyEvent;
    fPaintingNow: boolean;
    FOptions: TglTreeViewOptions;
    isEditing_: boolean;
    function GetCanvas: TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetWallpaper(Value: TBitmap);
    function GetWallpaper: TBitmap;
    procedure SetBoldSelection(Value: boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetCheckboxes(Value: Boolean);
    procedure SetToolTips(Value: Boolean);
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message
      CM_DESIGNHITTEST;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateFlatScrollBar; virtual;
    property Canvas: TCanvas read GetCanvas;
  public
    fDefaultPainting: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetNodeBoldState(Node: TTreeNode; fBold: boolean);
    procedure SetNodeState_(ItemID: HTreeItem; stateMask, State: UINT);
  protected
    property Wallpaper: TBitmap read GetWallpaper write SetWallpaper;
    property Options: TglTreeViewOptions read FOptions write FOptions;
    property BoldSelection: boolean read FBoldSelection write
      SetBoldSelection;
    property BevelSelection: TJvgBevelOptions read FBevel write FBevel;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property CheckBoxes: Boolean read FCheckboxes write SetCheckboxes default
      False;
    property ToolTips: Boolean read FToolTips write SetToolTips default True;
    property OnDraw: FOnTNDrawEvent read FOnDraw write FOnDraw;
    property OnEndEdit: TNotifyEvent read FOnEndEdit write FOnEndEdit;
  end;

  TJvgTreeView = class(TJvgCustomTreeView)
  published
    property ShowButtons;
    property BorderStyle;
    property DragCursor;
    property ShowLines;
    property ShowRoot;
    property ReadOnly;
    property RightClickSelect;
    property DragMode;
    property HideSelection;
    property Indent;
    property Items;
    property OnEditing;
    property OnEdited;
    property OnExpanding;
    property OnExpanded;
    property OnCollapsing;
    property OnCompare;
    property OnCollapsed;
    property OnChanging;
    property OnChange;
    property OnDeletion;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property Align;
    property Enabled;
    property Font;
    property Color;
    property ParentColor default False;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PopupMenu;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Images;
    property StateImages;
    property Wallpaper;
    property Options;
    property BoldSelection;
    property BevelSelection;
    property HotTrack;
    property CheckBoxes;
    property ToolTips;
    property OnDraw;
    property OnEndEdit;
  end;

  TJvgCheckTreeView = class(TJvgCustomTreeView)
  private
    FOnChangeCheck: TTVChangedEvent;
    FGlyphChecked: TBitmap;
    FGlyphUnChecked: TBitmap;
    FGlyphSemiChecked: TBitmap;
    FCheckKind: TglCheckKind;
    FChecksScheme: integer;
    FCheckStateInheritance: boolean;
    FCheckImageList: TImageList;

    procedure SetGlyphChecked(Value: TBitmap);
    procedure SetGlyphUnChecked(Value: TBitmap);
    procedure SetGlyphSemiChecked(Value: TBitmap);
    procedure SetChecksScheme(Value: integer);
    procedure SetChecksBitmap;
    function TestChildCheckState(Node: TTreeNode): integer;
    procedure TVMInsertItem(var Message: TMessage); message TVM_INSERTITEM;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message
      WM_LBUTTONDOWN;
  protected
    procedure Loaded; override;
  public
    procedure Assign(Source: TPersistent); override;
    function CheckedItem: TTreeNode;
    function Checked(Node: TTreeNode): boolean;
    procedure SetChecked(Node: TTreeNode; Value: boolean);
    procedure SetStateIndex(Node: TTreeNode; StateIndex: integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnChangeCheck: TTVChangedEvent read FOnChangeCheck write
      FOnChangeCheck;
    property GlyphChecked: TBitmap read FGlyphChecked write SetGlyphChecked;
    property GlyphUnChecked: TBitmap read FGlyphUnChecked write
      SetGlyphUnChecked;
    property GlyphSemiChecked: TBitmap read FGlyphSemiChecked write
      SetGlyphSemiChecked;
    property CheckKind: TglCheckKind read FCheckKind write FCheckKind default
      fckCheckBoxes;
    property ChecksScheme: integer read FChecksScheme write SetChecksScheme;
    property CheckStateInheritance: boolean
      read FCheckStateInheritance write FCheckStateInheritance default false;
  published
    property ShowButtons;
    property BorderStyle;
    property DragCursor;
    property ShowLines;
    property ShowRoot;
    property ReadOnly;
    property RightClickSelect;
    property DragMode;
    property HideSelection;
    property Indent;
    property Items;
    property OnEditing;
    property OnEdited;
    property OnExpanding;
    property OnExpanded;
    property OnCollapsing;
    property OnCompare;
    property OnCollapsed;
    property OnChanging;
    property OnChange;
    property OnDeletion;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property Align;
    property Enabled;
    property Font;
    property Color;
    property ParentColor default False;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PopupMenu;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Images;
    property Wallpaper;
    property Options;
    property BoldSelection;
    property BevelSelection;
    property HotTrack;
    property CheckBoxes;
    property ToolTips;
    property OnDraw;
    property OnEndEdit;
  end;

implementation

uses
  Math,
  JvgUtils;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvgTreeView.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvgTreeView.res}
{$ENDIF LINUX}

constructor TJvgCustomTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self; //...i can draw now! :)
  FBevel := TJvgBevelOptions.Create;
  FHotTrack := False;
  FCheckboxes := False;
  FToolTips := True;
  FMask := TBitmap.Create;
  FGradient := TJvgGradient.Create;
  if csDesigning in ComponentState then
    FWallpaper := TBitmap.Create;
end;

destructor TJvgCustomTreeView.Destroy;
begin
  // (ahuser) moved inherted to top otherwise it raises an AV
  inherited Destroy;
  FCanvas.Free;
  FBevel.Free;
  FWallpaper.Free;
  FMask.Free;
  FGradient.Free;
end;

procedure TJvgCustomTreeView.Assign(Source: TPersistent);
begin
  if Source is TJvgCustomTreeView then
  begin
    Width := TJvgCustomTreeView(Source).Width;
    Height := TJvgCustomTreeView(Source).Height;
    Align := TJvgCustomTreeView(Source).Align;
    Parent := TJvgCustomTreeView(Source).Parent;
    Items.Assign(TJvgCustomTreeView(Source).Items);
  end;
end;

procedure TJvgCustomTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if FHotTrack then
      Style := Style or TVS_TRACKSELECT;
    if FCheckBoxes then
      Style := Style or TVS_CHECKBOXES;
    if not FToolTips then
      Style := Style or TVS_NOTOOLTIPS;
  end;
end;

function TJvgCustomTreeView.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvgCustomTreeView.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  if htOnButton in GetHitTestInfoAt(Message.XPos, Message.YPos) then
    Message.Result := 1 //...expanding in Design Time
  else
    Message.Result := 0;
end;

procedure TJvgCustomTreeView.UpdateFlatScrollBar;
var
  MinPos, MaxPos: integer;
begin
  try
    InitializeFlatSB(Handle);
    FlatSB_SetScrollProp(handle, WSB_PROP_VSTYLE, FSB_ENCARTA_MODE, false);
    FlatSB_SetScrollProp(handle, WSB_PROP_HSTYLE, FSB_ENCARTA_MODE, false);

    GetScrollRange(Handle, SB_VERT, MinPos, MaxPos);

    FlatSB_SetScrollRange(handle, 1, MinPos, MaxPos, false);
  except
  end;
end;

procedure TJvgCustomTreeView.WMPaint(var Message: TWMPaint);
var
  x, y, IWidth, IHeight: integer;
  r: TRect;
  Bmp, Bmp2: TBitmap;
  mask, OldMask: HBITMAP;
  MaskDC: HDC;
  OldBkColor: COLORREF;

  procedure DrawBevel;
  var
    R: TRect;
  begin
    if Assigned(Selected) then
    begin
      R := Selected.DisplayRect(true);
      dec(R.Right);
      dec(r.Bottom);
      DrawBoxEx(FCanvas.Handle, r, FBevel.Sides, FBevel.Inner, FBevel.Outer,
        FBevel.Bold, 0, true);
    end;
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  try
    if not Assigned(FWallpaper) or (FWallpaper.handle = 0) then
    begin
      inherited;
      if Assigned(FOnDraw) then
        FOnDraw(Self, Message);
      exit;
    end;

    fPaintingNow := true;
    r := GetClientRect;
    IWidth := r.right - r.left;
    IHeight := r.bottom - r.top;
    Bmp := TBitmap.Create;
    Bmp2 := TBitmap.Create;
    mask := CreateBitmap(IWidth, IHeight, 1, 1, nil);
    MaskDC := CreateCompatibleDC(FCanvas.handle);
    OldMask := SelectObject(MaskDC, Mask);

    bmp.Width := IWidth;
    bmp.Height := IHeight;
    bmp2.Width := IWidth;
    bmp2.Height := IHeight;
    x := 0;
    y := 0;
    while x < r.Right - r.Left do
    begin
      while y < IHeight do
      begin
        BitBlt(bmp.Canvas.Handle, x, y, FWallpaper.Width, FWallpaper.Height,
          FWallpaper.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(y, FWallpaper.Height);
      end;
      Inc(x, FWallpaper.Width);
      y := 0;
    end;

    inherited;
    DrawBevel;
    //  BitBlt( bmp.Canvas.Handle, 0, 0, IWidth, IHeight, FWallpaper.Canvas.Handle, 0,0, SRCCOPY );
      //SetBkMode(FCanvas.handle,TRANSPARENT);
      //InvalidateRect(handle,@r,false);
    //  inherited;
    //  bmp2.Width := Width; bmp2.Height := Height;
    //  Self.PaintWindow(bmp2.Canvas.Handle);
    //exit;
    BitBlt(bmp2.Canvas.Handle, 0, 0, IWidth, IHeight, FCanvas.handle, 0, 0,
      SRCCOPY);

    OldBkColor := SetBkColor(bmp2.Canvas.handle, ColorToRGB(Color));
    BitBlt(MaskDC, 0, 0, IWidth, IHeight, bmp2.Canvas.handle, 0, 0, SRCCOPY);
    SetBkColor(bmp2.Canvas.handle, OldBkColor);

    BitBlt(bmp.Canvas.Handle, 0, 0, IWidth, IHeight, MaskDC, 0, 0, SRCAND);

    {put mask on FCanvas to change background color to white}
    if Color <> clWhite then
      BitBlt(bmp2.Canvas.Handle, 0, 0, IWidth, IHeight, MaskDC, 0, 0,
        SRCPAINT);

    BitBlt(MaskDC, 0, 0, IWidth, IHeight, MaskDC, 0, 0, NOTSRCCOPY);
    BitBlt(bmp.Canvas.Handle, 0, 0, IWidth, IHeight, MaskDC, 0, 0, SRCPAINT);

    BitBlt(bmp2.Canvas.handle, 0, 0, IWidth, IHeight, bmp.Canvas.Handle, 0, 0,
      SRCAND);
    BitBlt(FCanvas.handle, 0, 0, IWidth, IHeight, bmp2.Canvas.Handle, 0, 0,
      SRCCOPY);
    DeleteObject(SelectObject(MaskDC, OldMask));
    DeleteDC(MaskDC);
    Bmp.free;
    Bmp2.free;
    fPaintingNow := false;
    if Assigned(FOnDraw) then
      FOnDraw(Self, Message);

  finally
    if (ftvFlatScroll in Options) and not IsEditing_ then
      UpdateFlatScrollBar();
  end;
end;

procedure TJvgCustomTreeView.CNNotify(var Message: TWMNotify);
begin
  with Message.NMHdr^ do
    case code of
      TVN_ENDLABELEDIT:
        begin
          if Assigned(OnEndEdit) then
            OnEndEdit(Self);
          isEditing_ := false;
        end;
      TVN_SELCHANGING:
        with PNMTreeView(Pointer(Message.NMHdr))^ do
        begin
          if FBoldSelection then
          begin
            SetNodeState_(itemOld.hItem, TVIS_BOLD, 0);
            SetNodeState_(itemNew.hItem, TVIS_BOLD, TVIS_BOLD);
          end;
          inherited;
          exit;
        end;
      TVN_BEGINLABELEDIT:
        isEditing_ := true;
      //      TVN_SELCHANGED:
      //        with PNMTreeView(Pointer(Message.NMHdr))^ do
      //          Change(GetNodeFromItem(itemNew));
    end;
  inherited;
end;

procedure TJvgCustomTreeView.SetNodeBoldState(Node: TTreeNode; fBold: boolean);
begin
  if fBold then
    SetNodeState_(Node.ItemID, TVIS_BOLD, TVIS_BOLD)
  else
    SetNodeState_(Node.ItemID, TVIS_BOLD, 0)
end;

procedure TJvgCustomTreeView.SetNodeState_(ItemID: HTreeItem;
  stateMask, State: UINT);
var
  tvi: TTVItem;
begin
  FillChar(tvi, SizeOf(tvi), 0);
  tvi.hItem := ItemID;
  tvi.mask := TVIF_STATE;
  tvi.stateMask := stateMask; //TVIS_DROPHILITED, TVIS_BOLD;
  tvi.state := State;
  TreeView_SetItem(Handle, tvi);
end;

procedure TJvgCustomTreeView.SetCheckboxes(Value: Boolean);
begin
  if FCheckboxes = Value then
    exit;
  FCheckboxes := Value;
  RecreateWnd;
end;

procedure TJvgCustomTreeView.SetHotTrack(Value: Boolean);
begin
  if FHotTrack = Value then
    exit;
  FHotTrack := Value;
  RecreateWnd;
end;

procedure TJvgCustomTreeView.SetToolTips(Value: Boolean);
begin
  if FToolTips = Value then
    exit;
  FToolTips := Value;
  RecreateWnd;
end;

//---------------------------------------------propeties

procedure TJvgCustomTreeView.SetWallpaper(Value: TBitmap);
begin
  if Assigned(FWallpaper) then
    FWallpaper.Free;
  FWallpaper := TBitmap.Create;
  FWallpaper.Assign(Value);
  Invalidate;
end;

function TJvgCustomTreeView.GetWallpaper: TBitmap;
begin
  if not Assigned(FWallpaper) then
    FWallpaper := TBitmap.Create;
  Result := FWallpaper;
end;

procedure TJvgCustomTreeView.SetBoldSelection(Value: boolean);
begin
  FBoldSelection := Value;
  if Assigned(Selected) then
    SetNodeBoldState(Selected, FBoldSelection);
end;
//______________________________________________________TglCheckTreeView

constructor TJvgCheckTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphChecked := TBitmap.Create;
  FGlyphUnChecked := TBitmap.Create;
  FGlyphSemiChecked := TBitmap.Create;
  FCheckImageList := TImageList.Create(Self);
  StateImages := FCheckImageList;
  if csDesigning in ComponentState then
    ChecksScheme := 2; //...not FChecksScheme!
  FCheckStateInheritance := false;
end;

destructor TJvgCheckTreeView.Destroy;
begin
  FGlyphChecked.Free;
  FGlyphUnChecked.Free;
  FGlyphSemiChecked.Free;
  FCheckImageList.Free;
  FCheckImageList := nil;
  inherited Destroy;
end;

procedure TJvgCheckTreeView.Loaded;
var
  i: integer;
begin
  inherited;
  //  ChangeBitmapColor(FGlyphChecked, GetTransparentColor( FGlyphChecked, ftcLeftBottomPixel), Color);
  //  ChangeBitmapColor(FGlyphUnChecked, GetTransparentColor( FGlyphUnChecked, ftcLeftBottomPixel), Color);
  //  ChangeBitmapColor(FGlyphSemiChecked, GetTransparentColor( FGlyphSemiChecked, ftcLeftBottomPixel), Color);

  if ChecksScheme = -1 then
    SetChecksBitmap;
  if ChecksScheme = 0 then
    SetChecksScheme(2);

  if Items.Count > 0 then
    for i := 0 to Items.Count - 1 do
      Items[i].StateIndex := 1;
end;

procedure TJvgCheckTreeView.Assign(Source: TPersistent);
begin
  if Source is TJvgCustomTreeView then
    inherited Assign(Source);
  if Source is TJvgCheckTreeView then
  begin
    FOnChangeCheck := TJvgCheckTreeView(Source).OnChangeCheck;
    FGlyphChecked.Assign(TJvgCheckTreeView(Source).GlyphChecked);
    FGlyphUnChecked.Assign(TJvgCheckTreeView(Source).GlyphUnChecked);
    FChecksScheme := TJvgCheckTreeView(Source).ChecksScheme;
    FCheckStateInheritance := TJvgCheckTreeView(Source).CheckStateInheritance;
  end;
end;

procedure TJvgCheckTreeView.TVMInsertItem(var Message: TMessage);
var
  i: integer;
begin
  with TTVInsertStruct(pointer(Message.LParam)^).Item do
  begin
    mask := mask or TVIF_STATE or TVIF_HANDLE;
    stateMask := TVIS_STATEIMAGEMASK;
    state := IndexToStateImageMask(ncsUnChecked);
  end;
  inherited;
  if csDesigning in ComponentState then
    if Items.Count > 0 then
      for i := 0 to Items.Count - 1 do
        Items[i].StateIndex := 1;
end;

procedure TJvgCheckTreeView.WMLButtonDown(var Message: TWMLButtonDown);
var
  pt: TPoint;
  Node: TTreeNode;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  inherited;
  if htOnStateIcon in GetHitTestInfoAt(pt.x, pt.y) then
  begin
    if Selected.StateIndex = -1 then
      exit;
    if (Selected.StateIndex = ncsChecked) and (FCheckKind <> fckRadioButtons) then
      SetStateIndex(Selected, ncsUnChecked)
    else
      SetStateIndex(Selected, ncsChecked);

    if Assigned(FOnChangeCheck) then
      FOnChangeCheck(Self, Selected);
    if FCheckStateInheritance then
    begin
      if Assigned(Selected.Parent) then
        SetStateIndex(Selected.Parent,
          TestChildCheckState(Selected.Parent));
      Node := Selected;
      repeat
        Node := Node.GetNext;
        if Node = nil then
          break;
        if Selected.Level >= Node.Level then
          break;
        SetStateIndex(Node, Selected.StateIndex);
        if Assigned(FOnChangeCheck) then
          FOnChangeCheck(Self, Node);
      until Node = nil;
    end;
  end;
end;

function TJvgCheckTreeView.TestChildCheckState(Node: TTreeNode): integer;
var
  i, CheckedCount: integer;
  ChildNode: TTreeNode;
begin
  CheckedCount := 0;
  ChildNode := Node;
  for i := 1 to Node.Count do
  begin
    ChildNode := ChildNode.GetNext;
    if ChildNode.StateIndex = ncsChecked then
      inc(CheckedCount);
  end;

  if CheckedCount = 0 then
    Result := ncsUnChecked
  else
  if CheckedCount = Node.Count then
    Result := ncsChecked
  else
    Result := ncsPartChecked;

end;

procedure TJvgCheckTreeView.SetGlyphChecked(Value: TBitmap);
begin
  if not Assigned(Value) then
    exit;
  FGlyphChecked.Assign(Value);
  FChecksScheme := -1;
  SetChecksBitmap;
  Invalidate;
end;

procedure TJvgCheckTreeView.SetGlyphUnChecked(Value: TBitmap);
begin
  if not Assigned(Value) then
    exit;
  FGlyphUnChecked.Assign(Value);
  FChecksScheme := -1;
  SetChecksBitmap;
  Invalidate;
end;

procedure TJvgCheckTreeView.SetGlyphSemiChecked(Value: TBitmap);
begin
  if not Assigned(Value) then
    exit;
  FGlyphSemiChecked.Assign(Value);
  FChecksScheme := -1;
  SetChecksBitmap;
  Invalidate;
end;

procedure TJvgCheckTreeView.SetChecksScheme(Value: integer);
begin
  if FChecksScheme = Value then
    exit;
  FChecksScheme := Value;
  if ChecksScheme > -1 then
  begin
    StateImages.Clear;
    if StateImages.ResourceLoad(rtBitmap, 'CHECKS' + IntToStr(Value), clOlive) then
    begin
      StateImages.GetBitmap(1, FGlyphUnChecked);
      StateImages.GetBitmap(2, FGlyphChecked);
      StateImages.GetBitmap(3, FGlyphSemiChecked);
    end
    else
      SetChecksScheme(-1);
  end;
end;

function TJvgCheckTreeView.CheckedItem: TTreeNode;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Items.Count - 1 do
    if Items[i].StateIndex = ncsChecked then
    begin
      Result := Items[i];
      break;
    end;
end;

function TJvgCheckTreeView.Checked(Node: TTreeNode): boolean;
begin
  Result := false;
  if not Assigned(Node) then
    exit;
  Result := boolean(Node.StateIndex - 1);
end;

procedure TJvgCheckTreeView.SetChecked(Node: TTreeNode; Value: boolean);
begin
  if not Assigned(Node) then
    exit;
  if Node.StateIndex = integer(Value) + 1 then
    exit;
  SetStateIndex(Node, integer(Value) + 1);
  if Assigned(FOnChangeCheck) then
    FOnChangeCheck(Self, Node);
end;

procedure TJvgCheckTreeView.SetStateIndex(Node: TTreeNode; StateIndex: integer);
var
  i: integer;

  procedure SetState_(Node: TTreeNode; StateIndex: integer);
  var
    TextR: TRect;
  begin
    TextR := Node.DisplayRect(true);
    Node.StateIndex := StateIndex;
    ValidateRect(Handle, @TextR);
  end;
begin
  if FCheckKind = fckRadioButtons then
  begin
    if StateIndex = ncsChecked then
    begin
      for i := 0 to Items.Count - 1 do
      begin
        if (Items[i] <> Node) and (Items[i].StateIndex <> -1) then
          SetState_(Items[i], ncsUnChecked);
      end;
    end
    else
      SetState_(Node, StateIndex);
    if StateIndex <> ncsUnChecked then
      SetState_(Node, StateIndex);
  end
  else
    SetState_(Node, StateIndex);
  //  if StateIndex = ncsChecked then CheckedItem := Node;
end;
//______________________

procedure TJvgCheckTreeView.SetChecksBitmap;
begin
  if Assigned(FGlyphChecked) and (FGlyphChecked.handle <> 0) and
    Assigned(FGlyphUnChecked) and (FGlyphUnChecked.handle <> 0) and
    Assigned(StateImages) then
  begin
    StateImages.Clear;
    StateImages.Width := min(GlyphSemiChecked.Width, min(GlyphUnChecked.Width,
      GlyphChecked.Width));
    StateImages.Height := min(GlyphSemiChecked.Height,
      min(GlyphUnChecked.Height, GlyphChecked.Height));
    //    StateImages.AddMasked( FGlyphUnChecked, GetTransparentColor( FGlyphUnChecked, ftcLeftBottomPixel) );
    //    StateImages.AddMasked( FGlyphUnChecked, GetTransparentColor( FGlyphUnChecked, ftcLeftBottomPixel) );
    StateImages.AddMasked(FGlyphUnChecked,
      GetTransparentColor(FGlyphUnChecked, ftcLeftBottomPixel));
    StateImages.AddMasked(FGlyphUnChecked,
      GetTransparentColor(FGlyphUnChecked, ftcLeftBottomPixel));
    StateImages.AddMasked(FGlyphChecked, GetTransparentColor(FGlyphChecked,
      ftcLeftBottomPixel));
    StateImages.AddMasked(FGlyphSemiChecked,
      GetTransparentColor(FGlyphSemiChecked, ftcLeftBottomPixel));
  end;
end;

end.


{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTreeView.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgTreeView;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls, CommCtrl, ImgList, FlatSB,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses;

const
  ITEM_ENABLED = Ord(True);
  ITEM_DISABLED = Ord(False);
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
  FOnTNDrawEvent = procedure(Sender: TObject; Msg: TWMPaint) of object;

  {$IFDEF USEJVCL}
  TJvgCustomTreeView = class(TJvCustomTreeView)
  {$ELSE}
  TJvgCustomTreeView = class(TCustomTreeView)
  {$ENDIF USEJVCL}
  private
    FCanvas: TControlCanvas;
    FWallpaper: TBitmap;
    FBoldSelection: Boolean;
    FBevelSelection: TJvgBevelOptions;
    FHotTrack: Boolean;
    FCheckBoxes: Boolean;
    FToolTips: Boolean;
    FMask: TBitmap;
    FGradient: TJvgGradient;
    FOnDraw: FOnTNDrawEvent;
    FOnEndEdit: TNotifyEvent;
    FPaintingNow: Boolean;
    FOptions: TglTreeViewOptions;
    FIsEditing: Boolean;
    function GetCanvas: TCanvas;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetWallpaper(Value: TBitmap);
    function GetWallpaper: TBitmap;
    procedure SetBoldSelection(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetCheckBoxes(Value: Boolean);
    procedure SetToolTips(Value: Boolean);
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateFlatScrollBar; virtual;
    property Canvas: TCanvas read GetCanvas;
  public
    FDefaultPainting: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetNodeBoldState(Node: TTreeNode; ABold: Boolean);
    procedure SetNodeState_(ItemID: HTreeItem; StateMask, State: UINT);
  protected
    property Wallpaper: TBitmap read GetWallpaper write SetWallpaper;
    property Options: TglTreeViewOptions read FOptions write FOptions;
    property BoldSelection: Boolean read FBoldSelection write SetBoldSelection;
    property BevelSelection: TJvgBevelOptions read FBevelSelection write FBevelSelection;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property CheckBoxes: Boolean read FCheckBoxes write SetCheckBoxes default False;
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
    FChecksScheme: Integer;
    FCheckStateInheritance: Boolean;
    FCheckImageList: TImageList;
    procedure SetGlyphChecked(Value: TBitmap);
    procedure SetGlyphUnChecked(Value: TBitmap);
    procedure SetGlyphSemiChecked(Value: TBitmap);
    procedure SetChecksScheme(Value: Integer);
    procedure SetChecksBitmap;
    function TestChildCheckState(Node: TTreeNode): Integer;
    procedure TVMInsertItem(var Msg: TMessage); message TVM_INSERTITEM;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    procedure Loaded; override;
  public
    procedure Assign(Source: TPersistent); override;
    function CheckedItem: TTreeNode;
    function Checked(Node: TTreeNode): Boolean;
    procedure SetChecked(Node: TTreeNode; Value: Boolean);
    procedure SetStateIndex(Node: TTreeNode; StateIndex: Integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnChangeCheck: TTVChangedEvent read FOnChangeCheck write
      FOnChangeCheck;
    property GlyphChecked: TBitmap read FGlyphChecked write SetGlyphChecked;
    property GlyphUnChecked: TBitmap read FGlyphUnChecked write SetGlyphUnChecked;
    property GlyphSemiChecked: TBitmap read FGlyphSemiChecked write SetGlyphSemiChecked;
    property CheckKind: TglCheckKind read FCheckKind write FCheckKind default fckCheckBoxes;
    property ChecksScheme: Integer read FChecksScheme write SetChecksScheme;
    property CheckStateInheritance: Boolean read FCheckStateInheritance
      write FCheckStateInheritance default False;
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

//=== { TJvgCustomTreeView } =================================================

constructor TJvgCustomTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self; //...i can draw now! :)
  FBevelSelection := TJvgBevelOptions.Create;
  FHotTrack := False;
  FCheckBoxes := False;
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
  FBevelSelection.Free;
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
  end
  else
    inherited Assign(Source);
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

procedure TJvgCustomTreeView.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  //...expanding in Design Time
  Msg.Result := Ord(htOnButton in GetHitTestInfoAt(Msg.XPos, Msg.YPos));
end;

procedure TJvgCustomTreeView.UpdateFlatScrollBar;
var
  MinPos, MaxPos: Integer;
begin
  try
    InitializeFlatSB(Handle);
    FlatSB_SetScrollProp(Handle, WSB_PROP_VSTYLE, FSB_ENCARTA_MODE, False);
    FlatSB_SetScrollProp(Handle, WSB_PROP_HSTYLE, FSB_ENCARTA_MODE, False);

    GetScrollRange(Handle, SB_VERT, MinPos, MaxPos);

    FlatSB_SetScrollRange(Handle, 1, MinPos, MaxPos, False);
  except
  end;
end;

procedure TJvgCustomTreeView.WMPaint(var Msg: TWMPaint);
var
  X, Y, IWidth, IHeight: Integer;
  R: TRect;
  Bmp, Bmp2: TBitmap;
  Mask, OldMask: HBITMAP;
  MaskDC: HDC;
  OldBkColor: COLORREF;

  procedure DrawBevel;
  var
    R: TRect;
  begin
    if Assigned(Selected) then
    begin
      R := Selected.DisplayRect(True);
      Dec(R.Right);
      Dec(R.Bottom);
      DrawBoxEx(FCanvas.Handle, R, FBevelSelection.Sides, FBevelSelection.Inner,
        FBevelSelection.Outer, FBevelSelection.Bold, 0, True);
    end;
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  try
    if not Assigned(FWallpaper) or (FWallpaper.Handle = 0) then
    begin
      inherited;
      if Assigned(FOnDraw) then
        FOnDraw(Self, Msg);
      Exit;
    end;

    FPaintingNow := True;
    R := GetClientRect;
    IWidth := R.Right - R.Left;
    IHeight := R.Bottom - R.Top;
    Bmp := TBitmap.Create;
    Bmp2 := TBitmap.Create;
    Mask := CreateBitmap(IWidth, IHeight, 1, 1, nil);
    MaskDC := CreateCompatibleDC(FCanvas.Handle);
    OldMask := SelectObject(MaskDC, Mask);

    Bmp.Width := IWidth;
    Bmp.Height := IHeight;
    Bmp2.Width := IWidth;
    Bmp2.Height := IHeight;
    X := 0;
    Y := 0;
    while X < R.Right - R.Left do
    begin
      while Y < IHeight do
      begin
        BitBlt(Bmp.Canvas.Handle, X, Y, FWallpaper.Width, FWallpaper.Height,
          FWallpaper.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(Y, FWallpaper.Height);
      end;
      Inc(X, FWallpaper.Width);
      Y := 0;
    end;

    inherited;
    DrawBevel;
    //  BitBlt( Bmp.Canvas.Handle, 0, 0, IWidth, IHeight, FWallpaper.Canvas.Handle, 0,0, SRCCOPY );
      //SetBkMode(FCanvas.Handle,TRANSPARENT);
      //InvalidateRect(Handle,@R,False);
    //  inherited;
    //  Bmp2.Width := Width; Bmp2.Height := Height;
    //  Self.PaintWindow(Bmp2.Canvas.Handle);
    //Exit;
    BitBlt(Bmp2.Canvas.Handle, 0, 0, IWidth, IHeight, FCanvas.Handle, 0, 0,
      SRCCOPY);

    OldBkColor := SetBkColor(Bmp2.Canvas.Handle, ColorToRGB(Color));
    BitBlt(MaskDC, 0, 0, IWidth, IHeight, Bmp2.Canvas.Handle, 0, 0, SRCCOPY);
    SetBkColor(Bmp2.Canvas.Handle, OldBkColor);

    BitBlt(Bmp.Canvas.Handle, 0, 0, IWidth, IHeight, MaskDC, 0, 0, SRCAND);

    {put Mask on FCanvas to change background color to white}
    if Color <> clWhite then
      BitBlt(Bmp2.Canvas.Handle, 0, 0, IWidth, IHeight, MaskDC, 0, 0,
        SRCPAINT);

    BitBlt(MaskDC, 0, 0, IWidth, IHeight, MaskDC, 0, 0, NOTSRCCOPY);
    BitBlt(Bmp.Canvas.Handle, 0, 0, IWidth, IHeight, MaskDC, 0, 0, SRCPAINT);

    BitBlt(Bmp2.Canvas.Handle, 0, 0, IWidth, IHeight, Bmp.Canvas.Handle, 0, 0,
      SRCAND);
    BitBlt(FCanvas.Handle, 0, 0, IWidth, IHeight, Bmp2.Canvas.Handle, 0, 0,
      SRCCOPY);
    DeleteObject(SelectObject(MaskDC, OldMask));
    DeleteDC(MaskDC);
    Bmp.Free;
    Bmp2.Free;
    FPaintingNow := False;
    if Assigned(FOnDraw) then
      FOnDraw(Self, Msg);
  finally
    if (ftvFlatScroll in Options) and not FIsEditing then
      UpdateFlatScrollBar();
  end;
end;

procedure TJvgCustomTreeView.CNNotify(var Msg: TWMNotify);
begin
  with Msg.NMHdr^ do
    case code of
      TVN_ENDLABELEDIT:
        begin
          if Assigned(OnEndEdit) then
            OnEndEdit(Self);
          FIsEditing := False;
        end;
      TVN_SELCHANGING:
        with PNMTreeView(Pointer(Msg.NMHdr))^ do
        begin
          if FBoldSelection then
          begin
            SetNodeState_(itemOld.hItem, TVIS_BOLD, 0);
            SetNodeState_(itemNew.hItem, TVIS_BOLD, TVIS_BOLD);
          end;
          inherited;
          Exit;
        end;
      TVN_BEGINLABELEDIT:
        FIsEditing := True;
      //      TVN_SELCHANGED:
      //        with PNMTreeView(Pointer(Msg.NMHdr))^ do
      //          Change(GetNodeFromItem(itemNew));
    end;
  inherited;
end;

procedure TJvgCustomTreeView.SetNodeBoldState(Node: TTreeNode; ABold: Boolean);
begin
  if ABold then
    SetNodeState_(Node.ItemID, TVIS_BOLD, TVIS_BOLD)
  else
    SetNodeState_(Node.ItemID, TVIS_BOLD, 0)
end;

procedure TJvgCustomTreeView.SetNodeState_(ItemID: HTreeItem;
  StateMask, State: UINT);
var
  tvi: TTVItem;
begin
  FillChar(tvi, SizeOf(tvi), 0);
  tvi.hItem := ItemID;
  tvi.mask := TVIF_STATE;
  tvi.stateMask := StateMask; //TVIS_DROPHILITED, TVIS_BOLD;
  tvi.state := State;
  TreeView_SetItem(Handle, tvi);
end;

procedure TJvgCustomTreeView.SetCheckBoxes(Value: Boolean);
begin
  if FCheckBoxes = Value then
    Exit;
  FCheckBoxes := Value;
  RecreateWnd;
end;

procedure TJvgCustomTreeView.SetHotTrack(Value: Boolean);
begin
  if FHotTrack = Value then
    Exit;
  FHotTrack := Value;
  RecreateWnd;
end;

procedure TJvgCustomTreeView.SetToolTips(Value: Boolean);
begin
  if FToolTips = Value then
    Exit;
  FToolTips := Value;
  RecreateWnd;
end;

procedure TJvgCustomTreeView.SetWallpaper(Value: TBitmap);
begin
  if not Assigned(FWallpaper) then
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

procedure TJvgCustomTreeView.SetBoldSelection(Value: Boolean);
begin
  FBoldSelection := Value;
  if Assigned(Selected) then
    SetNodeBoldState(Selected, FBoldSelection);
end;

//=== { TJvgCheckTreeView } ==================================================

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
  FCheckStateInheritance := False;
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
  I: Integer;
begin
  inherited Loaded;
  //  ChangeBitmapColor(FGlyphChecked, GetTransparentColor( FGlyphChecked, ftcLeftBottomPixel), Color);
  //  ChangeBitmapColor(FGlyphUnChecked, GetTransparentColor( FGlyphUnChecked, ftcLeftBottomPixel), Color);
  //  ChangeBitmapColor(FGlyphSemiChecked, GetTransparentColor( FGlyphSemiChecked, ftcLeftBottomPixel), Color);

  if ChecksScheme = -1 then
    SetChecksBitmap;
  if ChecksScheme = 0 then
    SetChecksScheme(2);

  if Items.Count > 0 then
    for I := 0 to Items.Count - 1 do
      Items[I].StateIndex := 1;
end;

procedure TJvgCheckTreeView.Assign(Source: TPersistent);
begin
  if Source is TJvgCustomTreeView then
    inherited Assign(Source)
  else
  if Source is TJvgCheckTreeView then
  begin
    FOnChangeCheck := TJvgCheckTreeView(Source).OnChangeCheck;
    FGlyphChecked.Assign(TJvgCheckTreeView(Source).GlyphChecked);
    FGlyphUnChecked.Assign(TJvgCheckTreeView(Source).GlyphUnChecked);
    FChecksScheme := TJvgCheckTreeView(Source).ChecksScheme;
    FCheckStateInheritance := TJvgCheckTreeView(Source).CheckStateInheritance;
  end
  else
    inherited Assign(Source);
end;

procedure TJvgCheckTreeView.TVMInsertItem(var Msg: TMessage);
var
  I: Integer;
begin
  with TTVInsertStruct(Pointer(Msg.LParam)^).Item do
  begin
    mask := mask or TVIF_STATE or TVIF_HANDLE;
    stateMask := TVIS_STATEIMAGEMASK;
    state := IndexToStateImageMask(ncsUnChecked);
  end;
  inherited;
  if csDesigning in ComponentState then
    if Items.Count > 0 then
      for I := 0 to Items.Count - 1 do
        Items[I].StateIndex := 1;
end;

procedure TJvgCheckTreeView.WMLButtonDown(var Msg: TWMLButtonDown);
var
  Pt: TPoint;
  Node: TTreeNode;
begin
  GetCursorPos(Pt);
  Pt := ScreenToClient(Pt);
  inherited;
  if htOnStateIcon in GetHitTestInfoAt(Pt.X, Pt.Y) then
  begin
    if Selected.StateIndex = -1 then
      Exit;
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
          Break;
        if Selected.Level >= Node.Level then
          Break;
        SetStateIndex(Node, Selected.StateIndex);
        if Assigned(FOnChangeCheck) then
          FOnChangeCheck(Self, Node);
      until Node = nil;
    end;
  end;
end;

function TJvgCheckTreeView.TestChildCheckState(Node: TTreeNode): Integer;
var
  I, CheckedCount: Integer;
  ChildNode: TTreeNode;
begin
  CheckedCount := 0;
  ChildNode := Node;
  for I := 1 to Node.Count do
  begin
    ChildNode := ChildNode.GetNext;
    if ChildNode.StateIndex = ncsChecked then
      Inc(CheckedCount);
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
    Exit;
  FGlyphChecked.Assign(Value);
  FChecksScheme := -1;
  SetChecksBitmap;
  Invalidate;
end;

procedure TJvgCheckTreeView.SetGlyphUnChecked(Value: TBitmap);
begin
  if not Assigned(Value) then
    Exit;
  FGlyphUnChecked.Assign(Value);
  FChecksScheme := -1;
  SetChecksBitmap;
  Invalidate;
end;

procedure TJvgCheckTreeView.SetGlyphSemiChecked(Value: TBitmap);
begin
  if not Assigned(Value) then
    Exit;
  FGlyphSemiChecked.Assign(Value);
  FChecksScheme := -1;
  SetChecksBitmap;
  Invalidate;
end;

procedure TJvgCheckTreeView.SetChecksScheme(Value: Integer);
begin
  if FChecksScheme = Value then
    Exit;
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
  I: Integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
    if Items[I].StateIndex = ncsChecked then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJvgCheckTreeView.Checked(Node: TTreeNode): Boolean;
begin
  if Assigned(Node) then
    Result := (Node.StateIndex - 1) <> 0
  else
    Result := False;
end;

procedure TJvgCheckTreeView.SetChecked(Node: TTreeNode; Value: Boolean);
begin
  if not Assigned(Node) then
    Exit;
  if Node.StateIndex = Ord(Value) + 1 then
    Exit;
  SetStateIndex(Node, Ord(Value) + 1);
  if Assigned(FOnChangeCheck) then
    FOnChangeCheck(Self, Node);
end;

procedure TJvgCheckTreeView.SetStateIndex(Node: TTreeNode; StateIndex: Integer);
var
  I: Integer;

  procedure SetState_(Node: TTreeNode; StateIndex: Integer);
  var
    TextR: TRect;
  begin
    TextR := Node.DisplayRect(True);
    Node.StateIndex := StateIndex;
    ValidateRect(Handle, @TextR);
  end;

begin
  if FCheckKind = fckRadioButtons then
  begin
    if StateIndex = ncsChecked then
      for I := 0 to Items.Count - 1 do
        if (Items[I] <> Node) and (Items[I].StateIndex <> -1) then
          SetState_(Items[I], ncsUnChecked)
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
  if Assigned(FGlyphChecked) and (FGlyphChecked.Handle <> 0) and
    Assigned(FGlyphUnChecked) and (FGlyphUnChecked.Handle <> 0) and
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


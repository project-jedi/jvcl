{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QComboEdits.pas, released on 2004-01-19

The Initial Developer of the Original Code is Andreas Hausladen
                                              [Andreas dott Hausladen att gmx dott de]
Copyright (C) 2004 Andreas Hausladen
All Rights Reserved.

Contributor(s):

The TCustomComboEdit and TCustomComboMaskEdit controls allow the usage of an
edit rect and other client controls.

Usage:
  If you want to add a client control set the client control's parent to the
  ClientArea property.

Known Issues:
----------------------------------------------------------------------------}
// $Id$

unit QComboEdits;
interface
uses
  SysUtils, Classes,
  Qt, Types, QGraphics, QControls, QStdCtrls, QExtCtrls, QForms, QMask;

type
  TComboEditBorder = class(TPanel)
  private
    FEdit: TCustomEdit;
  protected
    procedure BoundsChanged; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Paint; override;
  end;

  TComboEditClientArea = class(TWidgetControl)
  private
    FEdit: TCustomEdit;
  protected
    procedure Click; override;
  end;

  IComboEditHelper = interface
    ['{1E70ED8C-5F1F-4693-BE86-EFEA7386332E}']
    function GetEditorRect: TRect;
    procedure SetEditorRect(Value: PRect);
    function GetClientArea: TComboEditClientArea;
    function GetFlat: Boolean;
  end;

  TCustomComboEdit = class(TCustomEdit, IComboEditHelper)
  private
    FBorder: TComboEditBorder;
    FClientArea: TComboEditClientArea;
    FUseEditRect: Boolean;
    FEditRect: TRect;
    FEditClientColor: Boolean;
    FFlat: Boolean;

    function GetBorderStyle: TControlBorderStyle;
    procedure SetBorderStyle(Value: TControlBorderStyle);
    function GetBevelInner: TPanelBevel;
    function GetBevelOuter: TPanelBevel;
    function GetBevelWidth: TBevelWidth;
    function GetBorderHandle: QWidgetH;
    procedure SetBevelInner(const Value: TPanelBevel);
    procedure SetBevelOuter(const Value: TPanelBevel);
    procedure SetBevelWidth(const Value: TBevelWidth);
    function GetClientColor: TColor;
    procedure SetClientColor(const Value: TColor);
    procedure SetEditClientColor(const Value: Boolean);
    function GetClientArea: TComboEditClientArea;
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
  protected
    procedure CreateWidget; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    function GetClientRect: TRect; override;
    procedure SetParent(const Value: TWidgetControl); override;
    procedure SetZOrder(TopMost: Boolean); override;

    procedure DoFlatChanged; virtual;
    procedure EnabledChanged; override;
    procedure CursorChanged; override;
    procedure VisibleChanged; override;
    procedure ColorChanged; override;
    procedure RequestAlign; override;

    procedure AdjustClientArea; virtual;

    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
  protected
    property ClientColor: TColor read GetClientColor write SetClientColor;
    property EditClientColor: Boolean read FEditClientColor write SetEditClientColor;

    property BevelInner: TPanelBevel read GetBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read GetBevelOuter write SetBevelOuter default bvRaised;
    property BevelWidth: TBevelWidth read GetBevelWidth write SetBevelWidth default 1;
    property BorderStyle: TControlBorderStyle read GetBorderStyle write SetBorderStyle stored False default bsSingle;
    property Flat: Boolean read GetFlat write SetFlat;

    property BorderHandle: QWidgetH read GetBorderHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetEditorRect: TRect; virtual;
    procedure SetEditorRect(Value: PRect); virtual;
     // SetEditorRect sets the rectangle for the editor relative to the Client
     // widget.
    property ClientArea: TComboEditClientArea read GetClientArea;
  end;

  TComboEdit = class(TCustomComboEdit)
  published
    property Flat default False;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property ClientColor default clBase;
    property EditClientColor default True;
  public
    property CursorPos;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property Alignment;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyString;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnReturnPressed;
    property OnStartDrag;
  end;

  TCustomComboMaskEdit = class(TCustomMaskEdit, IComboEditHelper)
  private
    FBorder: TComboEditBorder;
    FClientArea: TComboEditClientArea;
    FUseEditRect: Boolean;
    FEditRect: TRect;
    FEditClientColor: Boolean;
    FFlat: Boolean;

    function GetBorderStyle: TControlBorderStyle;
    procedure SetBorderStyle(Value: TControlBorderStyle);
    function GetBevelInner: TPanelBevel;
    function GetBevelOuter: TPanelBevel;
    function GetBevelWidth: TBevelWidth;
    function GetBorderHandle: QWidgetH;
    procedure SetBevelInner(const Value: TPanelBevel);
    procedure SetBevelOuter(const Value: TPanelBevel);
    procedure SetBevelWidth(const Value: TBevelWidth);
    function GetClientColor: TColor;
    procedure SetClientColor(const Value: TColor);
    procedure SetEditClientColor(const Value: Boolean);
    function GetClientArea: TComboEditClientArea;
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
  protected
    procedure CreateWidget; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    function GetClientRect: TRect; override;
    procedure SetParent(const Value: TWidgetControl); override;
    procedure SetZOrder(TopMost: Boolean); override;

    procedure DoFlatChanged; virtual;
    procedure EnabledChanged; override;
    procedure CursorChanged; override;
    procedure VisibleChanged; override;
    procedure ColorChanged; override;
    procedure RequestAlign; override;

    procedure AdjustClientArea; virtual;

    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
  protected
    property ClientColor: TColor read GetClientColor write SetClientColor;
    property EditClientColor: Boolean read FEditClientColor write SetEditClientColor;

    property BevelInner: TPanelBevel read GetBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read GetBevelOuter write SetBevelOuter default bvRaised;
    property BevelWidth: TBevelWidth read GetBevelWidth write SetBevelWidth default 1;
    property BorderStyle: TControlBorderStyle read GetBorderStyle write SetBorderStyle stored False default bsSingle;
    property Flat: Boolean read GetFlat write SetFlat;

    property BorderHandle: QWidgetH read GetBorderHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetEditorRect: TRect; virtual;
    procedure SetEditorRect(Value: PRect); virtual;
     // SetEditorRect sets the rectangle for the editor relative to the Client
     // widget.
    property ClientArea: TComboEditClientArea read GetClientArea;
  end;

  TComboMaskEdit = class(TCustomComboMaskEdit)
  published
    property Flat default False;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property ClientColor default clBase;
    property EditClientColor default True;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyString;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

{ TCustomComboEdit }

constructor TCustomComboEdit.Create(AOwner: TComponent);
begin
  FBorder := TComboEditBorder.Create(nil);
  FBorder.FEdit := Self;
  FClientArea := TComboEditClientArea.Create(nil);
  FClientArea.FEdit := Self;
  inherited Create(AOwner); // needs FBorder and FClientArea
  FWidth := 101;
  FHeight := 21;
  FBorder.Color := Color;
  FClientArea.Color := Color;
  FEditClientColor := True;
  BorderStyle := bsSingle;
  SetEditorRect(nil);
end;

destructor TCustomComboEdit.Destroy;
begin
  SetParent(nil); // prevent the release of needed controls
  inherited Destroy;
 // some methods in inherited Destroy may access the client area and border
  if not (csDestroying in FClientArea.ComponentState) then
    FClientArea.Free;
  if not (csDestroying in FBorder.ComponentState) then
    FBorder.Free;
end;

procedure TCustomComboEdit.CursorChanged;
begin
  FBorder.Cursor := Cursor;
  FClientArea.Cursor := Cursor;
  inherited CursorChanged;
end;

procedure TCustomComboEdit.EnabledChanged;
begin
  FBorder.Enabled := Enabled;
  FClientArea.Enabled := Enabled;
  inherited EnabledChanged;
end;

procedure TCustomComboEdit.VisibleChanged;
begin
  FBorder.Visible := Visible;
  FClientArea.Visible := Visible;
  inherited VisibleChanged;
end;

procedure TCustomComboEdit.ColorChanged;
begin
  inherited ColorChanged;
  if FEditClientColor then
    FClientArea.Color := Color;
end;

function TCustomComboEdit.GetClientColor: TColor;
begin
  Result := FClientArea.Color;
end;

procedure TCustomComboEdit.SetClientColor(const Value: TColor);
begin
  if Value <> ClientColor then
  begin
    FEditClientColor := False;
    FClientArea.Color := Value;
  end;
end;

procedure TCustomComboEdit.SetEditClientColor(const Value: Boolean);
begin
  FEditClientColor := Value;
  if FEditClientColor then
    ColorChanged;
end;

function TCustomComboEdit.GetBevelInner: TPanelBevel;
begin
  Result := FBorder.BevelInner;
end;

function TCustomComboEdit.GetBevelOuter: TPanelBevel;
begin
  Result := FBorder.BevelOuter;
end;

function TCustomComboEdit.GetBevelWidth: TBevelWidth;
begin
  Result := FBorder.BevelWidth;
end;

function TCustomComboEdit.GetBorderHandle: QWidgetH;
begin
  Result := FBorder.Handle;
end;

function TCustomComboEdit.GetBorderStyle: TControlBorderStyle;
begin
  Result := bsSingle;
  if FBorder.BevelInner = FBorder.BevelOuter then
  begin
    if FBorder.BevelInner = bvLowered then
      Result := bsSingle
    else if FBorder.BevelInner = bvNone then
      Result := bsNone;
  end;
  if FBorder.BorderWidth < 2 then
    Result := bsNone;
end;

procedure TCustomComboEdit.SetBevelInner(const Value: TPanelBevel);
begin
  FBorder.BevelInner := Value;
  AdjustClientArea;
end;

procedure TCustomComboEdit.SetBevelOuter(const Value: TPanelBevel);
begin
  FBorder.BevelOuter := Value;
  AdjustClientArea;
end;

procedure TCustomComboEdit.SetBevelWidth(const Value: TBevelWidth);
begin
  FBorder.BevelWidth := Value;
  AdjustClientArea;
end;

procedure TCustomComboEdit.SetBorderStyle(Value: TControlBorderStyle);
begin
  FBorder.BorderStyle := Value;
  if Value = bsNone then
    FBorder.BorderWidth := 0
  else
  if FFlat then
    FBorder.BorderWidth := 1
  else
    FBorder.BorderWidth := 2;
  AdjustClientArea;
end;

function TCustomComboEdit.GetFlat: Boolean;
begin
  Result := FFlat;
end;

procedure TCustomComboEdit.SetFlat(const Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    if FBorder.BorderStyle = bsSingle then
    begin
      if FFlat then
        FBorder.BorderWidth := 1
      else
        FBorder.BorderWidth := 2;
    end;
    FBorder.Invalidate;
    DoFlatChanged;
  end;
end;

function TCustomComboEdit.GetClientArea: TComboEditClientArea;
begin
  Result := FClientArea;
end;

function TCustomComboEdit.GetEditorRect: TRect;
begin
  Result := FEditRect;
end;

procedure TCustomComboEdit.SetEditorRect(Value: PRect);
begin
  FUseEditRect := Value <> nil;
  if FUseEditRect then
    FEditRect := Value^;
  AdjustClientArea;
end;

procedure TCustomComboEdit.AdjustClientArea;
var
  R: TRect;
begin
  if Parent <> nil then
  begin
    FClientArea.Align := alClient;
    if not FUseEditRect then
      FEditRect := FClientArea.ClientRect;

    R := FEditRect;
    with R do
    begin
     // Qt will move the Top value if the Height is too high.
      if Bottom > Top + ClientHeight then
        Dec(Top, Bottom - ClientHeight);
      QWidget_setGeometry(Handle, Left, Top, Right - Left, Bottom - Top);
    end;
  end;
end;

procedure TCustomComboEdit.AdjustClientRect(var Rect: TRect);
begin
 // act as the edit where the client area
  FClientArea.AdjustClientRect(Rect);
end;

function TCustomComboEdit.GetClientRect: TRect;
begin
  Result := FClientArea.ClientRect;
end;

procedure TCustomComboEdit.RequestAlign;
var
  Value: TAlign;
begin
 // redirect Align to FBorder. Align must be alNone before setting FBorder.Align
  Value := Align;
  Align := alNone;
  FBorder.Align := Value;
  AdjustClientArea;
end;

procedure TCustomComboEdit.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
begin
 // Do not use QWidget_setGeomentry because we only need the values but not the
 // widget at this position.
  R := BoundsRect;
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;

 // set the border panel's bounds rect to the requested rect
  if Parent <> nil then
  begin
    FBorder.SetBounds(Left, Top, Width, Height);
   // update because the above could be invalid
    FLeft := FBorder.Left;
    FTop := FBorder.Top;
    FWidth := FBorder.Width;
    FHeight := FBorder.Height;
  end;

  AdjustClientArea;
  if (R.Left <> FLeft) or (R.Top <> FTop) or
     (R.Right - R.Left <> FWidth) or (R.Bottom - R.Top <> FHeight) then
    BoundsChanged;
end;

procedure TCustomComboEdit.SetParent(const Value: TWidgetControl);
var
  Pt: TPoint;
  R: TRect;
begin
  if Value <> Parent then
  begin
    R := BoundsRect;
    try
      FBorder.Parent := Value;
      FClientArea.Parent := FBorder;

      inherited SetParent(Value);
      if Value <> nil then
      begin
        QWidget_pos(Handle, @Pt);
        QWidget_reparent(Handle, FClientArea.Handle, WidgetFlags, @Pt, True);
      end;
    finally
      BoundsRect := R; // calls AdjustClientArea
    end;
  end;
end;

procedure TCustomComboEdit.SetZOrder(TopMost: Boolean);
begin
  FBorder.SetZOrder(TopMost);
end;

function TCustomComboEdit.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
var
  AAnchors: TAnchors;
begin
 // redirect the anchors to FBorder
  if Anchors <> [akLeft, akTop] then
  begin
    AAnchors := Anchors;
    Anchors := [akLeft, akTop];
    if Assigned(FBorder) then
      if FBorder.Align = alNone then
        FBorder.Anchors := AAnchors;
  end;

 // ignore all CLX handling for reparent, move and resize events 
  Result := False;
  case QEvent_type(Event) of
    QEventType_Reparent,
    QEventType_Move,
    QEventType_Resize:
      Exit;
  end;
  Result := inherited EventFilter(Sender, Event);
end;

procedure TCustomComboEdit.CreateWidget;
begin
  inherited CreateWidget;
  inherited BorderStyle := bsNone;
  AdjustClientArea;
end;

procedure TCustomComboEdit.DoFlatChanged;
begin
end;

{ TCustomComboMaskEdit }

constructor TCustomComboMaskEdit.Create(AOwner: TComponent);
begin
  FBorder := TComboEditBorder.Create(nil);
  FBorder.FEdit := Self;
  FClientArea := TComboEditClientArea.Create(nil);
  FClientArea.FEdit := Self;
  inherited Create(AOwner); // needs FBorder and FClientArea
  FWidth := 101;
  FHeight := 21;
  FBorder.Color := Color;
  FClientArea.Color := Color;
  FEditClientColor := True;
  BorderStyle := bsSingle;
  SetEditorRect(nil);
end;

destructor TCustomComboMaskEdit.Destroy;
begin
  SetParent(nil); // prevent the release of needed controls
  inherited Destroy;
 // some methods in inherited Destroy may access the client area and border 
  if not (csDestroying in FClientArea.ComponentState) then
    FClientArea.Free;
  if not (csDestroying in FBorder.ComponentState) then
    FBorder.Free;
end;

procedure TCustomComboMaskEdit.CursorChanged;
begin
  FBorder.Cursor := Cursor;
  FClientArea.Cursor := Cursor;
  inherited CursorChanged;
end;

procedure TCustomComboMaskEdit.EnabledChanged;
begin
  FBorder.Enabled := Enabled;
  FClientArea.Enabled := Enabled;
  inherited EnabledChanged;
end;

procedure TCustomComboMaskEdit.VisibleChanged;
begin
  FBorder.Visible := Visible;
  FClientArea.Visible := Visible;
  inherited VisibleChanged;
end;

procedure TCustomComboMaskEdit.ColorChanged;
begin
  inherited ColorChanged;
  if FEditClientColor then
    FClientArea.Color := Color;
end;

function TCustomComboMaskEdit.GetClientColor: TColor;
begin
  Result := FClientArea.Color;
end;

procedure TCustomComboMaskEdit.SetClientColor(const Value: TColor);
begin
  if Value <> ClientColor then
  begin
    FEditClientColor := False;
    FClientArea.Color := Value;
  end;
end;

procedure TCustomComboMaskEdit.SetEditClientColor(const Value: Boolean);
begin
  FEditClientColor := Value;
  if FEditClientColor then
    ColorChanged;
end;

function TCustomComboMaskEdit.GetBevelInner: TPanelBevel;
begin
  Result := FBorder.BevelInner;
end;

function TCustomComboMaskEdit.GetBevelOuter: TPanelBevel;
begin
  Result := FBorder.BevelOuter;
end;

function TCustomComboMaskEdit.GetBevelWidth: TBevelWidth;
begin
  Result := FBorder.BevelWidth;
end;

function TCustomComboMaskEdit.GetBorderHandle: QWidgetH;
begin
  Result := FBorder.Handle;
end;

function TCustomComboMaskEdit.GetBorderStyle: TControlBorderStyle;
begin
  Result := bsSingle;
  if FBorder.BevelInner = FBorder.BevelOuter then
  begin
    if FBorder.BevelInner = bvLowered then
      Result := bsSingle
    else if FBorder.BevelInner = bvNone then
      Result := bsNone;
  end;
  if FBorder.BorderWidth < 2 then
    Result := bsNone;
end;

procedure TCustomComboMaskEdit.SetBevelInner(const Value: TPanelBevel);
begin
  FBorder.BevelInner := Value;
  AdjustClientArea;
end;

procedure TCustomComboMaskEdit.SetBevelOuter(const Value: TPanelBevel);
begin
  FBorder.BevelOuter := Value;
  AdjustClientArea;
end;

procedure TCustomComboMaskEdit.SetBevelWidth(const Value: TBevelWidth);
begin
  FBorder.BevelWidth := Value;
  AdjustClientArea;
end;

procedure TCustomComboMaskEdit.SetBorderStyle(Value: TControlBorderStyle);
begin
  FBorder.BorderStyle := Value;
  if Value = bsNone then
    FBorder.BorderWidth := 0
  else
  if FFlat then
    FBorder.BorderWidth := 1
  else
    FBorder.BorderWidth := 2;
  AdjustClientArea;
end;

function TCustomComboMaskEdit.GetFlat: Boolean;
begin
  Result := FFlat;
end;

procedure TCustomComboMaskEdit.SetFlat(const Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    if FBorder.BorderStyle = bsSingle then
    begin
      if FFlat then
        FBorder.BorderWidth := 1
      else
        FBorder.BorderWidth := 2;
    end;
    FBorder.Invalidate;
    DoFlatChanged;
  end;
end;

function TCustomComboMaskEdit.GetClientArea: TComboEditClientArea;
begin
  Result := FClientArea;
end;

function TCustomComboMaskEdit.GetEditorRect: TRect;
begin
  Result := FEditRect;
end;

procedure TCustomComboMaskEdit.SetEditorRect(Value: PRect);
begin
  FUseEditRect := Value <> nil;
  if FUseEditRect then
    FEditRect := Value^;
  AdjustClientArea;
end;

procedure TCustomComboMaskEdit.AdjustClientArea;
var
  R: TRect;
begin
  if Parent <> nil then
  begin
    FClientArea.Align := alClient;
    if not FUseEditRect then
      FEditRect := FClientArea.ClientRect;

    R := FEditRect;
    with R do
    begin
     // Qt will move the Top value if the Height is too high.
      if Bottom > Top + ClientHeight then
        Dec(Top, Bottom - ClientHeight);
      QWidget_setGeometry(Handle, Left, Top, Right - Left, Bottom - Top);
    end;
  end;
end;

procedure TCustomComboMaskEdit.AdjustClientRect(var Rect: TRect);
begin
 // act as the edit where the client area
  FClientArea.AdjustClientRect(Rect);
end;

function TCustomComboMaskEdit.GetClientRect: TRect;
begin
  Result := FClientArea.ClientRect;
end;

procedure TCustomComboMaskEdit.RequestAlign;
var
  Value: TAlign;
begin
 // redirect Align to FBorder. Align must be alNone before setting FBorder.Align
  Value := Align;
  Align := alNone;
  FBorder.Align := Value;
  AdjustClientArea;
end;

procedure TCustomComboMaskEdit.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
begin
 // Do not use QWidget_setGeomentry because we only need the values but not the
 // widget at this position.
  R := BoundsRect;
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;

 // set the border panel's bounds rect to the requested rect  
  if Parent <> nil then
  begin
    FBorder.SetBounds(Left, Top, Width, Height);
   // update because the above could be invalid 
    FLeft := FBorder.Left;
    FTop := FBorder.Top;
    FWidth := FBorder.Width;
    FHeight := FBorder.Height;
  end;

  AdjustClientArea;
  if (R.Left <> FLeft) or (R.Top <> FTop) or
     (R.Right - R.Left <> FWidth) or (R.Bottom - R.Top <> FHeight) then
    BoundsChanged;
end;

procedure TCustomComboMaskEdit.SetParent(const Value: TWidgetControl);
var
  Pt: TPoint;
  R: TRect;
begin
  if Value <> Parent then
  begin
    R := BoundsRect;
    try
      FBorder.Parent := Value;
      FClientArea.Parent := FBorder;

      inherited SetParent(Value);
      if Value <> nil then
      begin
        QWidget_pos(Handle, @Pt);
        QWidget_reparent(Handle, FClientArea.Handle, WidgetFlags, @Pt, True);
      end;
    finally
      BoundsRect := R; // calls AdjustClientArea
    end;
  end;
end;

procedure TCustomComboMaskEdit.SetZOrder(TopMost: Boolean);
begin
  FBorder.SetZOrder(TopMost);
end;

function TCustomComboMaskEdit.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
var
  AAnchors: TAnchors;
begin
 // redirect the anchors to FBorder
  if Anchors <> [akLeft, akTop] then
  begin
    AAnchors := Anchors;
    Anchors := [akLeft, akTop];
    if Assigned(FBorder) then
      if FBorder.Align = alNone then
        FBorder.Anchors := AAnchors;
  end;

 // ignore all CLX handling for reparent, move and resize events 
  Result := False;
  case QEvent_type(Event) of
    QEventType_Reparent,
    QEventType_Move,
    QEventType_Resize:
      Exit;
  end;
  Result := inherited EventFilter(Sender, Event);
end;

procedure TCustomComboMaskEdit.CreateWidget;
begin
  inherited CreateWidget;
  inherited BorderStyle := bsNone;
  AdjustClientArea;
end;

procedure TCustomComboMaskEdit.DoFlatChanged;
begin
end;

{ TEditBorder }

procedure TComboEditBorder.AdjustClientRect(var Rect: TRect);
var
  BevelSize: Integer;
begin
  inherited AdjustClientRect(Rect);

 // undo TCustomPanel changes
  InflateRect(Rect, BorderWidth, BorderWidth);
  BevelSize := 0;
  if BevelOuter <> bvNone then
    Inc(BevelSize, BevelWidth);
  if BevelInner <> bvNone then
    Inc(BevelSize, BevelWidth);
  InflateRect(Rect, BevelSize, BevelSize);

 // do out changes
  if BorderStyle = bsSingle then
    InflateRect(Rect, -BorderWidth, -BorderWidth)
  else
    InflateRect(Rect, -BevelSize, -BevelSize);
end;

procedure TComboEditBorder.BoundsChanged;
begin
  inherited BoundsChanged;
  if Assigned(FEdit) then
    if (FEdit.Left <> Left) or (FEdit.Top <> Top) or
       (FEdit.Width <> Width) or (FEdit.Height <> Height) then
      FEdit.SetBounds(Left, Top, Width, Height);
end;

procedure TComboEditBorder.Paint;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  if BorderStyle = bsSingle then
  begin
    if (FEdit as IComboEditHelper).GetFlat then
      Frame3D(Canvas, Rect, cl3DDkShadow, cl3DDkShadow, 1)
    else
    begin
      Canvas.Start;
      QStyle_drawPanel(QWidget_style(Handle), Canvas.Handle,
        Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
        QWidget_colorGroup(Handle), True,
        QStyle_defaultFrameWidth(QWidget_style(Handle)), nil);
      Canvas.Stop;
    end;
  end
  else
    inherited Paint;
end;

{ TComboEditClient }

procedure TComboEditClientArea.Click;
begin
  if Assigned(FEdit) and FEdit.CanFocus and FEdit.Showing then
    FEdit.SetFocus;
  inherited Click;
end;

end.


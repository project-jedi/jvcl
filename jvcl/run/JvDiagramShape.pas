{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDiagramShape.PAS, released on 2002-03-22.

Original Developer: Jim Cooper <jcooper att tabdee dott ltd dott uk>
Contributor(s): Michael Beck <mbeck1 att compuserve dott com>

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDiagramShape;

interface

uses
  Windows, 
  {$IFDEF VisualCLX}
  QTypes, 
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls, ExtCtrls, ImgList,
  JvComponent;

type
  TJvTextShape = class;

  // All controls descend from this, to help with streaming and unique naming
  TJvCustomDiagramShape = class(TJvGraphicControl)
  private
    FCanProcessMouseMsg: Boolean;
    FCaption: TJvTextShape;
    FSelected: Boolean;
    FWasCovered: Boolean;
    FMultiSelect: Boolean;
    FRightClickSelect: Boolean;
    FAlignment: TAlignment;
  protected
    procedure SetCaption(Value: TJvTextShape); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function GetCustomShapeAtPos(X, Y: Integer): TJvCustomDiagramShape;
    property CanProcessMouseMsg: Boolean read FCanProcessMouseMsg
      write FCanProcessMouseMsg;
    {$IFDEF VCL}
    procedure SetParent(AParent: TWinControl); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure SetParent(const AParent: TWidgetControl); override;
    {$ENDIF VisualCLX}
    procedure SetSelected(Value: Boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure AlignCaption(Alignment: TAlignment);
    // Class methods to save and load all TJvCustomDiagramShape components
    // that are children of a given control. They are class methods so that an
    // instance of TJvCustomDiagramShape is not required
    class procedure SaveToFile(const FileName: string; ParentControl: TWinControl);
    class procedure LoadFromFile(const FileName: string; ParentControl: TWinControl);
    class procedure DeleteAllShapes(ParentControl: TWinControl);
    class procedure DeleteSelectedShapes(ParentControl: TWinControl);
    class procedure UnselectAllShapes(ParentControl: TWinControl);
    class procedure SetMultiSelected(ParentControl: TWinControl; Value: Boolean);
    property Selected: Boolean read FSelected write SetSelected;
    property Caption: TJvTextShape read FCaption write SetCaption;
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default True;
    property OnDblClick;
  end;

  TJvMoveableShape = class(TJvCustomDiagramShape)
  private
    FOrigin: TPoint;
    FMoving: Boolean;
  protected
    procedure StartMove(X, Y: Integer);
    procedure Move(DeltaX, DeltaY: Integer);
    procedure EndMove;
    function ValidMove(DeltaX, DeltaY: Integer): Boolean;
    procedure MoveShapes(DeltaX, DeltaY: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    property Moving: Boolean read FMoving write FMoving;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property RightClickSelect;
    // Make these properties available
    property OnClick;
    property OnDblClick;
  end;

  TJvSizingMode = (smTopLeft, smTop, smTopRight, smLeft, smRight,
    smBottomLeft, smBottom, smBottomRight, smNone);

  TJvSizeableShape = class(TJvMoveableShape)
  private
    FSizingMode: TJvSizingMode;
    FSizeOrigin: TPoint;
    FSizeRectHeight: Integer;
    FSizeRectWidth: Integer;
    FMinHeight: Integer;
    FMinWidth: Integer;
  protected
    procedure SetSelected(Value: Boolean); override;
    procedure Paint; override;
    procedure DrawSizingRects;
    function GetSizeRect(SizeRectType: TJvSizingMode): TRect;
    procedure CheckForSizeRects(X, Y: Integer);
    procedure ResizeControl(X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    property SizingMode: TJvSizingMode read FSizingMode write FSizingMode;
    property SizeRectHeight: Integer read FSizeRectHeight write FSizeRectHeight;
    property SizeRectWidth: Integer read FSizeRectWidth write FSizeRectWidth;
    property MinHeight: Integer read FMinHeight write FMinHeight;
    property MinWidth: Integer read FMinWidth write FMinWidth;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  TJvTextShape = class(TJvSizeableShape)
  private
    FText: TCaption;
    FAutoSize: Boolean;
    FFont: TFont;
    {$IFDEF VCL}
    procedure SetText(const Value: TCaption);
    {$ENDIF VCL}
    procedure SetFont(Value: TFont);
    procedure FontChange(Sender: TObject);
  protected
    procedure SetAutoSize(Value: Boolean); {$IFDEF VCL} override; {$ENDIF}
    procedure RefreshText;
    {$IFDEF VCL}
    procedure SetParent(AParent: TWinControl); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure SetParent(const AParent: TWidgetControl); override;
    procedure SetText(const Value: TCaption); override;
    function GetText: TCaption; override;
    {$ENDIF VisualCLX}
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Text: TCaption read FText write SetText;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property Font: TFont read FFont write SetFont;
  end;

  TJvBitmapShape = class(TJvMoveableShape)
  private
    FImages: TImageList;
    FImageIndex: Integer;
    procedure SetImages(Value: TImageList);
    procedure SetImageIndex(Value: Integer);
  protected
    procedure SetSelected(Value: Boolean); override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Images: TImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    // Make these properties available
    property PopupMenu;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnClick;
    property OnDblClick;
  end;

  TJvStandardShape = class(TJvSizeableShape)
  private
    FShapeType: TShapeType;
    FLineColor: TColor;
    procedure SetShapeType(Value: TShapeType);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ShapeType: TShapeType read FShapeType write SetShapeType;
    // (rom) renamed from LineColour
    property LineColor: TColor read FLineColor write FLineColor default clBlack;
  end;

  TJvConnectionSide = (csLeft, csRight, csTop, csBottom);

  TJvConnection = class(TPersistent)
  private
    FShape: TJvCustomDiagramShape;
    FSide: TJvConnectionSide; // Side to connect to
    FOffset: Integer; // Distance from top or left of side
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    // Gets connection point in parent's coordinates
    function ConnPoint(TerminatorRect: TRect): TPoint;
    // Gets terminator connection point in parent's coordinates
    function TermPoint(TerminatorRect: TRect): TPoint;
    // Functions to get boundaries of the terminators
    function LeftMost(TerminatorRect: TRect): TPoint;
    function RightMost(TerminatorRect: TRect): TPoint;
    function TopMost(TerminatorRect: TRect): TPoint;
    function BottomMost(TerminatorRect: TRect): TPoint;
  published
    property Shape: TJvCustomDiagramShape read FShape write FShape;
    property Side: TJvConnectionSide read FSide write FSide;
    property Offset: Integer read FOffset write FOffset;
  end;

  TJvConnector = class(TJvCustomDiagramShape)
  private
    FLineWidth: Integer;
    FLineColor: TColor;
    // The shapes connected by this control
    FStartConn: TJvConnection;
    FEndConn: TJvConnection;
    // Area of the terminator symbol to be drawn (in horizontal position)
    FStartTermRect: TRect;
    FEndTermRect: TRect;
    // Used to track required movement of the caption
    FMidPoint: TPoint;
    procedure SetLineWidth(Value: Integer);
    function GetConn(Index: Integer): TJvConnection;
    procedure SetConn(Index: Integer; Value: TJvConnection);
    function GetTermRect(Index: Integer): TRect;
    procedure SetTermRect(Index: Integer; Value: TRect);
    procedure CheckSize(var AWidth, AHeight: Integer);
    function GetMidPoint: TPoint;
  protected
    procedure SetCaption(Value: TJvTextShape); override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // For drawing arrows etc. Called from Paint.
    procedure DrawStartTerminator; virtual;
    procedure DrawEndTerminator; virtual;
    procedure MoveCaption;
    // Converts point from parent's coordinates to own coordinates
    function Convert(APoint: TPoint): TPoint;
    function IsConnected(ConnectedShape: TJvCustomDiagramShape): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Restrict the minimum size
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    // Called when moving one of the connected shapes
    procedure SetBoundingRect;
    procedure SetConnections(TheStartConn, TheEndConn: TJvConnection);
    property StartTermRect: TRect index 1 read GetTermRect write SetTermRect;
    property EndTermRect: TRect index 2 read GetTermRect write SetTermRect;
  published
    // Publish these properties so that component streaming can be used to
    // store them in a file
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property LineColor: TColor read FLineColor write FLineColor default clBlack;
    property StartConn: TJvConnection index 1 read GetConn write SetConn;
    property EndConn: TJvConnection index 2 read GetConn write SetConn;
    property MidPoint: TPoint read GetMidPoint;
    property Caption;
    property RightClickSelect;
    // Make these properties available
    property OnClick;
    property OnDblClick;
  end;

  TJvSingleHeadArrow = class(TJvConnector)
  protected
    procedure DrawArrowHead(ConnPt, TermPt: TPoint);
    procedure DrawEndTerminator; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvSingleHeadOpenDashArrow = class(TJvConnector)
  protected
    procedure Paint; override;
    procedure DrawArrowHead(ConnPt, TermPt: TPoint);
    procedure DrawEndTerminator; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvBluntSingleHeadOpenDashArrow = class(TJvSingleHeadOpenDashArrow)
  protected
    procedure DrawStartTerminator; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvBluntSingleHeadArrow = class(TJvSingleHeadArrow)
  protected
    procedure DrawStartTerminator; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvSubCaseArrow = class(TJvConnector)
  protected
    procedure DrawArrowHead(ConnPt, TermPt: TPoint);
    procedure DrawEndTerminator; override;
    procedure DrawStartTerminator; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvDoubleHeadArrow = class(TJvSingleHeadArrow)
  protected
    procedure DrawStartTerminator; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  SysUtils, 
  JvTypes, JvConsts;

type
  // This type is solely for the acccess to the protected MouseDown method
  TCrackTControl = class(TControl);

var
  // Used in unique naming scheme. It is global in this unit to enable a
  // 'memory' of the component names used during the lifetime of this unit.
  GlobalShapeCount: Integer = 1;

procedure NoLessThan(var Value: Integer; Limit: Integer);
begin
  if Value < Limit then
    Value := Limit;
end;

function RectHeight(ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

function RectWidth(ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

function InRect(X, Y: Integer; ARect: TRect): Boolean;
begin
  Result := (X >= ARect.Left) and (X <= ARect.Right) and
    (Y >= ARect.Top) and (Y <= ARect.Bottom);
end;

function Min(A: array of Integer): Integer;
var
  I: Integer;
begin
  // (rom) the "Purely" comment is wrong
  // (rom) the function explicitly handles empty arrays
  Result := 0; // Purely to stop compiler warnings
  for I := Low(A) to High(A) do
    if I = Low(A) then
      Result := A[I]
    else
    if A[I] < Result then
      Result := A[I];
end;

function Max(A: array of Integer): Integer;
var
  I: Integer;
begin
  Result := 0; // Purely to stop compiler warnings
  for I := Low(A) to High(A) do
    if I = Low(A) then
      Result := A[I]
    else
    if A[I] > Result then
      Result := A[I];
end;

//=== { TJvCustomDiagramShape } ==============================================

constructor TJvCustomDiagramShape.Create(AOwner: TComponent);
var
  AlreadyUsed: Boolean;
  I: Integer;
  TempName: string;
begin
  inherited Create(AOwner);
  FCanProcessMouseMsg := True;
  FCaption := nil;
  FSelected := False;
  FWasCovered := False;

  // (rom) this was removed, but should be handled
  //if AOwner = nil then
    //Exit;
  // Give the component a name and ensure that it is unique
  repeat
    // Use a local variable to hold the name, so that don't get exceptions
    // raised on duplicate names
    TempName := 'Shape' + IntToStr(GlobalShapeCount);
    Inc(GlobalShapeCount);
    AlreadyUsed := False;

    // Loop through all the components on the form to ensure that this name
    // is not already in use
    for I := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[I].Name = TempName then
      begin
        // Try the next component name as this one is used already
        AlreadyUsed := True;
        Break;
      end;
  until not AlreadyUsed;
  Name := TempName;
end;

destructor TJvCustomDiagramShape.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FCaption);
  // First check that this control has been placed on a form
  if Assigned(Parent) then
  begin
    // Search parent control for TJvConnector components that connect
    // to this component
    I := 0;
    while I < Parent.ControlCount do
      if (Parent.Controls[I] is TJvConnector) and
        (TJvConnector(Parent.Controls[I]).IsConnected(Self)) then
        Parent.Controls[I].Free
      else
        Inc(I);
  end;
  inherited Destroy;
end;

procedure TJvCustomDiagramShape.SetCaption(Value: TJvTextShape);
begin
  if (Value = nil) and Assigned(FCaption) then
  begin
    FCaption.Free;
    FCaption := nil;
  end
  else
  if Value <> FCaption then
  begin
    FCaption := Value;
    FCaption.Parent := Self.Parent;
    // Ensure the caption gets aligned correctly. Ths only needs to happen if
    // the caption has not already been set in place (it will already be in the
    // right place if we are loading this from a file).
    if (FCaption.Left = 0) and (FCaption.Top = 0) then
      AlignCaption(taCenter);
  end;
end;

{$IFDEF VisualCLX}
procedure TJvCustomDiagramShape.SetParent(const AParent: TWidgetControl);
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvCustomDiagramShape.SetParent(AParent: TWinControl);
{$ENDIF VCL}
begin
  inherited SetParent(AParent);
  if Assigned(FCaption) then
    FCaption.Parent := AParent;
end;

procedure TJvCustomDiagramShape.SetSelected(Value: Boolean);
begin
  FSelected := Value;
  if Assigned(FCaption) then
    FCaption.SetSelected(Value);
end;

procedure TJvCustomDiagramShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  I: Integer;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if not Assigned(Parent) then
    Exit;
  // Search parent control for TJvConnector components
  for I := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[I] is TJvConnector then
      if TJvConnector(Parent.Controls[I]).IsConnected(Self) then
        // Resize the connector, but don't draw it yet
        TJvConnector(Parent.Controls[I]).SetBoundingRect;
  AlignCaption(FAlignment);
end;

procedure TJvCustomDiagramShape.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FCaption then
      FCaption := nil;
end;

procedure TJvCustomDiagramShape.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  TempPt: TPoint;
  CoveredShape: TJvCustomDiagramShape;
begin
  if CanProcessMouseMsg then
  begin
    BringToFront;
    MouseCapture := True;
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  // Pass message on to any covered control capable of handling it
  CoveredShape := GetCustomShapeAtPos(X, Y);
  TempPt := Point(X, Y);
  MouseCapture := False;

  if CoveredShape <> nil then
  begin
    SendToBack;
    // Convert coordinates to covered shape's coordinates
    TempPt := CoveredShape.ScreenToClient(ClientToScreen(TempPt));
    // Send the mouse down message to the covered shape
    CoveredShape.MouseDown(Button, Shift, TempPt.X, TempPt.Y);
    // Flag the control as having been covered because we lose a mouse click
    CoveredShape.FWasCovered := True;
  end
  else
  if Assigned(Parent) then
  begin
    // Send mouse down message to Parent. The typecast is purely to gain access
    // to the Parent.MouseDown method. Need to convert coordinates to parent's
    // coordinates
    TempPt := Parent.ScreenToClient(ClientToScreen(TempPt));
    TCrackTControl(Parent).MouseDown(Button, Shift, TempPt.X, TempPt.Y);
  end;
end;

procedure TJvCustomDiagramShape.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FWasCovered then
  begin
    // We will lose a mouse click, so replace it
    Click;
    FWasCovered := False;
  end;
end;

function TJvCustomDiagramShape.GetCustomShapeAtPos(X, Y: Integer): TJvCustomDiagramShape;
var
  I: Integer;
  Pt: TPoint;
begin
  Result := nil;
  if not Assigned(Parent) then
    Exit;

  Pt := Parent.ScreenToClient(ClientToScreen(Point(X, Y)));

  for I := 0 to Parent.ControlCount - 1 do
    if (Parent.Controls[I] <> Self) and
      (Parent.Controls[I] is TJvCustomDiagramShape) and
      TJvCustomDiagramShape(Parent.Controls[I]).CanProcessMouseMsg and
      InRect(Pt.X, Pt.Y, Parent.Controls[I].BoundsRect) then
    begin
      Result := TJvCustomDiagramShape(Parent.Controls[I]);
      Exit;
    end;
end;

procedure TJvCustomDiagramShape.AlignCaption(Alignment: TAlignment);
var
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  FAlignment := Alignment;
  if not Assigned(FCaption) then
    Exit;

  ALeft := Left;
  ATop := Top + Height + 5;
  AWidth := FCaption.Width;
  AHeight := FCaption.Height;

  case Alignment of
    taLeftJustify:
      ALeft := Left;
    taRightJustify:
      ALeft := Left + Width - 1;
    taCenter:
      ALeft := Left + ((Width - FCaption.Width) div 2);
  end;
  FCaption.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

class procedure TJvCustomDiagramShape.SaveToFile(const FileName: string;
  ParentControl: TWinControl);
var
  FS: TFileStream;
  Writer: TWriter;
  RealName: string;
begin
  FS := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  Writer := TWriter.Create(FS, 1024);
  try
    Writer.Root := ParentControl.Owner;
    RealName := ParentControl.Name;
    ParentControl.Name := '';
    Writer.WriteComponent(ParentControl);
    ParentControl.Name := RealName;
  finally
    Writer.Free;
    FS.Free;
  end;
end;

class procedure TJvCustomDiagramShape.LoadFromFile(const FileName: string;
  ParentControl: TWinControl);
var
  FS: TFileStream;
  Reader: TReader;
  RealName: string;
begin
  DeleteAllShapes(ParentControl);
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Reader := TReader.Create(FS, 1024);
  try
    // Save the parent's name, in case we are reading into a different
    // control than we saved the diagram from
    RealName := ParentControl.Name;
    Reader.Root := ParentControl.Owner;
    Reader.BeginReferences;
    Reader.ReadComponent(ParentControl);
    Reader.FixupReferences;
    // Restore the parent's name
    ParentControl.Name := RealName;
  finally
    Reader.EndReferences;
    Reader.Free;
    FS.Free;
  end;
end;

class procedure TJvCustomDiagramShape.DeleteAllShapes(ParentControl: TWinControl);
var
  I: Integer;
begin
  // Delete controls from ParentControl
  I := 0;
  // (rom) added Assigned for security
  if Assigned(ParentControl) then
    while I < ParentControl.ControlCount do
      if ParentControl.Controls[I] is TJvCustomDiagramShape then
        ParentControl.Controls[I].Free
        // Note that there is no need to increment the counter, because the
        // next component (if any) will now be at the same position in Controls[]
      else
        Inc(I);
end;

class procedure TJvCustomDiagramShape.DeleteSelectedShapes(ParentControl: TWinControl);
var
  I: Integer;
begin
  // Delete controls from ParentControl if they are flagged as selected
  I := 0;
  // (rom) added Assigned for security
  if Assigned(ParentControl) then
    while I < ParentControl.ControlCount do
      if (ParentControl.Controls[I] is TJvCustomDiagramShape) and
        (TJvCustomDiagramShape(ParentControl.Controls[I]).Selected) then
        ParentControl.Controls[I].Free
        // Note that there is no need to increment the counter, because the
        // next component (if any) will now be at the same position in Controls[]
      else
        Inc(I);
end;

class procedure TJvCustomDiagramShape.UnselectAllShapes(ParentControl: TWinControl);
var
  I: Integer;
begin
  // (rom) added Assigned for security
  if Assigned(ParentControl) then
    for I := 0 to ParentControl.ControlCount - 1 do
      if ParentControl.Controls[I] is TJvCustomDiagramShape then
        TJvCustomDiagramShape(ParentControl.Controls[I]).Selected := False;
end;

//=== { TJvMoveableShape } ===================================================

constructor TJvMoveableShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Selected := False;
  Moving := False;
  FOrigin := Point(0, 0);
end;

procedure TJvMoveableShape.StartMove(X, Y: Integer);
begin
  Selected := True;
  Moving := True;
  FOrigin := Point(X, Y);
end;

procedure TJvMoveableShape.Move(DeltaX, DeltaY: Integer);
begin
  SetBounds(Left + DeltaX, Top + DeltaY, Width, Height);
end;

procedure TJvMoveableShape.EndMove;
begin
  Moving := False;
  FOrigin := Point(0, 0);
end;

function TJvMoveableShape.ValidMove(DeltaX, DeltaY: Integer): Boolean;
begin
  Result := True;
  if not Assigned(Parent) then
    Exit;

  if Selected then
    Result := (Left + DeltaX >= 0) and (Top + DeltaY >= 0) and
      (Left + DeltaX + Width - 1 < Parent.ClientRect.Right - Parent.ClientRect.Left) and
      (Top + DeltaY + Height - 1 < Parent.ClientRect.Bottom - Parent.ClientRect.Top);
end;

procedure TJvMoveableShape.MoveShapes(DeltaX, DeltaY: Integer);
var
  I, Pass: Integer;
  TempControl: TControl;
begin
  if not Assigned(Parent) then
    Exit;

  // Do 2 passes through controls. The first one is to check that all
  // movements are valid
  for Pass := 1 to 2 do
  begin
    for I := 0 to Parent.ControlCount - 1 do
    begin
      TempControl := Parent.Controls[I];
      if TempControl is TJvMoveableShape then
      begin
        if (Pass = 1) and
          (not TJvMoveableShape(TempControl).ValidMove(DeltaX, DeltaY)) then
          Exit
        else
        if (Pass = 2) and TJvMoveableShape(TempControl).Selected then
          TJvMoveableShape(TempControl).Move(DeltaX, DeltaY);
      end;
    end;
  end;
end;

procedure TJvMoveableShape.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  // Only respond to left mouse button events
  if Button <> mbLeft then
    Exit;
  // If not holding down the shift key then not doing multiple selection
  if not (ssShift in Shift) then
    UnselectAllShapes(Parent);
  // Start moving the component
  StartMove(X, Y);
end;

procedure TJvMoveableShape.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  // Only need to move the component if the left mouse button is being held down
  if not (ssLeft in Shift) then
  begin
    Moving := False;
    Exit;
  end;

  if Moving then
    // Move all the selected shapes
    MoveShapes(X - FOrigin.X, Y - FOrigin.Y);
end;

procedure TJvMoveableShape.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
  TempControl: TControl;
begin
  inherited MouseUp(Button, Shift, X, Y);
  // Only interested in left mouse button events
  if Button <> mbLeft then
    Exit;

  EndMove;

  // If this shape is covering any smaller shapes then send it to the back,
  // so that we can get at the smaller ones
  if not Assigned(Parent) then
    Exit;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    TempControl := Parent.Controls[I];
    if (TempControl <> Self) and
      (TempControl is TJvCustomDiagramShape) and
      TJvCustomDiagramShape(TempControl).CanProcessMouseMsg and
      InRect(TempControl.Left, TempControl.Top, BoundsRect) and
      InRect(TempControl.Left + TempControl.Width,
      TempControl.Top + TempControl.Height, BoundsRect) then
    begin
      // TempControl is not this one, it is a custom shape, that can process
      // mouse messages (eg not a connector), and is completely covered by
      // this control. So bring the convered control to the top of the z-order
      // so that we can access it.
      TempControl.BringToFront;
      Exit;
    end;
  end;
end;

//=== { TJvSizeableShape } ===================================================

constructor TJvSizeableShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSizingMode := smNone;
  FSizeOrigin := Point(0, 0);
  FSizeRectHeight := 5;
  FSizeRectWidth := 5;
  FMinHeight := FSizeRectHeight;
  FMinWidth := FSizeRectWidth;
end;

procedure TJvSizeableShape.SetSelected(Value: Boolean);
begin
  if Value <> FSelected then
  begin
    inherited SetSelected(Value);
    // Force redraw to show sizing rectangles
    Invalidate;
  end;
end;

procedure TJvSizeableShape.Paint;
begin
  inherited Paint;
  if not Assigned(Parent) then
    Exit;
  DrawSizingRects;
end;

function TJvSizeableShape.GetSizeRect(SizeRectType: TJvSizingMode): TRect;
begin
  case SizeRectType of
    smTopLeft:
      Result := Bounds(0, 0, SizeRectWidth, SizeRectHeight);
    smTop:
      Result := Bounds(((ClientRect.Right - ClientRect.Left) div 2) -
        (SizeRectWidth div 2), 0, SizeRectWidth, SizeRectHeight);
    smTopRight:
      Result := Bounds(ClientRect.Right - SizeRectWidth, 0,
        SizeRectWidth, SizeRectHeight);
    smLeft:
      Result := Bounds(0, ((ClientRect.Bottom - ClientRect.Top) div 2) -
        (SizeRectHeight div 2), SizeRectWidth, SizeRectHeight);
    smRight:
      Result := Bounds(ClientRect.Right - SizeRectWidth,
        ((ClientRect.Bottom - ClientRect.Top) div 2) -
        (SizeRectHeight div 2), SizeRectWidth, SizeRectHeight);
    smBottomLeft:
      Result := Bounds(0, ClientRect.Bottom - SizeRectHeight,
        SizeRectWidth, SizeRectHeight);
    smBottom:
      Result := Bounds(((ClientRect.Right - ClientRect.Left) div 2) -
        (SizeRectWidth div 2), ClientRect.Bottom - SizeRectHeight,
        SizeRectWidth, SizeRectHeight);
    smBottomRight:
      Result := Bounds(ClientRect.Right - SizeRectWidth,
        ClientRect.Bottom - SizeRectHeight, SizeRectWidth, SizeRectHeight);
    smNone:
      Result := Bounds(0, 0, 0, 0);
  end;
end;

procedure TJvSizeableShape.DrawSizingRects;
var
  OldBrush: TBrush;
  SMode: TJvSizingMode;
begin
  if not FSelected or not CanProcessMouseMsg then
    Exit;
  with Canvas do
  begin
    // Draw the sizing rectangles
    OldBrush := TBrush.Create;
    try
      OldBrush.Assign(Brush);
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      Pen.Color := clBlack;
      for SMode := smTopLeft to smBottomRight do
        FillRect(GetSizeRect(SMode));
    finally
      Brush.Assign(OldBrush);
      OldBrush.Free;
    end;
  end;
end;

procedure TJvSizeableShape.CheckForSizeRects(X, Y: Integer);
const
  cCursors: array [TJvSizingMode] of TCursor =
    (crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeWE,
     crSizeNESW, crSizeNS, crSizeNWSE, crDefault);
var
  SMode: TJvSizingMode;
begin
  FSizingMode := smNone;
  if not Selected then
    Exit;

  for SMode := smTopLeft to smBottomRight do
    if InRect(X, Y, GetSizeRect(SMode)) then
    begin
      SizingMode := SMode;
      Break;
    end;
  Cursor := cCursors[SizingMode];
end;

procedure TJvSizeableShape.ResizeControl(X, Y: Integer);
var
  L, T, W, H, DeltaX, DeltaY: Integer;
begin
  L := Left;
  T := Top;
  W := Width;
  H := Height;
  DeltaX := X - FSizeOrigin.X;
  DeltaY := Y - FSizeOrigin.Y;
  // Calculate the new boundaries on the control. Also change FSizeOrigin to
  // reflect change in boundaries if necessary.
  case FSizingMode of
    smTopLeft:
      begin
        // Ensure that don't move the left edge if this would make the
        // control too narrow
        if (L + DeltaX >= 0) and (W - DeltaX > MinWidth) then
        begin
          L := L + DeltaX;
          W := W - DeltaX;
        end;
        // Ensure that don't move the top edge if this would make the
        // control too short
        if (T + DeltaY >= 0) and (H - DeltaY > MinHeight) then
        begin
          T := T + DeltaY;
          H := H - DeltaY;
        end;
      end;
    smTop:
      begin
        if (T + DeltaY >= 0) and (H - DeltaY > MinHeight) then
        begin
          T := T + DeltaY;
          H := H - DeltaY;
        end;
      end;
    smTopRight:
      begin
        W := W + DeltaX;
        if (T + DeltaY >= 0) and (H - DeltaY > MinHeight) then
        begin
          T := T + DeltaY;
          H := H - DeltaY;
        end;
        FSizeOrigin.X := X;
      end;
    smLeft:
      begin
        if (L + DeltaX >= 0) and (W - DeltaX > MinWidth) then
        begin
          L := L + DeltaX;
          W := W - DeltaX;
        end;
      end;
    smRight:
      begin
        W := W + DeltaX;
        FSizeOrigin.X := X;
      end;
    smBottomLeft:
      begin
        if (L + DeltaX >= 0) and (W - DeltaX > MinWidth) then
        begin
          L := L + DeltaX;
          W := W - DeltaX;
        end;
        H := H + DeltaY;
        FSizeOrigin.Y := Y;
      end;
    smBottom:
      begin
        H := H + DeltaY;
        FSizeOrigin.X := X;
        FSizeOrigin.Y := Y;
      end;
    smBottomRight:
      begin
        W := W + DeltaX;
        H := H + DeltaY;
        FSizeOrigin.X := X;
        FSizeOrigin.Y := Y;
      end;
    smNone: ;
  end;
  SetBounds(L, T, W, H);
end;

procedure TJvSizeableShape.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (FSizingMode = smNone) or (Button <> mbLeft) or (ssShift in Shift) then
  begin
    // Do moving instead of sizing
    FSizingMode := smNone;
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  // If sizing then make this the only selected control
  UnselectAllShapes(Parent);
  BringToFront;
  { TODO : check on all Shapes selected }
  //  FSelected   := True;
  FSizeOrigin := Point(X, Y);
end;

procedure TJvSizeableShape.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Moving then
    inherited MouseMove(Shift, X, Y)
  else
  if (FSizingMode <> smNone) and (ssLeft in Shift) then
    ResizeControl(X, Y)
  else
    // Check if over a sizing rectangle
    CheckForSizeRects(X, Y);
end;

procedure TJvSizeableShape.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
    FSizingMode := smNone;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvSizeableShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  // Check that the control bounds are sensible. The control must be at least
  // as large as a sizing rectangle
  NoLessThan(ALeft, 0);
  NoLessThan(ATop, 0);
  NoLessThan(AWidth, FMinWidth);
  NoLessThan(AHeight, FMinHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

//=== { TJvTextShape } =======================================================

constructor TJvTextShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSize := True;
  FText := '';
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
end;

destructor TJvTextShape.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TJvTextShape.RefreshText;
var
  I, Count: Integer;
  TempStr: string;
begin
  FMinHeight := FSizeRectHeight;
  FMinWidth := FSizeRectWidth;
  TempStr := '';
  Count := 1;
  if AutoSize and Assigned(Parent) then
  begin
    Canvas.Font := Font;
    for I := 1 to Length(FText) do
    begin
      if FText[I] = Lf then
      begin
        // Check the width of this line
        FMinWidth := Max([FMinWidth, Canvas.TextWidth(TempStr)]);
        TempStr := '';
        // Count the line feeds
        Inc(Count);
      end
      else
        TempStr := TempStr + FText[I];
    end;
    if Count = 1 then
      // In case there is only one line
      FMinWidth := Max([FMinWidth, Canvas.TextWidth(FText)]);
    // Calculate the height of the text rectangle
    FMinHeight := Max([FMinHeight, Canvas.TextHeight(FText) * Count]);
  end;
  SetBounds(Left, Top, FMinWidth, FMinHeight);
end;

{$IFDEF VisualCLX}
function TJvTextShape.GetText: TCaption;
begin
  Result := FText;
end;
{$ENDIF VisualCLX}

procedure TJvTextShape.SetText(const Value: TCaption);
begin
  if FText <> Value then
  begin
    FText := Value;
    RefreshText;
  end;
end;

procedure TJvTextShape.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    RefreshText;
  end;
end;

procedure TJvTextShape.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TJvTextShape.FontChange(Sender: TObject);
begin
  RefreshText;
end;

{$IFDEF VisualCLX}
procedure TJvTextShape.SetParent(const AParent: TWidgetControl);
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvTextShape.SetParent(AParent: TWinControl);
{$ENDIF VCL}
begin
  inherited SetParent(AParent);
  RefreshText;
end;

procedure TJvTextShape.Paint;
var
  TempRect: TRect;
begin
  if not Assigned(Parent) then
    Exit;
  Canvas.Font := Font;
  TempRect := ClientRect; // So can pass as a var parameter
  {$IFDEF VCL}
  DrawText(Canvas.Handle, PCaptionChar(FText), Length(FText), TempRect,
    DT_CENTER or DT_NOPREFIX or DT_WORDBREAK);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  DrawText(Canvas, FText, Length(FText), TempRect,
    DT_CENTER or DT_NOPREFIX or DT_WORDBREAK);
  {$ENDIF VisualCLX}
  inherited Paint;
end;

procedure TJvTextShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  // Check that the control bounds are sensible. Note that this also works
  // if try to set Left, Top etc properties, as their access methods call
  // SetBounds().
  NoLessThan(AWidth, FMinWidth);
  NoLessThan(AHeight, FMinHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

//=== { TJvBitmapShape } =====================================================

constructor TJvBitmapShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImages := nil;
  FImageIndex := 0;
end;

procedure TJvBitmapShape.SetSelected(Value: Boolean);
begin
  if Value <> FSelected then
  begin
    inherited SetSelected(Value);
    // Force redraw to show focus rectangle
    Invalidate;
  end;
end;

procedure TJvBitmapShape.SetImages(Value: TImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    if FImages <> nil then
      // Set the size of the component to the image size
      SetBounds(Left, Top, FImages.Width, FImages.Height);
  end;
end;

procedure TJvBitmapShape.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvBitmapShape.Paint;
var
  OldPen: TPen;
begin
  inherited Paint;
  if (not Assigned(Parent)) or (not Assigned(FImages)) or
    (FImageIndex < 0) or (FImageIndex >= FImages.Count) then
    // The component has not been placed on a form yet, or does not have an
    // associated image
    Exit;

  // Draw a focus rectangle
  OldPen := Canvas.Pen;
  Canvas.Pen.Style := psDot;
  Canvas.Brush.Style := bsClear;

  if Selected then
    Canvas.Pen.Mode := pmNot
  else
    Canvas.Pen.Mode := pmNop;

  // (rom) draws a rectangle
  Canvas.Polyline([Point(0, 0), Point(Width - 1, 0),
      Point(Width - 1, Height - 1), Point(0, Height - 1), Point(0, 0)]);
  Canvas.Pen := OldPen;

  // Draw the bitmap
  {$IFDEF VCL}
  FImages.DrawingStyle := dsTransparent;
  {$ENDIF VCL}
  FImages.Draw(Canvas, 0, 0, FImageIndex);
end;

procedure TJvBitmapShape.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      FImages := nil;
end;

//=== { TJvStandardShape } ===================================================

constructor TJvStandardShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Set a default shape and size and colors
  FShapeType := stRectangle;
  Width := 100;
  Height := 60;
  FLineColor := clBlack;
end;

procedure TJvStandardShape.SetShapeType(Value: TShapeType);
begin
  if FShapeType <> Value then
  begin
    FShapeType := Value;
    Invalidate;
  end;
end;

procedure TJvStandardShape.Paint;
var
  TempRect: TRect;
  S: Integer;
begin
  inherited Paint;
  if not Assigned(Parent) then
    Exit;

  TempRect := ClientRect; // So can pass as a var parameter
  InflateRect(TempRect, -SizeRectWidth, -SizeRectHeight);

  // Draw shape outline
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := FLineColor;
  S := Min([TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1]);

  if FShapeType in [stSquare, stRoundSquare, stCircle] then
  begin
    TempRect.Right := TempRect.Left + S;
    TempRect.Bottom := TempRect.Top + S;
  end;

  case FShapeType of
    stRectangle, stSquare:
      Canvas.Rectangle(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom);
    stRoundRect, stRoundSquare:
      Canvas.RoundRect(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom,
        S div 4, S div 4);
    stCircle, stEllipse:
      Canvas.Ellipse(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom);
  end;
end;

//=== { TJvConnection } ======================================================

constructor TJvConnection.Create;
begin
  inherited Create;
  FShape := nil;
  FSide := csRight;
  FOffset := 0;
end;

procedure TJvConnection.Assign(Source: TPersistent);
begin
  if Source is TJvConnection then
  begin
    FShape := TJvConnection(Source).FShape;
    FSide := TJvConnection(Source).FSide;
    FOffset := TJvConnection(Source).FOffset;
  end
  else
    inherited Assign(Source);
end;

function TJvConnection.ConnPoint(TerminatorRect: TRect): TPoint;
var
  X, Y, W: Integer;
begin
  Result := Point(0, 0);
  X := 0;
  Y := 0;
  W := TerminatorRect.Right - TerminatorRect.Left;

  if FShape = nil then
    Exit;

  case FSide of
    csLeft:
      begin
        X := FShape.Left - W;
        Y := FShape.Top + FOffset;
      end;
    csRight:
      begin
        X := FShape.Left + FShape.Width - 1 + W;
        Y := FShape.Top + FOffset;
      end;
    csTop:
      begin
        X := FShape.Left + FOffset;
        Y := FShape.Top - W;
      end;
    csBottom:
      begin
        X := FShape.Left + FOffset;
        Y := FShape.Top + FShape.Height - 1 + W;
      end;
  end;
  Result := Point(X, Y);
end;

function TJvConnection.TermPoint(TerminatorRect: TRect): TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
  if Shape = nil then
    Exit;
  with Result do
    case Side of
      csLeft:
        begin
          X := Shape.Left;
          Y := Shape.Top + Offset;
        end;
      csRight:
        begin
          X := Shape.Left + Shape.Width - 1;
          Y := Shape.Top + Offset;
        end;
      csTop:
        begin
          X := Shape.Left + Offset;
          Y := Shape.Top;
        end;
      csBottom:
        begin
          X := Shape.Left + Offset;
          Y := Shape.Top + Shape.Height - 1;
        end;
    else
      X := 0;
      Y := 0;
  end;
end;

function TJvConnection.LeftMost(TerminatorRect: TRect): TPoint;
begin
  Result := TermPoint(TerminatorRect);
  if Shape = nil then
    Exit;
  case Side of
    csLeft:
      Result.X := Shape.Left - RectWidth(TerminatorRect);
    csRight:
      Result.X := Shape.Left + Shape.Width;
    csTop, csBottom:
      Result.X := Shape.Left + Offset - (RectHeight(TerminatorRect) div 2);
  end;
end;

function TJvConnection.RightMost(TerminatorRect: TRect): TPoint;
begin
  Result := TermPoint(TerminatorRect);
  if Shape = nil then
    Exit;
  case Side of
    csLeft:
      Result.X := Shape.Left - 1;
    csRight:
      Result.X := Shape.Left + Shape.Width - 1 + RectWidth(TerminatorRect);
    csTop, csBottom:
      Result.X := Shape.Left + Offset + (RectHeight(TerminatorRect) div 2);
  end;
end;

function TJvConnection.TopMost(TerminatorRect: TRect): TPoint;
begin
  Result := TermPoint(TerminatorRect);
  if Shape = nil then
    Exit;
  case Side of
    csLeft, csRight:
      Result.Y := Shape.Top + Offset - (RectHeight(TerminatorRect) div 2);
    csTop:
      Result.Y := Shape.Top - RectWidth(TerminatorRect) - 1;
    csBottom:
      Result.Y := Shape.Top + Shape.Height;
  end;
end;

function TJvConnection.BottomMost(TerminatorRect: TRect): TPoint;
begin
  Result := TermPoint(TerminatorRect);
  if Shape = nil then
    Exit;
  case Side of
    csLeft, csRight:
      Result.Y := Shape.Top + Offset + (RectHeight(TerminatorRect) div 2);
    csTop:
      Result.Y := Shape.Top - 1;
    csBottom:
      Result.Y := Shape.Top + Shape.Height + RectWidth(TerminatorRect);
  end;
end;

//=== { TJvConnector } =======================================================

constructor TJvConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanProcessMouseMsg := False;
  FLineWidth := 1;
  FLineColor := clBlack;
  FStartTermRect := Rect(0, 0, 0, 0);
  FEndTermRect := Rect(0, 0, 0, 0);
  FStartConn := TJvConnection.Create;
  FEndConn := TJvConnection.Create;
  FMidPoint := Point(0, 0);
end;

destructor TJvConnector.Destroy;
begin
  FreeAndNil(FStartConn);
  FreeAndNil(FEndConn);
  inherited Destroy;
end;

procedure TJvConnector.Paint;
var
  EndPt: TPoint;
begin
  inherited Paint;
  if not Assigned(Parent) then
    Exit;
  if Assigned(FStartConn.Shape) and Assigned(FEndConn.Shape) then
  begin
    // Draw the terminators (arrows etc)
    DrawStartTerminator;
    DrawEndTerminator;
    with Canvas do
    begin
      // Draw the connecting line
      Brush.Style := bsClear;
      Pen.Width := FLineWidth;
      Pen.Color := FLineColor;
      // Convert from Parent coordinates to control coordinates
      PenPos := Convert(FStartConn.ConnPoint(FStartTermRect));
      EndPt := Convert(FEndConn.ConnPoint(FEndTermRect));
      LineTo(EndPt.X, EndPt.Y);
    end;
  end;
end;

procedure TJvConnector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    // (rom) added Assigned to fix a crash
    if Assigned(FStartConn) and (AComponent = FStartConn.FShape) then
      FStartConn.FShape := nil;
    if Assigned(FEndConn) and (AComponent = FEndConn.FShape) then
      FEndConn.FShape := nil;
  end;
end;

procedure TJvConnector.DrawStartTerminator;
begin
end;

procedure TJvConnector.DrawEndTerminator;
begin
end;

procedure TJvConnector.MoveCaption;
var
  NewMidPoint: TPoint;
  ALeft, ATop, ARight, ABottom: Integer;
begin
  if Assigned(FCaption) then
  begin
    if (FMidPoint.X = 0) and (FMidPoint.Y = 0) then
      FMidPoint := GetMidPoint;
    NewMidPoint := GetMidPoint;
    // Move the caption relative to the mid point of the connector
    // Not resizing anything, just moving an unconnected shape, so can use
    // faster update method than SetBounds
    FCaption.Invalidate;
    ALeft := FCaption.Left + NewMidPoint.X - FMidPoint.X;
    ATop := FCaption.Top + NewMidPoint.Y - FMidPoint.Y;
    ARight := ALeft + FCaption.Width;
    ABottom := ATop + FCaption.Height;
    FCaption.UpdateBoundsRect(Rect(ALeft, ATop, ARight, ABottom));
    // Save the new mid point
    FMidPoint := NewMidPoint;
  end;
end;

procedure TJvConnector.CheckSize(var AWidth, AHeight: Integer);
begin
  // Ensure the control is at least as big as the line width
  NoLessThan(AHeight, FLineWidth);
  NoLessThan(AWidth, FLineWidth);
  // Ensure the control is at least as big as the start terminator rectangle
  NoLessThan(AHeight, RectHeight(FStartTermRect));
  NoLessThan(AWidth, RectWidth(FStartTermRect));
  // Ensure the control is at least as big as the end terminator rectangle
  NoLessThan(AHeight, RectHeight(FEndTermRect));
  NoLessThan(AWidth, RectWidth(FEndTermRect));
end;

procedure TJvConnector.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  CheckSize(AWidth, AHeight);
  // Resize the connector
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  // Move the caption
  MoveCaption;
end;

procedure TJvConnector.SetBoundingRect;
var
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  if (FStartConn.Shape = nil) or (FEndConn.Shape = nil) then
    Exit;
  ALeft := Min([FStartConn.LeftMost(FStartTermRect).X,
    FEndConn.LeftMost(FEndTermRect).X]);
  ATop := Min([FStartConn.TopMost(FStartTermRect).Y,
    FEndConn.TopMost(FEndTermRect).Y]);
  AWidth := Max([FStartConn.RightMost(FStartTermRect).X,
    FEndConn.RightMost(FEndTermRect).X]) - ALeft + 2;
  AHeight := Max([FStartConn.BottomMost(FStartTermRect).Y,
    FEndConn.BottomMost(FEndTermRect).Y]) - ATop + 2;
  CheckSize(AWidth, AHeight);
  Invalidate;
  UpdateBoundsRect(Rect(ALeft, ATop, ALeft + AWidth - 1, ATop + AHeight - 1));
  MoveCaption;
end;

procedure TJvConnector.SetLineWidth(Value: Integer);
begin
  // Ensure that can always see the line!
  if Value >= 1 then
    FLineWidth := Value;
end;

function TJvConnector.GetConn(Index: Integer): TJvConnection;
begin
  case Index of
    1:
      Result := FStartConn;
    2:
      Result := FEndConn;
  else
    Result := nil;
  end;
end;

procedure TJvConnector.SetConn(Index: Integer; Value: TJvConnection);
begin
  case Index of
    1:
      FStartConn.Assign(Value);
    2:
      FEndConn.Assign(Value);
  end;
  SetBoundingRect;
end;

procedure TJvConnector.SetConnections(TheStartConn, TheEndConn: TJvConnection);
begin
  StartConn := TheStartConn;
  EndConn := TheEndConn;
end;

function TJvConnector.GetTermRect(Index: Integer): TRect;
begin
  case Index of
    1:
      Result := FStartTermRect;
    2:
      Result := FEndTermRect;
  end;
end;

procedure TJvConnector.SetTermRect(Index: Integer; Value: TRect);
begin
  if (Value.Right - Value.Left >= 0) and (Value.Bottom - Value.Top >= 0) then
  begin
    case Index of
      1:
        FStartTermRect := Value;
      2:
        FEndTermRect := Value;
    end;
  end;
end;

procedure TJvConnector.SetCaption(Value: TJvTextShape);
begin
  inherited SetCaption(Value);
  MoveCaption;
end;

function TJvConnector.Convert(APoint: TPoint): TPoint;
begin
  Result := ScreenToClient(Parent.ClientToScreen(APoint));
end;

function TJvConnector.IsConnected(ConnectedShape: TJvCustomDiagramShape): Boolean;
begin
  Result := (FStartConn <> nil) and (FEndConn <> nil) and (ConnectedShape <> nil) and
    ((FStartConn.Shape = ConnectedShape) or (FEndConn.Shape = ConnectedShape));
end;

function TJvConnector.GetMidPoint: TPoint;
var
  A, B: TPoint;
begin
  Result := Point(0, 0);
  if (not Assigned(FStartConn)) or (not Assigned(FEndConn)) then
    Exit;
  A := FStartConn.ConnPoint(FStartTermRect);
  B := FEndConn.ConnPoint(FEndTermRect);
  Result := Point(Min([A.X, B.X]) + Abs(A.X - B.X) div 2,
    Min([A.Y, B.Y]) + Abs(A.Y - B.Y) div 2);
end;

//=== { TJvSingleHeadArrow } =================================================

constructor TJvSingleHeadArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EndTermRect := Rect(0, 0, 25, 10);
end;

procedure TJvSingleHeadArrow.DrawArrowHead(ConnPt, TermPt: TPoint);
var
  PointPt, Corner1Pt, Corner2Pt: TPoint;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FLineColor;
    Pen.Color := FLineColor;

    // Draw a line connecting the Conn and Term points
    PenPos := ConnPt;
    LineTo(TermPt.X, TermPt.Y);
    // Set the basic points (to be modified depending on arrow head direction
    PointPt := TermPt;
    Corner1Pt := ConnPt;
    Corner2Pt := ConnPt;

    if ConnPt.X < TermPt.X then
    begin
      // Draw a right pointing arrow head
      Inc(Corner1Pt.X, 10);
      Inc(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, RectHeight(EndTermRect) div 2);
    end
    else
    if ConnPt.X > TermPt.X then
    begin
      // Draw a left pointing arrow head
      Dec(Corner1Pt.X, 10);
      Dec(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, RectHeight(EndTermRect) div 2);
    end
    else
    if ConnPt.Y < TermPt.Y then
    begin
      // Draw a down pointing arrow head
      Inc(Corner1Pt.Y, 10);
      Inc(Corner2Pt.Y, 10);
      Dec(Corner1Pt.X, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.X, RectHeight(EndTermRect) div 2);
    end
    else
    begin
      // Draw a up pointing arrow head
      Dec(Corner1Pt.Y, 10);
      Dec(Corner2Pt.Y, 10);
      Dec(Corner1Pt.X, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.X, RectHeight(EndTermRect) div 2);
    end;
    Polygon([PointPt, Corner1Pt, Corner2Pt]);
  end;
end;

procedure TJvSingleHeadArrow.DrawEndTerminator;
var
  ConnPt, TermPt: TPoint;
begin
  inherited DrawEndTerminator;
  if Assigned(FEndConn.Shape) then
  begin
    ConnPt := Convert(FEndConn.ConnPoint(EndTermRect));
    TermPt := Convert(FEndConn.TermPoint(EndTermRect));
    DrawArrowHead(ConnPt, TermPt);
  end;
end;

//=== { TJvSingleHeadOpenDashArrow } =========================================

constructor TJvSingleHeadOpenDashArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EndTermRect := Rect(0, 0, 25, 10);
end;

procedure TJvSingleHeadOpenDashArrow.Paint;
begin
  Canvas.Pen.Style := psDash;
  inherited Paint;
  Canvas.Pen.Style := psSolid;
end;

procedure TJvSingleHeadOpenDashArrow.DrawArrowHead(ConnPt, TermPt: TPoint);
var
  PointPt, Corner1Pt, Corner2Pt: TPoint;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    Brush.Color := clWindow;
    Pen.Color := FLineColor;

    // Draw a line connecting the Conn and Term points
    PenPos := ConnPt;
    LineTo(TermPt.X, TermPt.Y);
    // Set the basic points (to be modified depending on arrow head direction
    PointPt := TermPt;
    Corner1Pt := ConnPt;
    Corner2Pt := ConnPt;

    if ConnPt.X < TermPt.X then
    begin
      // Draw a right pointing arrow head
      Inc(Corner1Pt.X, 10);
      Inc(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, RectHeight(EndTermRect) div 2);
    end
    else
    if ConnPt.X > TermPt.X then
    begin
      // Draw a left pointing arrow head
      Dec(Corner1Pt.X, 10);
      Dec(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, RectHeight(EndTermRect) div 2);
    end
    else
    if ConnPt.Y < TermPt.Y then
    begin
      // Draw a down pointing arrow head
      Inc(Corner1Pt.Y, 10);
      Inc(Corner2Pt.Y, 10);
      Dec(Corner1Pt.X, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.X, RectHeight(EndTermRect) div 2);
    end
    else
    begin
      // Draw a up pointing arrow head
      Dec(Corner1Pt.Y, 10);
      Dec(Corner2Pt.Y, 10);
      Dec(Corner1Pt.X, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.X, RectHeight(EndTermRect) div 2);
    end;
    //    Polyline([Corner1Pt,PointPt,Corner2Pt]);
    MoveTo(PointPt.X, PointPt.Y);
    LineTo(Corner1Pt.X, Corner1Pt.Y);
    MoveTo(PointPt.X, PointPt.Y);
    LineTo(Corner2Pt.X, Corner2Pt.Y);
  end;
end;

procedure TJvSingleHeadOpenDashArrow.DrawEndTerminator;
var
  ConnPt, TermPt: TPoint;
begin
  inherited DrawEndTerminator;
  if Assigned(FEndConn.Shape) then
  begin
    ConnPt := Convert(FEndConn.ConnPoint(EndTermRect));
    TermPt := Convert(FEndConn.TermPoint(EndTermRect));
    DrawArrowHead(ConnPt, TermPt);
  end;
end;

//=== { TJvBluntSingleHeadOpenDashArrow } ====================================

constructor TJvBluntSingleHeadOpenDashArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StartTermRect := Rect(0, 0, 10, 10);
end;

procedure TJvBluntSingleHeadOpenDashArrow.DrawStartTerminator;
var
  ConnPt, TermPt: TPoint;
begin
  inherited DrawStartTerminator;
  if not Assigned(FStartConn.Shape) then
    Exit;
  ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
  TermPt := Convert(FStartConn.TermPoint(StartTermRect));
  with Canvas do
  begin
    // Draw a line connecting the Conn and Term points
    Pen.Color := FLineColor;
    PenPos := ConnPt;
    LineTo(TermPt.X, TermPt.Y);
  end;
end;

//=== { TJvBluntSingleHeadArrow } ============================================

constructor TJvBluntSingleHeadArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StartTermRect := Rect(0, 0, 10, 10);
end;

procedure TJvBluntSingleHeadArrow.DrawStartTerminator;
var
  ConnPt, TermPt: TPoint;
begin
  inherited DrawStartTerminator;
  if not Assigned(FStartConn.Shape) then
    Exit;
  ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
  TermPt := Convert(FStartConn.TermPoint(StartTermRect));
  with Canvas do
  begin
    // Draw a line connecting the Conn and Term points
    Pen.Color := FLineColor;
    PenPos := ConnPt;
    LineTo(TermPt.X, TermPt.Y);
  end;
end;

//=== { TJvSubCaseArrow } ====================================================

constructor TJvSubCaseArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EndTermRect := Rect(0, 0, 25, 10);
  StartTermRect := Rect(0, 0, 10, 10);
end;

procedure TJvSubCaseArrow.DrawArrowHead(ConnPt, TermPt: TPoint);
var
  PointPt, Corner1Pt, Corner2Pt: TPoint;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FLineColor;
    Pen.Color := FLineColor;

    // Draw a line connecting the Conn and Term points
    PenPos := ConnPt;
    LineTo(TermPt.X, TermPt.Y);
    // Set the basic points (to be modified depending on arrow head direction
    PointPt := TermPt;
    Corner1Pt := ConnPt;
    Corner2Pt := ConnPt;

    if ConnPt.X < TermPt.X then
    begin
      // Draw a right pointing arrow head
      Inc(Corner1Pt.X, 10);
      Inc(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, RectHeight(EndTermRect) div 2);
    end
    else
    if ConnPt.X > TermPt.X then
    begin
      // Draw a left pointing arrow head
      Dec(Corner1Pt.X, 10);
      Dec(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, RectHeight(EndTermRect) div 2);
    end
    else
    if ConnPt.Y < TermPt.Y then
    begin
      // Draw a down pointing arrow head
      Inc(Corner1Pt.Y, 10);
      Inc(Corner2Pt.Y, 10);
      Dec(Corner1Pt.X, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.X, RectHeight(EndTermRect) div 2);
    end
    else
    begin
      // Draw a up pointing arrow head
      Dec(Corner1Pt.Y, 10);
      Dec(Corner2Pt.Y, 10);
      Dec(Corner1Pt.X, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.X, RectHeight(EndTermRect) div 2);
    end;
    Brush.Color := clWindow;
    Polygon([PointPt, Corner1Pt, Corner2Pt]);
  end;
end;

procedure TJvSubCaseArrow.DrawEndTerminator;
var
  ConnPt, TermPt: TPoint;
begin
  inherited DrawEndTerminator;
  if Assigned(FEndConn.Shape) then
  begin
    ConnPt := Convert(FEndConn.ConnPoint(EndTermRect));
    TermPt := Convert(FEndConn.TermPoint(EndTermRect));
    DrawArrowHead(ConnPt, TermPt);
  end;
end;

procedure TJvSubCaseArrow.DrawStartTerminator;
var
  ConnPt, TermPt: TPoint;
begin
  inherited DrawStartTerminator;
  if not Assigned(FStartConn.Shape) then
    Exit;
  ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
  TermPt := Convert(FStartConn.TermPoint(StartTermRect));
  with Canvas do
  begin
    // Draw a line connecting the Conn and Term points
    Pen.Color := FLineColor;
    PenPos := ConnPt;
    LineTo(TermPt.X, TermPt.Y);
  end;
end;

//=== { TJvDoubleHeadArrow } =================================================

constructor TJvDoubleHeadArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StartTermRect := EndTermRect;
end;

procedure TJvDoubleHeadArrow.DrawStartTerminator;
var
  ConnPt, TermPt: TPoint;
begin
  inherited DrawStartTerminator;
  if Assigned(FStartConn.Shape) then
  begin
    ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
    TermPt := Convert(FStartConn.TermPoint(StartTermRect));
    DrawArrowHead(ConnPt, TermPt);
  end;
end;

// ------------------ Initialisation and cleanup routines --------------------

procedure RegisterStorageClasses;
begin
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvConnection, TControl);
  {$ENDIF COMPILER7_UP}
  RegisterClasses([TJvCustomDiagramShape, TJvMoveableShape,
    TJvSizeableShape, TJvConnection, TJvConnector, TJvSingleHeadArrow,
      TJvBluntSingleHeadArrow, TJvDoubleHeadArrow, TJvBitmapShape,
      TJvTextShape, TJvStandardShape, TJvSingleHeadOpenDashArrow,
      TJvBluntSingleHeadOpenDashArrow, TJvSubCaseArrow]);
end;

class procedure TJvCustomDiagramShape.SetMultiSelected(ParentControl: TWinControl;
  Value: Boolean);
var
  I: Integer;
begin
  if Assigned(ParentControl) then
    for I := 0 to ParentControl.ControlCount - 1 do
      if ParentControl.Controls[I] is TJvCustomDiagramShape then
        TJvCustomDiagramShape(ParentControl.Controls[I]).MultiSelect := Value;
end;

initialization
  RegisterStorageClasses;

end.


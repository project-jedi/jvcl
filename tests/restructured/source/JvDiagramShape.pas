{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDiagramShape.PAS, released on 2002-03-22.

Original Developer: Jim Cooper <jcooper@tabdee.ltd.uk>
Contributor(s): Michael Beck <mbeck1@compuserve.com>

Last Modified: 2002-03-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvDiagramShape;

interface

uses
  Windows, Controls, Classes, Forms, ExtCtrls, Graphics, Messages;

type
  TJvTextShape = class;

  TJvCustomDiagramShape = class(TGraphicControl)
    // All controls descend from this, to help with streaming and unique naming
  private
    FCanProcessMouseMsg: Boolean;
    FCaption: TJvTextShape;
    FSelected: Boolean;
    FWasCovered: Boolean;

  protected
    procedure SetCaption(Value: TJvTextShape); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function GetCustomShapeAtPos(X, Y: Integer): TJvCustomDiagramShape;

    property CanProcessMouseMsg: Boolean read FCanProcessMouseMsg
      write FCanProcessMouseMsg;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetSelected(Value: Boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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

    property Selected: Boolean read FSelected write SetSelected;
  published
    property Caption: TJvTextShape read FCaption write SetCaption;

    // Make these properties available
    property OnClick;
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
    FText: string;
    FAutosize: Boolean;
    FFont: TFont;

    procedure SetText(Value: string);
    procedure SetFont(Value: TFont);
    procedure FontChanged(Sender: TObject);
  protected
    procedure SetAutosize(Value: Boolean);{$IFDEF DELPHI6_UP}override;{$ENDIF}
    procedure RefreshText;
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Text: string read FText write SetText;
    property Autosize: Boolean read FAutosize write SetAutosize;
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
  end;

  TJvStandardShape = class(TJvSizeableShape)
  private
    FShapeType: TShapeType;
    FLineColour: TColor;

    procedure SetShapeType(Value: TShapeType);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ShapeType: TShapeType read FShapeType write SetShapeType;
    property LineColour: TColor read FLineColour write FLineColour default clBlack;
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
    FLineColour: TColor;
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
    property LineColour: TColor read FLineColour write FLineColour default clBlack;
    property StartConn: TJvConnection index 1 read GetConn write SetConn;
    property EndConn: TJvConnection index 2 read GetConn write SetConn;
    property MidPoint: TPoint read GetMidPoint;
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
  SysUtils, ImgList, Dialogs;

type
  // This type is purely so that can acccess the protected MouseDown method
  TJvMdControl = class(TControl);

var
  FShapeCount: Integer;
  // Used in unique naming scheme. It is global in this unit to enable a
  // 'memory' of the component names used during the lifetime of this unit.

procedure NoLessThan(var Value: Integer; Limit: Integer);
begin {NoLessThan}
  if Value < Limit then
  begin
    Value := Limit;
  end;
end; {NoLessThan}

function RectHeight(ARect: TRect): Integer;
begin {RectHeight}
  Result := ARect.Bottom - ARect.Top;
end; {RectHeight}

function RectWidth(ARect: TRect): Integer;
begin {RectWidth}
  Result := ARect.Right - ARect.Left;
end; {RectWidth}

function InRect(X, Y: Integer; ARect: TRect): Boolean;
begin {InRect}
  Result := (X >= ARect.Left) and (X <= ARect.Right) and
    (Y >= ARect.Top) and (Y <= ARect.Bottom);
end; {InRect}

function Min(A: array of Integer): Integer;
var
  i: Integer;
begin {Min}
  Result := 0; // Purely to stop compiler warnings

  for i := Low(A) to High(A) do
  begin
    if i = Low(A) then
    begin
      Result := A[i]
    end
    else if A[i] < Result then
    begin
      Result := A[i];
    end;
  end;
end; {Min}

function Max(A: array of Integer): Integer;
var
  i: Integer;
begin {Max}
  Result := 0; // Purely to stop compiler warnings

  for i := Low(A) to High(A) do
  begin
    if i = Low(A) then
    begin
      Result := A[i]
    end
    else if A[i] > Result then
    begin
      Result := A[i];
    end;
  end;
end; {Max}

// ---------------------------- TJvCustomDiagramShape ------------------------------

constructor TJvCustomDiagramShape.Create(AOwner: TComponent);
var
  AlreadyUsed: Boolean;
  i: Integer;
  TempName: string;
begin {Create}
  inherited Create(AOwner);
  FCanProcessMouseMsg := True;
  FCaption := nil;
  FSelected := False;
  FWasCovered := False;

  // Give the component a name and ensure that it is unique
  repeat
    // Use a local variable to hold the name, so that don't get exceptions
    // raised on duplicate names
    TempName := 'Shape' + IntToStr(FShapeCount);
    Inc(FShapeCount);
    AlreadyUsed := False;

    // Loop through all the components on the form to ensure that this name
    // is not already in use
    for i := 0 to Owner.ComponentCount - 1 do
    begin
      if Owner.Components[i].Name = TempName then
      begin
        // Try the next component name as this one is used already
        AlreadyUsed := True;
        Break;
      end;
    end;
  until not AlreadyUsed;

  Name := TempName;
end; {Create}

destructor TJvCustomDiagramShape.Destroy;
var
  i: Integer;
begin {Destroy}
  FCaption.Free;

  // First check that this control has been placed on a form
  if Assigned(Parent) then
  begin
    // Search parent control for TJvConnector components that connect
    // to this component
    i := 0;

    while i < Parent.ControlCount do
    begin
      if (Parent.Controls[i] is TJvConnector) and
        (TJvConnector(Parent.Controls[i]).IsConnected(Self)) then
      begin
        Parent.Controls[i].Free;
      end
      else
      begin
        Inc(i);
      end;
    end;
  end;

  inherited Destroy;
end; {Destroy}

procedure TJvCustomDiagramShape.SetCaption(Value: TJvTextShape);
begin {SetCaption}
  if (Value = nil) and Assigned(FCaption) then
  begin
    FCaption.Free;
    FCaption := nil;
  end
  else if (Value <> FCaption) then
  begin
    FCaption := Value;
    FCaption.Parent := Self.Parent;
    // Ensure the caption gets aligned correctly. Ths only needs to happen if
    // the caption has not already been set in place (it will already be in the
    // right place if we are loading this from a file).
    if (FCaption.Left = 0) and (FCaption.Top = 0) then
    begin
      AlignCaption(taCenter);
    end;
  end;
end; {SetCaption}

procedure TJvCustomDiagramShape.SetParent(AParent: TWinControl);
begin {SetParent}
  inherited SetParent(AParent);

  if Assigned(FCaption) then
  begin
    FCaption.Parent := AParent;
  end;
end; {SetParent}

procedure TJvCustomDiagramShape.SetSelected(Value: Boolean);
begin {SetSelected}
  FSelected := Value;

  if Assigned(FCaption) then
  begin
    FCaption.SetSelected(Value);
  end;
end; {SetSelected}

procedure TJvCustomDiagramShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  i: Integer;
begin {SetBounds}
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if not Assigned(Parent) then
  begin
    Exit;
  end;

  // Search parent control for TJvConnector components
  for i := 0 to Parent.ControlCount - 1 do
  begin
    if Parent.Controls[i] is TJvConnector then
    begin
      if TJvConnector(Parent.Controls[i]).IsConnected(Self) then
      begin
        // Resize the connector, but don't draw it yet
        TJvConnector(Parent.Controls[i]).SetBoundingRect;
      end;
    end;
  end;
end; {SetBounds}

procedure TJvCustomDiagramShape.Notification(AComponent: TComponent; Operation: TOperation);
begin {Notification}
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = FCaption then
    begin
      FCaption := nil;
    end;
  end;
end; {Notification}

procedure TJvCustomDiagramShape.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  TempPt: TPoint;
  CoveredShape: TJvCustomDiagramShape;
begin {MouseDown}
  if FCanProcessMouseMsg then
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
  else if Assigned(Parent) then
  begin
    // Send mouse down message to Parent. The typecast is purely to gain access
    // to the Parent.MouseDown method. Need to convert coordinates to parent's
    // coordinates
    TempPt := Parent.ScreenToClient(ClientToScreen(TempPt));
    TJvMdControl(Parent).MouseDown(Button, Shift, TempPt.X, TempPt.Y);
  end;
end; {MouseDown}

procedure TJvCustomDiagramShape.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin {MouseUp}
  inherited MouseUp(Button, Shift, X, Y);

  if FWasCovered then
  begin
    // We will lose a mouse click, so replace it
    Click;
    FWasCovered := False;
  end;
end; {MouseUp}

function TJvCustomDiagramShape.GetCustomShapeAtPos(X, Y: Integer): TJvCustomDiagramShape;
var
  i: Integer;
  Pt: TPoint;
begin {GetCustomShapeAtPos}
  Result := nil;

  if not Assigned(Parent) then
  begin
    Exit;
  end;

  Pt := Parent.ScreenToClient(ClientToScreen(Point(X, Y)));

  for i := 0 to Parent.ControlCount - 1 do
  begin
    if (Parent.Controls[i] <> Self) and
      (Parent.Controls[i] is TJvCustomDiagramShape) and
      TJvCustomDiagramShape(Parent.Controls[i]).CanProcessMouseMsg and
      InRect(Pt.X, Pt.Y, Parent.Controls[i].BoundsRect) then
    begin
      Result := TJvCustomDiagramShape(Parent.Controls[i]);
      Exit;
    end;
  end;
end; {GetCustomShapeAtPos}

procedure TJvCustomDiagramShape.AlignCaption(Alignment: TAlignment);
var
  ALeft, ATop, AWidth, AHeight: Integer;
begin {AlignCaption}
  if not Assigned(FCaption) then
  begin
    Exit;
  end;

  ALeft := Left;
  ATop := Top + Height + 5;
  AWidth := FCaption.Width;
  AHeight := FCaption.Height;

  case Alignment of
    taLeftJustify: ALeft := Left;
    taRightJustify: ALeft := Left + Width - 1;
    taCenter: ALeft := Left + ((Width - FCaption.Width) div 2);
  end;

  FCaption.SetBounds(ALeft, ATop, AWidth, AHeight);
end; {AlignCaption}

class procedure TJvCustomDiagramShape.SaveToFile(const FileName: string;
  ParentControl: TWinControl);
var
  FS: TFileStream;
  Writer: TWriter;
  RealName: string;
begin {SaveToFile}
  FS := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
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
end; {SaveToFile}

class procedure TJvCustomDiagramShape.LoadFromFile(const FileName: string;
  ParentControl: TWinControl);
var
  FS: TFileStream;
  Reader: TReader;
  RealName: string;
begin {LoadFromFile}
  DeleteAllShapes(ParentControl);

  FS := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
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
end; {LoadFromFile}

class procedure TJvCustomDiagramShape.DeleteAllShapes(ParentControl: TWinControl);
var
  i: Integer;
begin {DeleteAllShapes}
  // Delete controls from ParentControl
  i := 0;

  while i < ParentControl.ControlCount do
  begin
    if ParentControl.Controls[i] is TJvCustomDiagramShape then
    begin
      ParentControl.Controls[i].Free;
      // Note that there is no need to increment the counter, because the
      // next component (if any) will now be at the same position in Controls[]
    end
    else
    begin
      Inc(i);
    end;
  end;
end; {DeleteAllShapes}

class procedure TJvCustomDiagramShape.DeleteSelectedShapes(ParentControl: TWinControl);
var
  i: Integer;
begin {DeleteSelectedShapes}
  // Delete controls from ParentControl if they are flagged as selected
  i := 0;

  while i < ParentControl.ControlCount do
  begin
    if (ParentControl.Controls[i] is TJvCustomDiagramShape) and
      (TJvCustomDiagramShape(ParentControl.Controls[i]).Selected) then
    begin
      ParentControl.Controls[i].Free;
      // Note that there is no need to increment the counter, because the
      // next component (if any) will now be at the same position in Controls[]
    end
    else
    begin
      Inc(i);
    end;
  end;
end; {DeleteSelectedShapes}

class procedure TJvCustomDiagramShape.UnselectAllShapes(ParentControl: TWinControl);
var
  i: Integer;
begin {UnselectAllShapes}
  for i := 0 to ParentControl.ControlCount - 1 do
  begin
    if ParentControl.Controls[i] is TJvCustomDiagramShape then
    begin
      TJvCustomDiagramShape(ParentControl.Controls[i]).Selected := False;
    end;
  end;
end; {UnselectAllShapes}

// --------------------------- TJvMoveableShape  ----------------------------

constructor TJvMoveableShape.Create(AOwner: TComponent);
begin {Create}
  inherited Create(AOwner);
  Selected := False;
  Moving := False;
  FOrigin := Point(0, 0);
end; {Create}

procedure TJvMoveableShape.StartMove(X, Y: Integer);
begin {StartMove}
  Selected := True;
  Moving := True;
  FOrigin := Point(X, Y);
end; {StartMove}

procedure TJvMoveableShape.Move(DeltaX, DeltaY: Integer);
begin {Move}
  SetBounds(Left + DeltaX, Top + DeltaY, Width, Height);
end; {Move}

procedure TJvMoveableShape.EndMove;
begin {EndMove}
  Moving := False;
  FOrigin := Point(0, 0);
end; {EndMove}

function TJvMoveableShape.ValidMove(DeltaX, DeltaY: Integer): Boolean;
begin {ValidMove}
  Result := True;

  if not Assigned(Parent) then
  begin
    Exit;
  end;

  if Selected then
  begin
    Result := (Left + DeltaX >= 0) and
      (Top + DeltaY >= 0) and
      (Left + DeltaX + Width - 1 <
      Parent.ClientRect.Right - Parent.ClientRect.Left) and
      (Top + DeltaY + Height - 1 <
      Parent.ClientRect.Bottom - Parent.ClientRect.Top);
  end;
end; {ValidMove}

procedure TJvMoveableShape.MoveShapes(DeltaX, DeltaY: Integer);
var
  i, Pass: Integer;
  TempControl: TControl;
begin {MoveShapes}
  if not Assigned(Parent) then
  begin
    Exit;
  end;

  // Do 2 passes through controls. The first one is to check that all
  // movements are valid
  for Pass := 1 to 2 do
  begin
    for i := 0 to Parent.ControlCount - 1 do
    begin
      TempControl := Parent.Controls[i];

      if TempControl is TJvMoveableShape then
      begin
        if (Pass = 1) and
          (not TJvMoveableShape(TempControl).ValidMove(DeltaX, DeltaY)) then
        begin
          Exit;
        end
        else if (Pass = 2) and TJvMoveableShape(TempControl).Selected then
        begin
          TJvMoveableShape(TempControl).Move(DeltaX, DeltaY);
        end;
      end;
    end;
  end;
end; {MoveShapes}

procedure TJvMoveableShape.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin {MouseDown}
  inherited MouseDown(Button, Shift, X, Y);

  // Only respond to left mouse button events
  if Button <> mbLeft then
  begin
    Exit;
  end;

  // If not holding down the shift key then not doing multiple selection
  if not (ssShift in Shift) then
  begin
    UnselectAllShapes(Parent);
  end;

  // Start moving the component
  StartMove(X, Y);
end; {MouseDown}

procedure TJvMoveableShape.MouseMove(Shift: TShiftState; X, Y: Integer);
begin {MouseMove}
  inherited MouseMove(Shift, X, Y);

  // Only need to move the component if the left mouse button is being held down
  if not (ssLeft in Shift) then
  begin
    Moving := False;
    Exit;
  end;

  if Moving then
  begin
    // Move all the selected shapes
    MoveShapes(X - FOrigin.X, Y - FOrigin.Y);
  end;
end; {MouseMove}

procedure TJvMoveableShape.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
  TempControl: TControl;
begin {MouseUp}
  inherited MouseUp(Button, Shift, X, Y);

  // Only interested in left mouse button events
  if Button <> mbLeft then
  begin
    Exit;
  end;

  EndMove;

  // If this shape is covering any smaller shapes then send it to the back,
  // so that we can get at the smaller ones

  if not Assigned(Parent) then
  begin
    Exit;
  end;

  for i := 0 to Parent.ControlCount - 1 do
  begin
    TempControl := Parent.Controls[i];

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
end; {MouseUp}

// --------------------------- TJvSizeableShape -----------------------------

constructor TJvSizeableShape.Create(AOwner: TComponent);
begin {Create}
  inherited Create(AOwner);
  FSizingMode := smNone;
  FSizeOrigin := Point(0, 0);
  FSizeRectHeight := 5;
  FSizeRectWidth := 5;
  FMinHeight := FSizeRectHeight;
  FMinWidth := FSizeRectWidth;
end; {Create}

procedure TJvSizeableShape.SetSelected(Value: Boolean);
begin {SetSelected}
  if Value <> FSelected then
  begin
    inherited SetSelected(Value);
    // Force redraw to show sizing rectangles
    Invalidate;
  end;
end; {SetSelected}

procedure TJvSizeableShape.Paint;
begin {Paint}
  inherited Paint;

  if not Assigned(Parent) then
  begin
    Exit;
  end;

  DrawSizingRects;
end; {Paint}

function TJvSizeableShape.GetSizeRect(SizeRectType: TJvSizingMode): TRect;
begin {GetSizeRect}
  case SizeRectType of
    smTopLeft: Result := Bounds(0, 0, SizeRectWidth, SizeRectHeight);
    smTop: Result := Bounds(((ClientRect.Right - ClientRect.Left) div 2) -
        (SizeRectWidth div 2),
        0,
        SizeRectWidth, SizeRectHeight);
    smTopRight: Result := Bounds(ClientRect.Right - SizeRectWidth, 0,
        SizeRectWidth, SizeRectHeight);
    smLeft: Result := Bounds(0,
        ((ClientRect.Bottom - ClientRect.Top) div 2) -
        (SizeRectHeight div 2),
        SizeRectWidth, SizeRectHeight);
    smRight: Result := Bounds(ClientRect.Right - SizeRectWidth,
        ((ClientRect.Bottom - ClientRect.Top) div 2) -
        (SizeRectHeight div 2),
        SizeRectWidth, SizeRectHeight);
    smBottomLeft: Result := Bounds(0, ClientRect.Bottom - SizeRectHeight,
        SizeRectWidth, SizeRectHeight);
    smBottom: Result := Bounds(((ClientRect.Right - ClientRect.Left) div 2) -
        (SizeRectWidth div 2),
        ClientRect.Bottom - SizeRectHeight,
        SizeRectWidth, SizeRectHeight);
    smBottomRight: Result := Bounds(ClientRect.Right - SizeRectWidth,
        ClientRect.Bottom - SizeRectHeight,
        SizeRectWidth, SizeRectHeight);
    smNone: Result := Bounds(0, 0, 0, 0);
  end;
end; {GetSizeRect}

procedure TJvSizeableShape.DrawSizingRects;
var
  OldBrush: TBrush;
  SMode: TJvSizingMode;
begin {DrawSizingRects}
  if not FSelected then
  begin
    Exit;
  end;

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
      begin
        FillRect(GetSizeRect(SMode));
      end;
    finally
      Brush.Assign(OldBrush);
      OldBrush.Free;
    end;
  end;
end; {DrawSizingRects}

procedure TJvSizeableShape.CheckForSizeRects(X, Y: Integer);
var
  SMode: TJvSizingMode;
begin {CheckForSizeRects}
  FSizingMode := smNone;

  if not Selected then
  begin
    Exit;
  end;

  for SMode := smTopLeft to smBottomRight do
  begin
    if InRect(X, Y, GetSizeRect(SMode)) then
    begin
      FSizingMode := SMode;
      Break;
    end;
  end;

  case FSizingMode of
    smTopLeft: Cursor := crSizeNWSE;
    smTop: Cursor := crSizeNS;
    smTopRight: Cursor := crSizeNESW;
    smLeft: Cursor := crSizeWE;
    smRight: Cursor := crSizeWE;
    smBottomLeft: Cursor := crSizeNESW;
    smBottom: Cursor := crSizeNS;
    smBottomRight: Cursor := crSizeNWSE;
  else
    Cursor := crDefault;
  end;
end; {CheckForSizeRects}

procedure TJvSizeableShape.ResizeControl(X, Y: Integer);
var
  L, T, W, H, DeltaX, DeltaY: Integer;
begin {ResizeControl}
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
end; {ResizeControl}

procedure TJvSizeableShape.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin {MouseDown}
  if (FSizingMode = smNone) or
    (Button <> mbLeft) or
    (ssShift in Shift) then
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
end; {MouseDown}

procedure TJvSizeableShape.MouseMove(Shift: TShiftState; X, Y: Integer);
begin {MouseMove}
  if Moving then
  begin
    inherited MouseMove(Shift, X, Y);
  end
  else if (FSizingMode <> smNone) and (ssLeft in Shift) then
  begin
    ResizeControl(X, Y);
  end
  else
  begin
    // Check if over a sizing rectangle
    CheckForSizeRects(X, Y);
  end;
end; {MouseMove}

procedure TJvSizeableShape.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin {MouseUp}
  if Button = mbLeft then
  begin
    FSizingMode := smNone;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end; {MouseUp}

procedure TJvSizeableShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin {SetBounds}
  // Check that the control bounds are sensible. The control must be at least
  // as large as a sizing rectangle
  NoLessThan(ALeft, 0);
  NoLessThan(ATop, 0);
  NoLessThan(AWidth, FMinWidth);
  NoLessThan(AHeight, FMinHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end; {SetBounds}

// ----------------------------- TJvTextShape  ------------------------------

constructor TJvTextShape.Create(AOwner: TComponent);
begin {Create}
  inherited Create(AOwner);
  FAutosize := True;
  FText := '';
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
end; {Create}

destructor TJvTextShape.Destroy;
begin {Destroy}
  FFont.Free;
  inherited Destroy;
end; {Destroy}

procedure TJvTextShape.RefreshText;
var
  i, Count: Integer;
  TempStr: string;
begin {RefreshText}
  FMinHeight := FSizeRectHeight;
  FMinWidth := FSizeRectWidth;
  TempStr := '';
  Count := 1;

  if FAutosize and Assigned(Parent) then
  begin
    Canvas.Font := Font;

    for i := 1 to Length(FText) do
    begin
      if FText[i] = #10 then
      begin
        // Check the width of this line
        FMinWidth := Max([FMinWidth, Canvas.TextWidth(TempStr)]);
        TempStr := '';
        // Count the line feeds
        Inc(Count);
      end
      else
      begin
        TempStr := TempStr + FText[i];
      end;
    end;

    if Count = 1 then
    begin
      // In case there is only one line
      FMinWidth := Max([FMinWidth, Canvas.TextWidth(FText)]);
    end;

    // Calculate the height of the text rectangle
    FMinHeight := Max([FMinHeight, Canvas.TextHeight(FText) * Count]);
  end;

  SetBounds(Left, Top, FMinWidth, FMinHeight);
end; {RefreshText}

procedure TJvTextShape.SetText(Value: string);
begin {SetText}
  if FText <> Value then
  begin
    FText := Value;
    RefreshText;
  end;
end; {SetText}

procedure TJvTextShape.SetAutosize(Value: Boolean);
begin {SetAutosize}
  if FAutosize <> Value then
  begin
    FAutosize := Value;
    RefreshText;
  end;
end; {SetAutosize}

procedure TJvTextShape.SetFont(Value: TFont);
begin {SetFont}
  FFont.Assign(Value);
end; {SetFont}

procedure TJvTextShape.FontChanged(Sender: TObject);
begin {FontChanged}
  RefreshText;
end; {FontChanged}

procedure TJvTextShape.SetParent(AParent: TWinControl);
begin {SetParent}
  inherited SetParent(AParent);
  RefreshText;
end; {SetParent}

procedure TJvTextShape.Paint;
var
  TempRect: TRect;
begin {Paint}
  if not Assigned(Parent) then
  begin
    Exit;
  end;

  Canvas.Font := Font;
  TempRect := ClientRect; // So can pass as a var parameter
  DrawText(Canvas.Handle, PChar(FText), Length(FText), TempRect,
    DT_CENTER or DT_NOPREFIX or DT_WORDBREAK);
  inherited Paint;
end; {Paint}

procedure TJvTextShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin {SetBounds}
  // Check that the control bounds are sensible. Note that this also works
  // if try to set Left, Top etc properties, as their access methods call
  // SetBounds().
  NoLessThan(AWidth, FMinWidth);
  NoLessThan(AHeight, FMinHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end; {SetBounds}

// ---------------------------- TJvBitmapShape ------------------------------

constructor TJvBitmapShape.Create(AOwner: TComponent);
begin {Create}
  inherited Create(AOwner);
  FImages := nil;
  FImageIndex := 0;
end; {Create}

procedure TJvBitmapShape.SetSelected(Value: Boolean);
begin {SetSelected}
  if Value <> FSelected then
  begin
    inherited SetSelected(Value);
    // Force redraw to show focus rectangle
    Invalidate;
  end;
end; {SetSelected}

procedure TJvBitmapShape.SetImages(Value: TImageList);
begin {SetImages}
  if Value <> FImages then
  begin
    FImages := Value;

    if FImages <> nil then
    begin
      // Set the size of the component to the image size
      SetBounds(Left, Top, FImages.Width, FImages.Height);
    end;
  end;
end; {SetImages}

procedure TJvBitmapShape.SetImageIndex(Value: Integer);
begin {SetImageIndex}
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end; {SetImageIndex}

procedure TJvBitmapShape.Paint;
var
  OldPen: TPen;
begin {Paint}
  inherited Paint;

  if (not Assigned(Parent)) or
    (not Assigned(FImages)) or
    (FImageIndex < 0) or
    (FImageIndex >= FImages.Count) then
  begin
    // The component has not been placed on a form yet, or does not have an
    // associated image
    Exit;
  end;

  // Draw a focus rectangle
  OldPen := Canvas.Pen;
  Canvas.Pen.Style := psDot;
  Canvas.Brush.Style := bsClear;

  if Selected then
  begin
    Canvas.Pen.Mode := pmNot;
  end
  else
  begin
    Canvas.Pen.Mode := pmNop;
  end;

  Canvas.Polyline([Point(0, 0),
    Point(Width - 1, 0),
      Point(Width - 1, Height - 1),
      Point(0, Height - 1),
      Point(0, 0)]);
  Canvas.Pen := OldPen;

  // Draw the bitmap
  FImages.DrawingStyle := dsTransparent;
  FImages.Draw(Canvas, 0, 0, FImageIndex);
end; {Paint}

procedure TJvBitmapShape.Notification(AComponent: TComponent; Operation: TOperation);
begin {Notification}
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = FImages then
    begin
      FImages := nil;
    end;
  end;
end; {Notification}

// ---------------------------- TtabdStandardShape ---------------------------

constructor TJvStandardShape.Create(AOwner: TComponent);
begin {Create}
  inherited Create(AOwner);

  // Set a default shape and size and colours
  FShapeType := stRectangle;
  Width := 100;
  Height := 60;
  FLineColour := clBlack;
end; {Create}

procedure TJvStandardShape.SetShapeType(Value: TShapeType);
begin {SetShapeType}
  if FShapeType <> Value then
  begin
    FShapeType := Value;
    Invalidate;
  end;
end; {SetShapeType}

procedure TJvStandardShape.Paint;
var
  TempRect: TRect;
  S: Integer;
begin {Paint}
  inherited Paint;

  if not Assigned(Parent) then
  begin
    Exit;
  end;

  TempRect := ClientRect; // So can pass as a var parameter
  InflateRect(TempRect, -SizeRectWidth, -SizeRectHeight);

  // Draw shape outline
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := FLineColour;
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
end; {Paint}

// ----------------------------- TJvConnection ------------------------------

constructor TJvConnection.Create;
begin {Create}
  inherited Create;
  FShape := nil;
  FSide := csRight;
  FOffset := 0;
end; {Create}

procedure TJvConnection.Assign(Source: TPersistent);
begin {Assign}
  if Source is TJvConnection then
  begin
    FShape := TJvConnection(Source).FShape;
    FSide := TJvConnection(Source).FSide;
    FOffset := TJvConnection(Source).FOffset;
  end
  else
  begin
    inherited Assign(Source);
  end;
end; {Assign}

function TJvConnection.ConnPoint(TerminatorRect: TRect): TPoint;
var
  X, Y, W: Integer;
begin {ConnPoint}
  Result := Point(0, 0);
  X := 0;
  Y := 0;
  W := TerminatorRect.Right - TerminatorRect.Left;

  if FShape = nil then
  begin
    Exit;
  end;

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
end; {ConnPoint}

function TJvConnection.TermPoint(TerminatorRect: TRect): TPoint;
var
  X, Y: Integer;
begin {TermPoint}
  Result := Point(0, 0);
  X := 0;
  Y := 0;

  if FShape = nil then
  begin
    Exit;
  end;

  case FSide of
    csLeft:
      begin
        X := FShape.Left;
        Y := FShape.Top + FOffset;
      end;

    csRight:
      begin
        X := FShape.Left + FShape.Width - 1;
        Y := FShape.Top + FOffset;
      end;

    csTop:
      begin
        X := FShape.Left + FOffset;
        Y := FShape.Top;
      end;

    csBottom:
      begin
        X := FShape.Left + FOffset;
        Y := FShape.Top + FShape.Height - 1;
      end;
  end;

  Result := Point(X, Y);
end; {TermPoint}

function TJvConnection.LeftMost(TerminatorRect: TRect): TPoint;
begin {LeftMost}
  Result := TermPoint(TerminatorRect);

  if FShape = nil then
  begin
    Exit;
  end;

  case FSide of
    csLeft: Result.X := FShape.Left - RectWidth(TerminatorRect);
    csRight: Result.X := FShape.Left + FShape.Width;
    csTop,
      csBottom: Result.X := FShape.Left + FOffset - (RectHeight(TerminatorRect) div 2);
  end;
end; {LeftMost}

function TJvConnection.RightMost(TerminatorRect: TRect): TPoint;
begin {RightMost}
  Result := TermPoint(TerminatorRect);

  if FShape = nil then
  begin
    Exit;
  end;

  case FSide of
    csLeft: Result.X := FShape.Left - 1;
    csRight: Result.X := FShape.Left + FShape.Width - 1 + RectWidth(TerminatorRect);
    csTop,
      csBottom: Result.X := FShape.Left + FOffset + (RectHeight(TerminatorRect) div 2);
  end;
end; {RightMost}

function TJvConnection.TopMost(TerminatorRect: TRect): TPoint;
begin {TopMost}
  Result := TermPoint(TerminatorRect);

  if FShape = nil then
  begin
    Exit;
  end;

  case FSide of
    csLeft,
      csRight: Result.Y := FShape.Top + FOffset - (RectHeight(TerminatorRect) div 2);
    csTop: Result.Y := FShape.Top - RectWidth(TerminatorRect) - 1;
    csBottom: Result.Y := FShape.Top + FShape.Height;
  end;
end; {TopMost}

function TJvConnection.BottomMost(TerminatorRect: TRect): TPoint;
begin {BottomMost}
  Result := TermPoint(TerminatorRect);

  if FShape = nil then
  begin
    Exit;
  end;

  case FSide of
    csLeft,
      csRight: Result.Y := FShape.Top + FOffset + (RectHeight(TerminatorRect) div 2);
    csTop: Result.Y := FShape.Top - 1;
    csBottom: Result.Y := FShape.Top + FShape.Height + RectWidth(TerminatorRect);
  end;
end; {BottomMost}

// ----------------------------- TJvConnector -------------------------------

constructor TJvConnector.Create(AOwner: TComponent);
begin {Create}
  inherited Create(AOwner);
  FCanProcessMouseMsg := False;
  FLineWidth := 1;
  FLineColour := clBlack;
  FStartTermRect := Rect(0, 0, 0, 0);
  FEndTermRect := Rect(0, 0, 0, 0);
  FStartConn := TJvConnection.Create;
  FEndConn := TJvConnection.Create;
  FMidPoint := Point(0, 0);
end; {Create}

destructor TJvConnector.Destroy;
begin {Destroy}
  FStartConn.Free;
  FEndConn.Free;
  inherited Destroy;
end; {Destroy}

procedure TJvConnector.Paint;
var
  EndPt: TPoint;
begin {Paint}
  inherited Paint;

  if not Assigned(Parent) then
  begin
    Exit;
  end;

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
      Pen.Color := FLineColour;
      // Convert from Parent coordinates to control coordinates
      PenPos := Convert(FStartConn.ConnPoint(FStartTermRect));
      EndPt := Convert(FEndConn.ConnPoint(FEndTermRect));
      LineTo(EndPt.X, EndPt.Y);
    end;
  end;
end; {Paint}

procedure TJvConnector.Notification(AComponent: TComponent; Operation: TOperation);
begin {Notification}
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = FStartConn.FShape then
    begin
      FStartConn.FShape := nil;
    end;

    if AComponent = FEndConn.FShape then
    begin
      FEndConn.FShape := nil;
    end;
  end;
end; {Notification}

procedure TJvConnector.DrawStartTerminator;
begin {DrawStartTerminator}
end; {DrawStartTerminator}

procedure TJvConnector.DrawEndTerminator;
begin {DrawEndTerminator}
end; {DrawEndTerminator}

procedure TJvConnector.MoveCaption;
var
  NewMidPoint: TPoint;
  ALeft, ATop, ARight, ABottom: Integer;
begin {MoveCaption}
  if Assigned(FCaption) then
  begin
    if (FMidPoint.X = 0) and (FMidPoint.Y = 0) then
    begin
      FMidPoint := GetMidPoint;
    end;

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
end; {MoveCaption}

procedure TJvConnector.CheckSize(var AWidth, AHeight: Integer);
begin {CheckSize}
  // Ensure the control is at least as big as the line width
  NoLessThan(AHeight, FLineWidth);
  NoLessThan(AWidth, FLineWidth);
  // Ensure the control is at least as big as the start terminator rectangle
  NoLessThan(AHeight, RectHeight(FStartTermRect));
  NoLessThan(AWidth, RectWidth(FStartTermRect));
  // Ensure the control is at least as big as the end terminator rectangle
  NoLessThan(AHeight, RectHeight(FEndTermRect));
  NoLessThan(AWidth, RectWidth(FEndTermRect));
end; {CheckSize}

procedure TJvConnector.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin {SetBounds}
  CheckSize(AWidth, AHeight);
  // Resize the connector
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  // Move the caption
  MoveCaption;
end; {SetBounds}

procedure TJvConnector.SetBoundingRect;
var
  ALeft, ATop, AWidth, AHeight: Integer;
begin {SetBoundingRect}
  if (FStartConn.Shape = nil) or (FEndConn.Shape = nil) then
  begin
    Exit;
  end;

  ALeft := Min([FStartConn.LeftMost(FStartTermRect).X,
    FEndConn.LeftMost(FEndTermRect).X]);
  ATop := Min([FStartConn.TopMost(FStartTermRect).Y,
    FEndConn.TopMost(FEndTermRect).Y]);
  AWidth := Max([FStartConn.RightMost(FStartTermRect).X,
    FEndConn.RightMost(FEndTermRect).X]) -
    ALeft + 2;
  AHeight := Max([FStartConn.BottomMost(FStartTermRect).Y,
    FEndConn.BottomMost(FEndTermRect).Y]) -
    ATop + 2;
  CheckSize(AWidth, AHeight);
  Invalidate;
  UpdateBoundsRect(Rect(ALeft, ATop, ALeft + AWidth - 1, ATop + AHeight - 1));
  MoveCaption;
end; {SetBoundingRect}

procedure TJvConnector.SetLineWidth(Value: Integer);
begin {SetLineWidth}
  // Ensure that can always see the line!
  if Value >= 1 then
  begin
    FLineWidth := Value;
  end;
end; {SetLineWidth}

function TJvConnector.GetConn(Index: Integer): TJvConnection;
begin {GetConn}
  Result := nil;

  case Index of
    1: Result := FStartConn;
    2: Result := FEndConn;
  end;
end; {GetConn}

procedure TJvConnector.SetConn(Index: Integer; Value: TJvConnection);
begin {SetConn}
  case Index of
    1: FStartConn.Assign(Value);
    2: FEndConn.Assign(Value);
  end;

  SetBoundingRect;
end; {SetConn}

procedure TJvConnector.SetConnections(TheStartConn, TheEndConn: TJvConnection);
begin {SetConnections}
  StartConn := TheStartConn;
  EndConn := TheEndConn;
end; {SetConnections}

function TJvConnector.GetTermRect(Index: Integer): TRect;
begin {GetTermRect}
  case Index of
    1: Result := FStartTermRect;
    2: Result := FEndTermRect;
  end;
end; {GetTermRect}

procedure TJvConnector.SetTermRect(Index: Integer; Value: TRect);
begin {SetTermRect}
  if (Value.Right - Value.Left >= 0) and (Value.Bottom - Value.Top >= 0) then
  begin
    case Index of
      1: FStartTermRect := Value;
      2: FEndTermRect := Value;
    end;
  end;
end; {SetTermRect}

procedure TJvConnector.SetCaption(Value: TJvTextShape);
begin {SetCaption}
  inherited SetCaption(Value);
  MoveCaption;
end; {SetCaption}

function TJvConnector.Convert(APoint: TPoint): TPoint;
begin {Convert}
  Result := ScreenToClient(Parent.ClientToScreen(APoint));
end; {Convert}

function TJvConnector.IsConnected(ConnectedShape: TJvCustomDiagramShape): Boolean;
begin {IsConnected}
  Result := (FStartConn.Shape = ConnectedShape) or
    (FEndConn.Shape = ConnectedShape);
end; {IsConnected}

function TJvConnector.GetMidPoint: TPoint;
var
  A, B: TPoint;
begin {GetMidPoint}
  Result := Point(0, 0);

  if (not Assigned(FStartConn)) or (not Assigned(FEndConn)) then
  begin
    Exit;
  end;

  A := FStartConn.ConnPoint(FStartTermRect);
  B := FEndConn.ConnPoint(FEndTermRect);
  Result := Point(Min([A.X, B.X]) + Abs(A.X - B.X) div 2,
    Min([A.Y, B.Y]) + Abs(A.Y - B.Y) div 2);
end; {GetMidPoint}

// ------------------------- TJvSingleHeadArrow ---------------------------

constructor TJvSingleHeadArrow.Create(AOwner: TComponent);
begin {Create}
  inherited Create(AOwner);
  EndTermRect := Rect(0, 0, 25, 10);
end; {Create}

procedure TJvSingleHeadArrow.DrawArrowHead(ConnPt, TermPt: TPoint);
var
  PointPt, Corner1Pt, Corner2Pt: TPoint;
begin {DrawArrowHead}
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FLineColour;
    Pen.Color := FLineColour;

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
    else if ConnPt.X > TermPt.X then
    begin
      // Draw a left pointing arrow head
      Dec(Corner1Pt.X, 10);
      Dec(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, RectHeight(EndTermRect) div 2);
    end
    else if ConnPt.Y < TermPt.Y then
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
end; {DrawArrowHead}

procedure TJvSingleHeadArrow.DrawEndTerminator;
var
  ConnPt, TermPt: TPoint;
begin {DrawEndTerminator}
  inherited DrawEndTerminator;

  if Assigned(FEndConn.Shape) then
  begin
    ConnPt := Convert(FEndConn.ConnPoint(EndTermRect));
    TermPt := Convert(FEndConn.TermPoint(EndTermRect));
    ;
    DrawArrowHead(ConnPt, TermPt);
  end;
end; {DrawEndTerminator}

// ------------------------ TJvSingleHeadOpenDashArrow -------------------------

constructor TJvSingleHeadOpenDashArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EndTermRect := Rect(0, 0, 25, 10);
end;

procedure TJvSingleHeadOpenDashArrow.Paint;
//var
//  EndPt: TPoint;

begin {Paint}

  Canvas.Pen.Style := psDash;
  inherited Paint;
  Canvas.Pen.Style := psSolid;
end;

procedure TJvSingleHeadOpenDashArrow.DrawArrowHead(ConnPt, TermPt: TPoint);
var
  PointPt, Corner1Pt, Corner2Pt: TPoint;
begin {DrawArrowHead}
  with Canvas do
  begin
    Brush.Style := bsClear;
    Brush.Color := clWindow;
    Pen.Color := FLineColour;

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
    else if ConnPt.X > TermPt.X then
    begin
      // Draw a left pointing arrow head
      Dec(Corner1Pt.X, 10);
      Dec(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, RectHeight(EndTermRect) div 2);
    end
    else if ConnPt.Y < TermPt.Y then
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
    moveto(PointPt.x, PointPt.y);
    lineto(Corner1Pt.x, Corner1Pt.y);
    moveto(PointPt.x, PointPt.y);
    lineto(Corner2Pt.x, Corner2Pt.y);

  end;
end;

procedure TJvSingleHeadOpenDashArrow.DrawEndTerminator;
var
  ConnPt, TermPt: TPoint;
begin {DrawEndTerminator}
  inherited DrawEndTerminator;

  if Assigned(FEndConn.Shape) then
  begin
    ConnPt := Convert(FEndConn.ConnPoint(EndTermRect));
    TermPt := Convert(FEndConn.TermPoint(EndTermRect));
    ;
    DrawArrowHead(ConnPt, TermPt);
  end;

end;

// ------------------------ TJvBluntSingleHeadOpenDashArrow -------------------------

constructor TJvBluntSingleHeadOpenDashArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StartTermRect := Rect(0, 0, 10, 10);

end;

procedure TJvBluntSingleHeadOpenDashArrow.DrawStartTerminator;
var
  ConnPt, TermPt: TPoint;
begin {DrawStartTerminator}
  inherited DrawStartTerminator;

  if not Assigned(FStartConn.Shape) then
  begin
    Exit;
  end;

  ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
  TermPt := Convert(FStartConn.TermPoint(StartTermRect));
  ;

  with Canvas do
  begin
    // Draw a line connecting the Conn and Term points
    Pen.Color := FLineColour;
    PenPos := ConnPt;
    LineTo(TermPt.X, TermPt.Y);
  end;
end;

// ------------------------ TJvBluntSingleHeadArrow -------------------------

constructor TJvBluntSingleHeadArrow.Create(AOwner: TComponent);
begin {Create}
  inherited Create(AOwner);
  StartTermRect := Rect(0, 0, 10, 10);
end; {Create}

procedure TJvBluntSingleHeadArrow.DrawStartTerminator;
var
  ConnPt, TermPt: TPoint;
begin {DrawStartTerminator}
  inherited DrawStartTerminator;

  if not Assigned(FStartConn.Shape) then
  begin
    Exit;
  end;

  ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
  TermPt := Convert(FStartConn.TermPoint(StartTermRect));
  ;

  with Canvas do
  begin
    // Draw a line connecting the Conn and Term points
    Pen.Color := FLineColour;
    PenPos := ConnPt;
    LineTo(TermPt.X, TermPt.Y);
  end;
end; {DrawStartTerminator}

// --------------------------- TJvSubCaseArrow ---------------------------

constructor TJvSubCaseArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EndTermRect := Rect(0, 0, 25, 10);
  StartTermRect := Rect(0, 0, 10, 10);
end;

procedure TJvSubCaseArrow.DrawArrowHead(ConnPt, TermPt: TPoint);
var
  PointPt, Corner1Pt, Corner2Pt: TPoint;
begin {DrawArrowHead}
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FLineColour;
    Pen.Color := FLineColour;

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
    else if ConnPt.X > TermPt.X then
    begin
      // Draw a left pointing arrow head
      Dec(Corner1Pt.X, 10);
      Dec(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, RectHeight(EndTermRect) div 2);
    end
    else if ConnPt.Y < TermPt.Y then
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
begin {DrawEndTerminator}
  inherited DrawEndTerminator;

  if Assigned(FEndConn.Shape) then
  begin
    ConnPt := Convert(FEndConn.ConnPoint(EndTermRect));
    TermPt := Convert(FEndConn.TermPoint(EndTermRect));
    ;
    DrawArrowHead(ConnPt, TermPt);
  end;

end;

procedure TJvSubCaseArrow.DrawStartTerminator;
var
  ConnPt, TermPt: TPoint;
begin
  inherited DrawStartTerminator;

  if not Assigned(FStartConn.Shape) then
  begin
    Exit;
  end;

  ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
  TermPt := Convert(FStartConn.TermPoint(StartTermRect));
  ;

  with Canvas do
  begin
    // Draw a line connecting the Conn and Term points
    Pen.Color := FLineColour;
    PenPos := ConnPt;
    LineTo(TermPt.X, TermPt.Y);
  end;

end;

// --------------------------- TJvDoubleHeadArrow ---------------------------

constructor TJvDoubleHeadArrow.Create(AOwner: TComponent);
begin {Create}
  inherited Create(AOwner);
  StartTermRect := EndTermRect;
end; {Create}

procedure TJvDoubleHeadArrow.DrawStartTerminator;
var
  ConnPt, TermPt: TPoint;
begin {DrawStartTerminator}
  inherited DrawStartTerminator;

  if Assigned(FStartConn.Shape) then
  begin
    ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
    TermPt := Convert(FStartConn.TermPoint(StartTermRect));
    ;
    DrawArrowHead(ConnPt, TermPt);
  end;
end; {DrawStartTerminator}

// ------------------ Initialisation and cleanup routines --------------------

procedure RegisterStorageClasses;
begin {RegisterStorageClasses}
  RegisterClasses([TJvCustomDiagramShape,
    TJvMoveableShape,
      TJvSizeableShape,
      TJvConnection,
      TJvConnector,
      TJvSingleHeadArrow,
      TJvBluntSingleHeadArrow,
      TJvDoubleHeadArrow,
      TJvBitmapShape,
      TJvTextShape,
      TJvStandardShape,
      TJvSingleHeadOpenDashArrow,
      TJvBluntSingleHeadOpenDashArrow,
      TJvSubCaseArrow]);
end; {RegisterStorageClasses}

initialization
  RegisterStorageClasses;
  FShapeCount := 1;

end.


{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGradientCaption.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvGradientHeaderPanel;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Messages, Graphics, Controls, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QGraphics, QControls, QStdCtrls,
  {$ENDIF VisualCLX}
  JvGradient, JvTypes, JvComponent;

type
  TJvGradientHeaderPanel = class(TJvCustomControl)
  private
    FGradient: TJvGradient;
    FLabel: TLabel;
    FLabelLeft: Integer;
    FHint: Boolean;
    FOldLabelFontChange: TNotifyEvent;
    function GetGradientCursor: TCursor;
    procedure SetGradientCursor(Value: TCursor);
    function GetGradientHint: string;
    procedure SetGradientHint(Value: string);
    function GetGradientStartColor: TColor;
    procedure SetGradientStartColor(Value: TColor);
    function GetGradientEndColor: TColor;
    procedure SetGradientEndColor(Value: TColor);
    function GetGradientSteps: Integer;
    procedure SetGradientSteps(Value: Integer);
    function GetLabelLeft: Integer;
    procedure SetLabelLeft(Value: Integer);
    function GetLabelTop: Integer;
    procedure SetLabelTop(Value: Integer);
    function GetLabelCursor: TCursor;
    procedure SetLabelCursor(Value: TCursor);
    function GetLabelHint: string;
    procedure SetLabelHint(Value: string);
    function GetLabelCaption: string;
    procedure SetLabelCaption(Value: string);
    function GetLabelColor: TColor;
    procedure SetLabelColor(Value: TColor);
    procedure SetShowHint(const Value: Boolean);
    function GetLabelFont: TFont;
    procedure SetLabelFont(const Value: TFont);
    function GetGradientStyle: TJvGradientStyle;
    procedure SetGradientStyle(const Value: TJvGradientStyle);
    function GetLabelAlignment: TAlignment;
    procedure SetLabelAlignment(const Value: TAlignment);
    procedure AdjustLabelWidth;

    {$IFDEF VCL}
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    {$ENDIF VCL}
  protected
    {$IFDEF VisualCLX}
    procedure AdjustSize; override;
    {$ENDIF VisualCLX}
//    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure DoLabelFontChange(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GradientCursor: TCursor read GetGradientCursor write SetGradientCursor default crDefault;
    property GradientHint: string read GetGradientHint write SetGradientHint;
    property GradientStartColor: TColor read GetGradientStartColor write SetGradientStartColor default clBlack;
    property GradientEndColor: TColor read GetGradientEndColor write SetGradientEndColor default clWhite;
    property GradientSteps: Integer read GetGradientSteps write SetGradientSteps default 100;
    property GradientStyle: TJvGradientStyle read GetGradientStyle write SetGradientStyle;
    property LabelLeft: Integer read GetLabelLeft write SetLabelLeft default 10;
    property LabelTop: Integer read GetLabelTop write SetLabelTop default 8;
    property LabelCursor: TCursor read GetLabelCursor write SetLabelCursor default crDefault;
    property LabelHint: string read GetLabelHint write SetLabelHint;
    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    // LabelColor sets the background Color of the label (used for text in the control).
    // To get a transparent text background, set LabelColor to clNone
    property LabelColor: TColor read GetLabelColor write SetLabelColor default clNone;
    property LabelFont: TFont read GetLabelFont write SetLabelFont;
    property ShowHint: Boolean read FHint write SetShowHint default False;
    property LabelAlignment: TAlignment read GetLabelAlignment write SetLabelAlignment;
    property Align;
    property Anchors;
    {$IFDEF VCL}
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    {$ENDIF VCL}
    property Constraints;
    {$IFDEF VCL}
    property DockSite;
    property DoubleBuffered;
    {$ENDIF VCL}
    property DragMode;
    property Enabled;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF VCL}
    property OnCanResize;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
    {$ENDIF VCL}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses
  JvResources;

type
  TNoEventLabel = class(TLabel)
  public
    {$IFDEF VCL}
    procedure Dispatch(var Message); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    constructor Create(AOwner: TComponent); override;
    {$ENDIF VisualCLX}
  end;

  TNoEventGradient = class(TJvGradient)
  public
    {$IFDEF VCL}
    procedure Dispatch(var Message); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    constructor Create(AOwner: TComponent); override;
    {$ENDIF VisualCLX}
  end;

constructor TJvGradientHeaderPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
  Self.Width := 285;
  Self.Height := 30;
  FGradient := TNoEventGradient.Create(Self);
  FGradient.Parent := Self;
  FLabel := TNoEventLabel.Create(Self);
  FLabel.AutoSize := False;
  FLabel.Parent := Self;
  FGradient.Left := 0;
  FGradient.Top := 0;
  FGradient.StartColor := clBlack;
  FGradient.EndColor := clWhite;
  FGradient.Steps := 100;
  LabelLeft := 10;
  FLabel.Top := 8;
  LabelColor := clNone;
  FOldLabelFontChange := FLabel.Font.OnChange;
  FLabel.Font.OnChange := DoLabelFontChange;
  FLabel.Font.Color := clWhite;
  FLabel.Caption := RsYourTextHereCaption;
  FHint := False;
end;

destructor TJvGradientHeaderPanel.Destroy;
begin
  FGradient.Free;
  //  FLabel.OnChange := FOldLabelFontChange;
  FLabel.Free;
  inherited Destroy;
end;

function TJvGradientHeaderPanel.GetGradientCursor: TCursor;
begin
  Result := FGradient.Cursor;
end;

procedure TJvGradientHeaderPanel.SetGradientCursor(Value: TCursor);
begin
  FGradient.Cursor := Value;
end;

function TJvGradientHeaderPanel.GetGradientHint: string;
begin
  Result := FGradient.Hint;
end;

procedure TJvGradientHeaderPanel.SetGradientHint(Value: string);
begin
  FGradient.Hint := Value;
end;

function TJvGradientHeaderPanel.GetGradientStartColor: TColor;
begin
  Result := FGradient.StartColor;
end;

procedure TJvGradientHeaderPanel.SetGradientStartColor(Value: TColor);
begin
  FGradient.StartColor := Value;
end;

function TJvGradientHeaderPanel.GetGradientEndColor: TColor;
begin
  Result := FGradient.EndColor;
end;

procedure TJvGradientHeaderPanel.SetGradientEndColor(Value: TColor);
begin
  FGradient.EndColor := Value;
end;

function TJvGradientHeaderPanel.GetGradientSteps: Integer;
begin
  Result := FGradient.Steps;
end;

procedure TJvGradientHeaderPanel.SetGradientSteps(Value: Integer);
begin
  FGradient.Steps := Value;
end;

function TJvGradientHeaderPanel.GetLabelLeft: Integer;
begin
  Result := FLabelLeft;
end;

procedure TJvGradientHeaderPanel.SetLabelLeft(Value: Integer);
begin
  if FLabel.Left <> Value then
  begin
    if Value < 0 then
      Value := 0;
    FLabel.Left := Value;
    FLabelLeft := Value;
    AdjustLabelWidth;
  end;
end;

function TJvGradientHeaderPanel.GetLabelTop: Integer;
begin
  Result := FLabel.Top;
end;

procedure TJvGradientHeaderPanel.SetLabelTop(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  FLabel.Top := Value;
end;

function TJvGradientHeaderPanel.GetLabelCursor: TCursor;
begin
  Result := FLabel.Cursor;
end;

procedure TJvGradientHeaderPanel.SetLabelCursor(Value: TCursor);
begin
  FLabel.Cursor := Value;
end;

function TJvGradientHeaderPanel.GetLabelHint: string;
begin
  Result := FLabel.Hint;
end;

procedure TJvGradientHeaderPanel.SetLabelHint(Value: string);
begin
  FLabel.Hint := Value;
end;

function TJvGradientHeaderPanel.GetLabelCaption: string;
begin
  Result := FLabel.Caption;
end;

procedure TJvGradientHeaderPanel.SetLabelCaption(Value: string);
begin
  FLabel.Caption := Value;
  AdjustLabelWidth;
end;

function TJvGradientHeaderPanel.GetLabelColor: TColor;
begin
  Result := FLabel.Color;
end;

procedure TJvGradientHeaderPanel.SetLabelColor(Value: TColor);
begin
  FLabel.Color := Value;
  FLabel.Transparent := (Value = clNone);
end;

procedure TJvGradientHeaderPanel.SetShowHint(const Value: Boolean);
begin
  FHint := Value;
  FLabel.ShowHint := Value;
  FGradient.ShowHint := Value;
end;

function TJvGradientHeaderPanel.GetLabelFont: TFont;
begin
  Result := FLabel.Font;
end;

procedure TJvGradientHeaderPanel.SetLabelFont(const Value: TFont);
begin
  FLabel.Font := Value;
  AdjustLabelWidth;
end;

function TJvGradientHeaderPanel.GetGradientStyle: TJvGradientStyle;
begin
  Result := FGradient.Style;
end;

procedure TJvGradientHeaderPanel.SetGradientStyle(const Value: TJvGradientStyle);
begin
  FGradient.Style := Value;
end;

function TJvGradientHeaderPanel.GetLabelAlignment: TAlignment;
begin
  Result := FLabel.Alignment;
end;

procedure TJvGradientHeaderPanel.SetLabelAlignment(const Value: TAlignment);
begin
  FLabel.Alignment := Value;
  AdjustLabelWidth;
end;

{$IFDEF VisualCLX}
procedure TJvGradientHeaderPanel.AdjustSize;
begin
  inherited AdjustSize;
  AdjustLabelWidth;
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}
procedure TJvGradientHeaderPanel.WMSize(var Msg: TWMSize);
begin
  inherited;
  AdjustLabelWidth;
end;
{$ENDIF VCL}

procedure TJvGradientHeaderPanel.AdjustLabelWidth;
var
  W, L: Integer;
begin
  L := FLabel.Left;
  // make as large as we need:
  FLabel.AutoSize := True;
  FLabel.AutoSize := False;
  FLabel.Left := L;
  W := FGradient.Width - FLabelLeft - FLabelLeft;
  // make bigger if there's room
  if W > FLabel.Width then
  begin
    FLabel.Width := W;
    FLabel.Left := FLabelLeft;
  end
  else
  if W < FLabel.Width then // otherwise, just center
  begin
    FLabel.Left := (Width - FLabel.Width) div 2;
    //    if (FLabelLeft > FLabel.Left) and  then
    //      FLabelLeft := FLabel.Left;
  end;
end;

procedure TJvGradientHeaderPanel.DoLabelFontChange(Sender: TObject);
begin
  if Assigned(FOldLabelFontChange) then
    FOldLabelFontChange(Sender);
  AdjustLabelWidth;
end;

procedure TJvGradientHeaderPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if CanFocus then SetFocus;
end;


(*
function TJvGradientHeaderPanel.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  { Reduce flickering FGradient completely fills the TJvGradientHeaderPanel }
  Result := True;
end;
*)

//=== { TNoEventLabel } ======================================================

{$IFDEF VCL}

procedure TNoEventLabel.Dispatch(var Message);
begin
  with TMessage(Message) do
    if (Parent <> nil) and
    (((Msg >= WM_MOUSEFIRST) and (Msg <= WM_MOUSELAST)) or
    ((Msg >= WM_KEYFIRST) and (Msg <= WM_KEYLAST))) then
    Parent.Dispatch(Message)
  else
    inherited;
end;

//=== { TNoEventGradient } ===================================================

procedure TNoEventGradient.Dispatch(var Message);
begin
  with TMessage(Message) do
    if (Parent <> nil) and
    (((Msg >= WM_MOUSEFIRST) and (Msg <= WM_MOUSELAST)) or
    ((Msg >= WM_KEYFIRST) and (Msg <= WM_KEYLAST))) then
    Parent.Dispatch(Message)
  else
    inherited;
end;

{$ENDIF VCL}

{$IFDEF VisualCLX}
constructor TNoEventLabel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csNoStdEvents];
end;

constructor TNoEventGradient.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csNoStdEvents];
end;
{$ENDIF VisualCLX}

end.


{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGradientCaption.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvGradientCaption;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  JvGradient, JvTypes, JVCLVer;

type
  TJvGradientCaption = class(TWinControl)
  private
    FGradient: TJvGradient;
    FLabel: TLabel;
    FLabelLeft: Integer;
    FHint: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
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
    procedure SetHints(const Value: Boolean);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetGStyle: TJvGradStyle;
    procedure SetGstyle(const Value: TJvGradStyle);
    function GetAlignment: TAlignment;
    procedure Setalignment(const Value: TAlignment);
    procedure AdjustLabelWidth;
  protected
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure DoLabelFontChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property GradientCursor: TCursor read GetGradientCursor write SetGradientCursor default crDefault;
    property GradientHint: string read GetGradientHint write SetGradientHint;
    property GradientStartColor: TColor read GetGradientStartColor write SetGradientStartColor default clBlack;
    property GradientEndColor: TColor read GetGradientEndColor write SetGradientEndColor default clWhite;
    property GradientSteps: Integer read GetGradientSteps write SetGradientSteps default 100;
    property GradientStyle: TJvGradStyle read GetGStyle write SetGstyle;
    property LabelLeft: Integer read GetLabelLeft write SetLabelLeft default 10;
    property LabelTop: Integer read GetLabelTop write SetLabelTop default 8;
    property LabelCursor: TCursor read GetLabelCursor write SetLabelCursor default crDefault;
    property LabelHint: string read GetLabelHint write SetLabelHint;
    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    // LabelColor sets the background Color of the label (used for text in the control).
    // To get a transparent text background, set LabelColor to clNone
    property LabelColor: TColor read GetLabelColor write SetLabelColor default clNone;
    property LabelFont: TFont read GetFont write SetFont;
    property ShowHint: Boolean read FHint write SetHints default False;
    property LabelAlignment: TAlignment read GetAlignment write SetAlignment;
    property Align;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
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
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

resourcestring
  RC_YourTextHere = 'Put your text here ...';

constructor TJvGradientCaption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Self.Width := 285;
  Self.Height := 30;
  FGradient := TJvGradient.Create(Self);
  FGradient.Parent := Self;
  FLabel := TLabel.Create(Self);
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
  FLabel.Caption := RC_YourTextHere;
  FHint := False;
end;

destructor TJvGradientCaption.Destroy;
begin
  FGradient.Free;
  //  FLabel.OnChange := FOldLabelFontChange;
  FLabel.Free;
  inherited Destroy;
end;

function TJvGradientCaption.GetGradientCursor: TCursor;
begin
  Result := FGradient.Cursor;
end;

procedure TJvGradientCaption.SetGradientCursor(Value: TCursor);
begin
  FGradient.Cursor := Value;
end;

function TJvGradientCaption.GetGradientHint: string;
begin
  Result := FGradient.Hint;
end;

procedure TJvGradientCaption.SetGradientHint(Value: string);
begin
  FGradient.Hint := Value;
end;

function TJvGradientCaption.GetGradientStartColor: TColor;
begin
  Result := FGradient.StartColor;
end;

procedure TJvGradientCaption.SetGradientStartColor(Value: TColor);
begin
  FGradient.StartColor := Value;
end;

function TJvGradientCaption.GetGradientEndColor: TColor;
begin
  Result := FGradient.EndColor;
end;

procedure TJvGradientCaption.SetGradientEndColor(Value: TColor);
begin
  FGradient.EndColor := Value;
end;

function TJvGradientCaption.GetGradientSteps: Integer;
begin
  Result := FGradient.Steps;
end;

procedure TJvGradientCaption.SetGradientSteps(Value: Integer);
begin
  FGradient.Steps := Value;
end;

function TJvGradientCaption.GetLabelLeft: Integer;
begin
  Result := FLabelLeft;
end;

procedure TJvGradientCaption.SetLabelLeft(Value: Integer);
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

function TJvGradientCaption.GetLabelTop: Integer;
begin
  Result := FLabel.Top;
end;

procedure TJvGradientCaption.SetLabelTop(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  FLabel.Top := Value;
end;

function TJvGradientCaption.GetLabelCursor: TCursor;
begin
  Result := FLabel.Cursor;
end;

procedure TJvGradientCaption.SetLabelCursor(Value: TCursor);
begin
  FLabel.Cursor := Value;
end;

function TJvGradientCaption.GetLabelHint: string;
begin
  Result := FLabel.Hint;
end;

procedure TJvGradientCaption.SetLabelHint(Value: string);
begin
  FLabel.Hint := Value;
end;

function TJvGradientCaption.GetLabelCaption: string;
begin
  Result := FLabel.Caption;
end;

procedure TJvGradientCaption.SetLabelCaption(Value: string);
begin
  FLabel.Caption := Value;
  AdjustLabelWidth;
end;

function TJvGradientCaption.GetLabelColor: TColor;
begin
  Result := FLabel.Color;
end;

procedure TJvGradientCaption.SetLabelColor(Value: TColor);
begin
  FLabel.Color := Value;
  FLabel.Transparent := (Value = clNone);
end;

procedure TJvGradientCaption.SetHints(const Value: Boolean);
begin
  FHint := Value;
  FLabel.ShowHint := Value;
  FGradient.ShowHint := Value;
end;

function TJvGradientCaption.GetFont: TFont;
begin
  Result := FLabel.Font;
end;

procedure TJvGradientCaption.SetFont(const Value: TFont);
begin
  FLabel.Font := Value;
  AdjustLabelWidth;
end;

function TJvGradientCaption.GetGStyle: TJvGradStyle;
begin
  Result := FGradient.Style;
end;

procedure TJvGradientCaption.SetGStyle(const Value: TJvGradStyle);
begin
  FGradient.Style := Value;
end;

function TJvGradientCaption.GetAlignment: TAlignment;
begin
  Result := FLabel.Alignment;
end;

procedure TJvGradientCaption.Setalignment(const Value: TAlignment);
begin
  FLabel.Alignment := Value;
  AdjustLabelWidth;
end;

procedure TJvGradientCaption.WMSize(var Msg: TWMSize);
begin
  inherited;
  AdjustLabelWidth;
end;

procedure TJvGradientCaption.AdjustLabelWidth;
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

procedure TJvGradientCaption.DoLabelFontChange(Sender: TObject);
begin
  if Assigned(FOldLabelFontChange) then
    FOldLabelFontChange(Sender);
  AdjustLabelWidth;
end;

procedure TJvGradientCaption.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  { Reduce flickering FGradient completely fills the TJvGradientCaption }
  Msg.Result := 1;
end;

end.


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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, JvGradient, JvTypes, JVCLVer;

type
  TJvGradientCaption = class(TWinControl)
  private
    FJvGradient1: TJvGradient;
    FLabel1: TLabel;
    FLabelLeft: integer;
    FHint: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    FOldLabelFontChange: TNotifyEvent;
    function GetJvGradient1Cursor: TCursor;
    procedure SetJvGradient1Cursor(Value: TCursor);
    function GetJvGradient1Hint: string;
    procedure SetJvGradient1Hint(Value: string);
    function GetJvGradient1StartColor: TColor;
    procedure SetJvGradient1StartColor(Value: TColor);
    function GetJvGradient1EndColor: TColor;
    procedure SetJvGradient1EndColor(Value: TColor);
    function GetJvGradient1Steps: Integer;
    procedure SetJvGradient1Steps(Value: Integer);
    function GetLabel1Left: Integer;
    procedure SetLabel1Left(Value: Integer);
    function GetLabel1Top: Integer;
    procedure SetLabel1Top(Value: Integer);
    function GetLabel1Cursor: TCursor;
    procedure SetLabel1Cursor(Value: TCursor);
    function GetLabel1Hint: string;
    procedure SetLabel1Hint(Value: string);
    function GetLabel1Caption: string;
    procedure SetLabel1Caption(Value: string);
    function GetLabel1Color: TColor;
    procedure SetLabel1Color(Value: TColor);
    procedure SetHints(const Value: Boolean);
    function GetFont: Tfont;
    procedure SetFont(const Value: Tfont);
    function GetGStyle: TGradStyle;
    procedure SetGstyle(const Value: TGradStyle);
    function GetAlignment: TAlignment;
    procedure Setalignment(const Value: TAlignment);
    procedure AdjustLabelWidth;
  protected
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure DoLabelFontChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property GradientCursor: TCursor read GetJvGradient1Cursor write SetJvGradient1Cursor default crDefault;
    property GradientHint: string read GetJvGradient1Hint write SetJvGradient1Hint;
    property GradientStartColor: TColor read GetJvGradient1StartColor write SetJvGradient1StartColor default clBlack;
    property GradientEndColor: TColor read GetJvGradient1EndColor write SetJvGradient1EndColor default clWhite;
    property GradientSteps: Integer read GetJvGradient1Steps write SetJvGradient1Steps default 100;
    property GradientStyle: TGradStyle read GetGStyle write SetGstyle;
    property LabelLeft: Integer read GetLabel1Left write SetLabel1Left default 10;
    property LabelTop: Integer read GetLabel1Top write SetLabel1Top default 8;
    property LabelCursor: TCursor read GetLabel1Cursor write SetLabel1Cursor default crDefault;
    property LabelHint: string read GetLabel1Hint write SetLabel1Hint;
    property LabelCaption: string read GetLabel1Caption write SetLabel1Caption;
    // LabelColor sets the background Color of the label (used for text in the control).
    // To get a transparent text background, set LabelColor to clNone
    property LabelColor: TColor read GetLabel1Color write SetLabel1Color default clNone;
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

  {***************************************************}

constructor TJvGradientCaption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Width := 285;
  Self.Height := 30;
  FJvGradient1 := TJvGradient.Create(Self);
  FJvGradient1.Parent := Self;
  FLabel1 := TLabel.Create(Self);
  FLabel1.AutoSize := false;
  FLabel1.Parent := Self;
  FJvGradient1.Left := 0;
  FJvGradient1.Top := 0;
  FJvGradient1.StartColor := clBlack;
  FJvGradient1.EndColor := clWhite;
  FJvGradient1.Steps := 100;
  LabelLeft := 10;
  FLabel1.Top := 8;
  LabelColor := clNone;
  FOldLabelFontChange := FLabel1.Font.OnChange;
  FLabel1.Font.OnChange := DoLabelFontChange;
  FLabel1.Font.Color := clwhite;
  FLabel1.Caption := RC_YourTextHere;
  FHint := False;
end;
{***************************************************}

destructor TJvGradientCaption.Destroy;
begin
  FJvGradient1.Free;
  //  FLabel1.OnChange := FOldLabelFontChange;
  FLabel1.Free;
  inherited Destroy;
end;
{***************************************************}

function TJvGradientCaption.GetJvGradient1Cursor: TCursor;
begin
  Result := FJvGradient1.Cursor;
end;
{***************************************************}

procedure TJvGradientCaption.SetJvGradient1Cursor(Value: TCursor);
begin
  FJvGradient1.Cursor := Value;
end;
{***************************************************}

function TJvGradientCaption.GetJvGradient1Hint: string;
begin
  Result := FJvGradient1.Hint;
end;
{***************************************************}

procedure TJvGradientCaption.SetJvGradient1Hint(Value: string);
begin
  FJvGradient1.Hint := Value;
end;
{***************************************************}

function TJvGradientCaption.GetJvGradient1StartColor: TColor;
begin
  Result := FJvGradient1.StartColor;
end;
{***************************************************}

procedure TJvGradientCaption.SetJvGradient1StartColor(Value: TColor);
begin
  FJvGradient1.StartColor := Value;
end;
{***************************************************}

function TJvGradientCaption.GetJvGradient1EndColor: TColor;
begin
  Result := FJvGradient1.EndColor;
end;
{***************************************************}

procedure TJvGradientCaption.SetJvGradient1EndColor(Value: TColor);
begin
  FJvGradient1.EndColor := Value;
end;
{***************************************************}

function TJvGradientCaption.GetJvGradient1Steps: Integer;
begin
  Result := FJvGradient1.Steps;
end;
{***************************************************}

procedure TJvGradientCaption.SetJvGradient1Steps(Value: Integer);
begin
  FJvGradient1.Steps := Value;
end;
{***************************************************}

function TJvGradientCaption.GetLabel1Left: Integer;
begin
  Result := FLabelLeft;
end;
{***************************************************}

procedure TJvGradientCaption.SetLabel1Left(Value: Integer);
begin
  if FLabel1.Left <> Value then
  begin
    if Value < 0 then Value := 0;
    FLabel1.Left := Value;
    FLabelLeft := Value;
    AdjustLabelWidth;
  end;
end;
{***************************************************}

function TJvGradientCaption.GetLabel1Top: Integer;
begin
  Result := FLabel1.Top;
end;
{***************************************************}

procedure TJvGradientCaption.SetLabel1Top(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FLabel1.Top := Value;
end;
{***************************************************}

function TJvGradientCaption.GetLabel1Cursor: TCursor;
begin
  Result := FLabel1.Cursor;
end;
{***************************************************}

procedure TJvGradientCaption.SetLabel1Cursor(Value: TCursor);
begin
  FLabel1.Cursor := Value;
end;
{***************************************************}

function TJvGradientCaption.GetLabel1Hint: string;
begin
  Result := FLabel1.Hint;
end;
{***************************************************}

procedure TJvGradientCaption.SetLabel1Hint(Value: string);
begin
  FLabel1.Hint := Value;
end;
{***************************************************}

function TJvGradientCaption.GetLabel1Caption: string;
begin
  Result := FLabel1.Caption;
end;
{***************************************************}

procedure TJvGradientCaption.SetLabel1Caption(Value: string);
begin
  FLabel1.Caption := Value;
  AdjustLabelWidth;
end;
{***************************************************}

function TJvGradientCaption.GetLabel1Color: TColor;
begin
  Result := FLabel1.Color;
end;
{***************************************************}

procedure TJvGradientCaption.SetLabel1Color(Value: TColor);
begin
  FLabel1.Color := Value;
  FLabel1.Transparent := Value = clNone;
end;
{***************************************************}

procedure TJvGradientCaption.SetHints(const Value: Boolean);
begin
  FHint := Value;
  FLabel1.ShowHint := Value;
  FJvGradient1.showhint := Value;
end;
{***************************************************}

function TJvGradientCaption.GetFont: Tfont;
begin
  Result := FLabel1.Font;
end;
{***************************************************}

procedure TJvGradientCaption.SetFont(const Value: Tfont);
begin
  Flabel1.Font := Value;
  AdjustLabelWidth;
end;
{***************************************************}

function TJvGradientCaption.GetGStyle: TGradStyle;
begin
  Result := FJvGradient1.Style;
end;
{***************************************************}

procedure TJvGradientCaption.SetGStyle(const Value: TGradStyle);
begin
  FJvGradient1.Style := Value;
end;
{***************************************************}

function TJvGradientCaption.GetAlignment: TAlignment;
begin
  Result := FLabel1.Alignment;
end;
{***************************************************}

procedure TJvGradientCaption.Setalignment(const Value: TAlignment);
begin
  FLabel1.Alignment := Value;
  AdjustLabelWidth;
end;
{***************************************************}

procedure TJvGradientCaption.WMSize(var Message: TWMSize);
begin
  inherited;
  AdjustLabelWidth;
end;
{***************************************************}

procedure TJvGradientCaption.AdjustLabelWidth;
var W, L: integer;
begin
  L := FLabel1.Left;
  // make as large as we need:
  FLabel1.AutoSize := true;
  FLabel1.AutoSize := false;
  FLabel1.Left := L;
  W := FJvGradient1.Width - FLabelLeft - FLabelLeft;
  // make bigger if there's room
  if W > FLabel1.Width then
  begin
    FLabel1.Width := W;
    FLabel1.Left := FLabelLeft;
  end
  else if W < FLabel1.Width then // otherwise, just center
  begin
    FLabel1.Left := (Width - FLabel1.Width) div 2;
//    if (FLabelLeft > FLabel1.Left) and  then
//      FLabelLeft := FLabel1.Left;
  end;
end;

procedure TJvGradientCaption.DoLabelFontChange(Sender: TObject);
begin
  if Assigned(FOldLabelFontChange) then
    FOldLabelFontChange(Sender);
  AdjustLabelWidth;
end;

end.


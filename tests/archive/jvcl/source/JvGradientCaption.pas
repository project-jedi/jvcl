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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, JvGradient, JvTypes, JVCLVer;

type
  TJvGradientCaption = class(TWinControl)
  private
    FJvGradient1: TJvGradient;
    FLabel1: TLabel;
    FHint: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    function GetJvGradient1Cursor: TCursor;
    procedure SetJvGradient1Cursor(Value: TCursor);
    function GetJvGradient1Hint: string;
    procedure SetJvGradient1Hint(Value: string);
    function GetJvGradient1StarTColor: TColor;
    procedure SetJvGradient1StarTColor(Value: TColor);
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
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property GradientCursor: TCursor read GetJvGradient1Cursor write SetJvGradient1Cursor;
    property GradientHint: string read GetJvGradient1Hint write SetJvGradient1Hint;
    property GradientStarTColor: TColor read GetJvGradient1StarTColor write SetJvGradient1StarTColor default clBlack;
    property GradientEndColor: TColor read GetJvGradient1EndColor write SetJvGradient1EndColor default clWhite;
    property GradientSteps: Integer read GetJvGradient1Steps write SetJvGradient1Steps default 100;
    property LabelLeft: Integer read GetLabel1Left write SetLabel1Left default 18;
    property LabelTop: Integer read GetLabel1Top write SetLabel1Top default 8;
    property LabelCursor: TCursor read GetLabel1Cursor write SetLabel1Cursor;
    property LabelHint: string read GetLabel1Hint write SetLabel1Hint;
    property LabelCaption: string read GetLabel1Caption write SetLabel1Caption;
    property LabelColor: TColor read GetLabel1Color write SetLabel1Color;
    property LabelFont: Tfont read GetFont write SetFont;
    property ShowHint: Boolean read FHint write SetHints default False;
    property Gradientstyle: TGradStyle read GetGStyle write SetGstyle;
    property LabelAlignment: TAlignment read GetAlignment write Setalignment;
    property Align;
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
  FLabel1.Parent := Self;
  FJvGradient1.Left := 0;
  FJvGradient1.Top := 0;
  FJvGradient1.StarTColor := clBlack;
  FJvGradient1.EndColor := clWhite;
  FJvGradient1.Steps := 100;
  FLabel1.Left := 10;
  FLabel1.Top := 8;
  FLabel1.Color := clBlue;
  FLabel1.Transparent := True;
  FLabel1.Font.color := clwhite;
  FLabel1.caption := RC_YourTextHere;
  FHint := False;
end;
{***************************************************}

destructor TJvGradientCaption.Destroy;
begin
  FJvGradient1.Free;
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

function TJvGradientCaption.GetJvGradient1StarTColor: TColor;
begin
  Result := FJvGradient1.StarTColor;
end;
{***************************************************}

procedure TJvGradientCaption.SetJvGradient1StarTColor(Value: TColor);
begin
  FJvGradient1.StarTColor := Value;
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
var
  x: TColor;
begin
  FJvGradient1.Steps := Value;
  x := GetLabel1Color;
  SetLabel1color(clLime);
  SetLabel1color(x);
end;
{***************************************************}

function TJvGradientCaption.GetLabel1Left: Integer;
begin
  Result := FLabel1.Left;
end;
{***************************************************}

procedure TJvGradientCaption.SetLabel1Left(Value: Integer);
begin
  FLabel1.Left := Value;
end;
{***************************************************}

function TJvGradientCaption.GetLabel1Top: Integer;
begin
  Result := FLabel1.Top;
end;
{***************************************************}

procedure TJvGradientCaption.SetLabel1Top(Value: Integer);
begin
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
end;
{***************************************************}

function TJvGradientCaption.GetGStyle: TGradStyle;
begin
  Result := FJvGradient1.Style;
end;
{***************************************************}

procedure TJvGradientCaption.SetGStyle(const Value: TGradStyle);
var
  x: TColor;
begin
  FJvGradient1.Style := Value;
  x := GetLabel1Color;
  SetLabel1color(clLime);
  SetLabel1color(x);
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
end;
{***************************************************}

end.

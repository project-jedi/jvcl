{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGammaPanel.PAS, released on 2001-02-28.

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

unit JvGammaPanel;

{*******************************************************}
{  Modifications:                                       }
{     2/11/2000 Added the Align and AutoSize property   }
{               (Request of Brad T.)                    }
{*******************************************************}



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, JvTypes, JVCLVer;

type
  TJvGammaPanel = class(TWinControl)
  private
    FCol1: TColor;
    FCol2: TColor;
    LastCol: TColor;
    FPanel1: TPanel;
    FPanel2: TPanel;
    FPanel3: TPanel;
    FPanel4: TPanel;
    FRLabel: TLabel;
    FGLabel: TLabel;
    FBLabel: TLabel;
    FXLabel: TLabel;
    FGamma: TImage;
    FChoosed: TImage;
    FColor1: TImage;
    FColor2: TImage;
    FOnChangeCol: TOnChangeColor;
    FAboutJVCL: TJVCLAboutInfo;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure ChangeColor(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ColorSeek(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Exchange(Sender: TObject);
    procedure SetCol1(const Value: TColor);
    procedure Setcol2(const Value: TColor);
    procedure Color1Click(Sender: TObject);
    procedure Color2Click(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property AutoSize;
    property OnChangeColor: TOnChangeColor read FOnChangeCol write FOnChangeCol;
    property ForegroundColor: TColor read FCol1 write SetCol1 default clBlack;
    property BackgroundColor: TColor read FCol2 write Setcol2 default clWhite;
  end;

implementation

{$R RES_Gamma.res}

resourcestring
  RC_RedFormat = 'R : %3D';
  RC_GreenFormat = 'G : %3D';
  RC_BlueFormat = 'B : %3D';

  RC_Hint1 = 'Background Color';
  RC_Hint2 = 'Foreground Color';
  RC_LabelCaption = 'X';
  RC_LabelHint = 'Exchange colors';

  RC_DefaultB = 'B : ---';
  RC_DefaultG = 'G : ---';
  RC_DefaultR = 'R : ---';

  {*******************************************************}

procedure TJvGammaPanel.ChangeColor(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FCol1 := LastCol;
    FColor1.Canvas.Brush.Color := FCol1;
    FColor1.Canvas.Brush.Style := bsSolid;
    FColor1.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
    if Assigned(FOnChangeCol) then
      FOnChangeCol(Self, FCol1, FCol2);
  end
  else if Button = mbRight then
  begin
    FCol2 := LastCol;
    FColor2.Canvas.Brush.Color := FCol2;
    FColor2.Canvas.Brush.Style := bsSolid;
    FColor2.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
    if Assigned(FOnChangeCol) then
      FOnChangeCol(Self, FCol1, FCol2);
  end;
end;

{*******************************************************}

procedure TJvGammaPanel.Color1Click(Sender: TObject);
begin

  with TColorDialog.Create(Self) do
  begin
    if Execute then
      SetCol1(Color);
    Free;
  end;
end;

{*******************************************************}

procedure TJvGammaPanel.Color2Click(Sender: TObject);
begin
  with TColorDialog.Create(Self) do
  begin
    if Execute then
      SetCol2(Color);
    Free;
  end;
end;

{*******************************************************}

procedure TJvGammaPanel.ColorSeek(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  col: TColor;
begin
  col := FGamma.Picture.Bitmap.Canvas.Pixels[X, Y];
  LastCol := col;
  FRLabel.Caption := Format(RC_RedFormat, [GetRValue(col)]);
  FGLabel.Caption := Format(RC_GreenFormat, [GetGValue(col)]);
  FBLabel.Caption := Format(RC_BlueFormat, [GetBValue(col)]);
  FChoosed.Canvas.Brush.Color := col;
  FChoosed.Canvas.Brush.Style := bsSolid;
  FChoosed.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
end;

{*******************************************************}

constructor TJvGammaPanel.Create(AOwner: TComponent);
begin
  inherited;
  Width := 65;
  Height := 250;
  FCOl1 := clBlack;
  FCol2 := clWhite;

  FPanel1 := TPanel.Create(Self);
  FPanel1.Parent := Self;
  FPanel1.Width := 65;
  FPanel1.Height := 250;
  FPanel1.Align := alClient;
  FPanel1.BevelInner := bvLowered;
  FPanel1.BevelOuter := bvRaised;
  FPanel1.Visible := True;

  FPanel2 := TPanel.Create(FPanel1);
  FPanel2.Parent := FPanel1;
  FPanel2.Left := 5;
  FPanel2.Top := 5;
  FPanel2.Width := 55;
  FPanel2.Height := 105;
  FPanel2.BevelInner := bvLowered;
  FPanel2.BevelOuter := bvRaised;
  FPanel2.Visible := True;

  FPanel3 := TPanel.Create(FPanel1);
  FPanel3.Parent := FPanel1;
  FPanel3.Left := 5;
  FPanel3.Top := 115;
  FPanel3.Width := 55;
  FPanel3.Height := 50;
  FPanel3.BevelInner := bvLowered;
  FPanel3.BevelOuter := bvRaised;
  FPanel3.Visible := True;

  FPanel4 := TPanel.Create(FPanel1);
  FPanel4.Parent := FPanel1;
  FPanel4.Left := 5;
  FPanel4.Top := 170;
  FPanel4.Width := 55;
  FPanel4.Height := 75;
  FPanel4.BevelInner := bvLowered;
  FPanel4.BevelOuter := bvRaised;
  FPanel4.Visible := True;

  FRLabel := TLabel.Create(FPanel4);
  FRLabel.Top := 2;
  FRLabel.Left := 5;
  FRLabel.Font.Size := 8;
  FRLabel.AutoSize := True;
  FRLabel.Font.Name := 'arial';
  FRLabel.Caption := RC_DefaultR;
  FRLabel.Transparent := True;
  FRLabel.Parent := FPanel4;

  FGLabel := TLabel.Create(FPanel4);
  FGLabel.Top := 14;
  FGLabel.Left := 5;
  FGLabel.AutoSize := True;
  FGLabel.Font.Name := 'arial';
  FGLabel.Font.Size := 8;
  FGLabel.Caption := RC_DefaultG;
  FGLabel.Transparent := True;
  FGLabel.Parent := FPanel4;

  FBLabel := TLabel.Create(FPanel4);
  FBLabel.Top := 26;
  FBLabel.Left := 5;
  FBLabel.Font.Size := 8;
  FBLabel.Font.Name := 'arial';
  FBLabel.AutoSize := True;
  FBLabel.Caption := RC_DefaultB;
  FBLabel.Transparent := True;
  FBLabel.Parent := FPanel4;

  FGamma := TImage.Create(FPanel2);
  FGamma.Parent := FPanel2;
  FGamma.Stretch := False;
  FGamma.Center := True;
  FGamma.AutoSize := True;
  FGamma.Picture.Bitmap.PixelFormat := pf24bit;
  FGamma.Width := 55;
  FGamma.Height := 105;
  FGamma.OnMouseDown := ChangeColor;
  FGamma.OnMouseMove := ColorSeek;
  FGamma.Align := alClient;
  FGamma.Picture.Bitmap.LoadFromResourceName(HInstance, 'COLORS');
  FGamma.Cursor := crCross;

  FChoosed := TImage.Create(FPanel4);
  FChoosed.Top := 40;
  FChoosed.Left := 12;
  FChoosed.Width := 30;
  FChoosed.Height := 30;
  FChoosed.Parent := FPanel4;
  FChoosed.Visible := True;
  FChoosed.Stretch := False;
  FChoosed.Align := alNone;
  FChoosed.Picture.Bitmap := TBItmap.Create;
  FChoosed.Picture.Bitmap.Width := FChoosed.Width;
  FChoosed.Picture.Bitmap.Height := FChoosed.Height;
  FChoosed.Canvas.Brush.Color := clBlack;
  FChoosed.Canvas.Brush.Style := bsSolid;
  FChoosed.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));

  FColor1 := TImage.Create(FPanel3);
  FColor2 := TImage.Create(FPanel3);
  FColor1.Left := 5;
  FColor1.Top := 5;
  FColor1.Width := 25;
  FColor1.Height := 25;

  FColor2.Left := 25;
  FColor2.Top := 20;
  FColor2.Height := 25;
  FColor2.Width := 25;
  FColor2.Visible := True;
  FColor1.Visible := True;
  FColor1.Parent := FPanel3;
  FColor2.Parent := FPanel3;

  FColor2.Picture.Bitmap := TBItmap.Create;
  FColor2.Picture.Bitmap.Width := FChoosed.Width;
  FColor2.Picture.Bitmap.Height := FChoosed.Height;
  FColor2.Canvas.Brush.Color := clWhite;
  FColor2.Canvas.Brush.Style := bsSolid;
  FColor2.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
  FCOlor2.Hint := RC_Hint1;
  FColor2.ShowHint := True;
  FColor2.OnClick := Color2Click;

  FColor1.Picture.Bitmap := TBItmap.Create;
  FColor1.Picture.Bitmap.Width := FChoosed.Width;
  FColor1.Picture.Bitmap.Height := FChoosed.Height;
  FColor1.Canvas.Brush.Color := clBlack;
  FColor1.Canvas.Brush.Style := bsSolid;
  FColor1.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
  FCOlor1.Hint := RC_Hint2;
  FColor1.ShowHint := True;
  FColor1.OnClick := Color1Click;

  FXLabel := TLabel.Create(FPanel3);
  FXLabel.Left := 7;
  FXLabel.Top := 32;
  FXLabel.AutoSize := True;
  FXLabel.Caption := RC_LabelCaption;
  FXLabel.Hint := RC_LabelHint;
  FXLabel.OnClick := Exchange;
  FXLabel.ShowHint := True;
  FXLabel.Visible := True;
  FXLabel.Parent := FPanel3;
end;

{*******************************************************}

destructor TJvGammaPanel.Destroy;
begin
  FXLabel.Free;
  FColor2.Free;
  FColor1.Free;
  FGamma.Free;
  FChoosed.Free;
  FRLabel.Free;
  FGLabel.Free;
  FBLabel.Free;
  FPanel2.Free;
  FPanel3.Free;
  FPanel4.Free;
  FPanel1.Free;
  inherited;
end;

{*******************************************************}

procedure TJvGammaPanel.Exchange(Sender: TObject);
var
  t: TColor;
begin
  //exchange colors
  t := FCol1;
  FCol1 := FCol2;
  FCol2 := t;

  FColor1.Canvas.Brush.Color := FCol1;
  FColor1.Canvas.Brush.Style := bsSolid;
  FColor1.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));

  FColor2.Canvas.Brush.Color := FCol2;
  FColor2.Canvas.Brush.Style := bsSolid;
  FColor2.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));

  if Assigned(FOnChangeCol) then
    FOnChangeCol(Self, FCol1, FCol2);
end;

{*******************************************************}

procedure TJvGammaPanel.SetCol1(const Value: TColor);
begin
  FCol1 := Value;
  FColor1.Canvas.Brush.Color := FCol1;
  FColor1.Canvas.Brush.Style := bsSolid;
  FColor1.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
  if Assigned(FOnChangeCol) then
    FOnChangeCol(Self, FCol1, FCol2);
end;

{*******************************************************}

procedure TJvGammaPanel.Setcol2(const Value: TColor);
begin
  FCol2 := Value;
  FColor2.Canvas.Brush.Color := FCol2;
  FColor2.Canvas.Brush.Style := bsSolid;
  FColor2.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
  if Assigned(FOnChangeCol) then
    FOnChangeCol(Self, FCol1, FCol2);
end;

{*******************************************************}

procedure TJvGammaPanel.WMSize(var Msg: TWMSize);
begin
  Self.Width := 65;
  Self.Height := 250;
  Self.FColor1.BringToFront;
end;

end.

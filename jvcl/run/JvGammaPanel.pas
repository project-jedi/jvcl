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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs, ExtCtrls, StdCtrls,
  JvTypes, JVCLVer;

type
  TJvGammaPanel = class(TWinControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FForegroundColor: TColor;
    FBackgroundColor: TColor;
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
    FForegroundColorImg: TImage;
    FBackgroundColorImg: TImage;
    FOnChangeColor: TJvChangeColorEvent;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure ChangeColor(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ColorSeek(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Exchange(Sender: TObject);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetBackgroundColor(const Value: TColor);
    procedure Color1Click(Sender: TObject);
    procedure Color2Click(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property AutoSize;
    property OnChangeColor: TJvChangeColorEvent read FOnChangeColor write FOnChangeColor;
    property ForegroundColor: TColor read FForegroundColor write SetForegroundColor default clBlack;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWhite;
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

constructor TJvGammaPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 65;
  Height := 250;
  FForegroundColor := clBlack;
  FBackgroundColor := clWhite;

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

  FForegroundColorImg := TImage.Create(FPanel3);
  FBackgroundColorImg := TImage.Create(FPanel3);
  FForegroundColorImg.Left := 5;
  FForegroundColorImg.Top := 5;
  FForegroundColorImg.Width := 25;
  FForegroundColorImg.Height := 25;

  FBackgroundColorImg.Left := 25;
  FBackgroundColorImg.Top := 20;
  FBackgroundColorImg.Height := 25;
  FBackgroundColorImg.Width := 25;
  FBackgroundColorImg.Visible := True;
  FForegroundColorImg.Visible := True;
  FForegroundColorImg.Parent := FPanel3;
  FBackgroundColorImg.Parent := FPanel3;

  FBackgroundColorImg.Picture.Bitmap := TBItmap.Create;
  FBackgroundColorImg.Picture.Bitmap.Width := FChoosed.Width;
  FBackgroundColorImg.Picture.Bitmap.Height := FChoosed.Height;
  FBackgroundColorImg.Canvas.Brush.Color := clWhite;
  FBackgroundColorImg.Canvas.Brush.Style := bsSolid;
  FBackgroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
  FBackgroundColorImg.Hint := RC_Hint1;
  FBackgroundColorImg.ShowHint := True;
  FBackgroundColorImg.OnClick := Color2Click;

  FForegroundColorImg.Picture.Bitmap := TBItmap.Create;
  FForegroundColorImg.Picture.Bitmap.Width := FChoosed.Width;
  FForegroundColorImg.Picture.Bitmap.Height := FChoosed.Height;
  FForegroundColorImg.Canvas.Brush.Color := clBlack;
  FForegroundColorImg.Canvas.Brush.Style := bsSolid;
  FForegroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
  FForegroundColorImg.Hint := RC_Hint2;
  FForegroundColorImg.ShowHint := True;
  FForegroundColorImg.OnClick := Color1Click;

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

destructor TJvGammaPanel.Destroy;
begin
  FXLabel.Free;
  FBackgroundColorImg.Free;
  FForegroundColorImg.Free;
  FGamma.Free;
  FChoosed.Free;
  FRLabel.Free;
  FGLabel.Free;
  FBLabel.Free;
  FPanel2.Free;
  FPanel3.Free;
  FPanel4.Free;
  FPanel1.Free;
  inherited Destroy;
end;

procedure TJvGammaPanel.ChangeColor(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FForegroundColor := LastCol;
    FForegroundColorImg.Canvas.Brush.Color := FForegroundColor;
    FForegroundColorImg.Canvas.Brush.Style := bsSolid;
    FForegroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
    if Assigned(FOnChangeColor) then
      FOnChangeColor(Self, FForegroundColor, FBackgroundColor);
  end
  else
  if Button = mbRight then
  begin
    FBackgroundColor := LastCol;
    FBackgroundColorImg.Canvas.Brush.Color := FBackgroundColor;
    FBackgroundColorImg.Canvas.Brush.Style := bsSolid;
    FBackgroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
    if Assigned(FOnChangeColor) then
      FOnChangeColor(Self, FForegroundColor, FBackgroundColor);
  end;
end;

procedure TJvGammaPanel.Color1Click(Sender: TObject);
begin
  with TColorDialog.Create(Self) do
  begin
    if Execute then
      SetForegroundColor(Color);
    Free;
  end;
end;

procedure TJvGammaPanel.Color2Click(Sender: TObject);
begin
  with TColorDialog.Create(Self) do
  begin
    if Execute then
      SetBackgroundColor(Color);
    Free;
  end;
end;

procedure TJvGammaPanel.ColorSeek(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Col: TColor;
begin
  Col := FGamma.Picture.Bitmap.Canvas.Pixels[X, Y];
  LastCol := col;
  FRLabel.Caption := Format(RC_RedFormat, [GetRValue(Col)]);
  FGLabel.Caption := Format(RC_GreenFormat, [GetGValue(Col)]);
  FBLabel.Caption := Format(RC_BlueFormat, [GetBValue(Col)]);
  FChoosed.Canvas.Brush.Color := Col;
  FChoosed.Canvas.Brush.Style := bsSolid;
  FChoosed.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
end;

procedure TJvGammaPanel.Exchange(Sender: TObject);
var
  Col: TColor;
begin
  // exchange colors
  Col := FForegroundColor;
  FForegroundColor := FBackgroundColor;
  FBackgroundColor := Col;

  FForegroundColorImg.Canvas.Brush.Color := FForegroundColor;
  FForegroundColorImg.Canvas.Brush.Style := bsSolid;
  FForegroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));

  FBackgroundColorImg.Canvas.Brush.Color := FBackgroundColor;
  FBackgroundColorImg.Canvas.Brush.Style := bsSolid;
  FBackgroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));

  if Assigned(FOnChangeColor) then
    FOnChangeColor(Self, FForegroundColor, FBackgroundColor);
end;

procedure TJvGammaPanel.SetForegroundColor(const Value: TColor);
begin
  FForegroundColor := Value;
  FForegroundColorImg.Canvas.Brush.Color := FForegroundColor;
  FForegroundColorImg.Canvas.Brush.Style := bsSolid;
  FForegroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
  if Assigned(FOnChangeColor) then
    FOnChangeColor(Self, FForegroundColor, FBackgroundColor);
end;

procedure TJvGammaPanel.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  FBackgroundColorImg.Canvas.Brush.Color := FBackgroundColor;
  FBackgroundColorImg.Canvas.Brush.Style := bsSolid;
  FBackgroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
  if Assigned(FOnChangeColor) then
    FOnChangeColor(Self, FForegroundColor, FBackgroundColor);
end;

procedure TJvGammaPanel.WMSize(var Msg: TWMSize);
begin
  Self.Width := 65;
  Self.Height := 250;
  Self.FForegroundColorImg.BringToFront;
end;

end.


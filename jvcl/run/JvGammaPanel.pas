{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGammaPanel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Modifications:
  2/11/2000 Added the Align and AutoSize property (Request of Brad T.)
  2004/01/06 VisualCLX compatibilty

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGammaPanel;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Windows, Messages, Graphics, Controls, Dialogs,
  ExtCtrls, StdCtrls,
  JvTypes, JvComponent;

type
  TJvGammaPanel = class(TJvWinControl)
  private
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
    procedure ChangeColor(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ColorSeek(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Exchange(Sender: TObject);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetBackgroundColor(const Value: TColor);
    procedure Color1Click(Sender: TObject);
    procedure Color2Click(Sender: TObject);
  protected
    procedure DoBoundsChanged; override;
    procedure DoChangeColor(AForegroundColor, ABackgroundColor: TColor); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    {$IFDEF VCL}
    property AutoSize;
    {$ENDIF VCL}
    property Height default 250;
    property Width default 65;
    property ForegroundColor: TColor read FForegroundColor write SetForegroundColor default clBlack;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWhite;
    property OnChangeColor: TJvChangeColorEvent read FOnChangeColor write FOnChangeColor;
  end;

implementation

uses
  JvResources;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvGammaPanel.res}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvGammaPanel.res}
{$ENDIF UNIX}

constructor TJvGammaPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 65;
  Height := 250;
  FForegroundColor := clBlack;
  FBackgroundColor := clWhite;

  FPanel1 := TPanel.Create(Self);
  with FPanel1 do
  begin
    Parent := Self;
    Width := 65;
    Height := 250;
    Align := alClient;
    BevelInner := bvLowered;
    BevelOuter := bvRaised;
    Visible := True;
  end;

  FPanel2 := TPanel.Create(FPanel1);
  with FPanel2 do
  begin
    Parent := FPanel1;
    Left := 5;
    Top := 5;
    Width := 55;
    Height := 105;
    BevelInner := bvLowered;
    BevelOuter := bvRaised;
    Visible := True;
  end;

  FPanel3 := TPanel.Create(FPanel1);
  with FPanel3 do
  begin
    Parent := FPanel1;
    Left := 5;
    Top := 115;
    Width := 55;
    Height := 50;
    BevelInner := bvLowered;
    BevelOuter := bvRaised;
    Visible := True;
  end;

  FPanel4 := TPanel.Create(FPanel1);
  with FPanel4 do
  begin
    Parent := FPanel1;
    Left := 5;
    Top := 170;
    Width := 55;
    Height := 75;
    BevelInner := bvLowered;
    BevelOuter := bvRaised;
    Visible := True;
  end;

  FRLabel := TLabel.Create(FPanel4);
  with FRLabel do
  begin
    Top := 2;
    Left := 5;
    AutoSize := True;
    {$IFDEF MSWINDOWS}
    Font.Size := 8;
    Font.Name := 'Arial';
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    Font.Height := 13;
    Font.Name := 'Helvetica';
    {$ENDIF UNIX}
    Caption := RsDefaultR;
    Transparent := True;
    Parent := FPanel4;
  end;

  FGLabel := TLabel.Create(FPanel4);
  with FGLabel do
  begin
    Top := 14;
    Left := 5;
    AutoSize := True;
    Font.Name := 'Arial';
    Font.Size := 8;
    Caption := RsDefaultG;
    Transparent := True;
    Parent := FPanel4;
  end;

  FBLabel := TLabel.Create(FPanel4);
  with FBLabel do
  begin
    Top := 26;
    Left := 5;
    Font.Size := 8;
    Font.Name := 'arial';
    AutoSize := True;
    Caption := RsDefaultB;
    Transparent := True;
    Parent := FPanel4;
  end;

  FGamma := TImage.Create(FPanel2);
  with FGamma do
  begin
    Parent := FPanel2;
    Stretch := False;
    Center := True;
    AutoSize := True;
    Picture.Bitmap.PixelFormat := pf24bit;
    Width := 55;
    Height := 105;
    OnMouseDown := ChangeColor;
    OnMouseMove := ColorSeek;
    Align := alClient;
    Picture.Bitmap.LoadFromResourceName(HInstance, 'COLORS');
    Cursor := crCross;
  end;

  FChoosed := TImage.Create(FPanel4);
  with FChoosed do
  begin
    Top := 40;
    Left := 12;
    Width := 30;
    Height := 30;
    Parent := FPanel4;
    Visible := True;
    Stretch := False;
    Align := alNone;
//    Picture.Bitmap := TBitmap.Create;
    Picture.Bitmap.Width := Width;
    Picture.Bitmap.Height := Height;
    Canvas.Brush.Color := clBlack;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect(0, 0, Width, Height));
  end;

  FForegroundColorImg := TImage.Create(FPanel3);
  with FForegroundColorImg do
  begin
    Left := 5;
    Top := 5;
    Width := 25;
    Height := 25;
//    Picture.Bitmap := TBitmap.Create;
    Picture.Bitmap.Width := FChoosed.Width;
    Picture.Bitmap.Height := FChoosed.Height;
    Canvas.Brush.Color := clBlack;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
    Hint := RsHint2;
    ShowHint := True;
    OnClick := Color1Click;
    Parent := FPanel3;
    Visible := True;
  end;

  FBackgroundColorImg := TImage.Create(FPanel3);
  with FBackgroundColorImg do
  begin
    Left := 25;
    Top := 20;
    Height := 25;
    Width := 25;
//    Picture.Bitmap := TBitmap.Create;
    Picture.Bitmap.Width := FChoosed.Width;
    Picture.Bitmap.Height := FChoosed.Height;
    Canvas.Brush.Color := clWhite;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
    Hint := RsHint1;
    ShowHint := True;
    OnClick := Color2Click;
    Parent := FPanel3;
    Visible := True;
  end;

  FXLabel := TLabel.Create(FPanel3);
  with FXLabel do
  begin
    Left := 7;
    Top := 32;
    AutoSize := True;
    Caption := RsLabelCaption;
    Hint := RsLabelHint;
    OnClick := Exchange;
    ShowHint := True;
    Visible := True;
    Parent := FPanel3;
  end;
end;

procedure TJvGammaPanel.ChangeColor(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FForegroundColor := LastCol;
    FForegroundColorImg.Canvas.Brush.Color := FForegroundColor;
    FForegroundColorImg.Canvas.Brush.Style := bsSolid;
    FForegroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
    DoChangeColor(FForegroundColor, FBackgroundColor);
  end
  else
  if Button = mbRight then
  begin
    FBackgroundColor := LastCol;
    FBackgroundColorImg.Canvas.Brush.Color := FBackgroundColor;
    FBackgroundColorImg.Canvas.Brush.Style := bsSolid;
    FBackgroundColorImg.Canvas.FillRect(Rect(0, 0, FChoosed.Width, FChoosed.Height));
    DoChangeColor(FForegroundColor, FBackgroundColor);
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
  if not PtInRect(Bounds(0, 0, FGamma.Picture.Width, FGamma.Picture.Height), Point(X,Y)) then
    Exit; // asn for Linux/X11
  Col := FGamma.Picture.Bitmap.Canvas.Pixels[X, Y];
  LastCol := Col;
  FRLabel.Caption := Format(RsRedFormat, [GetRValue(Col)]);
  FGLabel.Caption := Format(RsGreenFormat, [GetGValue(Col)]);
  FBLabel.Caption := Format(RsBlueFormat, [GetBValue(Col)]);
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

procedure TJvGammaPanel.DoBoundsChanged;
begin
  Width := 65;
  Height := 250;
  if Assigned(FForegroundColorImg) then
  FForegroundColorImg.BringToFront;
end;

procedure TJvGammaPanel.DoChangeColor(AForegroundColor, ABackgroundColor: TColor);
begin
  if Assigned(FOnChangeColor) then
    FOnChangeColor(Self, FForegroundColor, FBackgroundColor);
end;

end.


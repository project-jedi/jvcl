{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpecialProgress.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvSpecialProgress;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,  StdCtrls ,JVCLVer;

type
  TJvSpecialProgress = class(TGraphicControl)
  private
    FSolid: Boolean;
    FTextVisible: Boolean;
    FCentered: Boolean;
    FTransparent: Boolean;
    FStep: Integer;
    FMaximum: Integer;
    FMinimum: Integer;
    FPosition: Integer;
    FEndColor: TColor;
    FStartColor: TColor;
    FColor: TColor;
    FFont: TFont;
    FOnMouseEnter: TNotifyEvent;
    FSColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SetCentered(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetEndColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetSolid(const Value: Boolean);
    procedure SetStartColor(const Value: TColor);
    procedure SetTextVisible(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure FontChanged(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL  stored False;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property StartColor: TColor read FStartColor write SetStartColor default clWhite;
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    property Step: Integer read FStep write FStep default 10;
    property Position: Integer read FPosition write SetPosition default 0;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Solid: Boolean read FSolid write SetSolid default False;
    property TextVisible: Boolean read FTextVisible write SetTextVisible default False;
    property TextFont: TFont read FFont write SetFont;
    property TextCentered: Boolean read FCentered write SetCentered default False;
    procedure StepIt;
    property Align;
    property Anchors;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

{**************************************************}

constructor TJvSpecialProgress.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];  // SMM 20020604
  FMaximum := 100;
  FMinimum := 0;
  FTransparent := False;
  FStartColor := clWhite;
  FEndColor := clBlack;
  FPosition := 0;
  FColor := clBtnFace;
  FSolid := False;
  FTextVisible := False;
  FCentered := False;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FSColor := clInfoBk;

  Width := 150;
  Height := 15;
end;

{**************************************************}

destructor TJvSpecialProgress.Destroy;
begin
  FFont.Free;
  inherited;
end;

{**************************************************}

procedure TJvSpecialProgress.FontChanged(Sender: TObject);
begin
  Canvas.Font.Assign(FFont);
  Invalidate;
end;

{**************************************************}

procedure TJvSpecialProgress.Paint;
var
  taille, savetaille: Integer; //Width of the progress to draw
  Fleft, FTop, FWidth, i: Integer;
  st: string;
  Red, Green, Blue: Real;
  FRed, FGreen, FBlue: Real;
  FStart, FEnd: TColor;
begin
  if FStartColor < 0 then
    FStart := GetSysColor(FStartColor and not $80000000)
  else
    FStart := FStartColor;
  if FEndColor < 0 then
    FEnd := GetSysColor(FEndColor and not $80000000)
  else
    FEnd := FEndColor;

  savetaille := 0;
  if FMaximum <> FMinimum then
  begin
    taille := (Width - 2) * FPosition div (FMaximum - FMinimum);
    savetaille := taille;

    if taille <> 0 then
    begin
      if FStart = FEnd then
      begin
        if FSolid then
        begin
          Canvas.Brush.Color := FStart;
          Canvas.Brush.Style := bsSolid;
          Canvas.FillRect(Rect(1, 1, 1 + Taille, Height - 1));
        end
        else
        begin
          FWidth := Round(Height * 2 div 3);
          FLeft := 1;
          if FWidth <> 0 then
          begin
            Dec(taille, taille div FWidth);
            savetaille := taille;
            Canvas.Brush.Color := FStart;
            Canvas.Brush.Style := bsSolid;
            while Taille > 0 do
            begin
              Canvas.FillRect(Rect(FLeft, 1, FLeft + FWidth, Height - 1));
              Dec(Taille, FWidth);
              Inc(FLeft, FWidth + 1);
            end;
          end;
        end;
      end
      else
      begin
            //Draw a gradient
        if FSolid then
        begin
          Red := (GetRValue(FEnd) - GetRValue(FStart)) / taille;
          Green := (GetGValue(FEnd) - GetGValue(FStart)) / taille;
          Blue := (GetBValue(FEnd) - GetBValue(FStart)) / taille;
          Canvas.Brush.Style := bsSolid;
          for i := 1 to taille do
          begin
            Canvas.Brush.Color := RGB(Round(GetRValue(FStart) + ((i - 1) * Red)),
              Round(GetGValue(FStart) + ((i - 1) * Green)),
              Round(GetBValue(FStart) + ((i - 1) * Blue)));
            Canvas.FillRect(Rect(i, 1, i + 1, Height - 1));
          end;
        end
        else
        begin
          FWidth := Round(Height * 2 div 3);
          FLeft := 1;
          Dec(taille, taille div FWidth);
          taille := taille + FWidth;
          savetaille := taille;

          Red := (GetRValue(FEnd) - GetRValue(FStart)) / taille;
          Green := (GetGValue(FEnd) - GetGValue(FStart)) / taille;
          Blue := (GetBValue(FEnd) - GetBValue(FStart)) / taille;
          FRed := GetRValue(FStart);
          FGreen := GetGValue(FStart);
          FBlue := GetBValue(FStart);
          taille := taille - FWidth;

          Canvas.Brush.Style := bsSolid;
          while Taille > 0 do
          begin
            for i := 0 to FWidth - 1 do
            begin
              Canvas.Brush.Color := RGB(Round(FRed), Round(FGreen), Round(FBlue));
              FRed := FRed + Red;
              FBlue := FBlue + Blue;
              FGreen := FGreen + Green;
              Canvas.FillRect(Rect(FLeft + i, 1, FLeft + i + 1, Height - 1));
            end;
            Dec(Taille, FWidth);
            Inc(FLeft, FWidth + 1);
          end;
        end;
      end;
    end;
  end;

  taille := savetaille;

  { SMM 20020604
    Only paint the background that remains. }
  if not (FTransparent) then
  begin
    Canvas.Brush.Color := FColor;
    Canvas.Brush.style := bsSolid;
    Canvas.FillRect(Rect(taille + 1, 1, Width - 1, Height - 1));
  end;

   //Draw text ?
  if FTextVisible then
  begin
    if FMaximum = FMinimum then
      st := '0%'
    else
      st := IntToStr(FPosition * 100 div (FMaximum - FMinimum)) + '%';
    if FCentered then
      taille := Width;
    FLeft := (taille - Canvas.TextWidth(st)) div 2;
    if FLeft < 0 then
      FLeft := 0;
    FTop := (Height - Canvas.TextHeight(st)) div 2;
    if FTop < 0 then FTop := 0;
    Canvas.Brush.Color := clNone;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(FLeft, FTop, st);
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetCentered(const Value: Boolean);
begin
  if FCentered <> Value then
  begin
    FCentered := Value;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetEndColor(const Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Canvas.Font.Assign(FFont);
  Invalidate;
end;

{**************************************************}

procedure TJvSpecialProgress.SetMaximum(const Value: Integer);
begin
  if FMaximum <> Value then
  begin
    FMaximum := Value;
    if FMaximum < FMinimum then
      FMaximum := FMinimum;
    if FPosition > Value then
      FPosition := Value;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetMinimum(const Value: Integer);
begin
  if FMinimum <> Value then
  begin
    FMinimum := Value;
    if FMinimum > FMaximum then
      FMinimum := FMaximum;
    if FPosition < Value then
      FPosition := Value;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    if FPosition > FMaximum then
      FPosition := FMaximum
    else
      if FPosition < FMinimum then
      FPosition := FMinimum;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetSolid(const Value: Boolean);
begin
  if FSolid <> Value then
  begin
    FSolid := Value;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetStartColor(const Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetTextVisible(const Value: Boolean);
begin
  if FTextVisible <> Value then
  begin
    FTextVisible := Value;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

{**************************************************}

procedure TJvSpecialProgress.StepIt;
begin
  if FPosition + FStep > FMaximum then
    Position := FMaximum
  else
  if FPosition + FStep < FMinimum then
    Position := FMinimum
  else
    Position := FPosition + FStep;
end;

{**************************************************}

procedure TJvSpecialProgress.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvSpecialProgress.MouseEnter(var Msg: TMessage);
begin
  FSaved := Application.HintColor;
  Application.HintColor := FSColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvSpecialProgress.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

end.

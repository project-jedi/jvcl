{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpecialProgress.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
  Michael Beck [mbeck att bigfoot dott com].
  [eldorado]

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQSpecialProgress;

interface

uses
  SysUtils, Classes,  
  QGraphics, QControls, QForms, QExtCtrls, QWindows, Types, 
  JvQComponent;

type
  TJvTextOption = (toCaption, toFormat, toNoText, toPercent);

  TJvSpecialProgress = class(TJvGraphicControl)
  private
    FBorderStyle: TBorderStyle;
    FEndColor: TColor;
    FGradientBlocks: Boolean;
    FMaximum: Integer;
    FMinimum: Integer;
    FPosition: Integer;
    FSolid: Boolean;
    FStartColor: TColor;
    FStep: Integer;
    FTextCentered: Boolean;
    FTextOption: TJvTextOption;
    FBuffer: TBitmap;
    FBlock: Integer;
    { FIsChanged indicates if the buffer needs to be redrawn }
    FIsChanged: Boolean;
    FStart: TColor;
    FEnd: TColor;
    { If Solid = False then the values of the following vars are valid: }
    { FBlockCount is # of blocks }
    FBlockCount: Integer;
    { FBlockWidth is length of block in pixels + 1 {seperator }
    FBlockWidth: Integer;
    { FLastBlockPartial indicates whether the last block is of length
      FBlockWidth; if FLastBlockPartial is True the progressbar is totally
      filled and the last block is *not* of length FBlockWidth, but of
      length FLastBlockWidth; if FLastBlockPartial is False the progressbar
      is not totally filled or the last block is of length FBlockWidth }
    FLastBlockPartial: Boolean;
    { FLastBlockWidth specifies the length of the last block if the
      progressbar is totally filled, note: *not* +1 for seperator }
    FLastBlockWidth: Integer;
    function GetPercentDone: Longint;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetEndColor(const Value: TColor);
    procedure SetGradientBlocks(const Value: Boolean);
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetSolid(const Value: Boolean);
    procedure SetStartColor(const Value: TColor);
    procedure SetTextCentered(const Value: Boolean);
    procedure SetTextOption(const Value: TJvTextOption);
    procedure PaintRectangle;
    procedure PaintNonSolid;
    procedure PaintSolid;
    procedure PaintBackground;
    procedure PaintText;
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure ColorChanged; override;
    procedure FontChanged; override;
    procedure TextChanged; override;
    procedure UpdateBuffer;
    procedure UpdateBlock;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StepIt;
    property PercentDone: Longint read GetPercentDone;
  published
    property Align;
    property Anchors;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property Caption;
    property Color;
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    property Font;
    property GradientBlocks: Boolean read FGradientBlocks write SetGradientBlocks default False;
    property HintColor;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property ParentColor;
    property ParentFont;
    property Position: Integer read FPosition write SetPosition default 0;
    property ShowHint;
    property Solid: Boolean read FSolid write SetSolid default False;
    property StartColor: TColor read FStartColor write SetStartColor default clWhite;
    property Step: Integer read FStep write FStep default 10;
    property TextCentered: Boolean read FTextCentered write SetTextCentered default False;
    property TextOption: TJvTextOption read FTextOption write SetTextOption default toNoText;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragOver;
    property OnDragDrop; 
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

implementation

constructor TJvSpecialProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuffer := TBitmap.Create;

  ControlStyle := ControlStyle + [csOpaque]; // SMM 20020604
  FBorderStyle := bsNone;
  FMaximum := 100;
  FMinimum := 0;
  FStartColor := clWhite;
  FStart := clWhite;
  FEndColor := clBlack;
  FEnd := clBlack;
  FPosition := 0;
  FSolid := False;
  FTextOption := toNoText;
  FTextCentered := False;
  FGradientBlocks := False;
  FStep := 10;

  Width := 150;
  Height := 15;
  FIsChanged := True;
end;

destructor TJvSpecialProgress.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TJvSpecialProgress.ColorChanged;
begin
  //inherited ColorChanged; calls CM_COLORCHANGED in VCL
  { No need to call inherited; Repaint is called in UpdateBuffer }
  FIsChanged := True;
  UpdateBuffer;
end;

procedure TJvSpecialProgress.FontChanged;
begin
  //inherited FontChanged; calls CM_COLORCHANGED in VCL
  { No need to call inherited; Repaint is called in UpdateBuffer }
  FBuffer.Canvas.Font := Font;

  { Only update if text is visible }
  if TextOption = toNoText then
    Exit;

  FIsChanged := True;
  UpdateBuffer;
end;

procedure TJvSpecialProgress.TextChanged;
begin
  if TextOption in [toCaption, toFormat] then
  begin
    FIsChanged := True;
    UpdateBuffer;
  end;
  inherited TextChanged;
end;

function TJvSpecialProgress.GetPercentDone: Longint;
begin
  if FMaximum - FMinimum = 0 then
    Result := 0
  else
    Result := 100 * (FPosition - FMinimum) div (FMaximum - FMinimum);
end;

procedure TJvSpecialProgress.Loaded;
begin
  inherited Loaded;
  UpdateBlock;
  UpdateBuffer;
end;

procedure TJvSpecialProgress.Paint;
begin
  if (FBuffer.Width <> ClientWidth) or (FBuffer.Height <> ClientHeight) then
  begin
    FIsChanged := True;
    UpdateBlock;
    UpdateBuffer;
  end;
  if (ClientWidth > 2) and (ClientHeight > 2) then
  begin    
    Canvas.Draw(0,0, FBuffer); 
  end;
end;

procedure TJvSpecialProgress.PaintBackground;
begin
  if FBlock >= ClientWidth - 2 then
    Exit;

  FBuffer.Canvas.Brush.Color := Color;
  FBuffer.Canvas.Brush.Style := bsSolid;
  FBuffer.Canvas.FillRect(Rect(FBlock + 1, 1, ClientWidth - 1, ClientHeight - 1));
end;

procedure TJvSpecialProgress.PaintNonSolid;
var
  RedInc, GreenInc, BlueInc: Real;
  Red, Green, Blue: Real;
  X: Integer;
  I, J: Integer;
  LBlockCount: Integer;
begin
  if (FBlock = 0) or (FBlockWidth = 0) then
    Exit;

  X := 1;

  { LBlockCount equals # blocks of size FBlockWidth }
  if FLastBlockPartial then
    LBlockCount := FBlockCount - 1
  else
    LBlockCount := FBlockCount;

  { Are the start and end colors equal? }
  if FStart = FEnd then
  begin
    { No gradient fill because the start color equals the end color }
    FBuffer.Canvas.Brush.Color := FStart;
    FBuffer.Canvas.Brush.Style := bsSolid;
    for I := 0 to LBlockCount - 1 do
    begin
      { Width of block is FBlockWidth -1 [-1 for seperator] }
      FBuffer.Canvas.FillRect(Bounds(X, 1, FBlockWidth - 1, ClientHeight - 2));
      Inc(X, FBlockWidth);
    end;
    if FLastBlockPartial then
      { Width of last block is FLastBlockWidth [no seperator] }
      FBuffer.Canvas.FillRect(Bounds(X, 1, FLastBlockWidth, ClientHeight - 2));
  end
  else
  begin
    RedInc := (GetRValue(FEnd) - GetRValue(FStart)) / FBlock;
    GreenInc := (GetGValue(FEnd) - GetGValue(FStart)) / FBlock;
    BlueInc := (GetBValue(FEnd) - GetBValue(FStart)) / FBlock;

    Red := GetRValue(FStart);
    Green := GetGValue(FStart);
    Blue := GetBValue(FStart);

    FBuffer.Canvas.Brush.Style := bsSolid;

    for I := 0 to LBlockCount - 1 do
    begin
      if not FGradientBlocks then
      begin
        FBuffer.Canvas.Brush.Color := RGB(Round(Red), Round(Green), Round(Blue));
        Red := Red + RedInc * FBlockWidth;
        Blue := Blue + BlueInc * FBlockWidth;
        Green := Green + GreenInc * FBlockWidth;
        { Width of block is FBlockWidth -1 [-1 for seperator] }
        FBuffer.Canvas.FillRect(Bounds(X, 1, FBlockWidth - 1, ClientHeight - 2));
      end
      else
      begin
        { Fill the progressbar with slices of 1 width }
        for J := 0 to FBlockWidth - 2 do
        begin
          FBuffer.Canvas.Brush.Color := RGB(Round(Red), Round(Green), Round(Blue));
          Red := Red + RedInc;
          Blue := Blue + BlueInc;
          Green := Green + GreenInc;
          FBuffer.Canvas.FillRect(Bounds(X + J, 1, 1, ClientHeight - 2));
        end;
        { Seperator is not filled, but increase the colors }
        Red := Red + RedInc;
        Blue := Blue + BlueInc;
        Green := Green + GreenInc;
      end;
      Inc(X, FBlockWidth);
    end;
    if FLastBlockPartial then
    begin
      if not FGradientBlocks then
      begin
        FBuffer.Canvas.Brush.Color := RGB(Round(Red), Round(Green), Round(Blue));
        { Width of last block is FLastBlockWidth [no seperator] }
        FBuffer.Canvas.FillRect(Bounds(X, 1, FLastBlockWidth, ClientHeight - 2));
      end
      else
        { Width of last block is FLastBlockWidth [no seperator] }
        for J := 0 to FLastBlockWidth - 1 do
        begin
          FBuffer.Canvas.Brush.Color := RGB(Round(Red), Round(Green), Round(Blue));
          Red := Red + RedInc;
          Blue := Blue + BlueInc;
          Green := Green + GreenInc;
          FBuffer.Canvas.FillRect(Bounds(X + J, 1, 1, ClientHeight - 2));
        end;
    end;
  end;

  { Draw the block seperators }
  X := FBlockWidth;
  FBuffer.Canvas.Brush.Color := Color;
  for I := 0 to LBlockCount - 1 do
  begin
    FBuffer.Canvas.FillRect(Bounds(X, 1, 1, ClientHeight - 2));
    Inc(X, FBlockWidth);
  end;
end;

procedure TJvSpecialProgress.PaintRectangle;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  if BorderStyle = bsNone then
  begin
    FBuffer.Canvas.Brush.Color := Color;  
    FrameRect(FBuffer.Canvas, Rect); 
  end
  else
  begin
    Frame3D(FBuffer.Canvas, Rect, clBtnFace, clBtnFace, 1);
    Frame3D(FBuffer.Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
    Frame3D(FBuffer.Canvas, Rect, cl3DDkShadow, clBtnFace, 1);
  end;
end;

procedure TJvSpecialProgress.PaintSolid;
var
  RedInc, BlueInc, GreenInc: Real;
  I: Integer;
begin
  if FBlock = 0 then
    Exit;

  if FStart = FEnd then
  begin
    { No gradient fill because the start color equals the end color }
    FBuffer.Canvas.Brush.Color := FStart;
    FBuffer.Canvas.Brush.Style := bsSolid;
    FBuffer.Canvas.FillRect(Rect(1, 1, 1 + FBlock, ClientHeight - 1));
  end
  else
  begin
    RedInc := (GetRValue(FEnd) - GetRValue(FStart)) / FBlock;
    GreenInc := (GetGValue(FEnd) - GetGValue(FStart)) / FBlock;
    BlueInc := (GetBValue(FEnd) - GetBValue(FStart)) / FBlock;
    FBuffer.Canvas.Brush.Style := bsSolid;
    { Fill the progressbar with slices of 1 width }
    for I := 1 to FBlock do
    begin
      FBuffer.Canvas.Brush.Color := RGB(
        Round(GetRValue(FStart) + ((I - 1) * RedInc)),
        Round(GetGValue(FStart) + ((I - 1) * GreenInc)),
        Round(GetBValue(FStart) + ((I - 1) * BlueInc)));
      FBuffer.Canvas.FillRect(Rect(I, 1, I + 1, ClientHeight - 1));
    end;
  end;
end;

procedure TJvSpecialProgress.PaintText;
var
  S: string;
  X, Y: Integer;
  LBlock: Integer;
begin
  case TextOption of
    toPercent:
      S := Format('%d%%', [PercentDone]);
    toFormat:
      S := Format(Caption, [PercentDone]);
    toCaption:
      S := Caption;
  else {toNoText}
    Exit;
  end;

  if TextCentered then
    LBlock := ClientWidth
  else
    LBlock := FBlock;

  X := (LBlock - FBuffer.Canvas.TextWidth(S)) div 2;
  if X < 0 then
    X := 0;

  Y := (ClientHeight - FBuffer.Canvas.TextHeight(S)) div 2;
  if Y < 0 then
    Y := 0;
  
  SetBkMode(FBuffer.Canvas.Handle, TRANSPARENT); 
  //    FBuffer.Canvas.Brush.Color := clNone;
  //    FBuffer.Canvas.Brush.Style := bsClear;
  FBuffer.Canvas.TextOut(X, Y, S);
end;

procedure TJvSpecialProgress.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;

    FIsChanged := True;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetEndColor(const Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    FEnd := ColorToRGB(FEndColor);

    FIsChanged := True;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetGradientBlocks(const Value: Boolean);
begin
  if Value <> FGradientBlocks then
  begin
    FGradientBlocks := Value;
    if not Solid then
    begin
      FIsChanged := True;
      UpdateBuffer;
    end;
  end;
end;

procedure TJvSpecialProgress.SetMaximum(const Value: Integer);
var
  OldPercentageDone: Integer;
begin
  if FMaximum <> Value then
  begin
    OldPercentageDone := GetPercentDone;

    FMaximum := Value;
    if FMaximum < FMinimum then
      FMaximum := FMinimum;
    if FPosition > Value then
      FPosition := Value;

    { If the percentage has changed we must update, otherwise check in
      UpdateBlock if we must update }
    FIsChanged := (TextOption in [toPercent, toFormat]) and (OldPercentageDone <> GetPercentDone);
    UpdateBlock;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetMinimum(const Value: Integer);
var
  OldPercentageDone: Integer;
begin
  if FMinimum <> Value then
  begin
    OldPercentageDone := GetPercentDone;

    FMinimum := Value;
    if FMinimum > FMaximum then
      FMinimum := FMaximum;
    if FPosition < Value then
      FPosition := Value;

    { If the percentage has changed we must update, otherwise check in
      UpdateBlock if we must update }
    FIsChanged := (TextOption in [toPercent, toFormat]) and (OldPercentageDone <> GetPercentDone);
    UpdateBlock;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetPosition(const Value: Integer);
var
  OldPercentageDone: Integer;
begin
  if FPosition <> Value then
  begin
    OldPercentageDone := GetPercentDone;

    FPosition := Value;
    if FPosition > FMaximum then
      FPosition := FMaximum
    else
    if FPosition < FMinimum then
      FPosition := FMinimum;

    { If the percentage has changed we must update, otherwise check in
      UpdateBlock if we must update }
    FIsChanged := (TextOption in [toPercent, toFormat]) and (OldPercentageDone <> GetPercentDone);
    UpdateBlock;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetSolid(const Value: Boolean);
begin
  if FSolid <> Value then
  begin
    FSolid := Value;
    FIsChanged := True;
    UpdateBlock;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetStartColor(const Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    FStart := ColorToRGB(FStartColor);
    FIsChanged := True;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetTextCentered(const Value: Boolean);
begin
  if FTextCentered <> Value then
  begin
    FTextCentered := Value;
    if TextOption <> toNoText then
    begin
      FIsChanged := True;
      UpdateBuffer;
    end;
  end;
end;

procedure TJvSpecialProgress.SetTextOption(const Value: TJvTextOption);
begin
  if FTextOption <> Value then
  begin
    FTextOption := Value;
    FIsChanged := True;
    UpdateBuffer;
  end;
end;

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

procedure TJvSpecialProgress.UpdateBuffer;
begin
  if not FIsChanged or (csLoading in ComponentState) then
    Exit;
  FIsChanged := False;

  if (ClientWidth <= 0) or (ClientHeight <= 0) then
    Exit;
  FBuffer.Width := ClientWidth;
  FBuffer.Height := ClientHeight;

  if FSolid then
    PaintSolid
  else
    PaintNonSolid;

  PaintBackground;
  PaintText;
  PaintRectangle;

  Repaint;
end;

procedure TJvSpecialProgress.UpdateBlock;
var
  NewBlock: Integer;
  NextBlockWidth: Integer;
begin
  if csLoading in ComponentState then
    Exit;

  if (FMaximum = FMinimum) or (ClientWidth < 2) then
    Exit;

  { Max width of the progressbar is ClientWidth -2 [-2 for the border],
    NewBlock specifies the new length of the progressbar }
  NewBlock := (ClientWidth - 2) * (FPosition - FMinimum) div (FMaximum - FMinimum);
  if not FSolid then
  begin
    { The Block of a solid bar can have a different size than the Block
      of a non-solid bar }
    FBlockWidth := Round(ClientHeight * 2 div 3);
    if FBlockWidth = 0 then
      NewBlock := 0
    else
    begin
      { The block count equals 'Block div blockwidth'. We add 1 to
        that number if the Block is further than 1/2 of the next block.
        Note that the next block doesn't have to be of size FBlockWidth,
        because it can be the last block, which can be smaller than
        FBlockWidth }

      FBlockCount := NewBlock div FBlockWidth;
      NextBlockWidth := ClientWidth - 2 - (FBlockCount * FBlockWidth);
      if NextBlockWidth > FBlockWidth then
        NextBlockWidth := FBlockWidth;

      if 2 * (NewBlock mod FBlockWidth) > NextBlockWidth then
      begin
        Inc(FBlockCount);
        FLastBlockPartial := NextBlockWidth < FBlockWidth;
        FLastBlockWidth := NextBlockWidth;
        NewBlock := FBlockWidth * FBlockCount;
        { If FLastBlockPartial equals True then the progressbar is totally
          filled: }
        if FLastBlockPartial then
          NewBlock := ClientWidth - 2;
      end
      else
      begin
        FLastBlockPartial := False;
        NewBlock := FBlockWidth * FBlockCount;
      end;
    end;
  end;

  if NewBlock = FBlock then
    Exit;

  FBlock := NewBlock;

  FIsChanged := True;
  UpdateBuffer;
end;

end.


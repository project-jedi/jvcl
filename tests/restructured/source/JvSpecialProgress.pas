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

{$I JVCL.INC}

unit JvSpecialProgress;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JVCLVer;

type
  TJvSpecialProgress = class(TGraphicControl)
  private
    FTextVisible: Boolean;
    FTransparent: Boolean;
    FSolid: Boolean;
    FCentered: Boolean;
    FPosition: Integer;
    FMaximum: Integer;
    FStep: Integer;
    FMinimum: Integer;
    FEndColor: TColor;
    FColor: TColor;
    FStartColor: TColor;
    FFont: TFont;
    FHintColor: TColor;
    FGradientBlocks: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseEnter: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;

    FBuffer: TBitmap;
    FSavedHintColor: TColor;
    FTaille: Integer;
    { FIsChanged indicates if the buffer needs to be redrawn }
    FIsChanged: Boolean;
    FEnd, FStart: TColor;

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
    procedure SetCentered(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetEndColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetGradientBlocks(const Value: Boolean);
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetSolid(const Value: Boolean);
    procedure SetStartColor(const Value: TColor);
    procedure SetTextVisible(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);

    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message
      CM_PARENTCOLORCHANGED;
    procedure FontChanged(Sender: TObject);

    procedure PaintRectangle;
    procedure PaintNonSolid;
    procedure PaintSolid;
    procedure PaintBackground;
    procedure PaintText;
  protected
    procedure Paint; override;
    procedure Loaded; override;

    procedure UpdateBuffer;
    procedure UpdateTaille;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PercentDone: Longint read GetPercentDone;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
      False;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property Transparent: Boolean read FTransparent write SetTransparent default
      False;
    property StartColor: TColor read FStartColor write SetStartColor default
      clWhite;
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    property Step: Integer read FStep write FStep default 10;
    property Position: Integer read FPosition write SetPosition default 0;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Solid: Boolean read FSolid write SetSolid default False;
    property GradientBlocks: Boolean read FGradientBlocks write SetGradientBlocks
      default
      False;
    property TextVisible: Boolean read FTextVisible write SetTextVisible default
      False;
    property TextFont: TFont read FFont write SetFont;
    property TextCentered: Boolean read FCentered write SetCentered default
      False;
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
    property HintColor: TColor read FHintColor write FHintColor default
      clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write
      FOnParentColorChanged;
  end;

implementation

type
  TControlAccess = class(TControl);

  { TJvSpecialProgress }

procedure TJvSpecialProgress.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Transparent then
  begin
    FIsChanged := True;
    UpdateBuffer;
  end;

  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

constructor TJvSpecialProgress.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer := TBitmap.Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;

  ControlStyle := ControlStyle + [csOpaque]; // SMM 20020604
  FMaximum := 100;
  FMinimum := 0;
  FTransparent := False;
  FStartColor := clWhite;
  FStart := clWhite;
  FEndColor := clBlack;
  FEnd := clBlack;
  FPosition := 0;
  FColor := clBtnFace;
  FSolid := False;
  FTextVisible := False;
  FCentered := False;
  FGradientBlocks := False;
  FStep := 10;

  Width := 150;
  Height := 15;
  FIsChanged := True;
end;

destructor TJvSpecialProgress.Destroy;
begin
  FBuffer.Free;
  FFont.Free;
  inherited;
end;

procedure TJvSpecialProgress.FontChanged(Sender: TObject);
begin
  Canvas.Font.Assign(FFont);

  { Only update if text is visible }
  if not TextVisible then
    Exit;

  FIsChanged := True;
  UpdateBuffer;
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
  inherited;
  UpdateTaille;
  UpdateBuffer;
end;

procedure TJvSpecialProgress.MouseEnter(var Msg: TMessage);
begin
  FSavedHintColor := Application.HintColor;
  Application.HintColor := FHintColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvSpecialProgress.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSavedHintColor;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvSpecialProgress.Paint;
begin
  if (FBuffer.Width <> ClientWidth) or (FBuffer.Height <> ClientHeight) then
  begin
    FIsChanged := True;
    UpdateTaille;
    UpdateBuffer;
  end;
  if (ClientWidth > 2) and (ClientHeight > 2) then
    BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
      FBuffer.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TJvSpecialProgress.PaintBackground;
begin
  if FTaille >= ClientWidth - 2 then
    Exit;

  if Transparent and Assigned(Parent) then
    FBuffer.Canvas.Brush.Color := TControlAccess(Parent).Color
  else
    FBuffer.Canvas.Brush.Color := FColor;

  FBuffer.Canvas.Brush.style := bsSolid;
  FBuffer.Canvas.FillRect(Rect(FTaille + 1, 1, ClientWidth - 1, ClientHeight -
    1));
end;

procedure TJvSpecialProgress.PaintNonSolid;
var
  RedInc, GreenInc, BlueInc: Real;
  Red, Green, Blue: Real;
  X: Integer;
  i, j: Integer;
  LBlockCount: Integer;
begin
  if (FTaille = 0) or (FBlockWidth = 0) then
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
    for i := 0 to LBlockCount - 1 do
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
    RedInc := (GetRValue(FEnd) - GetRValue(FStart)) / FTaille;
    GreenInc := (GetGValue(FEnd) - GetGValue(FStart)) / FTaille;
    BlueInc := (GetBValue(FEnd) - GetBValue(FStart)) / FTaille;

    Red := GetRValue(FStart);
    Green := GetGValue(FStart);
    Blue := GetBValue(FStart);

    FBuffer.Canvas.Brush.Style := bsSolid;

    for i := 0 to LBlockCount - 1 do
    begin
      if not FGradientBlocks then
      begin
        FBuffer.Canvas.Brush.Color := RGB(Round(Red), Round(Green),
          Round(Blue));
        Red := Red + RedInc * FBlockWidth;
        Blue := Blue + BlueInc * FBlockWidth;
        Green := Green + GreenInc * FBlockWidth;
        { Width of block is FBlockWidth -1 [-1 for seperator] }
        FBuffer.Canvas.FillRect(Bounds(X, 1, FBlockWidth - 1, ClientHeight - 2));
      end
      else
      begin
        { Fill the progressbar with slices of 1 width }
        for j := 0 to FBlockWidth - 2 do
        begin
          FBuffer.Canvas.Brush.Color := RGB(Round(Red), Round(Green),
            Round(Blue));
          Red := Red + RedInc;
          Blue := Blue + BlueInc;
          Green := Green + GreenInc;
          FBuffer.Canvas.FillRect(Bounds(X + j, 1, 1, ClientHeight - 2));
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
        FBuffer.Canvas.Brush.Color := RGB(Round(Red), Round(Green),
          Round(Blue));
        { Width of last block is FLastBlockWidth [no seperator] }
        FBuffer.Canvas.FillRect(Bounds(X, 1, FLastBlockWidth, ClientHeight - 2));
      end
      else
        { Width of last block is FLastBlockWidth [no seperator] }
        for j := 0 to FLastBlockWidth - 1 do
        begin
          FBuffer.Canvas.Brush.Color := RGB(Round(Red), Round(Green),
            Round(Blue));
          Red := Red + RedInc;
          Blue := Blue + BlueInc;
          Green := Green + GreenInc;
          FBuffer.Canvas.FillRect(Bounds(X + j, 1, 1, ClientHeight - 2));
        end;
    end;
  end;

  { Draw the block seperators }
  X := FBlockWidth;
  if Transparent and Assigned(Parent) then
    FBuffer.Canvas.Brush.Color := TControlAccess(Parent).Color
  else
    FBuffer.Canvas.Brush.Color := FColor;
  for i := 0 to LBlockCount - 1 do
  begin
    FBuffer.Canvas.FillRect(Bounds(X, 1, 1, ClientHeight - 2));
    Inc(X, FBlockWidth);
  end;
end;

procedure TJvSpecialProgress.PaintRectangle;
begin
  if Transparent and Assigned(Parent) then
    FBuffer.Canvas.Brush.Color := TControlAccess(Parent).Color
  else
    FBuffer.Canvas.Brush.Color := FColor;
  FBuffer.Canvas.FrameRect(Rect(0, 0, ClientWidth, ClientHeight));
end;

procedure TJvSpecialProgress.PaintSolid;
var
  RedInc, BlueInc, GreenInc: Real;
  i: Integer;
begin
  if FTaille = 0 then
    Exit;

  if FStart = FEnd then
  begin
    { No gradient fill because the start color equals the end color }
    FBuffer.Canvas.Brush.Color := FStart;
    FBuffer.Canvas.Brush.Style := bsSolid;
    FBuffer.Canvas.FillRect(Rect(1, 1, 1 + FTaille, ClientHeight - 1));
  end
  else
  begin
    RedInc := (GetRValue(FEnd) - GetRValue(FStart)) / FTaille;
    GreenInc := (GetGValue(FEnd) - GetGValue(FStart)) / FTaille;
    BlueInc := (GetBValue(FEnd) - GetBValue(FStart)) / FTaille;
    FBuffer.Canvas.Brush.Style := bsSolid;
    { Fill the progressbar with slices of 1 width }
    for i := 1 to FTaille do
    begin
      FBuffer.Canvas.Brush.Color := RGB(
        Round(GetRValue(FStart) + ((i - 1) * RedInc)),
        Round(GetGValue(FStart) + ((i - 1) * GreenInc)),
        Round(GetBValue(FStart) + ((i - 1) * BlueInc)));
      FBuffer.Canvas.FillRect(Rect(i, 1, i + 1, ClientHeight - 1));
    end;
  end;
end;

procedure TJvSpecialProgress.PaintText;
var
  S: string;
  X, Y: Integer;
  LTaille: Integer;
begin
  if not TextVisible then
    Exit;

  FBuffer.Canvas.Font := TextFont;

  S := Format('%d%%', [PercentDone]);

  if TextCentered then
    LTaille := ClientWidth
  else
    LTaille := FTaille;

  X := (LTaille - FBuffer.Canvas.TextWidth(S)) div 2;
  if X < 0 then
    X := 0;

  Y := (ClientHeight - FBuffer.Canvas.TextHeight(S)) div 2;
  if Y < 0 then
    Y := 0;

  SetBkMode(FBuffer.Canvas.Handle, Windows.TRANSPARENT);
  //    FBuffer.Canvas.Brush.Color := clNone;
  //    FBuffer.Canvas.Brush.Style := bsClear;
  FBuffer.Canvas.TextOut(X, Y, S);
end;

procedure TJvSpecialProgress.SetCentered(const Value: Boolean);
begin
  if FCentered <> Value then
  begin
    FCentered := Value;

    FIsChanged := True;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;

    if Transparent then
      Exit;

    FIsChanged := True;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetEndColor(const Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    if FEndColor < 0 then
      FEnd := GetSysColor(FEndColor and not $80000000)
    else
      FEnd := FEndColor;

    FIsChanged := True;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);

  if not FTextVisible then
    Exit;

  FIsChanged := True;
  UpdateBuffer;
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
  FOldPercentageDone: Integer;
begin
  if FMaximum <> Value then
  begin
    FOldPercentageDone := GetPercentDone;

    FMaximum := Value;
    if FMaximum < FMinimum then
      FMaximum := FMinimum;
    if FPosition > Value then
      FPosition := Value;

    { If the percentage has changed we must update, otherwise check in
      UpdateTaille if we must update }
    FIsChanged := TextVisible and (FOldPercentageDone <> GetPercentDone);
    UpdateTaille;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetMinimum(const Value: Integer);
var
  FOldPercentageDone: Integer;
begin
  if FMinimum <> Value then
  begin
    FOldPercentageDone := GetPercentDone;

    FMinimum := Value;
    if FMinimum > FMaximum then
      FMinimum := FMaximum;
    if FPosition < Value then
      FPosition := Value;

    { If the percentage has changed we must update, otherwise check in
      UpdateTaille if we must update }
    FIsChanged := TextVisible and (FOldPercentageDone <> GetPercentDone);
    UpdateTaille;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetPosition(const Value: Integer);
var
  FOldPercentageDone: Integer;
begin
  if FPosition <> Value then
  begin
    FOldPercentageDone := GetPercentDone;

    FPosition := Value;
    if FPosition > FMaximum then
      FPosition := FMaximum
    else if FPosition < FMinimum then
      FPosition := FMinimum;

    { If the percentage has changed we must update, otherwise check in
      UpdateTaille if we must update }
    FIsChanged := TextVisible and (FOldPercentageDone <> GetPercentDone);
    UpdateTaille;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetSolid(const Value: Boolean);
begin
  if FSolid <> Value then
  begin
    FSolid := Value;

    FIsChanged := True;
    UpdateTaille;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetStartColor(const Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    if FStartColor < 0 then
      FStart := GetSysColor(FStartColor and not $80000000)
    else
      FStart := FStartColor;

    FIsChanged := True;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetTextVisible(const Value: Boolean);
begin
  if FTextVisible <> Value then
  begin
    FTextVisible := Value;

    FIsChanged := True;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;

    FIsChanged := True;
    UpdateBuffer;
  end;
end;

procedure TJvSpecialProgress.StepIt;
begin
  if FPosition + FStep > FMaximum then
    Position := FMaximum
  else if FPosition + FStep < FMinimum then
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
  FBuffer.Canvas.Brush.Color := Color;

  if FSolid then
    PaintSolid
  else
    PaintNonSolid;

  PaintBackground;
  PaintText;
  PaintRectangle;
  Repaint;
end;

procedure TJvSpecialProgress.UpdateTaille;
var
  NewTaille: Integer;
  NextBlockWidth: Integer;
begin
  if csLoading in ComponentState then
    Exit;

  if (FMaximum = FMinimum) or (ClientWidth < 2) then
    Exit;

  { Max width of the progressbar is ClientWidth -2 [-2 for the border],
    NewTaille specifies the new length of the progressbar }
  NewTaille := (ClientWidth - 2) * (FPosition - FMinimum) div (FMaximum -
    FMinimum);
  if not FSolid then
  begin
    { The taille of a solid bar can have a different size than the taille
      of a non-solid bar }
    FBlockWidth := Round(ClientHeight * 2 div 3);
    if FBlockWidth = 0 then
      NewTaille := 0
    else
    begin
      { The block count equals 'taille div blockwidth'. We add 1 to
        that number if the taille is further than 1/2 of the next block.
        Note that the next block doesn't have to be of size FBlockWidth,
        because it can be the last block, which can be smaller than
        FBlockWidth }

      FBlockCount := NewTaille div FBlockWidth;
      NextBlockWidth := ClientWidth - 2 - (FBlockCount * FBlockWidth);
      if NextBlockWidth > FBlockWidth then
        NextBlockWidth := FBlockWidth;

      if 2 * (NewTaille mod FBlockWidth) > NextBlockWidth then
      begin
        Inc(FBlockCount);
        FLastBlockPartial := NextBlockWidth < FBlockWidth;
        FLastBlockWidth := NextBlockWidth;
        NewTaille := FBlockWidth * FBlockCount;
        { If FLastBlockPartial equals True then the progressbar is totally
          filled: }
        if FLastBlockPartial then
          NewTaille := ClientWidth - 2;
      end
      else
      begin
        FLastBlockPartial := False;
        NewTaille := FBlockWidth * FBlockCount;
      end;
    end;
  end;

  if NewTaille = FTaille then
    Exit;

  FTaille := NewTaille;

  FIsChanged := True;
  UpdateBuffer;
end;

end.


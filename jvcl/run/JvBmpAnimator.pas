{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBmpAnim.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A bitmap animator: animates an imagelist consisting of multiple likesized bitmaps
  like the explorer logo in Internet Explorer or Netscape Navigator.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBmpAnimator;

interface

uses
  Classes,
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QWindows,
  {$ENDIF VisualCLX}
  Graphics, Controls, ExtCtrls, ImgList,
  JvComponent;

type
  TJvAnimateDirection = (tdForward, tdBack, tdFwdBack, tdBackFwd);

  TJvCustomBmpAnimator = class(TJvGraphicControl)
  private
    FImageList: TCustomImageList;
    FTimer: TTimer;
    FIndex: Integer;
    FActive: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FNumGlyphs: Integer;
    FSpeed: Integer;
    FTransparent: Boolean;
    FAutoSize: Boolean;
    FStart: Integer;
    FStop: Integer;
    FPosition: Integer;
    FDirection: TJvAnimateDirection;
    FGoingUp: Boolean;
    FCenter: Boolean;
    FImageChangeLink: TChangeLink;
    procedure SetCenter(Value: Boolean);
    procedure SetDirection(Value: TJvAnimateDirection);
    procedure SetPosition(Value: Integer);
    procedure SetStart(Value: Integer);
    procedure SetStop(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure SetImage(Value: TCustomImageList);
    procedure SetActive(Value: Boolean);
    procedure SetNumGlyphs(Value: Integer);
    procedure SetSpeed(Value: Integer);
    procedure TimerEvent(Sender: TObject);
    procedure DoChange(Sender: TObject);
  protected
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); {$IFDEF VCL} override; {$ENDIF}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    property Centered: Boolean read FCenter write SetCenter;
    property Color default clBtnFace;
    property Direction: TJvAnimateDirection read FDirection write SetDirection;
    property Active: Boolean read FActive write SetActive default False;
    property Images: TCustomImageList read FImageList write SetImage;
    property NumFrames: Integer read FNumGlyphs write SetNumGlyphs default 0;
    property Position: Integer read FPosition write SetPosition default 0;
    property Speed: Integer read FSpeed write SetSpeed default 100;
    property Min: Integer read FStart write SetStart default 0;
    property Max: Integer read FStop write SetStop default 0;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvBmpAnimator = class(TJvCustomBmpAnimator)
  published
    property Active;
    property Align;
    property AutoSize;
    property Centered;
    property Color;
    property Direction;
    property Height;
    property Images;
    property Left;
    property Name;
    property NumFrames;
    property Position;
    property Speed;
    property Min;
    property Max;
    property Tag;
    property Top;
    property Transparent;
    property Width;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnDragOver;
  end;

implementation

constructor TJvCustomBmpAnimator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := DoChange;
  FWidth := 60;
  FHeight := 60;
  Width := FWidth;
  Height := FHeight;
  FTransparent := False;
  FAutoSize := False;
  FSpeed := 15;
  FNumGlyphs := 0;
  FIndex := 0;
  FStart := 0;
  FStop := 0;
  FPosition := 0;
  FActive := False;
  Color := clBtnFace;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := TimerEvent;
  FTimer.Enabled := FActive;
  FTimer.Interval := 100;
  FDirection := tdForward;
  FGoingUp := True;
end;

destructor TJvCustomBmpAnimator.Destroy;
begin
  FImageChangeLink.Free;
  FTimer.Enabled := False;
  FTimer.Free;
  inherited Destroy;
end;

procedure TJvCustomBmpAnimator.DoChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomBmpAnimator.TimerEvent(Sender: TObject);
begin
  if not Assigned(FImageList) then
    Exit;

  if not Active then
    FIndex := FPosition
  else
    case FDirection of
      tdForward:
        begin
          Inc(FIndex);
          if (FIndex > FNumGlyphs) or (FIndex > FStop) then
            FIndex := FStart;
        end;
      tdBack:
        begin
          Dec(FIndex);
          if (FIndex < 0) or (FIndex < FStart) then
            FIndex := FStop;
        end;
      tdFwdBack, tdBackFwd:
        begin
          if FGoingUp then
          begin
            if (FIndex >= FStop) then
            begin
              FGoingUp := False;
              Dec(FIndex);
            end
            else
              Inc(FIndex);
          end
          else
          begin
            if FIndex <= FStart then
            begin
              FGoingUp := True;
              Inc(FIndex);
            end
            else
              Dec(FIndex);
          end;
        end;
    end;
  Refresh;
end;

procedure TJvCustomBmpAnimator.SetStart(Value: Integer);
begin
  if FStart <> Value then
  begin
    FStart := Value;
    if FStart > FStop then
      FStart := FStop;
    if FStart >= FNumGlyphs then
      FStart := FNumGlyphs - 1;
    if FStart < 0 then
      FStart := 0;
  end;
end;

procedure TJvCustomBmpAnimator.SetStop(Value: Integer);
begin
  if FStop <> Value then
  begin
    FStop := Value;
    if FStop < FStart then
      FStop := FStart;
    if FStop >= FNumGlyphs then
      FStop := FNumGlyphs - 1;
    if FStop < 0 then
      FStop := 0;
  end;
end;

procedure TJvCustomBmpAnimator.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize and Assigned(FImageList) then
    begin
      Width := FImageList.Width;
      Height := FImageList.Height;
    end;
  end;
end;

procedure TJvCustomBmpAnimator.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Repaint;
  end;
end;

procedure TJvCustomBmpAnimator.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FImageList) then
  begin
    SetImage(nil);
    Active := False;
  end;
end;

procedure TJvCustomBmpAnimator.SetImage(Value: TCustomImageList);
begin
  if FImageList <> nil then
  begin
    FImageList.UnRegisterChanges(FImageChangeLink);
    SetNumGlyphs(0);
  end;

  FImageList := Value;
  if FImageList <> nil then
  begin
    FImageList.RegisterChanges(FImageChangeLink);
    SetNumGlyphs(FImageList.Count);
  end
  else
    Active := False;
  Repaint;
end;

procedure TJvCustomBmpAnimator.SetActive(Value: Boolean);
begin
{  if not Assigned(FImageList) then
    Value := False;}
  if FActive <> Value then
  begin
    FActive := Value;
    FTimer.Enabled := FActive;
    FIndex := FStart;
  end;
  Repaint;
end;

procedure TJvCustomBmpAnimator.SetNumGlyphs(Value: Integer);
begin
  if FNumGlyphs <> Value then
  begin
    FNumGlyphs := Value;
    SetStop(FNumGlyphs - 1);
  end;
end;

procedure TJvCustomBmpAnimator.SetSpeed(Value: Integer);
begin
  if FSpeed <> Value then
  begin
    FSpeed := Value;
    FTimer.Interval := 1000 div FSpeed;
  end;
end;

procedure TJvCustomBmpAnimator.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TJvCustomBmpAnimator.SetDirection(Value: TJvAnimateDirection);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    case FDirection of
      tdForward, tdFwdBack:
        begin
          FGoingUp := True;
          FIndex := FStart;
        end;
      tdBack, tdBackFwd:
        begin
          FGoingUp := False;
          FIndex := FStop;
        end;
    end;
  end;
end;

procedure TJvCustomBmpAnimator.SetPosition(Value: Integer);
begin
  FPosition := Value;
  if FPosition > FNumGlyphs - 1 then
    FPosition := FNumGlyphs - 1;
  Invalidate;
end;

procedure TJvCustomBmpAnimator.Paint;
var
  dX, dY: Integer;
begin
  if Assigned(FImageList) then
  begin
    if FCenter then
    begin
      dX := (Width - FImageList.Width) div 2;
      dY := (Height - FImageList.Height) div 2;
    end
    else
    begin
      dX := 0;
      dY := 0;
    end;
    if not FTransparent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
    end
    else
      Canvas.Brush.Style := bsClear;
    FImageList.Draw(Canvas, dX, dY, FIndex);

    if not Active then
      FIndex := FPosition;
    FImageList.Draw(Canvas, dX, dY, FIndex)
  end;
  if csDesigning in ComponentState then
    with Canvas do
    begin
      Brush.Color := clBlack;
      {$IFDEF VCL}
      FrameRect(GetClientRect);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      FrameRect(Canvas, GetClientRect);
      {$ENDIF VisualCLX}
    end;
end;

end.


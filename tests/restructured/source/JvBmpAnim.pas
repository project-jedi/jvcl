{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBmpAnim.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ A bitmap animator: animates an imagelist consisting of multiple likesized bitmaps
  like the explorer logo in Internet Explorer and Netscape Navigator. }

unit JvBmpAnim;

interface
uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,  CommCtrl,
  ExtCtrls,ImgList, JvComponent;

type
  TJvAnimateDirection=(tdForward,tdBack,tdFwdBack,tdBackFwd);
  TJvCustomBmpAnimator = class(TJvGraphicControl)
  private
    { Private declarations }
    FImageList:TImageList;
    FTimer:TTimer;
    FIndex:integer;
    FActive:boolean;
    FWidth,FHeight:integer;
    FNumGlyphs:integer;
    FSpeed:integer;
    FTransparent:boolean;
    FAutoSize:boolean;
    FStart,FStop:integer;
    FPosition:integer;
    FColor:TColor;
    FDirection:TJvAnimateDirection;
    FGoingUp:boolean;
    FCenter:boolean;
    FImageChangeLink:TChangeLink;
    procedure SetCenter(Value:boolean);
    procedure SeTJvAnimateDirection(Value:TJvAnimateDirection);
    procedure SetColor(Value:TColor);
    procedure SetPosition(Value:integer);
    procedure SetStart(Value:integer);
    procedure SetStop(Value:integer);
    procedure SetTransparent(Value:boolean);
    procedure SetImage(Value:TImagelist);
    procedure SetActive(Value:boolean);
    procedure SetNumGlyphs(Value:integer);
    procedure SetSpeed(Value:integer);
    procedure TimerEvent(Sender:TObject);
    procedure DoChange(Sender:TObject);
  protected
    { Protected declarations }
    procedure Paint; override;
{$IFDEF DELPHI6_UP}
    procedure SetAutoSize(Value:boolean);override;
    property  AutoSize:boolean read FAutoSize write SetAutoSize default False;
{$ENDIF}
    procedure Notification(AComponent:TComponent;aOperation:TOperation);override;
    property  Centered:boolean read FCenter write SetCenter;
    property  Color:TColor read FColor write SetColor default clBtnFace;
    property  Direction: TJvAnimateDirection read FDirection write SeTJvAnimateDirection;
    property  Active:boolean read FActive write SetActive default False;
    property  ImageList:TImagelist read FImageList write SetImage;
    property  NumFrames:integer read FNumGlyphs write SetNumGlyphs default 0;
    property  Position:integer read FPosition write SetPosition default 0;
    property  Speed:integer read FSpeed write SetSpeed default 100;
    property  Min:integer read FStart write SetStart default 0;
    property  Max:integer read FStop write SetStop default 0;
    property  Transparent:boolean read FTransparent write SetTransparent default False;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  TJvBmpAnimator=class(TJvCustomBmpAnimator)
  private
  protected
  public
  published
    property Active;
    property Align;
    property AutoSize;
    property Centered;
    property Color;
    property Direction;
    property Height;
    property ImageList;
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

constructor TJvCustomBmpAnimator.Create(AOwner:TComponent);
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
  FColor := clBtnFace;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := TimerEvent;
  FTimer.Enabled := FActive;
  FTimer.Interval := 100;
  FDirection := tdForward;
  FGoingUp := True;
end;

procedure TJvCustomBmpAnimator.DoChange(Sender:TObject);
begin
  Invalidate;
end;

destructor TJvCustomBmpAnimator.Destroy;
begin
  FImageChangeLink.Free;
  FTimer.Enabled := False;
  FTimer.Free;
  inherited Destroy;
end;

procedure TJvCustomBmpAnimator.TimerEvent(Sender:TObject);
var dX,dY:integer;
begin
  if not Assigned(FImageList) then Exit;
    
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

      tdFwdBack,tdBackFwd:
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
          if (FIndex <= FStart) then
          begin
            FGoingUp := True;
            Inc(FIndex);
          end
          else
            Dec(Findex);
        end;
      end;
    end;

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
    Canvas.Brush.Color := FColor;
    Canvas.FillRect(ClientRect);
  end;
  FImageList.Draw(Canvas,dX,dY,FIndex);
end;


procedure TJvCustomBmpAnimator.SetStart(Value:integer);
begin
  if FStart <> Value then
  begin
    FStart := Value;
    if FStart > FStop then FStart := FStop;
    if FStart >= FNumGlyphs then FStart := FNumGlyphs-1;
    if FStart < 0 then FStart := 0;
  end;
end;

procedure TJvCustomBmpAnimator.SetStop(Value:integer);
begin
  if FStop <> Value then
  begin
    FStop := Value;
    if FStop < FStart then FStop := FStart;
    if FStop >= FNumGlyphs then FStop := FNumGlyphs-1;
    if FStop < 0 then FStop := 0;
  end;
end;

{$IFDEF DELPHI6_UP}
procedure TJvCustomBmpAnimator.SetAutoSize(Value:boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize and Assigned(FImageList) then
    begin
      Width := FimageList.Width;
      Height := FImageList.Height;
    end;
  end;
end;
{$ENDIF}

procedure TJvCustomBmpAnimator.SetTransparent(Value:boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Repaint;
  end;
end;


procedure TJvCustomBmpAnimator.Notification(AComponent:TComponent;aOperation:TOperation);
begin
  inherited Notification(AComponent,AOperation);
  if (AOperation = opRemove) and (AComponent = FImageList) then
  begin
    SetImage(nil);
    Active := false;
  end;
end;

procedure TJvCustomBmpAnimator.SetImage(Value:TImagelist);
begin
  if FImagelist <> nil then
  begin
    FImageList.UnRegisterChanges(FImageChangeLink);
    SetNumGlyphs(0);
  end;

  FImagelist := Value;
  if FImageList <> nil then
  begin
    FImageList.RegisterChanges(FImageChangeLink);
    SetNumGlyphs(FImageList.Count);
  end
  else
    Active := false;
  Repaint;
end;

procedure TJvCustomBmpAnimator.SetActive(Value:boolean);
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


procedure TJvCustomBmpAnimator.SetNumGlyphs(Value:integer);
begin
  if FNumGlyphs <> Value then
  begin
    FNumGlyphs := Value;
    SetStop(FNumGlyphs-1);
  end;
end;


procedure TJvCustomBmpAnimator.SetSpeed(Value:integer);
begin
  if FSpeed <> Value then
  begin
    FSpeed := Value;
    FTimer.Interval := 1000 div FSpeed;
  end;
end;

procedure TJvCustomBmpAnimator.SetCenter(Value:boolean);
begin
  if FCenter <> value then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TJvCustomBmpAnimator.SeTJvAnimateDirection(Value:TJvAnimateDirection);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    case FDirection of
      tdForward,tdFwdBack: begin
        FGoingUp := True;
        FIndex := FStart;
      end;
      tdBack,tdBackFwd:
      begin
        FGoingUp := False;
        FIndex := FStop;
      end;
    end;
  end;
end;

procedure TJvCustomBmpAnimator.SetColor(Value:TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomBmpAnimator.SetPosition(Value:integer);
begin
  FPosition := Value;
  if FPosition > FNumGlyphs-1 then
    FPosition := FNumGlyphs-1;
  Invalidate;
end;

procedure TJvCustomBmpAnimator.Paint;
var dX,dY:integer;
begin
  if Assigned(FImagelist) then
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
      Canvas.Brush.Color := FColor;
      Canvas.FillRect(ClientRect);
    end;
    if not Active then FIndex := FPosition;
    FImageList.Draw(Canvas,dX,dY,FIndex)
  end;
  if (csDesigning in ComponentState) then
    with Canvas do
    begin
      Brush.Color := clBlack;
      FrameRect(GetClientRect);
    end;
end;

end.

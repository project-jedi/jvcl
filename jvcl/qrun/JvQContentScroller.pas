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

The Original Code is: JvCntScr.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A TCustomPanel descendant that can scroll its content.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQContentScroller;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, QExtCtrls, 
  QTypes, 
  JvQComponent;

type
  TJvContentScrollDirection = (sdUp, sdDown);
  TJvScrollAmount = 1..MaxInt;

  TJvContentScroller = class(TJvCustomPanel)
  private
    FTimer: TTimer;
    FActive: Boolean;
    FPosition: Integer;
    FScrollAmount: TJvScrollAmount;
    FScrollIntervall: TJvScrollAmount;
    FMediaFile: TFileName;
    FOnBeforeScroll: TNotifyEvent;
    FOnAfterScroll: TNotifyEvent;
    FLoopMedia: Boolean;
    FScrollLength: TJvScrollAmount;
    FScrollDirection: TJvContentScrollDirection;
    FLoopCount: Integer;
    FCurLoop: Integer;
    // FScrollStart: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetScrollAmount(Value: TJvScrollAmount);
    procedure SetScrollIntervall(Value: TJvScrollAmount);
    procedure SetMediaFile(Value: TFileName);
    procedure DoTimer(Sender: TObject);
    procedure CreateTimer;
    procedure FreeTimer;
    procedure SetLoopMedia(Value: Boolean);
    procedure SetScrollLength(Value: TJvScrollAmount);
    procedure SetScrollDirection(Value: TJvContentScrollDirection);
    procedure SetLoopCount(Value: Integer);
    // procedure SetScrollStart(const Value: Integer);
  protected
    procedure Paint; override;
    procedure DoBeforeScroll; dynamic;
    procedure DoAfterScroll; dynamic;  
    procedure SetText(const Value: TCaption); override; 
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollContent(Amount: TJvScrollAmount);
  published
    property Active: Boolean read FActive write SetActive;
    property ScrollAmount: TJvScrollAmount read FScrollAmount write SetScrollAmount default 10;
    property ScrollIntervall: TJvScrollAmount read FScrollIntervall write SetScrollIntervall default 50;
    property ScrollLength: TJvScrollAmount read FScrollLength write SetScrollLength default 250;
    property ScrollDirection: TJvContentScrollDirection read FScrollDirection write SetScrollDirection default sdUp;
    // property ScrollStart: Integer read FScrollStart write SetScrollStart;
    {$IFDEF MSWINDOWS}
    property MediaFile: TFileName read FMediaFile write SetMediaFile;
    property LoopMedia: Boolean read FLoopMedia write SetLoopMedia default True;
    property LoopCount: Integer read FLoopCount write SetLoopCount default -1;
    {$ENDIF MSWINDOWS} 
    property Action;
    property Anchors;
    property Constraints;
    property Align;
    property BorderStyle;
    property BorderWidth;
    property DragMode;
    property Enabled;
    property HelpContext;
    property Hint;
    property Color;
    property Cursor;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Visible;
    property OnAfterScroll: TNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnBeforeScroll: TNotifyEvent read FOnBeforeScroll write FOnBeforeScroll;
    property OnConstrainedResize;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  MMSystem,
  {$ENDIF MSWINDOWS}
  QGraphics, QControls;

constructor TJvContentScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelInner := bvNone;
  BevelOuter := bvNone;
  ParentColor := True;
  FScrollAmount := 10;
  FScrollIntervall := 50;
  FScrollLength := 250;
  FScrollDirection := sdUp;
  FLoopMedia := True;
  FLoopCount := -1;
end;

destructor TJvContentScroller.Destroy;
begin
  FreeTimer;
  inherited Destroy;
end;

procedure TJvContentScroller.CreateTimer;
var
  Flag: Integer;
begin
  if not Assigned(FTimer) then
    FTimer := TTimer.Create(nil);
  // FPosition := -Abs(FScrollStart);
  // ScrollBy(0,FScrollStart);
  FTimer.Enabled := False;
  FTimer.OnTimer := DoTimer;
  FTimer.Interval := ScrollIntervall;
  FTimer.Enabled := True;
  {$IFDEF MSWINDOWS}
  Flag := SND_ASYNC or SND_FILENAME;
  if FLoopMedia then
    Flag := Flag or SND_LOOP;
  if FileExists(FMediaFile) then
    PlaySound(PChar(FMediaFile), 0, Flag);
  {$ENDIF MSWINDOWS}
  FCurLoop := FLoopCount;
end;

procedure TJvContentScroller.FreeTimer;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Enabled := False;
    FTimer.OnTimer := nil;
    FTimer.Free;
    FTimer := nil;
  end;
  if FScrollDirection = sdUp then
    ScrollBy(0, FPosition)
  else
    ScrollBy(0, -FPosition);
  FPosition := 0;
  {$IFDEF MSWINDOWS}
  if FileExists(FMediaFile) then
    PlaySound(nil, 0, SND_ASYNC);
  {$ENDIF MSWINDOWS}
end;

procedure TJvContentScroller.DoTimer(Sender: TObject);
var
  B: Boolean;
begin
  B := FTimer.Enabled;
  FTimer.Enabled := False;
  try
    FTimer.Interval := ScrollIntervall;
    DoBeforeScroll;
    ScrollContent(FScrollAmount);
    DoAfterScroll;
  finally
    if Assigned(FTimer) then
      FTimer.Enabled := B;
  end;
end;

procedure TJvContentScroller.DoAfterScroll;
begin
  if Assigned(FOnAfterScroll) then
    FOnAfterScroll(Self);
end;

procedure TJvContentScroller.DoBeforeScroll;
begin
  if Assigned(FOnBeforeScroll) then
    FOnBeforeScroll(Self);
end;

procedure TJvContentScroller.ScrollContent(Amount: TJvScrollAmount);
var
  I: Integer;
begin
  DisableAlign;
  try
    if FPosition = 0 then
    begin
      if FCurLoop = 0 then
        Active := False
      else
      if FCurLoop > 0 then
        Dec(FCurLoop);
    end;

    if FScrollDirection = sdUp then
    begin
      if FPosition >= FScrollLength then
      begin
        I := FPosition + FScrollLength;
        FPosition := -FScrollLength;
        ScrollBy(0, I);
      end;
      I := -Amount;
    end
    else
    begin
      if FPosition >= FScrollLength then
      begin
        I := -FPosition - FScrollLength;
        FPosition := -FScrollLength;
        ScrollBy(0, I);
      end;
      I := Amount;
    end;

    if Active then
    begin
      ScrollBy(0, I);
      FPosition := FPosition + Amount;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TJvContentScroller.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if not FActive then
      FreeTimer
    else
      CreateTimer;
  end;
end;

procedure TJvContentScroller.SetScrollAmount(Value: TJvScrollAmount);
begin
  FScrollAmount := Value;
end;

procedure TJvContentScroller.SetScrollIntervall(Value: TJvScrollAmount);
begin
  FScrollIntervall := Value;
end;

procedure TJvContentScroller.SetMediaFile(Value: TFileName);
begin
  FMediaFile := Value;
end;

procedure TJvContentScroller.SetLoopMedia(Value: Boolean);
begin
  FLoopMedia := Value;
end;

procedure TJvContentScroller.SetScrollLength(Value: TJvScrollAmount);
begin
  FScrollLength := Value;
end;

procedure TJvContentScroller.SetScrollDirection(Value: TJvContentScrollDirection);
begin
  if (FScrollDirection <> Value) and not FActive then
    FScrollDirection := Value;
end;

procedure TJvContentScroller.SetLoopCount(Value: Integer);
begin
  if FLoopCount <> Value then
  begin
    FLoopCount := Value;
    if (FLoopCount > -1) and (FScrollLength mod FScrollAmount <> 0) then
      FScrollLength := FScrollLength - (FScrollLength mod FScrollAmount) + FScrollAmount;
  end;
end;



{
procedure TJvContentScroller.SetScrollStart(const Value: Integer);
begin
  if FScrollStart <> Value then
    FScrollStart := Value;
end;
}

procedure TJvContentScroller.Paint;
begin
  inherited Paint;
  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;


procedure TJvContentScroller.SetText(const Value: TCaption);
begin
  inherited SetText('');
end;


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.


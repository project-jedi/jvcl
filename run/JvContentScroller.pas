{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCntScr.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3 at sourceforge dot net]
Portions created by Peter Th�rnqvist are Copyright (C) 2002 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  A TCustomPanel descendant that can scroll its content.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvContentScroller;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, ExtCtrls,
  JvExtComponent;

type
  TJvContentScrollDirection = (sdUp, sdDown, sdLeft, sdRight);
  TJvScrollAmount = 1..MaxInt;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvContentScroller = class(TJvCustomPanel)
  private
    FTimer: TTimer;
    FActive: Boolean;
    FYPosition: Integer;
    FXPosition: Integer;
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
  protected
    procedure Paint; override;
    procedure DoBeforeScroll; dynamic;
    procedure DoAfterScroll; dynamic;
    procedure CreateWnd; override;
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
    {$IFDEF MSWINDOWS}
    property MediaFile: TFileName read FMediaFile write SetMediaFile;
    property LoopMedia: Boolean read FLoopMedia write SetLoopMedia default True;
    property LoopCount: Integer read FLoopCount write SetLoopCount default -1;
    {$ENDIF MSWINDOWS}
    property BiDiMode;
    property DockSite;
    property DragKind;
    property FullRepaint;
    property ParentBiDiMode;
    property UseDockManager;
    property DragCursor;
    property OnCanResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
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

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF MSWINDOWS}
  MMSystem,
  {$ENDIF MSWINDOWS}
  Graphics, Controls;

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

  FYPosition := 0;
  FXPosition := 0;
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

  case FScrollDirection of
    sdUp:
      ScrollBy(0, FYPosition);
    sdDown:
      ScrollBy(0, -FYPosition);
    sdLeft:
      ScrollBy(FXPosition, 0);
    sdRight:
      ScrollBy(-FXPosition, 0);
  end;

  FYPosition := 0;
  FXPosition := 0;
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
  DeltaY: Integer;
  DeltaX: Integer;
begin
  DisableAlign;
  try
    if ((FScrollDirection in [sdUp, sdDown]) and (FYPosition = 0)) or 
       ((FScrollDirection in [sdLeft, sdRight]) and (FXPosition = 0)) then
    begin
      if FCurLoop = 0 then
        Active := False
      else
      if FCurLoop > 0 then
        Dec(FCurLoop);
    end;

    // Set to 0 to avoid warning
    DeltaX := 0;
    DeltaY := 0;
    case FScrollDirection of
      sdUp:
        begin
          if FYPosition >= FScrollLength then
          begin
            DeltaY := FScrollLength + Height;
            FYPosition := -Height;
            ScrollBy(0, DeltaY);
          end;
          DeltaY := -Amount;
        end;
      sdDown:
        begin
          if FYPosition >= Height then
          begin
            DeltaY := -FYPosition - FScrollLength;
            FYPosition := -FScrollLength;
            ScrollBy(0, DeltaY);
          end;
          DeltaY := Amount;
        end;
      sdLeft:
        begin
          if FXPosition >= FScrollLength then
          begin
            DeltaX := FScrollLength + Width;
            FXPosition := -Width;
            ScrollBy(DeltaX, 0);
          end;
          DeltaX := -Amount;
        end;
      sdRight:
        begin
          if FXPosition >= Width then
          begin
            DeltaX := -FXPosition - FScrollLength;
            FXPosition := -FScrollLength;
            ScrollBy(DeltaX, 0);
          end;
          DeltaX := Amount;
        end;
    end;

    if Active then
    begin
      ScrollBy(DeltaX, DeltaY);
      
      FXPosition := FXPosition + Abs(DeltaX);
      FYPosition := FYPosition + Abs(DeltaY);
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
  if (FScrollDirection <> Value) then
  begin
    case FScrollDirection of
      sdUp: 
        if Value = sdDown then
          FYPosition := -FYPosition;
      sdDown:
        if Value = sdUp then
          FYPosition := -FYPosition;
      sdLeft:
        if Value = sdRight then
          FXPosition := -FXPosition;
      sdRight:
        if Value = sdLeft then
          FXPosition := -FXPosition;
    end;
    
    FScrollDirection := Value;
  end;
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


procedure TJvContentScroller.CreateWnd;
begin
  inherited CreateWnd;
  Caption := '';
end;

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



{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

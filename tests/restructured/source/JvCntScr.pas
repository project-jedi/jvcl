{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCntScr.PAS, released on 2002-05-26.

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

{ @abstract(A TCustomPanel descendant that can scroll it's content.) }

unit JvCntScr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvComponent;

type
  TMediaFilename = string;
  TJvScrollDirection = (sdUp,sdDown);
  TJvScrollAmount = 1..MaxWord;

  TJvContentScroller = class(TJvCustomPanel)
  private
    FTimer:TTimer;
    FActive: boolean;
    FPosition: integer;
    FScrollAmount: TJvScrollAmount;
    FScrollIntervall: TJvScrollAmount;
    FMediaFile: TMediaFilename;
    FOnBeforeScroll: TNotifyEvent;
    FOnAfterScroll: TNotifyEvent;
    FLoopMedia: boolean;
    FScrollLength: TJvScrollAmount;
    FScrollDirection: TJvScrollDirection;
    FLoopCount: integer;
    FCurLoop:integer;
//    FScrollStart: integer;
    procedure SetActive(Value: boolean);
    procedure SeTJvScrollAmount(Value: TJvScrollAmount);
    procedure SetScrollIntervall(Value: TJvScrollAmount);
    procedure SetMediaFile(Value: TMediaFilename);
    procedure DoTimer(Sender: TObject);
    procedure CreateTimer;
    procedure FreeTimer;
    procedure SetLoopMedia(Value: boolean);
    procedure SetScrollLength(Value: TJvScrollAmount);
    procedure SeTJvScrollDirection(Value: TJvScrollDirection);
    procedure SetLoopCount(Value: integer);
//    procedure SetScrollStart(const Value: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure DoBeforeScroll;virtual;
    procedure DoAfterScroll;virtual;
    procedure CreateWnd;override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure ScrollContent(Amount:TJvScrollAmount);
  published
    { Published declarations }
    property Action;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragKind;
    property FullRepaint;
    property ParentBiDiMode;
    property UseDockManager;
    {*****************}
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUndock;
    { redclared properties }
    property Align;
    property BorderStyle;
    property BorderWidth;
    property DragCursor;
    property DragMode;
    property Enabled;
    property HelpContext;
    property Hint;
    property Color;
    property Ctl3D;
    property Cursor;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopUpMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Visible;
    {*****************}
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
    { new properties }
    property Active:boolean read FActive write SetActive;
    property ScrollAmount:TJvScrollAmount read FScrollAmount write SeTJvScrollAmount default 10;
    property ScrollIntervall:TJvScrollAmount read FScrollIntervall write SetScrollIntervall default 50;
    property ScrollLength:TJvScrollAmount read FScrollLength write SetScrollLength default 250;
    property ScrollDirection:TJvScrollDirection read FScrollDirection write SeTJvScrollDirection default sdUp;
//    property ScrollStart:integer read FScrollStart write SetScrollStart;
    property MediaFile:TMediaFilename read FMediaFile write SetMediaFile;
    property LoopMedia:boolean read FLoopMedia write SetLoopMedia default true;
    property LoopCount:integer read FLoopCount write SetLoopCount default -1;
    {*****************}
    property OnAfterScroll:TNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnBeforeScroll:TNotifyEvent read FOnBeforeScroll write FOnBeforeScroll;
  end;


implementation
uses
  MMSystem;

{ TJvContentScroller }

constructor TJvContentScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelInner := bvNone;
  BevelOuter := bvNone;
  ParentColor := true;
  FScrollAmount := 10;
  FScrollIntervall := 50;
  FScrollLength := 250;
  FScrollDirection := sdUp;
  FLoopMedia := true;
  FLoopCount := -1;
end;

destructor TJvContentScroller.Destroy;
begin
  FreeTimer;
  inherited Destroy;
end;

procedure TJvContentScroller.CreateTimer;
var Flag:integer;
begin
  if not Assigned(FTimer) then
    FTimer := TTimer.Create(nil);
//  FPosition := -Abs(FScrollStart);
//  ScrollBy(0,FScrollStart);
  FTimer.Enabled := false;
  FTimer.OnTimer := DoTimer;
  FTimer.Interval := ScrollIntervall;
  FTimer.Enabled := true;
  Flag := SND_ASYNC or SND_FILENAME;
  if FLoopMedia then
    Flag := Flag or SND_LOOP;
  if FileExists(FMediaFile) then
    PlaySound(PChar(FMediaFile),0,Flag);
  FCurLoop := FLoopCount;
end;

procedure TJvContentScroller.FreeTimer;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Enabled := false;
    FTimer.OnTimer := nil;
    FTimer.Free;
    FTimer := nil;
  end;
  if FScrollDirection = sdUp then
    ScrollBy(0,FPosition)
  else
    ScrollBy(0,-FPosition);
  FPosition := 0;
  if FileExists(FMediaFile) then
    PlaySound(nil,0,SND_ASYNC);
end;

procedure TJvContentScroller.DoTimer(Sender:TObject);
var b:boolean;
begin
  b := FTimer.Enabled;
  FTimer.Enabled := false;
  try
    FTimer.Interval := ScrollIntervall;
    DoBeforeScroll;
    ScrollContent(FScrollAmount);
    DoAfterScroll;
  finally
    if Assigned(FTimer) then
      FTimer.Enabled := b;
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
    FOnBeforeScroll(self);
end;

procedure TJvContentScroller.ScrollContent(Amount:TJvScrollAmount);
var i:integer;
begin
  DisableAlign;
  try
    if (FPosition = 0) then
    begin
      if (FCurLoop = 0) then
        Active := false
      else if FCurLoop > 0 then
        Dec(FCurLoop);
    end;

    if FScrollDirection = sdUp then
    begin
      if (FPosition >= FScrollLength) then
      begin
        i := FPosition + FScrollLength;
        FPosition := -FScrollLength;
        ScrollBy(0,i);
      end;
      i := -Amount;
    end
    else
    begin
      if (FPosition >= FScrollLength) then
      begin
        i := -FPosition-FScrollLength;
        FPosition := -FScrollLength;
        ScrollBy(0,i);
      end;
      i := Amount;
    end;

    if Active then
    begin
      ScrollBy(0,i);
      FPosition := FPosition + Amount;
    end;
  finally
    EnableAlign;
  end;
end;


procedure TJvContentScroller.SetActive(Value: boolean);
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

procedure TJvContentScroller.SeTJvScrollAmount(Value: TJvScrollAmount);
begin
  if FScrollAmount <> Value then
    FScrollAmount := Value;
end;

procedure TJvContentScroller.SetScrollIntervall(Value: TJvScrollAmount);
begin
  if FScrollIntervall <> Value then
    FScrollIntervall := Value;
end;

procedure TJvContentScroller.SetMediaFile(Value: TMediaFilename);
begin
  if FMediaFile <> Value then
    FMediaFile := Value;
end;

procedure TJvContentScroller.SetLoopMedia(Value: boolean);
begin
  if FLoopMedia <> Value then
    FLoopMedia := Value;
end;

procedure TJvContentScroller.SetScrollLength(Value: TJvScrollAmount);
begin
  if FScrollLength <> Value then
    FScrollLength := Value;
end;

procedure TJvContentScroller.SeTJvScrollDirection(Value: TJvScrollDirection);
begin
  if (FScrollDirection <> Value) and not FActive then
    FScrollDirection := Value;
end;

procedure TJvContentScroller.SetLoopCount(Value: integer);
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
{
procedure TJvContentScroller.SetScrollStart(const Value: integer);
begin
  if FScrollStart <> Value then
    FScrollStart := Value;
end;
}
end.

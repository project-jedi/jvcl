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

The Original Code is: JvScrollText.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com]
                Michael Freislich [mikef att korbi dott net]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQScrollText;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, QWindows, QMessages, QGraphics, QControls, QForms, QStdCtrls,
  JvQStaticText, JvQTypes, JvQImageDrawThread, JVCLXVer, JvQComponent;

type
  TJvScrollTextDirection = (drFromLeft, drFromRight, drFromTop, drFromBottom); // also in JvMoveableBevel, JvAppearingLabel
  TJvScrollText = class(TJvCustomControl)
  private
    FText: TJvStaticText;
    FTimerTag: Integer;
    FActive: Boolean;
    FDelay: Cardinal;
    FPixel: Integer;
    FCurrPos: Integer;
    FSelectable: Boolean;
    FScrollDirection: TJvScrollTextDirection;
    FScrollSaved: Integer;
    FItems: TStringList;
    FDeja: Cardinal;
    FScroll: TJvImageDrawThread;
    FFont: TFont;
    FStartY: Integer;
    FDown: Boolean;
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    procedure OnScroll(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetPixel(const Value: Integer);
    procedure SetScrollDirection(const Value: TJvScrollTextDirection);
    procedure CalculateText(Sender: TObject);
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure FontChange(Sender: TObject);
    procedure SetFont(const Value: TFont);
    procedure TextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TextMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure BoundsChanged; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Items: TStrings read GetItems write SetItems;
    property Active: Boolean read FActive write SetActive default False;
    property Delay: Cardinal read FDelay write SetDelay default 50;
    property ScrollPixels: Integer read FPixel write SetPixel default 1;
    property ScrollDirection: TJvScrollTextDirection read FScrollDirection write SetScrollDirection default drFromBottom;
    property BackgroundColor: TColor read GetColor write SetColor;
    property Font: TFont read FFont write SetFont;
    procedure Pause;
    procedure Unpause;
    procedure Reset;
    property Align;
    property ShowHint;
    property ParentShowHint;
    property Height default 150;
    property Width default 200; 
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvQJVCLUtils, JvQThemes, JvQResources;

constructor TJvScrollText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 
  Width := 200;
  Height := 150;
  FActive := False;
  FDelay := 50;
  FPixel := 1;
  FCurrPos := 0;
  FSelectable := True;
  FScrollDirection := drFromBottom;
  FItems := TStringList.Create;

  FText := TJvStaticText.Create(Self);
  FText.Parent := Self;
  // FText.SetBounds(2, 2, Width-4, Height-4);
  FText.Width := Width;
  FText.Height := Height; 
  FText.TabStop := False;
  FText.Enabled := FSelectable;
  FText.AutoSize := False;
  FText.OnMouseDown := TextMouseDown;
  FText.OnMouseMove := TextMouseMove;
  FText.OnMouseUp := TextMouseUp;

  FFont := TFont.Create;
  FFont.Assign(FText.Font);
  FFont.OnChange := FontChange;

  FTimerTag := 0;
  FDown := False;
  FDeja := Application.HintPause;

  if not (csDesigning in ComponentState) then
  begin
    FScroll := TJvImageDrawThread.Create(True);
    FScroll.FreeOnTerminate := False;
    FScroll.Delay := FDelay;
    FScroll.OnDraw := OnScroll;
  end;
end;

destructor TJvScrollText.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    FScroll.OnDraw := nil;
    FScroll.Terminate;
    // FScroll.WaitFor;
    FreeAndNil(FScroll);
  end;
  Application.HintPause := FDeja;
  FItems.Free;
  FText.Free;
  FFont.OnChange := nil;
  FFont.Free;
  inherited Destroy;
end;

procedure TJvScrollText.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    SetItems(FItems);
end;

procedure TJvScrollText.SetFont(const Value: TFont);
var
  Al: TAlignment;
begin
  FFont.Assign(Value);
  FText.Font.Assign(FFont);
  CalculateText(Self);
  Al := FText.Alignment;
  if FText.Alignment = taCenter then
    FText.Alignment := taLeftJustify
  else
    FText.Alignment := taCenter;
  FText.Alignment := Al;
end;

procedure TJvScrollText.TextMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P.X := X;
  P.Y := Y;
  P := FText.ClientToScreen(P);

  if ScrollDirection in [drFromTop, drFromBottom] then
    FStartY := P.Y
  else
    FStartY := P.X;
  if not (csDesigning in ComponentState) then
    FScroll.OnDraw := nil;
  FDown := True;
end;

procedure TJvScrollText.TextMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewY: Integer;
  P: TPoint;
begin
  if FDown then
  begin
    //if NewY>0, going up, NewY<0, going down
    P.X := X;
    P.Y := Y;
    P := FText.ClientToScreen(P);
    Y := P.Y;
    X := P.X;

    if ScrollDirection in [drFromTop, drFromBottom] then
    begin
      NewY := FStartY - Y;
      FStartY := Y;
      FCurrPos := FCurrPos - NewY;

      if FCurrPos < -FText.Height then
        FCurrPos := Height
      else
      if FCurrPos > Height then
        FCurrPos := -FText.Height;

      FText.Top := FCurrPos;
    end
    else
    begin
      NewY := FStartY - X;
      FStartY := X;
      FCurrPos := FCurrPos - NewY;

      if FCurrPos < -FText.Width then
        FCurrPos := Width
      else
      if FCurrPos > Width then
        FCurrPos := -FText.Width;

      FText.Left := FCurrPos;
    end;
  end;
end;

procedure TJvScrollText.TextMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (csDesigning in ComponentState) then
    FScroll.OnDraw := OnScroll;
  FDown := False;
end;

procedure TJvScrollText.OnScroll(Sender: TObject);
var
  T: Integer;
begin
  //tag=1 pause
  if FTimerTag = 1 then
  begin
    if FScrollSaved <= 0 then
    begin
      SetActive(False);
      FTimerTag := 0;
      Exit;
    end
    else
    begin
      T := FScrollSaved;
      Dec(FScrollSaved);
    end;
  end
  else
  if FTimerTag = 2 then
  begin
    if FScrollSaved >= FPixel then
    begin
      FTimerTag := 0;
      T := FPixel;
    end
    else
    begin
      T := FScrollSaved;
      Inc(FScrollSaved);
    end;
  end
  else
    T := FPixel;

  //tag=2 unpause
  //FScrollDirection

  case ScrollDirection of
    drFromTop:
      begin
        if FCurrPos > Height then
          FCurrPos := -FText.Height
        else
          FCurrPos := FCurrPos + T;
        FText.Top := FCurrPos;
      end;
    drFromLeft:
      begin
        if - FCurrPos > FText.Width then
          FCurrPos := Width
        else
          FCurrPos := FCurrPos - T;
        FText.Left := FCurrPos;
      end;
    drFromRight:
      begin
        if FCurrPos > Width then
          FCurrPos := -Width
        else
          FCurrPos := FCurrPos + T;
        FText.Left := FCurrPos;
      end;
    drFromBottom:
      begin
        if - FCurrPos > FText.Height then
          FCurrPos := Height
        else
          FCurrPos := FCurrPos - T;
        FText.Top := FCurrPos;
      end;
  end;
end;

procedure TJvScrollText.Pause;
begin
  if FActive then
  begin
    FScrollSaved := FPixel;
    FTimerTag := 1;
  end;
end;

procedure TJvScrollText.SetActive(const Value: Boolean);
begin
  SetItems(FItems);
  FActive := Value;
  if not (csDesigning in ComponentState) then
  begin
    if Value then
      FScroll.Resume
    else
      FScroll.Suspend;
  end;
end;

procedure TJvScrollText.SetDelay(const Value: Cardinal);
begin
  if Value > FDeja then
    Application.HintPause := FDeja
  else
  if Value > 10 then
    Application.HintPause := Value - 10
  else
    Application.HintPause := Abs(Value - 1);
  FDelay := Value;
  if not (csDesigning in ComponentState) then
    FScroll.Delay := Value;
end;

procedure TJvScrollText.SetScrollDirection(const Value: TJvScrollTextDirection);
begin
  FScrollDirection := Value;
  FText.Left := 0;
  FText.Top := 0;
  Reset;
end;

procedure TJvScrollText.CalculateText(Sender: TObject);
var
  I, J: Integer;
  Ts: TStringList;
  Canvas: TCanvas;
begin
  // calculate the Size of the memo (vertically)
  Canvas := TCanvas.Create;
  with Canvas do
  begin
    Handle := GetDC(HWND_DESKTOP);
    Font.Assign(FText.Font);
    J := 0;
    Ts := TStringList.Create;
    Ts.Text := FText.Caption;
    for I := 0 to Ts.Count - 1 do
    try
      if Ts[I] <> '' then
        J := J + TextHeight(Ts[I]) * ((TextWidth(Ts[I]) div Width) + 1)
      else
        J := J + CanvasMaxTextHeight(Canvas);
    except
    end;
    if J <= 0 then
      J := Height;
    FText.Height := J;
    ReleaseDC(HWND_DESKTOP, Handle);
    Ts.Free;
    Free;
  end;
  if FText.Height < Height then
    FText.Height := Height;
  Reset;
end;

function TJvScrollText.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TJvScrollText.SetItems(const Value: TStrings);
begin
  FItems.Text := Value.Text;
  FText.Caption := Value.Text;
  CalculateText(Self);
end;

function TJvScrollText.GetColor: TColor;
begin
  Result := FText.Color;
end;

procedure TJvScrollText.SetColor(const Value: TColor);
begin
  FText.Color := Value;
  Color := Value;
  Invalidate;
end;

procedure TJvScrollText.FontChange(Sender: TObject);
var
  Al: TAlignment;
begin
  FText.Font.Assign(FFont);
  CalculateText(Self);
  Al := FText.Alignment;
  if FText.Alignment = taCenter then
    FText.Alignment := taLeftJustify
  else
    FText.Alignment := taCenter;
  FText.Alignment := Al;
end;

procedure TJvScrollText.SetPixel(const Value: Integer);
begin
  FPixel := Value;
end;

procedure TJvScrollText.Reset;
begin
  case ScrollDirection of
    drFromTop:
      FCurrPos := Height;
    drFromLeft:
      FCurrPos := -Width;
    drFromRight:
      FCurrPos := Width;
    drFromBottom:
      FCurrPos := -FText.Height;
  end;
end;

procedure TJvScrollText.Unpause;
begin
  if not FActive then
  begin
    FScrollSaved := 0;
    FTimerTag := 2;
    SetActive(True);
  end;
end;

procedure TJvScrollText.BoundsChanged;
begin
  if FText <> nil then
  begin
    FText.Width := Width;
    if FText.Height < Height then
      FText.Height := Height;
  end;    
  inherited BoundsChanged;
end;

function TJvScrollText.GetAlignment: TAlignment;
begin
  Result := FText.Alignment;
end;

procedure TJvScrollText.SetAlignment(const Value: TAlignment);
begin
  FText.Alignment := Value;
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

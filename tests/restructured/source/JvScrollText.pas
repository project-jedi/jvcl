{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollText.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvScrollText;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, JvTypes,
  JvImageDrawThread, JVCLVer;

type
  TJvScrollText = class(TCustomControl)
  private
    FMemo: TStaticText;
    FTimerTag: Integer;
    FActive: Boolean;
    FDelay: Cardinal;
    FPixel: Integer;
    FCurrPos: Integer;
    FSelectable: Boolean;
    FDirection: TDirection;
    FScrollSaved: Integer;
    FStrings: TStringList;
    FDeja: Cardinal;
    FScroll: TJvImageDrawThread;
    FFont: TFont;
    FStartY: Integer;
    FDown: Boolean;
    FAboutJVCL: TJVCLAboutInfo;

    procedure SetItems(const Value: TStringList);
    procedure OnScroll(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetPixel(const Value: Integer);
    procedure SetDirection(const Value: TDirection);
    procedure CalculateMemo(Sender: TObject);
    function GetAlignment: TAlignment;
    procedure Setalignment(const Value: TAlignment);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure FontChanged(Sender: TObject);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure MouseD(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseM(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseU(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    // (p3) this should be just Alignment
    property TextAlignment: TAlignment read GetAlignment write Setalignment;
    property Items: TStringList read FStrings write SetItems;
    property Active: Boolean read FActive write SetActive default False;
    property Delay: Cardinal read FDelay write SetDelay default 50;
    property ScrollPixels: Integer read FPixel write SetPixel default 1;
    property ScrollDirection: TDirection read FDirection write SetDirection default drFromBottom;
    property BackgroundColor: TColor read GetColor write SetColor;
    property Font: TFont read GetFont write SetFont;
    procedure Pause;
    procedure Unpause;
    procedure Reset;
    property Align;
    property ShowHint;
    property ParentShowHint;
  end;

implementation

resourcestring
  RC_TestText = 'abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

///////////////////////////////////////////////////////////
// TJvScrollText
///////////////////////////////////////////////////////////

constructor TJvScrollText.Create(AOwner: TComponent);
begin
  inherited;
  Width := 200;
  Height := 150;
  FActive := False;
  FDelay := 50;
  FPixel := 1;
  FCurrPos := 0;
  FSelectable := True;
  FDirection := drFromBottom;
  FStrings := TStringList.Create;

  FMemo := TStaticText.Create(Self);
  FMemo.Parent := Self;
//  FMemo.SetBounds(2,2,Width-4,Height-4);
  FMemo.Width := Width;
  FMemo.Height := Height;
  FMemo.Borderstyle := sbsNone;
  FMemo.TabStop := False;
  FMemo.Enabled := FSelectable;
  FMemo.AutoSize := False;
  FMemo.OnMouseDown := MouseD;
  FMemo.OnMouseMove := MouseM;
  FMemo.OnMouseUp := MouseU;

  FFont := TFont.Create;
  FFont := FMemo.Font;
  FFont.OnChange := FontChanged;

  FTimerTag := 0;
  FDown := False;
  FDeja := Application.HintPause;

  FScroll := TJvImageDrawThread.Create(True);
  FScroll.FreeOnTerminate := False;
  FScroll.Delay := FDelay;
  FScroll.OnDraw := OnScroll;
end;

{*******************************************************}

function TJvScrollText.GetFont: TFont;
begin
  Result := FMemo.Font;
end;

{*******************************************************}

procedure TJvScrollText.SetFont(const Value: TFont);
var
  FAl: TAlignment;
begin
  FMemo.Font.Assign(Value);
  CalculateMemo(Self);
  Fal := FMemo.Alignment;
  if FMemo.Alignment = taCenter then
    FMemo.Alignment := taLeftJustify
  else
    FMemo.Alignment := taCenter;
  FMemo.Alignment := FAl;
end;

{*******************************************************}

procedure TJvScrollText.MouseD(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  p: TPoint;
begin
  p.x := x;
  p.y := y;
  p := FMemo.ClientToScreen(p);

  if ScrollDirection in [drFromTop, drFromBottom] then
    FStartY := p.y
  else
    FStartY := p.x;
  FScroll.OnDraw := nil;
  FDown := True;
end;

{*******************************************************}

procedure TJvScrollText.MouseM(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewY: Integer;
  p: TPoint;
begin
  if FDown then
  begin
    //if NewY>0, going up, NewY<0, going down
    p.x := x;
    p.y := y;
    p := FMemo.ClientToScreen(p);
    y := p.y;
    x := p.x;

    if ScrollDirection in [drFromTop, drFromBottom] then
    begin
      NewY := FStartY - Y;
      FStartY := Y;
      FCurrPos := FCurrPos - NewY;

      if FCurrPos < -FMemo.Height then
        FCurrPos := Height
      else if FCurrPos > Height then
        FCurrPos := -FMemo.Height;

      FMemo.Top := FCurrPos;
    end
    else
    begin
      NewY := FStartY - x;
      FStartY := x;
      FCurrPos := FCurrPos - NewY;

      if FCurrPos < -FMemo.Width then
        FCurrPos := Width
      else if FCurrPos > Width then
        FCurrPos := -FMemo.Width;

      FMemo.Left := FCurrPos;
    end;
  end;
end;

{*******************************************************}

procedure TJvScrollText.MouseU(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FScroll.OnDraw := OnScroll;
  FDown := False;
end;

{*******************************************************}

destructor TJvScrollText.Destroy;
begin
  FScroll.OnDraw := nil;
  FScroll.Terminate;
//  FScroll.WaitFor;
  FreeAndnil(FScroll);
  Application.HintPause := FDeja;
  FStrings.Free;
  FMemo.Free;
  inherited;
end;

{*******************************************************}

procedure TJvScrollText.OnScroll(Sender: TObject);
var
  t: Integer;
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
      t := FScrollSaved;
      Dec(FScrollSaved);
    end;
  end
  else if FTimerTag = 2 then
  begin
    if FScrollSaved >= FPixel then
    begin
      FTimerTag := 0;
      t := FPixel;
    end
    else
    begin
      t := FScrollSaved;
      Inc(FSCrollSaved);
    end;
  end
  else
    t := FPixel;

  //tag=2 unpause
  //FDirection

  case ScrollDirection of
    drFromTop:
      begin
        if FCurrPos > Height then
          FCurrPos := -FMemo.Height
        else
          FCurrPos := FCurrPos + t;
        FMemo.Top := FCurrPos;
      end;
    drFromLeft:
      begin
        if - FCurrPos > FMemo.Width then
          FCurrPos := Width
        else
          FCurrpos := FCurrPos - t;
        Fmemo.Left := FCurrPos;
      end;
    drFromRight:
      begin
        if FCurrPos > Width then
          FCurrPos := -Width
        else
          FCurrpos := FCurrPos + t;
        Fmemo.Left := FCurrPos;
      end;
    drFromBottom:
      begin
        if - FCurrPos > FMemo.Height then
          FCurrPos := Height
        else
          FCurrPos := FCurrPos - t;
        FMemo.Top := FCurrPos;
      end;
  end;
end;

{*******************************************************}

procedure TJvScrollText.Pause;
begin
  if FActive then
  begin
    FScrollSaved := FPixel;
    FTimerTag := 1;
  end;
end;

{*******************************************************}

procedure TJvScrollText.SetActive(const Value: Boolean);
begin
  SetItems(FStrings);
  FActive := Value;
  if Value then
    FScroll.Resume
  else
    FScroll.Suspend;
end;

{*******************************************************}

procedure TJvScrollText.SetDelay(const Value: Cardinal);
begin
  if Value > FDeja then
    Application.HintPause := FDeja
  else if Value > 10 then
    Application.HintPause := Value - 10
  else
    Application.HintPause := Abs(Value - 1);
  FDelay := Value;
  FScroll.Delay := Value;
end;

{*******************************************************}

procedure TJvScrollText.SetDirection(const Value: TDirection);
begin
  FDirection := Value;
  FMemo.Left := 0;
  FMemo.Top := 0;
  Reset;
end;

{*******************************************************}

procedure TJvScrollText.CalculateMemo(Sender: TObject);
var
  i, j: Integer;
  ts: TStringList;
begin
  //calculate the Size of the memo (vertically)
  with TCanvas.Create do
  begin
    Handle := GetDC(HWND_DESKTOP);
    Font.Assign(FMemo.Font);
    j := 0;
    ts := TStringList.Create;
    ts.Text := FMemo.Caption;
    for i := 0 to ts.Count - 1 do
    try
      if ts[i] <> '' then
        j := j + TextHeight(ts[i])
      else
        j := j + TextHeight(RC_TestText);
    except
    end;
    if j <= 0 then
      j := Height;
    FMemo.Height := j;
    ReleaseDC(HWND_DESKTOP, Handle);
    ts.Free;
    Free;
  end;
  if FMemo.Height < Height then
    FMemo.Height := Height;
  Reset;
end;

{*******************************************************}

procedure TJvScrollText.SetItems(const Value: TStringList);
begin
  FStrings.Text := Value.Text;
  FMemo.Caption := Value.text;
  CalculateMemo(Self);
end;

{*******************************************************}

function TJvScrollText.GetColor: TColor;
begin
  Result := FMemo.Color;
end;

{*******************************************************}

procedure TJvScrollText.SetColor(const Value: TColor);
begin
  FMemo.Color := Value;
  Color := Value;
  Invalidate;
end;

{*******************************************************}

procedure TJvScrollText.FontChanged(Sender: TObject);
var
  FAl: TAlignment;
begin
  CalculateMemo(Self);
  FAl := FMemo.Alignment;
  if FMemo.Alignment = taCenter then
    FMemo.Alignment := taLeftJustify
  else
    FMemo.Alignment := taCenter;
  FMemo.Alignment := FAl;
end;

{*******************************************************}

procedure TJvScrollText.SetPixel(const Value: Integer);
begin
  FPixel := Value;
end;

{*******************************************************}

procedure TJvScrollText.Reset;
begin
  case ScrollDirection of
    drFromTop:
      FCurrPos := Height;
    drFromLeft:
      FCurrPos := -Width;
    drFromRight:
      FCurrPos := Width;
    drFrombottom:
      FCurrPos := -FMemo.Height;
  end;
end;

{*******************************************************}

procedure TJvScrollText.Unpause;
begin
  if not FActive then
  begin
    FScrollSaved := 0;
    FTimerTag := 2;
    SetActive(True);
  end;
end;

{*******************************************************}

procedure TJvScrollText.WMSize(var Msg: TWMSize);
begin
  FMemo.Width := Width;
  if FMemo.Height < Height then
    FMemo.Height := Height;
end;

{*******************************************************}

function TJvScrollText.GetAlignment: TAlignment;
begin
  Result := FMemo.Alignment;
end;

{*******************************************************}

procedure TJvScrollText.SetAlignment(const Value: TAlignment);
begin
  FMemo.Alignment := Value;
end;

end.


{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvArrayButton.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}

unit JvArrayButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, shellApi,
  ExtCtrls;

type
  TarrayButtonClicked = procedure(acol, arow: integer) of object;

  TJvArrayButton = class(TGraphicControl)
  private
    FptDown: TPoint;
    FPushDown: boolean;
    FColor: TColor;
    Frows: integer;
    Fcols: integer;
    FonArrayButtonClicked: TarrayButtonClicked;
    FCaptions: Tstringlist;
    FColors: TStringlist;
    FHints: TStringlist;
    procedure Setcols(const Value: integer);
    procedure Setrows(const Value: integer);
    procedure SetonArrayButtonClicked(const Value: TarrayButtonClicked);
    procedure SetCaptions(const Value: Tstringlist);
    procedure SetColors(const Value: TStringlist);
    procedure MouseToCell(const x, y: integer; var acol, arow: integer);
    function CellRect(acol, arow: integer): TRect;
    procedure SetHints(const Value: TStringlist);
    { Private declarations }
  protected
    { Protected declarations }
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    {Creates and initializes an instance of TJvArrayButton. }
    destructor Destroy; override;
    {Destroys an instance of TJvArrayButton.}
    procedure Paint; override;
    {Renders the image of the button.}
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    {this procedure can be used in response to a application.onshowhint event
     button hints are stored in the hints property from arry top-left to array bottom right
     in your application create a separate onshowhint event handler
     within that handler test hintinfo.hintcontrol is this object. If it is dispatch to this objects doShowHint.
     In the formcreate eventhandler include:
       application.OnShowHint:=drawhint;

     procedure TDrawF.DrawHint(var HintStr: string; var CanShow: Boolean;
     var HintInfo: THintInfo);
     begin
       if HintInfo.HintControl=janArrayButton1 then
          janArrayButton1.DoShowHint(Hintstr,canshow,hintinfo);
     end;

     I could have set the application.onshowhint handler directly in this component,
     but if you have more components that do this then only the last one would work
     }
  published
    { Published declarations }
    property Align;
    property rows: integer read Frows write Setrows;
    property cols: integer read Fcols write Setcols;
    property Font;
    property Captions: Tstringlist read FCaptions write SetCaptions;
    {A list of button captions from the top-left to the bottom-right button}
    property Hints: TStringlist read FHints write SetHints;
    {A list of button hints from the top-left to the bottom-right button}
    property Colors: TStringlist read FColors write SetColors;
    {A list of button colors from the top-left to the bottom-right button
     values must standard delphi color names like clred, clblue or hex color strings like $0000ff for red.
     please note the hex order in Delphi is BGR i.s.o. the RGB order you may know from HTML hex color triplets}
    property Hint;
    property ShowHint;
    property onArrayButtonClicked: TarrayButtonClicked read FonArrayButtonClicked write SetonArrayButtonClicked;
    {provides you with the column and row of the clicked button
    the topleft button has column=0 and row=0}
  end;

implementation

{ TJvArrayButton }

constructor TJvArrayButton.Create(AOwner: TComponent);
begin
  inherited;
  width := 35;
  height := 35;
  FColor := clSilver;
  FPushDown := false;
  FCols := 1;
  FRows := 1;
  showhint := true;
  FCaptions := TStringlist.create;
  FHints := TStringlist.create;
  FColors := TStringlist.create;
end;

procedure TJvArrayButton.MouseToCell(const x, y: integer; var acol, arow: integer);
var
  dh, dw: integer;
begin
  dh := (height - 2) div FRows;
  dw := (width - 2) div Fcols;
  acol := x div dw;
  arow := y div dh;
end;

procedure TJvArrayButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  col, row: integer;
begin
  if button = mbleft then
  begin
    FPushDown := true;
    {  dh:=(height-2) div FRows;
      dw:=(width-2) div Fcols;
      col:=x div dw;
      row:=y div dh;}
    MouseToCell(x, y, col, row);
    FptDown := point(col, row);
    invalidate;
  end;
end;

procedure TJvArrayButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if button = mbleft then
  begin
    FPushDown := false;
    invalidate;
    if assigned(FonArraybuttonClicked) then
      onarraybuttonclicked(FptDown.x, FptDown.y);
  end
end;

procedure TJvArrayButton.Paint;
var
  R: TRect;
  col, row: integer;
  dh, dw: integer;
  x0, y0: integer;
  cap: string;
  backcolor: TColor;
  index: integer;

  procedure drawbackground(AColor: TColor);
  begin
    canvas.brush.color := AColor;
    canvas.FillRect(R);
  end;

  procedure drawup;
  begin
    drawbackground(Backcolor);
    Frame3D(Self.Canvas, R, clBtnHighlight, clBlack, 1);
    if cap <> '' then
      drawtext(canvas.handle, pchar(cap), -1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

  procedure drawdown;
  begin
    drawbackground(BackColor);
    Frame3D(Self.Canvas, R, clblack, clBtnHighlight, 1);
    if cap <> '' then
      drawtext(canvas.handle, pchar(cap), -1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

begin
  dh := (height - 2) div FRows;
  dw := (width - 2) div Fcols;
  for row := 0 to FRows - 1 do
  begin
    y0 := 1 + row * dh;
    for col := 0 to FCols - 1 do
    begin
      x0 := 1 + col * dw;
      R := rect(x0, y0, x0 + dw, y0 + dh);
      index := row * Fcols + col;
      if index < FCaptions.count then
        cap := FCaptions[index]
      else
        cap := '';
      if index < FColors.count then
      try
        backcolor := stringtocolor(FColors[index]);
      except
        backcolor := clsilver;
      end
      else
        backcolor := clsilver;
      if (csDesigning in ComponentState) then
        drawup
      else if (FptDown.x = col) and (FptDown.y = row) then
      begin
        if FPushDown then
          drawdown
        else
          drawup;
      end
      else
        drawup;
    end;
  end;
end;

destructor TJvArrayButton.Destroy;
begin
  FCaptions.free;
  FHints.Free;
  FColors.free;
  inherited;
end;

procedure TJvArrayButton.Setcols(const Value: integer);
begin
  if Fcols <> Value then
  begin
    if (value >= 1) and (value <= 10) then
    begin
      FCols := value;
      invalidate;
    end
  end;
end;

procedure TJvArrayButton.Setrows(const Value: integer);
begin
  if FRows <> Value then
  begin
    if (value >= 1) and (value <= 10) then
    begin
      FRows := value;
      invalidate;
    end
  end;
end;

procedure TJvArrayButton.SetonArrayButtonClicked(
  const Value: TarrayButtonClicked);
begin
  FonArrayButtonClicked := Value;
end;

procedure TJvArrayButton.SetCaptions(const Value: Tstringlist);
begin
  FCaptions.assign(Value);
  invalidate;
end;

procedure TJvArrayButton.CMFontChanged(var Message: TMessage);
begin
  canvas.font.Assign(font);
  invalidate;
end;

procedure TJvArrayButton.SetColors(const Value: TStringlist);
begin
  FColors.assign(Value);
  invalidate;
end;

function TJvArrayButton.CellRect(acol, arow: integer): TRect;
var
  dh, dw, x0, y0: integer;
begin
  dh := (height - 2) div FRows;
  dw := (width - 2) div Fcols;
  y0 := 1 + arow * dh;
  x0 := 1 + acol * dw;
  //  pt1:=clienttoscreen(point(x0,y0));
  //  pt2:=clienttoscreen(point(x0+dw,y0+dh));
  //  result:=rect(pt1.x,pt1.y,pt2.x,pt2.y);
  result := rect(x0, y0, x0 + dw, y0 + dh);
end;

procedure TJvArrayButton.DoShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
var
  acol, arow, x, y: integer;
  index: integer;
begin
  if HintInfo.HintControl = self then
  begin
    x := HintInfo.CursorPos.x;
    y := HintInfo.cursorPos.y;
    MouseToCell(x, y, acol, arow);
    if ((acol < 0) or (arow < 0)) then exit;
    index := arow * Fcols + acol;
    if index < FHints.count then
      Hintstr := FHints[index]
    else
      Hintstr := Hint;
    Hintinfo.CursorRect := CellRect(acol, arow);
    canshow := true;
  end;
end;

procedure TJvArrayButton.SetHints(const Value: TStringlist);
begin
  FHints.assign(Value);
end;

end.

{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: CategCh.pas, released 2002-01-06.

The Initial Developer of the Original Code is David Polberger <dpol@swipnet.se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-01-06;
Current Version: 1.00

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
  None.
Description:
  TCategoryChooser displays an attractive list of categories that users can
  choose from. TJvLinkLabel's demo project makes use of this component, by
  creating instances of it at run-time.

  It is not intended to be a part of the JEDI VCL, as it does one specific thing
  well, but not much else. Having said that, it's licened under the MPL license,
  just like TJvLinkLabel, so you're free to use it in your own projects, if you
  like. Simply add it to the package of your choice, and install it into the
  IDE.
-----------------------------------------------------------------------------}

unit CategCh;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs;

type
  TJvCategoryChooser = class(TGraphicControl)
  private
    FCatList: TStringList;
    FBackgroundColor: TColor;
    FActiveColor: TColor;
    FCatHeight: Integer;
    FActiveCat: Integer;
    FSelectedCat: Integer;
    FLastOutOfBounds: Boolean;

    FCatChange: TNotifyEvent;
    procedure SetCatList(const Value: TStringList);
    function GetLineColor: TColor;
    procedure SetLineColor(const Value: TColor);
    function IsCursorWithinBounds: Boolean;
    procedure DrawCat(Index: Integer; Color: TColor);
    function GetCatAtPos(Y: Integer) : Integer;
    procedure MouseLeave;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetActiveColor(const Value: TColor);
    procedure SetCatHeight(const Value: Integer);
    procedure SetSelectedCat(const Value: Integer);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
  protected
     procedure Paint; override;
     procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
     procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
     procedure DoCatChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property ActiveCat: Integer read FActiveCat;
  published
    property CatList: TStringList read FCatList write SetCatList;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property ActiveColor: TColor read FActiveColor write SetActiveColor;
    property LineColor: TColor read GetLineColor write SetLineColor;
    property Font: TFont read GetFont write SetFont;
    property CatHeight: Integer read FCatHeight write SetCatHeight;
    property SelectedCat: Integer read FSelectedCat write SetSelectedCat;

    property OnCatChange: TNotifyEvent read FCatChange write FCatChange;
  end;

implementation

{ TJvCategoryChooser }

procedure TJvCategoryChooser.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseLeave;
end;

constructor TJvCategoryChooser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCatList := TStringList.Create;

  // Set default values
  FBackgroundColor := $00F0CAA6;
  FActiveColor     := clWhite;
  FCatHeight       := 24;
  FSelectedCat     := 0;
  FLastOutOfBounds := False;

  Canvas.Font.Name  := 'Arial';
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Size  := 8;
  Canvas.Font.Color := clWindowText;

  Width := 100;
  Height := 200;
end;

function TJvCategoryChooser.IsCursorWithinBounds: Boolean;
var
  P: TPoint;
begin
  P := ScreenToClient(Mouse.CursorPos);
  Result :=
    (P.X >= 0) and (P.X <= Width) and
    (P.Y >= 0) and (P.Y <= FCatList.Count * FCatHeight - 1);
end;

destructor TJvCategoryChooser.Destroy;
begin
  inherited Destroy;
  FCatList.Free;
end;

procedure TJvCategoryChooser.DoCatChange;
begin
  if Assigned(FCatChange) then
    FCatChange(Self);
end;

procedure TJvCategoryChooser.DrawCat(Index: Integer; Color: TColor);

  function ValueToAddToTop: Integer;
  begin
    { The first category does not have a line drawn at the top; thus we fill
      the entire rectangle in this case. }
    if Index = 0 then
      Result := 0
    else
      Result := 1;
  end;

begin
  if (Index >= 0) and (Index < FCatList.Count) then
  with Canvas do
  begin
    Brush.Color := Color;
    FillRect(Rect(0, Index * FCatHeight + ValueToAddToTop, Width,
      (Index + 1) * FCatHeight));

    TextOut(8, (FCatHeight div 2 - (Abs(Font.Height) div 2)) +
      Index * FCatHeight - 2, FCatList[Index]);

    MoveTo(0, (Index + 1) * FCatHeight);
    LineTo(Width + 1, (Index + 1) * FCatHeight);
  end;
end;

function TJvCategoryChooser.GetCatAtPos(Y: Integer): Integer;
begin
  Result := Y div FCatHeight;
  if Result >= FCatList.Count then
    Result := -1;
end;

function TJvCategoryChooser.GetFont: TFont;
begin
  Result := Canvas.Font;
end;

function TJvCategoryChooser.GetLineColor: TColor;
begin
  Result := Canvas.Pen.Color;
end;

procedure TJvCategoryChooser.MouseLeave;
begin
  DrawCat(FActiveCat, FBackgroundColor);
  FActiveCat := FSelectedCat;
  DrawCat(FActiveCat, FActiveColor);
end;

procedure TJvCategoryChooser.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  CatIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);

  if IsCursorWithinBounds then
    Cursor := crHandPoint
  else
    Cursor := crDefault;

  CatIndex := GetCatAtPos(Y);

  if CatIndex <> FActiveCat then
    if IsCursorWithinBounds then
    begin
      // Let's remove the highlight from the last active category
      DrawCat(FActiveCat, FBackgroundColor);

      FActiveCat := CatIndex;
      DrawCat(FActiveCat, FActiveColor);
    end else
      if not FLastOutOfBounds then MouseLeave;

  FLastOutOfBounds := not IsCursorWithinBounds;
end;

procedure TJvCategoryChooser.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldSelectedCat: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if GetCatAtPos(Y) <> -1 then
  begin
    OldSelectedCat := FSelectedCat;
    FSelectedCat := GetCatAtPos(Y);

    if OldSelectedCat <> FSelectedCat then
      DoCatChange;
  end;
end;

procedure TJvCategoryChooser.Paint;
var
  I: Integer;

  function GetColor: TColor;
  begin
    if I = FActiveCat then
      Result := FActiveColor
    else
      Result := FBackgroundColor;
  end;

begin
  inherited Paint;

  with Canvas do
  begin
    Brush.Color := FBackgroundColor;
    FillRect(Rect(0, 0, Width, Height));

    for I := 0 to FCatList.Count - 1 do
      DrawCat(I, GetColor);
  end;
end;

procedure TJvCategoryChooser.SetActiveColor(const Value: TColor);
begin
  FActiveColor := Value;
  Paint;
end;

procedure TJvCategoryChooser.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  Paint;
end;

procedure TJvCategoryChooser.SetCatHeight(const Value: Integer);
begin
  FCatHeight := Value;
  Paint;
end;

procedure TJvCategoryChooser.SetCatList(const Value: TStringList);
begin
  FCatList.Assign(Value);
  Paint;
end;

procedure TJvCategoryChooser.SetFont(const Value: TFont);
begin
  Canvas.Font.Assign(Value);
end;

procedure TJvCategoryChooser.SetLineColor(const Value: TColor);
begin
  Canvas.Pen.Color := Value;
end;

procedure TJvCategoryChooser.SetSelectedCat(const Value: Integer);
begin
  FSelectedCat := Value;
  FActiveCat   := Value;

  DoCatChange;
  Paint;
end;

end.

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInstallLabel.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A component that makes it dead easy to have those nifty installation screens
    with a list of tasks to perform and some formatting and icons to make sure the
    user don't get lost when the big software company is stuffing his PC with rubbish. }

unit JvInstallLabel;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, JvComponent;

type
  TJvInstallLabel = class(TJvGraphicControl)
  private
    { Private declarations }
    FImageList: TImageList;
    FImageChangeLink: TChangeLink;
    FLines: TStrings;
    FStyles: TList;
    FTextOffset: integer;
    FImageOffset: integer;
    FLineSpacing: integer;
    FIndex: integer;
    procedure SetIndex(Value: integer);
    procedure SetStyles(Index: integer; Value: TFontStyles);
    function GetStyles(Index: integer): TFontStyles;
    procedure SetImageList(Value: TImageList);
    procedure SetLines(Value: TStrings);
    procedure SetImageOffset(Value: integer);
    procedure SetTextOffset(Value: integer);
    procedure SetLineSpacing(Value: integer);
    procedure ImageListChange(Sender: TObject);
    procedure UpdateStyles;
    function CheckBounds(INdex: integer): boolean;
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetStyle(LineIndex, ImageIndex: integer; LineStyle: TFontStyles);
    procedure SetExclusive(LineIndex, ImageIndex: integer; LineStyle:
      TFontStyles);
    procedure SetImage(LineIndex, ImageIndex: integer);
    property Styles[Index: integer]: TFontStyles read GetStyles write SetStyles;
  published
    { Published declarations }
    property Align;
    property Font;
    property Color default clBtnFace;
    property DefaultImage: integer read FIndex write SetIndex default -1;
    property ImageList: TImageList read FImageList write SetImageList;
    property Lines: TStrings read FLines write SetLines;
    property LineSpacing: integer read FLineSpacing write SetLineSpacing default
      10;
    property ShowHint;
    property ParentShowHint;
    property ParentFont;
    property TextOffset: integer read FTextOffset write SetTextOffset default
      24;
    property ImageOffset: integer read FImageOffset write SetImageOffset default
      2;
    property DragCursor;
    property DragMode;
    property PopupMenu;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

resourcestring
  SListOutOfBounds = 'List index out of bounds (%d)';

implementation
uses
  JvTypes;

type
  PStyles = ^TStyles;
  TStyles = record
    Style: TFontStyles;
    Index: integer;
  end;

procedure Error(Msg: string; Args: array of const);
begin
  raise EJVCLException.CreateFmt(Msg, Args);
end;

constructor TJvInstallLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TStringList.Create;
  FStyles := TList.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FTextOffset := 24;
  FImageOffset := 2;
  FLineSpacing := 10;
  FIndex := -1;
  SetBounds(0, 0, 180, 120);
end;

destructor TJvInstallLabel.Destroy;
var i: integer;
begin
  FLines.Free;
  FImageChangeLink.Free;
  for i := 0 to FStyles.Count - 1 do
    if FStyles[i] <> nil then
      Dispose(PStyles(FStyles[i]));
  FStyles.Free;
  inherited Destroy;
end;

{ make sure FLines.Count = FStyles.Count }

procedure TJvInstallLabel.UpdateStyles;
var aStyle: PStyles;
begin
  while FStyles.Count > FLines.Count do
  begin
    if FStyles.Last <> nil then
      Dispose(PStyles(FStyles.Last));
    FStyles.Delete(FStyles.Count - 1);
  end;

  while FStyles.Count < FLines.Count do
  begin
    New(aStyle);
    aStyle^.Style := Font.Style;        { default }
    aStyle^.Index := FIndex;
    FStyles.Add(aStyle);
  end;
end;

procedure TJvInstallLabel.SetIndex(Value: integer);
var i: integer;
begin
  if FIndex <> Value then
  begin
    for i := 0 to FStyles.Count - 1 do
      if PStyles(FStyles[i])^.Index = FIndex then
        PStyles(FStyles[i])^.Index := Value;
    FIndex := Value;
    Invalidate;
  end;
end;

procedure TJvInstallLabel.SetStyles(Index: integer; Value: TFontStyles);
begin
  SetStyle(Index, FIndex, Value);
end;

function TJvInstallLabel.GetStyles(Index: integer): TFontStyles;
begin
  if not CheckBounds(Index) then
    Error(SListOutOfBounds, [Index])
  else
    Result := PStyles(FStyles[Index])^.Style;
end;

procedure TJvInstallLabel.SetImageList(Value: TImageList);
begin
  if ImageList <> nil then
    ImageList.UnRegisterChanges(FImageChangeLink);
  FImageList := Value;
  if ImageList <> nil then
    ImageList.RegisterChanges(FImageChangeLink);
end;

procedure TJvInstallLabel.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
  UpdateStyles;
  Invalidate;
end;

procedure TJvInstallLabel.SetImageOffset(Value: integer);
begin
  if FImageOffset <> Value then
  begin
    FImageOffset := Value;
    Invalidate;
  end;
end;

{ offset from left edge }

procedure TJvInstallLabel.SetTextOffset(Value: integer);
begin
  if FTextOffset <> Value then
  begin
    FTextOffset := Value;
    Invalidate;
  end;
end;

{ space between lines }

procedure TJvInstallLabel.SetLineSpacing(Value: integer);
begin
  if FLineSpacing <> Value then
  begin
    FLineSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvInstallLabel.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FImageList) and (Operation = opRemove) then
    FImageList := nil;
end;

procedure TJvInstallLabel.Paint;
var tmp, H, W, i: integer;
  aRect: TRect;
  aHandle: THandle;
begin
  inherited Paint;
  with inherited Canvas do
  begin
    Brush.Color := Color;
    FillRect(ClientRect);
  end;

  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  if (FLines = nil) or (FStyles = nil) then
    Exit;

  UpdateStyles;
  Canvas.Font := Font;
  aHandle := Canvas.Handle;
  SetBkMode(aHandle, Windows.Transparent);

  H := Canvas.TextHeight('Wg');
  for i := 0 to FLines.Count - 1 do
  begin
    Canvas.Font.Style := PStyles(FStyles[i])^.Style;
    W := Canvas.TextWidth(FLines[i]);
    tmp := i * (H + FLineSpacing) + FLineSpacing;
    aRect := Rect(FTextOffset, tmp, FTextOffset + W, tmp + H);
    DrawText(aHandle, PChar(FLines[i]), -1, aRect, DT_CENTER or DT_VCENTER or
      DT_SINGLELINE or DT_NOPREFIX or DT_NOCLIP);
    if Assigned(FImageList) then
    begin
      aRect.Top := aRect.Top + ((aRect.Bottom - aRect.Top) div 2);
      FImageList.Draw(Canvas, FImageOffset, aRect.Top - FImageList.Height div 2,
        PStyles(FStyles[i])^.Index);
    end;
  end;
end;

{ set the style of this line without affecting any others }

procedure TJvInstallLabel.SetStyle(LineIndex, ImageIndex: integer; LineStyle:
  TFontStyles);
begin
  CheckBounds(LineIndex);
  UpdateStyles;
  PStyles(FStyles[LineIndex])^.Style := LineStyle;
  PStyles(FStyles[LineIndex])^.Index := ImageIndex;
  Invalidate;
end;

{ reset all lines to default style except this one  }

procedure TJvInstallLabel.SetExclusive(LineIndex, ImageIndex: integer;
  LineStyle: TFontStyles);
var i: integer;
begin
  CheckBounds(LineIndex);
  UpdateStyles;
  for i := 0 to FStyles.Count - 1 do
  begin
    PStyles(FStyles[i])^.Style := Font.Style;
    PStyles(FStyles[i])^.Index := FIndex;
  end;

  PStyles(FStyles[LineIndex])^.Style := LineStyle;
  PStyles(FStyles[LineIndex])^.Index := ImageIndex;
  Invalidate;
end;

procedure TJvInstallLabel.SetImage(LineIndex, ImageIndex: integer);
begin
  CheckBounds(LineIndex);
  UpdateStyles;
  PStyles(FStyles[LineIndex])^.Index := ImageIndex;
  Invalidate;
end;

procedure TJvInstallLabel.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

function TJvInstallLabel.CheckBounds(Index: integer): boolean;
begin
  Result := (Index > -1) and (Index < FLines.Count);
  if not Result then
    Error(SListOutOfBounds, [Index])
end;

end.


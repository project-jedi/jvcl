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
  Windows, SysUtils, Classes, Graphics, Controls, ImgList,
  JvComponent;

type
  TJvInstallLabel = class(TJvGraphicControl)
  private
    FImageList: TImageList;
    FImageChangeLink: TChangeLink;
    FLines: TStrings;
    FStyles: TList;
    FTextOffset: Integer;
    FImageOffset: Integer;
    FLineSpacing: Integer;
    FIndex: Integer;
    procedure SetIndex(Value: Integer);
    procedure SetStyles(Index: Integer; Value: TFontStyles);
    function GetStyles(Index: Integer): TFontStyles;
    procedure SetImageList(Value: TImageList);
    procedure SetLines(Value: TStrings);
    procedure SetImageOffset(Value: Integer);
    procedure SetTextOffset(Value: Integer);
    procedure SetLineSpacing(Value: Integer);
    procedure ImageListChange(Sender: TObject);
    procedure UpdateStyles;
    function CheckBounds(INdex: Integer): Boolean;
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetStyle(LineIndex, ImageIndex: Integer; LineStyle: TFontStyles);
    procedure SetExclusive(LineIndex, ImageIndex: Integer; LineStyle:
      TFontStyles);
    procedure SetImage(LineIndex, ImageIndex: Integer);
    property Styles[Index: Integer]: TFontStyles read GetStyles write SetStyles;
  published
    property Align;
    property Font;
    property Color default clBtnFace;
    property DefaultImage: Integer read FIndex write SetIndex default -1;
    property ImageList: TImageList read FImageList write SetImageList;
    property Lines: TStrings read FLines write SetLines;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 10;
    property ShowHint;
    property ParentShowHint;
    property ParentFont;
    property TextOffset: Integer read FTextOffset write SetTextOffset default 24;
    property ImageOffset: Integer read FImageOffset write SetImageOffset default 2;
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
    Index: Integer;
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
var
  I: Integer;
begin
  FLines.Free;
  FImageChangeLink.Free;
  for I := 0 to FStyles.Count - 1 do
    if FStyles[I] <> nil then
      Dispose(PStyles(FStyles[I]));
  FStyles.Free;
  inherited Destroy;
end;

{ make sure FLines.Count = FStyles.Count }

procedure TJvInstallLabel.UpdateStyles;
var
  aStyle: PStyles;
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
    aStyle^.Style := Font.Style; { default }
    aStyle^.Index := FIndex;
    FStyles.Add(aStyle);
  end;
end;

procedure TJvInstallLabel.SetIndex(Value: Integer);
var
  I: Integer;
begin
  if FIndex <> Value then
  begin
    for I := 0 to FStyles.Count - 1 do
      if PStyles(FStyles[I])^.Index = FIndex then
        PStyles(FStyles[I])^.Index := Value;
    FIndex := Value;
    Invalidate;
  end;
end;

procedure TJvInstallLabel.SetStyles(Index: Integer; Value: TFontStyles);
begin
  SetStyle(Index, FIndex, Value);
end;

function TJvInstallLabel.GetStyles(Index: Integer): TFontStyles;
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

procedure TJvInstallLabel.SetImageOffset(Value: Integer);
begin
  if FImageOffset <> Value then
  begin
    FImageOffset := Value;
    Invalidate;
  end;
end;

{ offset from left edge }

procedure TJvInstallLabel.SetTextOffset(Value: Integer);
begin
  if FTextOffset <> Value then
  begin
    FTextOffset := Value;
    Invalidate;
  end;
end;

{ space between lines }

procedure TJvInstallLabel.SetLineSpacing(Value: Integer);
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
var
  Tmp, H, W, I: Integer;
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
  for I := 0 to FLines.Count - 1 do
  begin
    Canvas.Font.Style := PStyles(FStyles[I])^.Style;
    W := Canvas.TextWidth(FLines[I]);
    Tmp := I * (H + FLineSpacing) + FLineSpacing;
    aRect := Rect(FTextOffset, Tmp, FTextOffset + W, Tmp + H);
    DrawText(aHandle, PChar(FLines[I]), -1, aRect, DT_CENTER or DT_VCENTER or
      DT_SINGLELINE or DT_NOPREFIX or DT_NOCLIP);
    if Assigned(FImageList) then
    begin
      aRect.Top := aRect.Top + ((aRect.Bottom - aRect.Top) div 2);
      FImageList.Draw(Canvas, FImageOffset, aRect.Top - FImageList.Height div 2,
        PStyles(FStyles[I])^.Index);
    end;
  end;
end;

{ set the style of this line without affecting any others }

procedure TJvInstallLabel.SetStyle(LineIndex, ImageIndex: Integer; LineStyle:
  TFontStyles);
begin
  CheckBounds(LineIndex);
  UpdateStyles;
  PStyles(FStyles[LineIndex])^.Style := LineStyle;
  PStyles(FStyles[LineIndex])^.Index := ImageIndex;
  Invalidate;
end;

{ reset all lines to default style except this one  }

procedure TJvInstallLabel.SetExclusive(LineIndex, ImageIndex: Integer;
  LineStyle: TFontStyles);
var
  I: Integer;
begin
  CheckBounds(LineIndex);
  UpdateStyles;
  for I := 0 to FStyles.Count - 1 do
  begin
    PStyles(FStyles[I])^.Style := Font.Style;
    PStyles(FStyles[I])^.Index := FIndex;
  end;

  PStyles(FStyles[LineIndex])^.Style := LineStyle;
  PStyles(FStyles[LineIndex])^.Index := ImageIndex;
  Invalidate;
end;

procedure TJvInstallLabel.SetImage(LineIndex, ImageIndex: Integer);
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

function TJvInstallLabel.CheckBounds(Index: Integer): Boolean;
begin
  Result := (Index > -1) and (Index < FLines.Count);
  if not Result then
    Error(SListOutOfBounds, [Index])
end;

end.


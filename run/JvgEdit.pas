{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgEdit.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgEdit;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, Forms,
  StdCtrls, ExtCtrls, SysUtils, Mask,
  JvgTypes, JvgCommClasses, JvgUtils, JvMaskEdit, Jvg3DColors;

type
  TJvgMaskEdit = class(TJvMaskEdit)
  private
    FScrollBars: TScrollStyle;
    FAlignment: TAlignment;
    FMultiLine: Boolean;
    FWordWrap: Boolean;
    FOnAfterPaint: TNotifyEvent;
    FCanvas: TControlCanvas;
    function GetCanvas: TCanvas;
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetAlignment(Value: TAlignment);
    procedure SetMultiLine(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    property Canvas: TCanvas read GetCanvas;
  published
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
  end;

implementation

//{$R JvgShadow.res}

constructor TJvgMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self; //...i can draw now! :)

  FScrollBars := ssNone;
  FAlignment := taLeftJustify;
  FMultiLine := False;
  FWordWrap := False;

  {$IFDEF FR_RUS}
  Font.CharSet := RUSSIAN_CHARSET;
  {$ENDIF FR_RUS}
end;

destructor TJvgMaskEdit.Destroy;
begin
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvgMaskEdit.WMPaint(var Msg: TWMPaint);
begin
  if csDestroying in ComponentState then
    Exit;
  inherited;
  if Assigned(FOnAfterPaint) then
    FOnAfterPaint(Self);
end;

procedure TJvgMaskEdit.CreateParams(var Params: TCreateParams);
const
  cAlignments: array [TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
  cMultiLine: array [Boolean] of DWORD = (0, ES_MULTILINE);
  cScrollBar: array [TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  cWordWraps: array [Boolean] of DWORD = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or cMultiLine[FMultiLine] or WS_CLIPCHILDREN or
    cAlignments[FAlignment] or cScrollBar[FScrollBars] or cWordWraps[FWordWrap];
end;

function TJvgMaskEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvgMaskEdit.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TJvgMaskEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TJvgMaskEdit.SetMultiLine(Value: Boolean);
begin
  if FMultiLine <> Value then
  begin
    FMultiLine := Value;
    RecreateWnd;
  end;
end;

procedure TJvgMaskEdit.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RecreateWnd;
  end;
end;
{
procedure TJvgMaskEdit.SetText( Value: string );
var
  i: Integer;
  fIsDigit: Boolean;
begin
  if DigitsOnly then
  begin
    Value := trim( Value );
    fIsDigit := True;
    try
      i := StrToInt( Value );
    except
      fIsDigit := False;
    end;
    if fIsDigit then Control.Text := Value;
  end
 else Control.Text := Value;

end;
}
{procedure TJvgMaskEdit.SetDigitsOnly( Value: Boolean );
var
  Text: string;
  i: Integer;
begin
  if DigitsOnly = Value then exit;
  FDigitsOnly := Value;
  if DigitsOnly then
  begin
    Control.Text := trim( Control.Text );
     try
      i := StrToInt( Control.Text );
    except
      Control.Text := '';
    end;
  end;
end;}

end.


{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgEdit.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgEdit;

interface
uses Dialogs,
  Windows, Messages, Classes, Controls, Graphics, forms,  JvMaskEdit,
  JvgTypes, JvgCommClasses, JvgUtils, StdCtrls, ExtCtrls, SysUtils, Mask, Jvg3DColors;
type

  TJvgMaskEdit = class(TJvMaskEdit)
  private
    FScrollBars: TScrollStyle;
    FAlignment: TAlignment;
    FMultiline: boolean;
    FWordWrap: boolean;
    FAfterPaint: TNotifyEvent;
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetAlignment(Value: TAlignment);
    procedure SetMultiline(Value: boolean);
    procedure SetWordWrap(Value: boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    Canvas: TCanvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint(var Message: TWMPaint); message WM_PAINT;
  published
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
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

    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars
      default ssNone;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property Multiline: boolean read FMultiline write SetMultiline
      default false;
    property WordWrap: boolean read FWordWrap write SetWordWrap
      default false;
    property AfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
  end;

implementation
//{$R JvgShadow.res}

constructor TJvgMaskEdit.Create(AOwner: TComponent);
begin
  inherited;
  Canvas := TControlCanvas.Create;
  TControlCanvas(Canvas).Control := Self; //...i can draw now! :)
  {$IFDEF FR_RUS}
  Font.CharSet := RUSSIAN_CHARSET;
  {$ENDIF}
end;

destructor TJvgMaskEdit.Destroy;
begin
  Canvas.Free;
  inherited;
end;

procedure TJvgMaskEdit.Paint(var Message: TWMPaint);
begin
  inherited;
  if Assigned(FAfterPaint) then FAfterPaint(self);
end;

procedure TJvgMaskEdit.CreateParams(var Params: TCreateParams);
const
  aAlignments: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
  aMultiline: array[boolean] of DWORD = (0, ES_MULTILINE);
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or aMultiline[FMultiline] or WS_CLIPCHILDREN
    or aAlignments[FAlignment] or ScrollBar[FScrollBars] or WordWraps[FWordWrap];
end;

procedure TJvgMaskEdit.SetScrollBars(Value: TScrollStyle);
begin
  FScrollBars := Value;
  RecreateWnd;
end;

procedure TJvgMaskEdit.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  RecreateWnd;
end;

procedure TJvgMaskEdit.SetMultiline(Value: boolean);
begin
  FMultiline := Value;
  RecreateWnd;
end;

procedure TJvgMaskEdit.SetWordWrap(Value: boolean);
begin
  FWordWrap := Value;
  RecreateWnd;
end;
{
procedure TJvgMaskEdit.SetText( Value: string );
var
  i: integer;
  fIsDigit: boolean;
begin
  if DigitsOnly then
  begin
    Value := trim( Value );
    fIsDigit := true;
    try
      i := StrToInt( Value );
    except
      fIsDigit := false;
    end;
    if fIsDigit then Control.Text := Value;
  end
 else Control.Text := Value;

end;
}
{procedure TJvgMaskEdit.SetDigitsOnly( Value: boolean );
var
  Text: string;
  i: integer;
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

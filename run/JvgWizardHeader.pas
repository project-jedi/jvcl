{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgWizardHeader.PAS, released on 2003-01-15.

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

unit JvgWizardHeader;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvgCommClasses;

type
  {$IFDEF USEJVCL}
  TJvgWizardHeader = class(TJvGraphicControl)
  {$ELSE}
  TJvgWizardHeader = class(TGraphicControl)
  {$ENDIF USEJVCL}
  private
    FComments: TStringList;
    FCaptions: TStringList;
    FPageControl: TPageControl;
    FPageNo: Integer;
    FCommentFont: TFont;
    FCaptionFont: TFont;
    FSymbolFont: TFont;
    FSymbol: string;
    FGradient: TJvgGradient;
    FGlyph: TBitmap;
    FBufferedDraw: Boolean;
    function GetCaptions: TStrings;
    function GetComments: TStrings;
    function GetGlyph: TBitmap;
    procedure SetCaptions(const Value: TStrings);
    procedure SetComments(const Value: TStrings);
    procedure SetPageNo(const Value: Integer);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetCommentFont(const Value: TFont);
    procedure SetSymbolFont(const Value: TFont);
    procedure SetSymbol(const Value: string);
    procedure SetGradient(const Value: TJvgGradient);
    procedure SetGlyph(const Value: TBitmap);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Align default alTop;
    property Anchors;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CommentFont: TFont read FCommentFont write SetCommentFont;
    property SymbolFont: TFont read FSymbolFont write SetSymbolFont;
    property PageNo: Integer read FPageNo write SetPageNo default 1;
    property Captions: TStrings read GetCaptions write SetCaptions;
    property Comments: TStrings read GetComments write SetComments;
    property Symbol: string read FSymbol write SetSymbol;
    property Gradient: TJvgGradient read FGradient write SetGradient;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property BufferedDraw: Boolean read FBufferedDraw write FBufferedDraw;
    //    property PageControl: TPageControl read FPageControl write SetPageControl;
  end;

implementation

uses
  Math,
  {$IFDEF USEJVCL}
  JvJVCLUtils,
  {$ENDIF USEJVCL}
  JvgTypes, JvgUtils;

constructor TJvgWizardHeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Align := alTop;
  Height := 60;
  FCaptions := TStringList.Create;
  FComments := TStringList.Create;
  FGradient := TJvgGradient.Create;
  FGradient.Active := True;
  FGradient.FromColor := clHighlight;
  FGradient.ToColor := clWindow;
  FGradient.Orientation := fgdVertical;
  FCaptionFont := Font;
  FCaptionFont.Style := [fsBold];
  FCommentFont := TFont.Create;
  FSymbolFont := TFont.Create;
  FSymbolFont.Name := 'Wingdings';
  FSymbolFont.Size := 26;
  FSymbolFont.Color := clHighlightText;
  FSymbolFont.Style := [fsBold];
  FPageNo := 1;
  //  FSymbol := '4';
end;

destructor TJvgWizardHeader.Destroy;
begin
  FCaptions.Free;
  FComments.Free;
  FGradient.Free;
  FCommentFont.Free;
  FSymbolFont.Free;
  FGlyph.Free;
  inherited Destroy;
end;

function TJvgWizardHeader.GetCaptions: TStrings;
begin
  Result := FCaptions;
end;

function TJvgWizardHeader.GetComments: TStrings;
begin
  Result := FComments;
end;

function TJvgWizardHeader.GetGlyph: TBitmap;
begin
  if not Assigned(FGlyph) then
    FGlyph := TBitmap.Create;
  Result := FGlyph;
end;

procedure TJvgWizardHeader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FPageControl) and (Operation = opRemove) then
    FPageControl := nil;
end;

procedure TJvgWizardHeader.Paint;
var
  R, SR: TRect;
  Offset: Integer;
  Buffer: TBitmap;
  Caption, Comment: string;
  TargetCanvas: TCanvas;
begin
  FSymbol := Copy(FSymbol, 1, 1);

  if BufferedDraw then
  begin
    Buffer := TBitmap.Create;
    Buffer.Width := Width;
    Buffer.Height := Height;
    TargetCanvas := Buffer.Canvas;
  end
  else
  begin
    Buffer := nil;
    TargetCanvas := Canvas;
  end;

  try
    if FCaptions.Count = 0 then
      Caption := 'Caption'
    else
      Caption := FCaptions[Min(FCaptions.Count-1, PageNo)];
    if FComments.Count = 0 then
      Comment := 'Some comment text' // do not localize
    else
      Comment := FComments[Min(FComments.Count - 1, PageNo)];

    R := ClientRect;

    TargetCanvas.Brush.Color := clWindow;
    TargetCanvas.FillRect(R);

    Inc(R.Left, 20);
    Dec(R.Right, 60);
    Inc(R.Top, 8);
    Dec(R.Bottom, 5);

    TargetCanvas.Font.Assign(CaptionFont);
    Windows.DrawText(TargetCanvas.Handle, PChar(Caption), Length(Caption), R,
      DT_SINGLELINE);

    Inc(R.Top, CanvasMaxTextHeight(TargetCanvas));
    Inc(R.Left, 20);

    TargetCanvas.Font.Assign(CommentFont);
    SR := R;
    Windows.DrawText(TargetCanvas.Handle, PChar(Comment), Length(Comment), SR,
      DT_WORDBREAK or DT_CALCRECT);

    OffsetRect(SR, 0, (R.Bottom - SR.Bottom) div 2);
    Windows.DrawText(TargetCanvas.Handle, PChar(Comment), Length(Comment), SR,
      DT_WORDBREAK);

    if Assigned(FGlyph) and (FGlyph.Width > 0) then
    begin
      R := ClientRect;
      Offset := (Height - FGlyph.Height) div 2;

      //    BitBlt(TargetCanvas.Handle, R.Right-FGlyph.Width-Offset, R.Top+Offset, FGlyph.Width, FGlyph.Height, FGlyph.TargetCanvas.Handle, 0, 0, SRCCOPY);
      DrawBitmapExt(TargetCanvas.Handle, FGlyph, R, R.Right - FGlyph.Width -
        Offset, R.Top + Offset,
        fwoNone, fdsDefault, True, GetTransparentColor(FGlyph,
        ftcLeftBottomPixel), 0);
    end
    else
    if Length(FSymbol) > 0 then
    begin
      TargetCanvas.Brush.Color := clHighlight;
      R := ClientRect;
      SR := Rect(R.Right - 50, R.Top + 5, R.Right - 5, R.Bottom - 5);
      if Assigned(Gradient) and Gradient.Active then
        Dec(SR.Bottom, 3);
      TargetCanvas.FillRect(SR);

      TargetCanvas.Font.Assign(SymbolFont);
      SetBkMode(TargetCanvas.Handle, TRANSPARENT);
      Windows.DrawText(TargetCanvas.Handle, PChar(Symbol), Length(FSymbol), SR,
        DT_SINGLELINE or DT_CENTER or DT_VCENTER);
    end;

    R := ClientRect;
    DrawEdge(TargetCanvas.Handle, R, EDGE_ETCHED, BF_BOTTOM);

    if Gradient.Active then
      GradientBox(TargetCanvas.Handle, Rect(R.Left, R.Bottom - 5, R.Right,
        R.Bottom - 1), Gradient, 1, 1);

    if BufferedDraw then
      BitBlt(Canvas.Handle, 0, 0, Width, Height, TargetCanvas.Handle, 0, 0,
        SRCCOPY);
  finally
    if BufferedDraw then
      Buffer.Free;
  end;
end;

procedure TJvgWizardHeader.SetCaptionFont(const Value: TFont);
begin
  if FCaptionFont <> Value then
  begin
    FCaptionFont.Assign(Value);
    Invalidate;
  end;
end;

procedure TJvgWizardHeader.SetCaptions(const Value: TStrings);
begin
  FCaptions.Assign(Value);
  Invalidate;
end;

procedure TJvgWizardHeader.SetCommentFont(const Value: TFont);
begin
  if FCommentFont <> Value then
  begin
    FCommentFont.Assign(Value);
    Invalidate;
  end;
end;

procedure TJvgWizardHeader.SetComments(const Value: TStrings);
begin
  FComments.Assign(Value);
  Invalidate;
end;

procedure TJvgWizardHeader.SetGlyph(const Value: TBitmap);
begin
  if not Assigned(FGlyph) then
    FGlyph := TBitmap.Create;
  FGlyph.Assign(Value);
end;

procedure TJvgWizardHeader.SetGradient(const Value: TJvgGradient);
begin
  FGradient.Assign(Value);
end;

procedure TJvgWizardHeader.SetPageNo(const Value: Integer);
begin
  if FPageNo <> Value then
  begin
    FPageNo := Value;
    Invalidate;
  end;
end;

procedure TJvgWizardHeader.SetSymbol(const Value: string);
begin
  if FSymbol <> Value then
  begin
    FSymbol := Value;
    Invalidate;
  end;
end;

procedure TJvgWizardHeader.SetSymbolFont(const Value: TFont);
begin
  FSymbolFont.Assign(Value);
  Invalidate;
end;

end.


{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMarkupLabel.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvMarkupLabel;

{$OBJEXPORTALL On}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Messages,
  JvComponent, JvMarkupCommon;

type
  TJvMarkupLabel = class(TJvGraphicControl)
  private
    ElementStack: TJvHTMLElementStack;
    TagStack: TJvHTMLElementStack;
    FText: string;
    FBackColor: TColor;
    FMarginLeft: integer;
    FMarginRight: integer;
    FMarginTop: integer;
    FAlignment: TAlignment;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    procedure Refresh;
    procedure ParseHTML(s: string);
    procedure RenderHTML;
    procedure HTMLClearBreaks;
    procedure HTMLElementDimensions;
    procedure SetBackColor(const Value: TColor);
    procedure SetText(const Value: string);
    procedure SetMarginLeft(const Value: integer);
    procedure SetMarginRight(const Value: integer);
    procedure SetMarginTop(const Value: integer);
    procedure SetAlignment(const Value: TAlignment);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure SetAutoSize(Value: Boolean); override; 
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property BackColor: TColor read FBackColor write SetBackColor default clWhite;
    property Height default 100;
    property Width default 200;
    property MarginLeft: integer read FMarginLeft write SetMarginLeft default 5;
    property MarginRight: integer read FMarginRight write SetMarginRight default 5;
    property MarginTop: integer read FMarginTop write SetMarginTop default 5;
    property Text: string read FText write SetText;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize;
    property Align;
    property Font;

    property Anchors;
    {$IFDEF MSWINDOWS}
    property BiDiMode;
    {$ENDIF}
//    property Color;   // Replace by BackColor
    property Constraints;
    {$IFDEF MSWINDOWS}
    property DragCursor;
    property DragKind;
    property DragMode;
    {$ENDIF}
    property Enabled;
    {$IFDEF MSWINDOWS}
    property ParentBiDiMode;
    {$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF MSWINDOWS}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF MSWINDOWS}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnStartDock;
    {$ENDIF}
    property OnStartDrag;
  end;

implementation

uses
  JvConsts, JvThemes;


{ TJvMarkupLabel }

constructor TJvMarkupLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);
  ElementStack := TJvHTMLElementStack.Create;
  TagStack := TJvHTMLElementStack.Create;
  FBackColor := clWhite;
  FAlignment := taLeftJustify;
  Width := 200;
  Height := 100;
  FMarginLeft := 5;
  FMarginRight := 5;
  FMarginTop := 5;
end;

destructor TJvMarkupLabel.Destroy;
begin
  ElementStack.Free;
  TagStack.Free;
  inherited Destroy;
end;

procedure TJvMarkupLabel.HTMLClearBreaks;
var
  I, C: integer;
  El: TJvHTMLElement;
begin
  C := ElementStack.Count;
  if C = 0 then Exit;
  for I := 0 to C - 1 do
  begin
    El := TJvHTMLElement(ElementStack.Items[I]);
    El.SolText := '';
    El.EolText := '';
  end;
end;

procedure TJvMarkupLabel.HTMLElementDimensions;
var
  I, C: integer;
  El: TJvHTMLElement;
  H, A, W: integer;
  Tm: TextMetric;
  S: string;
begin
  C := ElementStack.Count;
  if C = 0 then Exit;
  for I := 0 to C - 1 do
  begin
    El := TJvHTMLElement(ElementStack.Items[I]);
    S := El.Text;
    Canvas.Font.Name := El.FontName;
    Canvas.Font.Size := El.FontSize;
    Canvas.Font.Style := El.FontStyle;
    Canvas.Font.Color := El.FontColor;
    GetTextMetrics(Canvas.Handle, Tm);
    H := Tm.tmHeight;
    A := Tm.tmAscent;
    W := Canvas.TextWidth(S);
    El.Height := H;
    El.Ascent := A;
    El.Width := W;
  end;
end;

procedure TJvMarkupLabel.Refresh;
begin
  ParseHTML(FText);
  HTMLElementDimensions;
  Invalidate;
end;

procedure TJvMarkupLabel.Paint;
begin
  RenderHTML;
end;

procedure TJvMarkupLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Refresh;
end;

procedure TJvMarkupLabel.ParseHTML(S: string);
var
  P: integer;
  SE, ST: string;
  FText: string;
  FStyle: TFontStyles;
  FName: string;
  FSize: integer;
  FBreakLine: boolean;
  AColor, FColor: TColor;
  Element: TJvHTMLElement;

  function HTMLStringToColor(V: string; var Col: TColor): boolean;
  var
    VV: string;
  begin
    if Copy(V, 1, 1) <> '#' then
    begin
      VV := 'cl' + V;
      try
        Col := StringToColor(VV);
        Result := True;
      except
        Result := False;
      end;
    end
    else
    begin
      try
        VV := '$' + Copy(V, 6, 2) + Copy(V, 4, 2) + Copy(V, 2, 2);
        Col := StringToColor(VV);
        Result := True;
      except
        Result := False;
      end
    end
  end;

  procedure PushTag;
  begin
    Element := TJvHTMLElement.Create;
    Element.FontName := FName;
    Element.FontSize := FSize;
    Element.FontStyle := FStyle;
    Element.FontColor := FColor;
    TagStack.Push(Element);
  end;

  procedure PopTag;
  begin
    Element := TagStack.Pop;
    if Element <> nil then
    begin
      FName := Element.FontName;
      FSize := Element.FontSize;
      FStyle := Element.FontStyle;
      FColor := Element.FontColor;
      Element.Free;
    end;
  end;

  procedure PushElement;
  begin
    Element := TJvHTMLElement.Create;
    Element.Text := FText;
    Element.FontName := FName;
    Element.FontSize := FSize;
    Element.FontStyle := FStyle;
    Element.FontColor := FColor;
    Element.BreakLine := FBreakLine;
    FBreakLine := False;
    ElementStack.Push(Element);
  end;

  procedure ParseTag(SS: string);
  var
    PP: integer;
    ATag, APar, AVal: string;
    HavePar: boolean;
  begin
    SS := Trim(SS);
    HavePar := False;
    PP := Pos(' ', SS);
    if PP = 0 then
    begin // tag only
      ATag := SS;
    end
    else
    begin // tag + atrributes
      ATag := Copy(SS, 1, PP - 1);
      SS := Trim(Copy(SS, PP + 1, Length(SS)));
      HavePar := True;
    end;
    // handle ATag
    ATag := LowerCase(ATag);
    if ATag = 'br' then
      FBreakLine := True
    else if ATag = 'b' then
    begin // bold
      PushTag;
      FStyle := FStyle + [fsBold];
    end
    else if ATag = '/b' then
    begin // cancel bold
      FStyle := FStyle - [fsBold];
      PopTag;
    end
    else if ATag = 'I' then
    begin // italic
      PushTag;
      FStyle := FStyle + [fsItalic];
    end
    else if ATag = '/I' then
    begin // cancel italic
      FStyle := FStyle - [fsItalic];
      PopTag;
    end
    else if ATag = 'u' then
    begin // underline
      PushTag;
      FStyle := FStyle + [fsUnderline];
    end
    else if ATag = '/u' then
    begin // cancel underline
      FStyle := FStyle - [fsUnderline];
      PopTag;
    end
    else if ATag = 'font' then
    begin
      PushTag;
    end
    else if ATag = '/font' then
    begin
      PopTag;
    end;
    if HavePar then
    begin
      repeat
        PP := Pos('="', SS);
        if PP > 0 then
        begin
          APar := LowerCase(Trim(Copy(SS, 1, PP - 1)));
          Delete(SS, 1, PP + 1);
          PP := Pos('"', SS);
          if PP > 0 then
          begin
            AVal := Copy(SS, 1, PP - 1);
            Delete(SS, 1, PP);
            if APar = 'face' then
            begin
              FName := AVal;
            end
            else if APar = 'size' then
            try
              FSize := StrToInt(AVal);
            except
            end
            else if APar = 'color' then
            try
              if HTMLStringToColor(AVal, AColor) then
                FColor := AColor;
            except
            end
          end;
        end;
      until PP = 0;
    end;
  end;
begin
  ElementStack.Clear;
  TagStack.Clear;
  FStyle := Font.Style;
  FName := Font.Name;
  FSize := Font.Size;
  FColor := Font.Color;
  FBreakLine := False;
  repeat
    P := Pos('<', S);
    if P = 0 then
    begin
      FText := S;
      PushElement;
    end
    else
    begin
      if P > 1 then
      begin
        SE := Copy(S, 1, P - 1);
        FText := SE;
        PushElement;
        Delete(S, 1, P - 1);
      end;
      P := Pos('>', S);
      if P > 0 then
      begin
        ST := Copy(S, 2, P - 2);
        Delete(S, 1, P);
        ParseTag(ST);
      end;
    end;
  until P = 0;
end;

procedure TJvMarkupLabel.RenderHTML;
var
  R: TRect;
  I, C, X, Y,
  ATotalWidth, AClientWidth, ATextWidth,
  BaseLine,
  iSol, iEol,
  MaxHeight, MaxWidth, MaxAscent : integer;
  El: TJvHTMLElement;
  Eol: boolean;
  PendingBreak: boolean;

  procedure SetFont(EE: TJvHTMLElement);
  begin
    with Canvas do
    begin
      Font.Name := EE.FontName;
      Font.Size := EE.FontSize;
      Font.Style := EE.FontStyle;
      Font.Color := EE.FontColor;
    end;
  end;

  procedure RenderString(EE: TJvHTMLElement; Test: boolean);
  var
    SS: string;
    WW: integer;
  begin
    SetFont(EE);
    if EE.SolText <> '' then
    begin
      SS := EE.SolText;
      WW := Canvas.TextWidth(SS);
      if not Test then
        Canvas.TextOut(X, Y + BaseLine - EE.Ascent, SS);
      X := X + WW;
    end;
  end;

begin
  iEol := 0; // Not Needed but removes warning.
  R := ClientRect;
  Canvas.Brush.Color := BackColor;
  DrawThemedBackground(Self, Canvas, R);
  C := ElementStack.Count;
  if C = 0 then Exit;
  HTMLClearBreaks;
  AClientWidth := ClientWidth - MarginLeft - MarginRight;
  Canvas.Brush.Style := bsClear;
  Y := MarginTop;
  iSol := 0;
  PendingBreak := False;
  MaxWidth := 0;
  repeat
    I := iSol;
    ATotalWidth := AClientWidth;
    ATextWidth := 0;
    MaxHeight := 0;
    MaxAscent := 0;
    Eol := False;
    repeat // scan line
      El := TJvHTMLElement(ElementStack.Items[I]);
      if El.BreakLine then
      begin
        if not PendingBreak then
        begin
          PendingBreak := True;
          iEol := I;
          Break;
        end
        else
          PendingBreak := False;
      end;
      if El.Height > MaxHeight then MaxHeight := El.Height;
      if El.Ascent > MaxAscent then MaxAscent := El.Ascent;
      El.Breakup(Canvas, ATotalWidth);
      if El.SolText <> '' then
      begin
        ATotalWidth := ATotalWidth - Canvas.TextWidth(El.SolText);
        ATextWidth := ATextWidth + Canvas.TextWidth(El.SolText);
        if El.EolText = '' then
        begin
          if I >= C - 1 then
          begin
            Eol := True;
            iEol := I;
          end
          else
          begin
            inc(I);
          end
        end
        else
        begin
          Eol := True;
          iEol := I;
        end;
      end
      else
      begin // Eol
        Eol := True;
        iEol := I;
      end;
    until Eol;
    
    // render line
    BaseLine := MaxAscent;

    if AutoSize then
    begin
      X := MarginLeft;
      if (ATextWidth + MarginLeft + MarginRight) > MaxWidth then
        MaxWidth := (ATextWidth + MarginLeft + MarginRight);
    end
    else
    case FAlignment of
      taLeftJustify  : X := MarginLeft;
      taRightJustify : X := Width - MarginRight - ATextWidth;
      taCenter       : X := MarginLeft + (Width - MarginLeft - MarginRight - ATextWidth) div 2;
    end;

    for I := iSol to iEol do
    begin
      El := TJvHTMLElement(ElementStack.Items[I]);
      RenderString(El,False);
    end;

    Y := Y + MaxHeight;
    iSol := iEol;
  until (iEol >= C - 1) and (El.EolText = '');
  if AutoSize then
  begin
    Width := MaxWidth;
    Height := y + 5;
  end;
end;

procedure TJvMarkupLabel.SetAlignment(const Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupLabel.SetBackColor(const Value: TColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TJvMarkupLabel.SetAutoSize(Value: Boolean);
begin
  inherited;
  Invalidate;
end;

procedure TJvMarkupLabel.SetMarginLeft(const Value: integer);
begin
  FMarginLeft := Value;
  Invalidate;
end;

procedure TJvMarkupLabel.SetMarginRight(const Value: integer);
begin
  FMarginRight := Value;
  Invalidate;
end;

procedure TJvMarkupLabel.SetMarginTop(const Value: integer);
begin
  FMarginTop := Value;
  Invalidate;
end;

procedure TJvMarkupLabel.SetText(const Value: string);
var
  S: string;
begin
  if Value = FText then Exit;
  S := Value;
  S := StringReplace(S, SLineBreak, ' ', [rfReplaceAll]);
  S := TrimRight(S);
  FText := S;
  Refresh;
end;

end.

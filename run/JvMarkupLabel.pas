{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMarkupLabel.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s):
Robert Love [rlove att slcdug dott org].
Lionel Reynaud

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvMarkupLabel;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Graphics, Controls,
  SysUtils, Classes,
  JvComponent, JvMarkupCommon;

type
  TJvMarkupLabel = class(TJvPubGraphicControl)
  private
    FElementStack: TJvHTMLElementStack;
    FTagStack: TJvHTMLElementStack;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer;
    FAlignment: TAlignment;
    FText: TCaption;
    {$IFDEF VisualCLX}
    FAutoSize: Boolean;
    {$ENDIF VisualCLX}
    procedure Refresh;
    procedure ParseHTML(S: string);
    procedure RenderHTML;
    procedure HTMLClearBreaks;
    procedure HTMLElementDimensions;
    procedure SetMarginLeft(const Value: Integer);
    procedure SetMarginRight(const Value: Integer);
    procedure SetMarginTop(const Value: Integer);
    procedure SetAlignment(const Value: TAlignment);
    procedure DoReadBackColor(Reader: TReader);
  protected
    procedure FontChanged; override;
    procedure SetText(const Value: TCaption); {$IFDEF VisualCLX} override; {$ENDIF}
    procedure SetAutoSize(Value: Boolean); {$IFDEF VCL} override; {$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Height default 100;
    property Width default 200;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft default 5;
    property MarginRight: Integer read FMarginRight write SetMarginRight default 5;
    property MarginTop: Integer read FMarginTop write SetMarginTop default 5;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Text: TCaption read FText write SetText;
    {$IFDEF VCL}
    property AutoSize;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    {$ENDIF VisualCLX}
    property Align;
    property Font;
    property Anchors;
    property Enabled;
    property Color default clBtnFace;   // Duplicates BackColor
    property Constraints;
    property ParentColor default True;
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
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDrag;
  end;

implementation

uses
  JvConsts, JvThemes;

constructor TJvMarkupLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF VCL}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF VCL}
  FElementStack := TJvHTMLElementStack.Create;
  FTagStack := TJvHTMLElementStack.Create;
  FAlignment := taLeftJustify;
  Width := 200;
  Height := 100;
  FMarginLeft := 5;
  FMarginRight := 5;
  FMarginTop := 5;
  Color := clBtnFace;
  ParentColor := True;
end;

destructor TJvMarkupLabel.Destroy;
begin
  FElementStack.Free;
  FTagStack.Free;
  inherited Destroy;
end;

procedure TJvMarkupLabel.HTMLClearBreaks;
var
  I, C: Integer;
  El: TJvHTMLElement;
begin
  C := FElementStack.Count;
  if C = 0 then
    Exit;
  for I := 0 to C - 1 do
  begin
    El := TJvHTMLElement(FElementStack.Items[I]);
    El.SolText := '';
    El.EolText := '';
  end;
end;

procedure TJvMarkupLabel.HTMLElementDimensions;
var
  I, C: Integer;
  El: TJvHTMLElement;
  H, A, W: Integer;
  Tm: TTextMetric;
  S: string;
begin
  C := FElementStack.Count;
  if C = 0 then
    Exit;
  for I := 0 to C - 1 do
  begin
    El := TJvHTMLElement(FElementStack.Items[I]);
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

procedure TJvMarkupLabel.FontChanged;
begin
  inherited FontChanged;
  Refresh;
end;

procedure TJvMarkupLabel.ParseHTML(S: string);
var
  P: Integer;
  SE, ST: string;
  FText: string;
  FStyle: TFontStyles;
  FName: string;
  FSize: Integer;
  FBreakLine: Boolean;
  AColor, FColor: TColor;
  Element: TJvHTMLElement;

  function HTMLStringToColor(V: string; var Col: TColor): Boolean;
  var
    VV: string;
  begin
    Result := False;
    if Length(V) < 2 then
      Exit;
    if not (V[1] in ['#', '$']) then
    begin
      // allow the use of both "clBlack" and "Black" 
      if Pos('cl', AnsiLowerCase(V)) = 1 then
        VV := V
      else
        VV := 'cl' + V;
      try
        Col := StringToColor(VV);
        Result := True;
      except
        Result := False;
      end;
    end
    else
    // this is either #FFFFFF or $FFFFFF - we treat them the same
    begin
      try
        VV := '$' + Copy(V, 6, 2) + Copy(V, 4, 2) + Copy(V, 2, 2);
        Col := StringToColor(VV);
        Result := True;
      except
        Result := False;
      end
    end;
  end;

  procedure PushTag;
  begin
    Element := TJvHTMLElement.Create;
    Element.FontName := FName;
    Element.FontSize := FSize;
    Element.FontStyle := FStyle;
    Element.FontColor := FColor;
    FTagStack.Push(Element);
  end;

  procedure PopTag;
  begin
    Element := FTagStack.Pop;
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
    FElementStack.Push(Element);
  end;

  procedure ParseTag(SS: string);
  var
    PP: Integer;
    ATag, APar, AVal: string;
    HaveParams: Boolean;
  begin
    SS := Trim(SS);
    HaveParams := False;
    PP := Pos(' ', SS);
    if PP = 0 then
      ATag := SS // tag only
    else
    begin // tag + attributes
      ATag := Copy(SS, 1, PP - 1);
      SS := Trim(Copy(SS, PP + 1, Length(SS)));
      HaveParams := True;
    end;
    // handle ATag
    ATag := LowerCase(ATag);
    if ATag = 'br' then
      FBreakLine := True
    else
    if ATag = 'b' then
    begin // bold
      PushTag;
      FStyle := FStyle + [fsBold];
    end
    else
    if ATag = '/b' then
    begin // cancel bold
      FStyle := FStyle - [fsBold];
      PopTag;
    end
    else
    if ATag = 'i' then
    begin // italic
      PushTag;
      FStyle := FStyle + [fsItalic];
    end
    else
    if ATag = '/i' then
    begin // cancel italic
      FStyle := FStyle - [fsItalic];
      PopTag;
    end
    else
    if ATag = 'u' then
    begin // underline
      PushTag;
      FStyle := FStyle + [fsUnderline];
    end
    else
    if ATag = '/u' then
    begin // cancel underline
      FStyle := FStyle - [fsUnderline];
      PopTag;
    end
    else
    if ATag = 'font' then
      PushTag
    else
    if ATag = '/font' then
      PopTag;
    if HaveParams then
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
              FName := AVal
            else
            if APar = 'size' then
              try
                FSize := StrToInt(AVal);
              except
              end
            else
            if APar = 'color' then
              try
                if HTMLStringToColor(AVal, AColor) then
                  FColor := AColor;
              except
              end;
          end;
        end;
      until PP = 0;
    end;
  end;

begin
  FElementStack.Clear;
  FTagStack.Clear;
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
  I, C, X, Y: Integer;
  ATotalWidth, AClientWidth, ATextWidth, BaseLine: Integer;
  iSol, iEol, PendingCount, MaxHeight, MaxAscent: Integer;
  El: TJvHTMLElement;
  Eol: Boolean;
  PendingBreak: Boolean;
  lSolText: string;
  MaxWidth: Integer;

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

  procedure RenderString(EE: TJvHTMLElement; Test: Boolean);
  var
    SS: string;
    WW: Integer;
  begin
    SetFont(EE);
    if EE.SolText <> '' then
    begin
      SS := TrimLeft(EE.SolText);
      WW := Canvas.TextWidth(SS);
      if not Test then
        Canvas.TextOut(X, Y + BaseLine - EE.Ascent, SS);
      X := X + WW;
    end;
  end;

begin
  iEol := 0; // Not Needed but removes warning.
  R := ClientRect;
  Canvas.Brush.Color := Color;
  DrawThemedBackground(Self, Canvas, R);
  C := FElementStack.Count;
  if C = 0 then
    Exit;
  HTMLClearBreaks;
  if AutoSize then
    AClientWidth := 10000
  else
    AClientWidth := ClientWidth - MarginLeft - MarginRight;

  Canvas.Brush.Style := bsClear;
  Y := MarginTop;
  iSol := 0;
  PendingBreak := False;
  PendingCount := -1;
  MaxWidth := 0;
  repeat
    I := iSol;
    ATotalWidth := AClientWidth;
    ATextWidth := 0;
    MaxHeight := 0;
    MaxAscent := 0;
    Eol := False;
    repeat // scan line
      El := TJvHTMLElement(FElementStack.Items[I]);
      if El.BreakLine then
      begin
        if not PendingBreak and (PendingCount <> I) then
        begin
          PendingBreak := True;
          PendingCount := I;
          iEol := I;
          Break;
        end
        else
          PendingBreak := False;
      end;
      if El.Height > MaxHeight then
        MaxHeight := El.Height;
      if El.Ascent > MaxAscent then
        MaxAscent := El.Ascent;
      if El.Text <> '' then
      begin
        lSolText := El.SolText;
        // (Lionel) If Breakup can do something, I increase a bit the space until
        // it can do the break ...
        repeat
          El.Breakup(Canvas, ATotalWidth);
          Inc(ATotalWidth, 5);
        until lSolText <> El.SolText;
      end;
      if El.SolText <> '' then
      begin
        ATotalWidth := ATotalWidth - Canvas.TextWidth(El.SolText) - 5;
        ATextWidth := ATextWidth + Canvas.TextWidth(El.SolText);
        if El.EolText = '' then
        begin
          if I >= C - 1 then
          begin
            Eol := True;
            iEol := I;
          end
          else
            Inc(I);
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
      case Alignment of
        taLeftJustify:
          X := MarginLeft;
        taRightJustify:
          X := Width - MarginRight - ATextWidth;
        taCenter:
          X := MarginLeft + (Width - MarginLeft - MarginRight - ATextWidth) div 2;
      end;

    for I := iSol to iEol do
    begin
      El := TJvHTMLElement(FElementStack.Items[I]);
      RenderString(El, False);
    end;

    Y := Y + MaxHeight;
    iSol := iEol;
  until (iEol >= C - 1) and (El.EolText = '');
  if AutoSize then
  begin
    Width := MaxWidth;
    Height := Y + 5;
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

procedure TJvMarkupLabel.SetAutoSize(Value: Boolean);
begin
  {$IFDEF VCL}
  inherited SetAutoSize(Value);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  FAutoSize := Value;
  {$ENDIF VisualCLX}
  Invalidate;
end;

procedure TJvMarkupLabel.SetMarginLeft(const Value: Integer);
begin
  FMarginLeft := Value;
  Invalidate;
end;

procedure TJvMarkupLabel.SetMarginRight(const Value: Integer);
begin
  FMarginRight := Value;
  Invalidate;
end;

procedure TJvMarkupLabel.SetMarginTop(const Value: Integer);
begin
  FMarginTop := Value;
  Invalidate;
end;

procedure TJvMarkupLabel.SetText(const Value: TCaption);
var
  S: string;
begin
  if Value = FText then
    Exit;
  S := Value;
  S := StringReplace(S, SLineBreak, ' ', [rfReplaceAll]);
  S := TrimRight(S);
  FText := S;
  {$IFDEF VisualCLX}
  inherited SetText(FText);
  {$ENDIF VisualCLX}
  Refresh;
end;

{function TJvMarkupLabel.GetBackColor: TColor;
begin
  Result := Color;
end;

procedure TJvMarkupLabel.SetBackColor(const Value: TColor);
begin
  Color := Value;
end;}

procedure TJvMarkupLabel.DoReadBackColor(Reader: TReader);
begin
  if Reader.NextValue = vaIdent then
    Color := StringToColor(Reader.ReadIdent)
  else
    Color := Reader.ReadInteger;
end;

procedure TJvMarkupLabel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('BackColor', DoReadBackColor, nil, False);
end;

end.


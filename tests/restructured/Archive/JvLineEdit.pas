{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLineEdit.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{  A TEdit displayed as a single line. Idea "stolen" from an add for
    Raize components. The TJvLineCombo works the same way) }

unit JvLineEdit;

{
  Bugs:
    TJvLineCombo doesn't draw correctly. Probably has something to do with the
    embedded editcontrol but haven't solved it yet
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCTrls, JVCLVer;

type
  { TJvLineEdit }
  TJvLineEdit = class(TEdit)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FColorLo, FColorHi: TColor;
    FSingle: boolean;
    procedure SetSingle(Value: boolean);
    procedure SetColorHi(Value: TColor);
    procedure SetColorLo(Value: TColor);
    procedure RedrawBorder;
    procedure AdjustHeight;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property AutoSize default false;
    property SingleLine: boolean read FSingle write SetSingle default true;
    property Color default clBtnFace;
    property Ctl3D default false;
    property BorderStyle default bsNone;
    property ColorHi: TColor read FColorHi write SetColorHi default clBtnHighLight;
    property ColorLo: TColor read FColorLo write SetColorLo default clBtnShadow;
  end;

  { TJvLineCombo }
  TJvLineCombo = class(TComboBox)
  private
    FColorLo, FColorHi: TColor;
    FComplete: boolean;
    FSingle: boolean;
    FKey: word;
    procedure SetSingle(Value: boolean);
    procedure SetComplete(Value: boolean);
    procedure SetColorHi(Value: TColor);
    procedure SetColorLo(Value: TColor);
    procedure RedrawBorder;
    procedure AdjustHeight;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
  protected
    procedure Loaded; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoComplete: boolean read FComplete write SetComplete default true;
    property Color default clBtnFace;
    property Ctl3D default false;
    property ColorHi: TColor read FColorHi write SetColorHi default clBtnHighLight;
    property ColorLo: TColor read FColorLo write SetColorLo default clBtnShadow;
    property SingleLine: boolean read FSingle write SetSingle default true;
  end;

implementation

{ TJvLineEdit }

constructor TJvLineEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := false;
  FSingle := true;
  ParentCtl3d := false;
  Ctl3D := False;
  BorderStyle := bsNone;
  Color := clBtnFace;
  FColorLo := clBtnShadow;
  FColorHi := clBtnHighLight;
  ControlStyle := ControlStyle - [csFramed];
  Height := 21;
end;

procedure TJvLineEdit.SetSingle(Value: boolean);
begin
  if FSingle <> Value then
  begin
    FSingle := Value;
    if FSingle then
    begin
      ControlStyle := ControlStyle - [csFramed];
      BorderSTyle := bsNone;
      Ctl3D := false;
    end
    else
    begin
      ControlStyle := ControlStyle + [csFramed];
      BorderSTyle := bsSingle;
      Ctl3D := true;
    end;
    RecreateWnd;
  end;
end;

procedure TJvLineEdit.SetColorHi(Value: TColor);
begin
  if FColorHi <> Value then
  begin
    FColorHi := Value;
    RedrawBorder;
  end;
end;

procedure TJvLineEdit.SetColorLo(Value: TColor);
begin
  if FColorLo <> Value then
  begin
    FColorLo := Value;
    RedrawBorder;
  end;
end;

procedure TJvLineEdit.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  if not FSingle then
    Exit;

  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Height := Metrics.tmHeight + 6;
end;

procedure TJvLineEdit.Loaded;
begin
  inherited Loaded;
  AdjustHeight;
end;

procedure TJvLineEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TJvLineEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not ((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    AdjustHeight;
end;

procedure TJvLineEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if FSingle then
    InflateRect(Message.CalcSize_Params^.rgrc[0], -2, -2);
end;

procedure TJvLineEdit.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorder;
end;

procedure TJvLineEdit.RedrawBorder;
var
  DC: HDC;
  R: TRect;
  FCanvas: TCanvas;
begin
  if not FSingle then
    Exit;

  DC := GetWindowDC(Handle);
  if DC = 0 then
    Exit;
  FCanvas := TCanvas.Create;
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    FCanvas.Handle := DC;
    with FCanvas do
    begin
      Frame3d(FCanvas, R, Color, Color, 2);
      Pen.Color := ColorToRGB(ColorHi);
      MoveTo(R.Left - 2, R.Bottom);
      LineTo(R.Right + 2, R.Bottom);
      Pen.Color := ColorToRGB(ColorLo);
      MoveTo(R.Left - 2, R.Bottom + 1);
      LineTo(R.Right + 2, R.Bottom + 1);
    end;
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
    ReleaseDC(Handle, DC);
  end;
end;

{ TJvLineCombo }

constructor TJvLineCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Ctl3D := False;
  Color := clBtnFace;
  FColorLo := clBtnShadow;
  FColorHi := clBtnHighLight;
  ControlStyle := ControlStyle - [csFramed];
  FSingle := true;
  FComplete := true;
  Height := 21;
end;

procedure TJvLineCombo.SetSingle(Value: boolean);
begin
  if FSingle <> Value then
  begin
    FSingle := Value;
    if not FSingle then
    begin
      ControlStyle := ControlStyle + [csFramed];
      Ctl3D := true;
    end
    else
    begin
      ControlStyle := ControlStyle - [csFramed];
      Ctl3D := false;
    end;
    RecreateWnd;
  end;
end;

procedure TJvLineCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FKey := Key;
end;

procedure TJvLineCombo.Change;
var i, j: integer;
begin
  inherited Change;
  if (FComplete) and (Items.Count > 0) and (FKey > 31)
    and (FKey <> VK_DELETE) and (Style in [csDropDown, csSimple]) then
  begin
    i := SendMessage(Handle, CB_FINDSTRING, -1, longint(PChar(Text)));
    if (i in [0..Items.Count - 1]) then
    begin
      j := Length(Text);
      Text := Items[i];
      SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(j, Length(Text)));
    end;
  end;
end;

procedure TJvLineCombo.SetComplete(Value: boolean);
begin
  if FComplete <> Value then
    FComplete := Value
end;

procedure TJvLineCombo.SetColorHi(Value: TColor);
begin
  if FColorHi <> Value then
  begin
    FColorHi := Value;
    RecreateWnd;
  end;
end;

procedure TJvLineCombo.SetColorLo(Value: TColor);
begin
  if FColorLo <> Value then
  begin
    FColorLo := Value;
    RecreateWnd;
  end;
end;

procedure TJvLineCombo.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if FSingle then
    Height := Metrics.tmHeight + 6
  else
    Height := Metrics.tmHeight + 5;
end;

procedure TJvLineCombo.Loaded;
begin
  inherited Loaded;
  AdjustHeight;
end;

procedure TJvLineCombo.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TJvLineCombo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not ((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    AdjustHeight;
end;

procedure TJvLineCombo.WMNCPaint(var Message: TMessage);
begin
  inherited;
  RedrawBorder;
end;

procedure TJvLineCombo.WMPaint(var Message: TMessage);
begin
  inherited;
  ReDrawBorder;
end;

procedure TJvLineCombo.RedrawBorder;
var
  DC: HDC;
  R: TRect;
  FCanvas: TCanvas;
begin
  if not FSingle then
    Exit;

  DC := GetWindowDC(Handle);
  if DC = 0 then
    Exit;
  FCanvas := TCanvas.Create;
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    FCanvas.Handle := DC;
    with FCanvas do
    begin
      Frame3d(FCanvas, R, Color, Color, 2);
      Pen.Color := ColorToRGB(ColorHi);
      MoveTo(R.Left, R.Bottom);
      LineTo(R.Right, R.Bottom);
      Pen.Color := ColorToRGB(ColorLo);
      MoveTo(R.Left, R.Bottom + 1);
      LineTo(R.Right, R.Bottom + 1);
    end;
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
    ReleaseDC(Handle, DC);
  end;
end;

end.


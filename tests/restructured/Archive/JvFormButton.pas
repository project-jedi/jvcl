{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormButton.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvFormButton;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  JvTypes, JvComponent;

type
  TSystemGlyph = (sgNone, sgBtnCorners, sgBtSize, sgCheck, sgCheckBoxes, sgClose,
    sgCombo, sgDnArrow, sgDnArrowD, sgDnArrowI, sgLfArrow, sgLfArrowD,
    sgLfArrowI, sgMnArrow, sgOldClose, sgOldDnArrow, sgOldLfArrow, sgOldReduce,
    sgOldRestore, sgOldRgArrow, sgOldUpArrow, sgOldZoom, sgReduce, sgReduced,
    sgRestore, sgRestored, sgRgArrow, sgRgArrowD, sgRgArrowI, sgSize, sgUpArrow,
    sgUpArrowD, sgUpArrowI, sgZoom, sgZommD);
{$EXTERNALSYM TSystemGlyph}

  TJvFormButton = class(TJvComponent)
  private
    FGlyph: TBitmap;
    FOnClick: TNotifyEvent;
    FForm: TCustomForm;
    FOldWndProc: Pointer;
    FRect: TRect;
    FDown: Boolean;
    FButtonDown: Boolean;
    FVisible: Boolean;
    FRight: Cardinal;
    FDownMenu: TPopupMenu;
    FPopup: TPopupMenu;
    FSystemGlyph: TSystemGlyph;
    FLastPosition: TRect;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetVisible(const Value: Boolean);
    procedure NewWndProc(var Mesg: TMessage);
    procedure PaintIt(Pushed: Boolean);
    procedure SetRight(const Value: Cardinal);
    procedure SetSystemGlyph(const Value: TSystemGlyph);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Visible: Boolean read FVisible write SetVisible default False;
    property RightMargin: Cardinal read FRight write SetRight default 100;
    property PopupMenu: TPopupMenu read FPopup write FPopup;
    property DropDownMenu: TPopupMenu read FDownMenu write FDownMenu;
    property SystemGlyph: TSystemGlyph read FSystemGlyph write SetSystemGlyph default sgNone;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

type
  TSystGlyphOBM = record
    SystemGlyph: TSystemGlyph;
    SystemValue: Integer;
  end;

const
  Jv_PUSHED = 666;

  Glyphs: array[0..33] of TSystGlyphOBM = (
    (SystemGlyph: sgCheckBoxes; SystemValue: OBM_CHECKBOXES),
    (SystemGlyph: sgClose; SystemValue: OBM_CLOSE),
    (SystemGlyph: sgCombo; SystemValue: OBM_COMBO),
    (SystemGlyph: sgDnArrow; SystemValue: OBM_DNARROW),
    (SystemGlyph: sgDnArrowD; SystemValue: OBM_DNARROWD),
    (SystemGlyph: sgDnArrowI; SystemValue: OBM_DNARROWI),
    (SystemGlyph: sgLfArrow; SystemValue: OBM_LFARROW),
    (SystemGlyph: sgLfArrowD; SystemValue: OBM_LFARROWD),
    (SystemGlyph: sgLfArrowI; SystemValue: OBM_LFARROWI),
    (SystemGlyph: sgMnArrow; SystemValue: OBM_MNARROW),
    (SystemGlyph: sgOldClose; SystemValue: OBM_OLD_CLOSE),
    (SystemGlyph: sgOldDnArrow; SystemValue: OBM_OLD_DNARROW),
    (SystemGlyph: sgOldLfArrow; SystemValue: OBM_OLD_LFARROW),
    (SystemGlyph: sgOldReduce; SystemValue: OBM_OLD_REDUCE),
    (SystemGlyph: sgOldZoom; SystemValue: OBM_OLD_ZOOM),
    (SystemGlyph: sgReduce; SystemValue: OBM_REDUCE),
    (SystemGlyph: sgReduced; SystemValue: OBM_REDUCED),
    (SystemGlyph: sgRestore; SystemValue: OBM_RESTORE),
    (SystemGlyph: sgRestored; SystemValue: OBM_RESTORED),
    (SystemGlyph: sgRgArrow; SystemValue: OBM_RGARROW),
    (SystemGlyph: sgRgArrowD; SystemValue: OBM_RGARROWD),
    (SystemGlyph: sgRgArrowI; SystemValue: OBM_RGARROWI),
    (SystemGlyph: sgSize; SystemValue: OBM_SIZE),
    (SystemGlyph: sgUpArrow; SystemValue: OBM_UPARROW),
    (SystemGlyph: sgUpArrowD; SystemValue: OBM_UPARROWD),
    (SystemGlyph: sgUpArrowI; SystemValue: OBM_UPARROWI),
    (SystemGlyph: sgZoom; SystemValue: OBM_ZOOM),
    (SystemGlyph: sgZommD; SystemValue: OBM_ZOOMD),
    (SystemGlyph: sgBtnCorners; SystemValue: OBM_BTNCORNERS),
    (SystemGlyph: sgBtSize; SystemValue: OBM_BTSIZE),
    (SystemGlyph: sgCheck; SystemValue: OBM_CHECK),
    (SystemGlyph: sgOldRestore; SystemValue: OBM_OLD_RESTORE),
    (SystemGlyph: sgOldRgArrow; SystemValue: OBM_OLD_RGARROW),
    (SystemGlyph: sgOldUpArrow; SystemValue: OBM_OLD_UPARROW));

  {****************************************************}

constructor TJvFormButton.Create(AOwner: TComponent);
var
  ptr: Pointer;
begin
  inherited;
  FForm := GetParentForm(TControl(AOwner));
  FGlyph := TBitmap.Create;
  FVisible := False;
  FRight := 100;
  FButtonDown := False;
  FDown := False;
  FSystemGlyph := sgNone;

  FOldWndProc := Pointer(GetWindowLong(FForm.Handle, GWL_WNDPROC));
  ptr := Classes.MakeObjectInstance(NewWndProc);
  SetWindowLong(FForm.Handle, GWL_WNDPROC, Longint(ptr));
end;

{****************************************************}

destructor TJvFormButton.Destroy;
begin
  if not (csDestroying in FForm.ComponentState) then
  begin
    SetVisible(False);
    SetWindowLong(FForm.Handle, GWL_WNDPROC, LongInt(FOldWndProc));
  end;
  FGlyph.Free;
  inherited;
end;

{****************************************************}

procedure TJvFormButton.NewWndProc(var Mesg: TMessage);
var
  p: TPoint;

  function HitButton(Point: TPoint): Boolean;
  begin
    Point.x := Point.x - FForm.Left;
    Point.y := Point.y - FForm.Top;
    Result := (Point.x >= FLastPosition.Left) and (Point.x <= FLastPosition.Right) and
      (Point.y >= FLastPosition.Top) and (Point.y <= FLastPosition.Bottom);
  end;

begin
  with Mesg do
  begin
    if FVisible then
    begin
      case Msg of
        WM_NCPAINT, WM_NCACTIVATE:
          begin
            Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
            PaintIt(False);
          end;
        WM_NCHITTEST:
          begin
            Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
            if Result = HTCAPTION then
            begin
              p.x := LoWord(LParam);
              ScreenToClient(FForm.Handle, p);
              if (p.x >= FRect.Left) and (p.x <= FRect.Right) then
              begin
                if not FDown then
                  PaintIt(FButtonDown);
                Result := Jv_PUSHED;
              end
              else if not (FDOwn) then
                PaintIt(False);
            end
            else if not FDown then
              PaintIt(False);
          end;
        WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK:
          begin
            Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
            if not HitButton(Point((TWMNCLButtonDown(Mesg)).XCursor,
              (TWMNCLButtonDown(Mesg)).YCursor)) then
              Exit;
            FDown := True;
            if WParam = Jv_PUSHED then
            begin
              PaintIt(True);
              if not FButtonDown then
              begin
                if FDownMenu <> nil then
                begin
                  p.x := FForm.Left + FRect.Left;
                  p.y := FForm.Top + FRect.Bottom;
                  FDownMenu.Popup(p.x + 5, p.y + 6);
                end
                else
                begin
                  FButtonDown := True;
                  SetCapture(FForm.Handle);
                end;
              end;
            end
            else
            begin
              PaintIt(False);
              if FButtonDown then
              begin
                FButtonDown := False;
                ReleaseCapture;
              end;
            end;
            FDown := False;
          end;
        WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK:
          begin
            if WParam = Jv_PUSHED then
            begin
              if FPopup <> nil then
              begin
                p.x := FForm.Left + FRect.Left;
                p.y := FForm.Top + FRect.Bottom;
                FPopup.Popup(p.x + 5, p.y + 6);
              end;
            end
            else
              Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
          end;
        WM_NCLBUTTONUP, WM_LBUTTONUP:
          begin
            Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
            if FButtonDown then
            begin
              FButtonDown := False;
              ReleaseCapture;
              if Assigned(FOnClick) then
                FOnClick(self);
            end;
            PaintIt(False);
          end;
      else
        Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
      end;
    end
    else
      Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
  end;
end;

{****************************************************}

procedure TJvFormButton.PaintIt(Pushed: Boolean);
var
  r: TRect;
  x, y, x2, y2: Integer;
  FDec: Byte;
begin
  if not FVisible then
    Exit;
  with TCanvas.Create do
  try
    Handle := GetWindowDC(FForm.Handle);
    GetWindowRect(FForm.Handle, r);
    R.Right := R.Right - R.Left;

    case FForm.Borderstyle of
      bsSingle:
        y2 := GetSystemMetrics(SM_CYFRAME) + 1;
      bsDialog:
        y2 := GetSystemMetrics(SM_CYBORDER) + 4;
      bsSizeToolWin:
        y2 := GetSystemMetrics(SM_CYSIZEFRAME) + 2;
      bsToolWindow:
        y2 := GetSystemMetrics(SM_CYBORDER) + 4;
    else
      y2 := GetSystemMetrics(SM_CYFRAME) + 2;
    end;
    x2 := R.Right - Integer(RightMargin) - y2;

    if FForm.BorderStyle in [bsSizeToolWin, bsToolWindow] then
    begin
      x := GetSystemMetrics(SM_CXSMSIZE) - 5;
      y := GetSystemMetrics(SM_CYSMCAPTION) - 8;
    end
    else
    begin
      x := GetSystemMetrics(SM_CXSIZE) - 5;
      y := GetSystemMetrics(SM_CYCAPTION) - 8;
    end;

    with FRect do
    begin
      Left := x2 - y2;
      Top := y2;
      Right := Left + x + 3;
      Bottom := y + 2;
    end;

    if FButtonDown then
      Pen.Color := clBlack
    else
      Pen.Color := clBtnHighLight;
    MoveTo(x2, y2 + y + 1);
    LineTo(x2, y2);
    LineTo(x2 + x + 3, y2);
    if FButtonDown then
      Pen.Color := clBtnHighlight
    else
      Pen.Color := clBlack;
    MoveTo(x2, y2 + y + 2);
    LineTo(x2 + x + 2, y2 + y + 2);
    LineTo(x2 + x + 2, y2 - 1);
    Pen.Color := clGray;
    if FButtonDown then
    begin
      MoveTo(x2 + x, y2 + 1);
      LineTo(x2 + 1, y2 + 1);
      LineTo(x2 + 1, y2 + y + 1);
      Pen.Color := clSilver;
      MoveTo(x2 + x + 1, y2 + 1);
      LineTo(x2 + x + 1, y2 + y + 1);
      LineTo(x2, y2 + y + 1);
      FDec := 2;
    end
    else
    begin
      MoveTo(x2 + x + 1, y2 + 1);
      LineTo(x2 + x + 1, y2 + y + 1);
      LineTo(x2, y2 + y + 1);
      FDec := 1;
    end;

    FLastPosition.Left := X2 + FDec;
    FLastPosition.Right := FLastPosition.Left + x;
    FLastPosition.Top := y2 + FDec;
    FLastPosition.Bottom := FLastPosition.Top + y;

    StretchBlt(Handle, X2 + FDec, y2 + FDec, x, y, FGlyph.Canvas.Handle, 0, 0, FGlyph.Width,
      FGlyph.Height, SRCCOPY);
    ReleaseDC(FForm.Handle, Handle);
  finally
    Free;
  end;
end;

{****************************************************}

procedure TJvFormButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
  SendMessage(FForm.Handle, WM_NCACTIVATE, 0, 0);
end;

{****************************************************}

procedure TJvFormButton.SetRight(const Value: Cardinal);
begin
  FRight := Value;
  SendMessage(FForm.Handle, WM_NCACTIVATE, 0, 0);
end;

{****************************************************}

procedure TJvFormButton.SetSystemGlyph(const Value: TSystemGlyph);
var
  i: Integer;
begin
  FSystemGlyph := Value;
  for i := 0 to 30 do
    if Glyphs[i].SystemGlyph = Value then
    begin
      Glyph.Handle := LoadBitmap(0, PChar(Glyphs[i].SystemValue));
      Exit;
    end;
  SendMessage(FForm.Handle, WM_NCACTIVATE, 0, 0);
end;

{****************************************************}

procedure TJvFormButton.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  SendMessage(FForm.Handle, WM_NCACTIVATE, 0, 0);
end;

end.

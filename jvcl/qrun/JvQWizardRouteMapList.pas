{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWizardRouteMapList.PAS, released on 2004-02-14.

The Initial Developer of the Original Code is Peter Thornqvist.
Portions created by Peter Thornqvist are Copyright (C) 2004 Peter Thornqvist

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Purpose:
  Route map that displays pages as a list

History:

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQWizardRouteMapList;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  QWindows, QMessages, QGraphics, QControls, QForms, 
  QImgList,  
  JvQTypes, JvQConsts, JvQJVCLUtils, 
  JvQWizard;



type
  TJvWizardDrawRouteMapListItem = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; MousePos: TPoint; PageIndex: Integer; var DefaultDraw: Boolean) of object;
  TRouteMapListItemText = (itNone, itCaption, itTitle, itSubtitle);

  TJvWizardRouteMapList = class(TJvWizardRouteMapControl)
  private
    FItemHeight: Integer;
    FVertOffset: Integer;
    FHorzOffset: Integer;
    FClickable: Boolean;
    FIncludeDisabled: Boolean;
    FHotTrackFont: TFont;
    FActiveFont: TFont;
    FHotTrackCursor, FOldCursor: TCursor;
    FOnDrawItem: TJvWizardDrawRouteMapListItem;
    FAlignment: TAlignment;
    FTextOffset: Integer;
    FShowImages: Boolean;
    FItemColor: TColor;
    FRounded: Boolean;
    FItemText: TRouteMapListItemText;
    FHotTrack: Boolean;
    FCurvature: Integer;
    FHotTrackBorder: Integer;
    FBorderColor: TColor;
    FTextOnly: Boolean; 
    FHotTrackFontOptions: TJvTrackFontOptions;
    FActiveFontOptions: TJvTrackFontOptions; 
    procedure SetItemHeight(const Value: Integer);
    procedure SetHorzOffset(const Value: Integer);
    procedure SetVertOffset(const Value: Integer);
    procedure SetIncludeDisabled(const Value: Boolean);
    procedure SetActiveFont(const Value: TFont);
    procedure SetHotTrackFont(const Value: TFont);
    procedure DoFontChange(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetTextOffset(const Value: Integer);
    procedure SetShowImages(const Value: Boolean);
    procedure SetItemColor(const Value: TColor);
    procedure SetRounded(const Value: Boolean);
    procedure SetItemText(const Value: TRouteMapListItemText);
    procedure SetCurvature(const Value: Integer);
    procedure SetTextOnly(const Value: Boolean);
    procedure SetBorderColor(Value: TColor); 
    procedure SetActiveFontOptions(const Value: TJvTrackFontOptions);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions); 
  protected
    procedure DrawPageItem(ACanvas: TCanvas; ARect: TRect; MousePos: TPoint; PageIndex: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function PageAtPos(Pt: TPoint): TJvWizardCustomPage; override;
    procedure Paint; override;
    procedure Loaded; override; 
    procedure CursorChanged;  override; 
    procedure FontChanged;  override; 
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ActiveFont: TFont read FActiveFont write SetActiveFont; 
    property ActiveFontOptions: TJvTrackFontOptions read FActiveFontOptions write SetActiveFontOptions default
      DefaultTrackFontOptions; 
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Clickable: Boolean read FClickable write FClickable default True;
    property Color default $00C08000;
    property Curvature: Integer read FCurvature write SetCurvature default 9;
    property Font;
    property HorzOffset: Integer read FHorzOffset write SetHorzOffset default 8;
    property HotTrackBorder: Integer read FHotTrackBorder write FHotTrackBorder default 2;
    property HotTrackCursor: TCursor read FHotTrackCursor write FHotTrackCursor default crHandPoint;
    property HotTrack: Boolean read FHotTrack write FHotTrack default True;

    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont; 
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default
      DefaultTrackFontOptions; 
    property Image;
    property TextOnly: Boolean read FTextOnly write SetTextOnly default False;
    property IncludeDisabled: Boolean read FIncludeDisabled write SetIncludeDisabled default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNavy;
    property ItemColor: TColor read FItemColor write SetItemColor default clCream;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 25;
    property ItemText: TRouteMapListItemText read FItemText write SetItemText default itCaption;
    property Rounded: Boolean read FRounded write SetRounded default False;
    property ShowImages: Boolean read FShowImages write SetShowImages default False;
    property TextOffset: Integer read FTextOffset write SetTextOffset default 8;
    property VertOffset: Integer read FVertOffset write SetVertOffset default 8;
    property OnDrawItem: TJvWizardDrawRouteMapListItem read FOnDrawItem write FOnDrawItem;
  end;



implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

constructor TJvWizardRouteMapList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveFont := TFont.Create;
  FActiveFont.Style := [fsBold];
  FActiveFont.OnChange := DoFontChange;
  FHotTrackFont := TFont.Create;
  FHotTrackFont.Color := clNavy;
  FHotTrackFont.Style := [fsUnderline];
  FHotTrackFont.OnChange := DoFontChange; 
  FActiveFontOptions := DefaultTrackFontOptions;
  FHotTrackFontOptions := DefaultTrackFontOptions; 
  Color := $00C08000;
  FHotTrackCursor := crHandPoint;
  FVertOffset := 8;
  FHorzOffset := 8;
  FItemHeight := 25;
  FClickable := True;
  FAlignment := taCenter;
  FTextOffset := 8;
  FBorderColor := clNavy;
  FItemColor := clCream;
  FItemText := itCaption;
  FHotTrack := True;
  FCurvature := 9;
  FHotTrackBorder := 2;
  FTextOnly := False;
end;

destructor TJvWizardRouteMapList.Destroy;
begin
  FHotTrackFont.Free;
  FActiveFont.Free;
  inherited Destroy;
end;

procedure TJvWizardRouteMapList.Loaded;
begin
  inherited Loaded;
  FOldCursor := Cursor;
end;

procedure TJvWizardRouteMapList.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  P: TJvWizardCustomPage;
begin
  inherited MouseMove(Shift, X, Y);
  if Clickable and HotTrack then
  begin
    P := PageAtPos(Point(X, Y));
    if (P <> nil) and P.Enabled then
    begin
      if Cursor <> FHotTrackCursor then
        FOldCursor := Cursor;
      Cursor := FHotTrackCursor;
      Refresh;
    end
    else
    if Cursor <> FOldCursor then
    begin
      Cursor := FOldCursor;
      Refresh;
    end;
  end;
end;

function TJvWizardRouteMapList.PageAtPos(Pt: TPoint): TJvWizardCustomPage;
var
  R: TRect;
  I: Integer;
begin
  Result := nil;
  if not Clickable then
    Exit;
  R := ClientRect;
  InflateRect(R, -HorzOffset, -VertOffset);
  R.Bottom := R.Top + ItemHeight;
  for I := 0 to PageCount - 1 do
  begin
    if Pages[I].Enabled or IncludeDisabled then
    begin
      if PtInRect(R, Pt) then
      begin
        Result := Pages[I];
        Exit;
      end;
      OffsetRect(R, 0, ItemHeight);
    end;
  end;
end;

procedure TJvWizardRouteMapList.Paint;
var
  I: Integer;
  R: TRect;
  P: TPoint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  if BorderColor = clNone then
    Canvas.Pen.Color := Color
  else
    Canvas.Pen.Color := BorderColor;
  GetCursorPos(P);
  P := ScreenToClient(P);
  R := ClientRect;
  if not HasPicture then
    Canvas.Rectangle(R)
  else
    Image.PaintTo(Canvas, R);
  if ItemHeight <= 0 then
    Exit;
  InflateRect(R, -HorzOffset, -VertOffset);
  R.Bottom := R.Top + ItemHeight;
  for I := 0 to PageCount - 1 do
    if Pages[I].Enabled or IncludeDisabled then
    begin
      DrawPageItem(Canvas, R, P, I);
      OffsetRect(R, 0, ItemHeight);
      if R.Bottom >= ClientHeight - 2 then
        Break;
    end;
end;

procedure TJvWizardRouteMapList.DrawPageItem(ACanvas: TCanvas; ARect: TRect; MousePos: TPoint; PageIndex: Integer);
const
  cAlignment: array [TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);
  cWordWrap: array [Boolean] of Cardinal = (DT_SINGLELINE, DT_WORDBREAK);
var
  DefaultDraw: Boolean;
  ATop, ALeft: Integer;
  AOrigRect: TRect;
  BkColor: TColor;
  S: string;
begin
  ACanvas.Lock;
  try
    AOrigRect := ARect;
    ACanvas.Font := Font;
    if Assigned(Wizard) and (Pages[PageIndex] = Wizard.ActivePage) then
      ACanvas.Font := ActiveFont
    else
    if PtInRect(ARect, MousePos) and Pages[PageIndex].Enabled and HotTrack and Clickable then
      ACanvas.Font := HotTrackFont
    else
    if not Pages[PageIndex].Enabled then
      ACanvas.Font.Color := clGrayText;

    ACanvas.Brush.Color := ItemColor;
    ACanvas.Pen.Color := Color;
    DefaultDraw := True;
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, ACanvas, ARect, MousePos, PageIndex, DefaultDraw);
    if DefaultDraw then
    begin
      case ItemText of
        itCaption:
          S := Pages[PageIndex].Caption;
        itTitle:
          S := Pages[PageIndex].Title.Text;
        itSubtitle:
          S := Pages[PageIndex].Subtitle.Text;
      end;

      if not TextOnly then
      begin
        if ItemColor = clNone then
          ACanvas.Brush.Style := bsClear;
        if Rounded then
          ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, Curvature, Curvature)
        else
          ACanvas.Rectangle(ARect);
        if ShowImages and Assigned(Wizard) and Assigned(Wizard.HeaderImages) then
        begin
          ATop := ((ARect.Bottom - ARect.Top) - Wizard.HeaderImages.Height) div 2;
          BkColor := ACanvas.Brush.Color;
          case Alignment of
            taLeftJustify:
              begin
                Wizard.HeaderImages.Draw(ACanvas, ARect.Left + 4, ARect.Top + ATop, Pages[PageIndex].Header.ImageIndex,  itImage,  Pages[PageIndex].Enabled);
                Inc(ARect.Left, Wizard.HeaderImages.Width + 4);
              end;
            taRightJustify:
              begin
                Wizard.HeaderImages.Draw(ACanvas, ARect.Right - Wizard.HeaderImages.Width - 4, ARect.Top + ATop,
                  Pages[PageIndex].Header.ImageIndex,  itImage,  Pages[PageIndex].Enabled);
                Dec(ARect.Right, Wizard.HeaderImages.Width + 4);
              end;
            taCenter:
              begin
                ALeft := ((ARect.Right - ARect.Left) - Wizard.HeaderImages.Width) div 2;
                Inc(ARect.Top, 4);
                Wizard.HeaderImages.Draw(ACanvas, ARect.Left + ALeft, ARect.Top + 8,
                  Pages[PageIndex].Header.ImageIndex,  itImage,  Pages[PageIndex].Enabled);
                Inc(ARect.Top, Wizard.HeaderImages.Height);
  //              if ItemText = itSubtitle then
  //                Inc(ARect.Top, 16);
              end;
          end;
          if not Pages[PageIndex].Enabled then
          begin
            // (p3) TImageList changes the canvas colors when drawing disabled images, so we reset them explicitly
            SetBkColor(ACanvas.Handle, BkColor);
            SetTextColor(ACanvas.Handle, ColorToRGB(clGrayText));
          end;
        end;
      end
      else
        ACanvas.Brush.Style := bsClear;

      case Alignment of
        taLeftJustify:
          Inc(ARect.Left, TextOffset);
        taRightJustify:
          Dec(ARect.Right, TextOffset);
        taCenter:
          InflateRect(ARect, -TextOffset div 2, -TextOffset div 2);
      end;
      if ItemText = itSubtitle then
      begin
        Inc(ARect.Top, TextOffset);
        InflateRect(ARect, -TextOffset, 0);
      end;
      if (ItemText <> itNone) and ((ARect.Bottom - ARect.Top) > abs(ACanvas.Font.Height)) then
        DrawText(ACanvas.Handle, PChar(S), Length(S), ARect,
          cAlignment[Alignment] or cWordWrap[ItemText = itSubtitle] or DT_VCENTER or DT_EDITCONTROL or  DT_END_ELLIPSIS);
      if not TextOnly and HotTrack and (HotTrackBorder > 0) and PtInRect(AOrigRect, MousePos) then
      begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Color := HotTrackFont.Color;
        ACanvas.Pen.Width := HotTrackBorder;
        if Rounded then
          with AOrigRect do
            ACanvas.RoundRect(Left, Top, Right, Bottom, Curvature, Curvature)
        else
          ACanvas.Rectangle(AOrigRect);
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Pen.Width := 1;
      end;
    end;
  finally
    ACanvas.Unlock;
  end;
end;

procedure TJvWizardRouteMapList.SetHorzOffset(const Value: Integer);
begin
  if FHorzOffset <> Value then
  begin
    FHorzOffset := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetItemHeight(const Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetVertOffset(const Value: Integer);
begin
  if FVertOffset <> Value then
  begin
    FVertOffset := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetIncludeDisabled(const Value: Boolean);
begin
  if FIncludeDisabled <> Value then
  begin
    FIncludeDisabled := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

procedure TJvWizardRouteMapList.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvWizardRouteMapList.DoFontChange(Sender: TObject);
begin
  Invalidate;
end;



procedure TJvWizardRouteMapList.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetTextOffset(const Value: Integer);
begin
  if FTextOffset <> Value then
  begin
    FTextOffset := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetShowImages(const Value: Boolean);
begin
  if FShowImages <> Value then
  begin
    FShowImages := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetItemColor(const Value: TColor);
begin
  if FItemColor <> Value then
  begin
    FItemColor := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetRounded(const Value: Boolean);
begin
  if FRounded <> Value then
  begin
    FRounded := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetItemText(const Value: TRouteMapListItemText);
begin
  if FItemText <> Value then
  begin
    FItemText := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetCurvature(const Value: Integer);
begin
  if FCurvature <> Value then
  begin
    FCurvature := Value;
    Invalidate;
  end;
end;



procedure TJvWizardRouteMapList.SetActiveFontOptions(const Value: TJvTrackFontOptions);
begin
  if FActiveFontOptions <> Value then
  begin
    FActiveFontOptions := Value;
    UpdateTrackFont(ActiveFont, Font, FActiveFontOptions);
  end;
end;

procedure TJvWizardRouteMapList.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;



procedure TJvWizardRouteMapList.SetBorderColor(Value: TColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.SetTextOnly(const Value: Boolean);
begin
  if Value <> FTextOnly then
  begin
    FTextOnly := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapList.CursorChanged;
begin 
  inherited CursorChanged; 
  if (Cursor <> FHotTrackCursor) and (Cursor <> FOldCursor) then
    FOldCursor := Cursor;
end;

procedure TJvWizardRouteMapList.FontChanged;
begin 
  inherited FontChanged;  
  UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  UpdateTrackFont(ActiveFont, Font, FActiveFontOptions); 
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.


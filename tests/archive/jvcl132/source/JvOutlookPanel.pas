{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOutlookPanel.PAS, released on 2001-02-28.

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

unit JvOutlookPanel;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, Buttons, JVCLVer;

type
  TTitlePosition = alTop..alRight;
{$EXTERNALSYM TTitlePosition}

  TJvOutlookTitle = class(TPersistent)
  private
    FPosition: TTitlePosition;
    FOnChange: TNotifyEvent;
    FCaption: TCaption;
    FFont: TFont;
    FAlignment: TAlignment;
    FGlyph: TBitmap;
    FHeight: Integer;
    FColor: TColor;
    procedure SetPosition(const Value: TTitlePosition);
    procedure SetCaption(const Value: TCaption);
    procedure SetFont(const Value: TFont);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetHeight(const Value: Integer);
    procedure SetColor(const Value: TColor);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure FontChanged(Sender: TObject);
  public
    constructor Create;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Caption: TCaption read FCaption write SetCaption;
    property Font: TFont read FFont write SetFont;
    property Position: TTitlePosition read FPosition write SetPosition default alTop;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Height: Integer read FHeight write SetHeight default 20;
  end;

  TJvOutlookPanel = class(TWinControl)
  private
    FTitle: TJvOutlookTitle;
    FOnHide: TNotifyEvent;
    FPanel: TPanel;
    FButton: TSpeedButton;
    FAboutJVCL: TJVCLAboutInfo;
  protected
    procedure TitleChanged(Sender: TObject);
    procedure ButtonClick(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property BorderWidth;
    property Anchors;
    property Visible;
    property Constraints;
    property Width;
    property Height;
    property ClientWidth;
    property ClientHeight;
    property Color;
    property Hint;
    property ShowHint;
    property Title: TJvOutlookTitle read FTitle write FTitle;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
  end;

implementation

{$R RES_Outlook.res}

///////////////////////////////////////////////////////////
// TJvOutlookPanel
///////////////////////////////////////////////////////////

procedure TJvOutlookPanel.ButtonClick(Sender: TObject);
begin
  Visible := False;
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

{***************************************************}

constructor TJvOutlookPanel.Create(AOwner: TComponent);
begin
  inherited;
  FTitle := TJvOutlookTitle.Create;
  FTitle.OnChange := TitleChanged;
  Width := 275;
  Height := 200;
  FTitle.FCaption := 'Outlook Panel';

  FPanel := TPanel.Create(Self);
  with FPanel do
  begin
    Align := alTop;
    Alignment := taCenter;
    AutoSize := False;
    Caption := FTitle.Caption;
    BevelOuter := bvRaised;
    Parent := Self;
    Height := 20;
    Font.Style := [fsBold];
    ControlStyle := ControlStyle - [csAcceptsControls];
  end;

  FButton := TSpeedButton.Create(Self);
  with FButton do
  begin
    Parent := FPanel;
    Align := alRight;
    Flat := True;
    Caption := '';
    Width := 20;
    Glyph.LoadFromResourceName(HInstance, 'CLOSE');
    OnClick := ButtonClick;
  end;
  ControlStyle := ControlStyle - [csFixedWidth, csFixedHeight];
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := clGrayText;
end;

{***************************************************}

destructor TJvOutlookPanel.Destroy;
begin
  FTitle.Free;
  FButton.Free;
  FPanel.Free;
  inherited;
end;

{***************************************************}

procedure TJvOutlookPanel.TitleChanged(Sender: TObject);
begin
  FPanel.Align := Title.Position;
  case Title.Position of
    alTop:
      begin
        FButton.Align := alRight;
        FPanel.Caption := Title.Caption;
      end;
    alLeft:
      begin
        FButton.Align := alTop;
        FPanel.Caption := '';
      end;
    alRight:
      begin
        FButton.Align := alBottom;
        FPanel.Caption := '';
      end;
    alBottom:
      begin
        FButton.Align := alRight;
        FPanel.Caption := Title.Caption;
      end;
  end;
  FPanel.Font.Assign(Title.Font);
  FPanel.Alignment := Title.Alignment;
  FPanel.Color := Title.Color;
  if Title.Glyph.Width > 0 then
    FButton.Glyph.Assign(Title.Glyph)
  else
    FButton.Glyph.LoadFromResourceName(hInstance, 'CLOSE');
  FPanel.Height := Title.Height;
end;

///////////////////////////////////////////////////////////
// TJvOutlookTitle
///////////////////////////////////////////////////////////

constructor TJvOutlookTitle.Create;
begin
  FPosition := alTop;
  FAlignment := taCenter;
  FGlyph := TBitmap.Create;
  FFont := TFont.Create;
  FFont.Style := [fsBold];
  FFont.OnChange := FontChanged;
  FHeight := 20;
  FColor := clBtnFace;
end;

{***************************************************}

procedure TJvOutlookTitle.FontChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***************************************************}

procedure TJvOutlookTitle.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***************************************************}

procedure TJvOutlookTitle.SetCaption(const Value: TCaption);
begin
  FCaption := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***************************************************}

procedure TJvOutlookTitle.SetColor(const Value: TColor);
begin
  FColor := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***************************************************}

procedure TJvOutlookTitle.SetFont(const Value: TFont);
begin
  FFont := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***************************************************}

procedure TJvOutlookTitle.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***************************************************}

procedure TJvOutlookTitle.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***************************************************}

procedure TJvOutlookTitle.SetPosition(const Value: TTitlePosition);
begin
  FPosition := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.

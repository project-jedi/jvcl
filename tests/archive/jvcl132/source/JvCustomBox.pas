{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCustomBox.PAS, released on 2001-02-28.

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

unit JvCustomBox;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Buttons, ActnList,
  JvTypes;

type
  // (rom) name and values changed
  TJvPosition = alLeft..alRight;

  TJvCustomEditOptions = class(TPersistent)
  private
    FEnabled: Boolean;
    FCtl3d: Boolean;
    FReadOnly: Boolean;
    FShowHint: Boolean;
    FMaxLength: Integer;
    FHint: string;
    FColor: TColor;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FText: string;
    FCursor: TCursor;
    procedure SetColor(const Value: TColor);
    procedure SetCtl3d(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetHint(const Value: string);
    procedure SetMaxLength(const Value: Integer);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetShowHint(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetCursor(const Value: TCursor);
  protected
    property OnChanged: TNotifyEvent read FOnChange write FOnChange;
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Text: string read FText write SetText;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Font: TFont read FFont write SetFont;
    property Hint: string read FHint write SetHint;
    property Ctl3d: Boolean read FCtl3d write SetCtl3d default True;
    property ShowHint: Boolean read FShowHint write SetShowHint default False;
    property Color: TColor read FColor write SetColor default clWindow;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
  end;

  TJvCustomButtonOptions = class(TPersistent)
  private
    FFlat: Boolean;
    FEnabled: Boolean;
    FTransparent: Boolean;
    FShowHint: Boolean;
    FHint: string;
    FCaption: string;
    FGlyph: TBitmap;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FPosition: TJvPosition;
    FWidth: Integer;
    FCursor: TCursor;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetHint(const Value: string);
    procedure SetPosition(const Value: TJvPosition);
    procedure SetShowHint(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetCursor(const Value: TCursor);
  protected
    property OnChanged: TNotifyEvent read FOnChange write FOnChange;
    procedure Changed;
    procedure GlyphChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Flat: Boolean read FFlat write SetFlat default False;
    property Hint: string read FHint write SetHint;
    property ShowHint: Boolean read FShowHint write SetShowHint default False;
    property Font: TFont read FFont write SetFont;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Caption: string read FCaption write SetCaption;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Position: TJvPosition read FPosition write SetPosition default alRight;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Width: Integer read FWidth write SetWidth default 20;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
  end;

  TJvCustomBox = class(TWinControl)
  private
    FEditOptions: TJvCustomEditOptions;
    FButtonOptions: TJvCustomButtonOptions;
    FEdit: TEdit;
    FButton: TSpeedButton;
    procedure EditChanged(Sender: TObject);
    procedure ButtonChanged(Sender: TObject);
  protected
    procedure BtnClick(Sender: TObject); virtual;
    procedure EditChange(Sender: TObject); virtual;
    function GetEdit: TEdit;
    function GetButton: TSpeedButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; virtual;
  published
    property Align;
    property ShowHint;
    property BorderWidth;
    property Color;
    property Visible;
    property ParentShowHint;
    property Enabled;
    property OnKeyDown;
    property OnKeyPress;
    property OnEnter;
    property OnExit;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnUnDock;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;

    property Edit: TJvCustomEditOptions read FEditOptions write FEditOptions;
    property Button: TJvCustomButtonOptions read FButtonOptions write FButtonOptions;
  end;

implementation

{$R RES_CustomBox.res}

///////////////////////////////////////////////////////////
// TJvCustomBox
///////////////////////////////////////////////////////////

procedure TJvCustomBox.BtnClick(Sender: TObject);
begin
  //Do nothing
end;

{*****************************************************}

procedure TJvCustomBox.ButtonChanged(Sender: TObject);
begin
  FButton.Font.Assign(FButtonOptions.Font);
  FButton.Glyph.Assign(FButtonOptions.Glyph);
  FButton.Flat := FButtonOptions.Flat;
  FButton.Hint := FButtonOptions.Hint;
  FButton.ShowHint := FButtonOptions.ShowHint;
  FButton.Enabled := FButtonOptions.Enabled;
  FButton.Caption := FButtonOptions.Caption;
  FButton.Transparent := FButtonOptions.Transparent;
  FButton.Width := FButtonOptions.Width;
  FButton.Cursor := FButtonOptions.Cursor;
  FButton.Align := FButtonOptions.FPosition;
end;

{*****************************************************}

constructor TJvCustomBox.Create(AOwner: TComponent);
begin
  inherited;
  FEditOptions := TJvCustomEditOptions.Create;
  FButtonOptions := TJvCustomButtonOptions.Create;

  FButton := TSpeedButton.Create(Self);
  with FButton do
  begin
    Glyph := TBitmap.Create;
    Parent := Self;
    Align := alRight;
    Width := 20;
    OnClick := BtnClick;
  end;

  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
    Parent := Self;
    Align := alClient;
    OnChange := EditChange;
  end;

  Width := 150;
  Height := 20;

  FButtonOptions.OnChanged := ButtonChanged;
  FEditOptions.OnChanged := EditChanged;
end;

{*****************************************************}

destructor TJvCustomBox.Destroy;
begin
  FEditOptions.Free;
  FButtonOptions.Free;
  FButton.Free;
  FEdit.Free;
  inherited;
end;

{*****************************************************}

procedure TJvCustomBox.EditChange(Sender: TObject);
begin
  FEditOptions.Text := FEdit.Text;
end;

{*****************************************************}

procedure TJvCustomBox.EditChanged(Sender: TObject);
begin
  FEdit.Text := FEditOptions.Text;
  FEdit.ReadOnly := FEditOptions.ReadOnly;
  FEdit.Hint := FEditOptions.Hint;
  FEdit.Ctl3d := FEditOptions.Ctl3d;
  FEdit.ShowHint := FEditOptions.ShowHint;
  FEdit.MaxLength := FEditOptions.MaxLength;
  FEdit.Enabled := FEditOptions.Enabled;
  FEdit.Color := FEditOptions.Color;
  FEdit.Cursor := FEditOptions.Cursor;
  FEdit.Font.Assign(FEditOptions.Font);
  FEdit.OnChange := EditChange;
end;

{*****************************************************}

procedure TJvCustomBox.Execute;
begin
  BtnClick(Self);
end;

{*****************************************************}

function TJvCustomBox.GetButton: TSpeedButton;
begin
  Result := FButton;
end;

{*****************************************************}

function TJvCustomBox.GetEdit: TEdit;
begin
  Result := FEdit;
end;

///////////////////////////////////////////////////////////
// TJvCustomEditOptions
///////////////////////////////////////////////////////////

procedure TJvCustomEditOptions.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{*****************************************************}

constructor TJvCustomEditOptions.Create;
begin
  FReadOnly := False;
  FFont := TFont.Create;
  FCtl3d := True;
  FShowHint := False;
  FColor := clWindow;
  FMaxLength := 0;
  FEnabled := True;
  FCursor := crDefault;
end;

{*****************************************************}

destructor TJvCustomEditOptions.Destroy;
begin
  FFont.Free;
  inherited;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetColor(const Value: TColor);
begin
  FColor := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetCtl3d(const Value: Boolean);
begin
  FCtl3d := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetHint(const Value: string);
begin
  FHint := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetMaxLength(const Value: Integer);
begin
  FMaxLength := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetShowHint(const Value: Boolean);
begin
  FShowHint := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomEditOptions.SetText(const Value: string);
begin
  FText := Value;
  Changed;
end;

///////////////////////////////////////////////////////////
// TJvCustomButtonOptions
///////////////////////////////////////////////////////////

procedure TJvCustomButtonOptions.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{*****************************************************}

constructor TJvCustomButtonOptions.Create;
begin
  FFlat := False;
  FShowHint := False;
  FEnabled := True;
  FPosition := alRight;
  FTransparent := True;
  FFont := TFont.Create;
  FGlyph := TBitmap.Create;
  FWidth := 20;
  FCursor := crDefault;
  FGlyph.OnChange := GlyphChanged;
end;

{*****************************************************}

destructor TJvCustomButtonOptions.Destroy;
begin
  FFont.Free;
  FGlyph.Free;
  inherited;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.GlyphChanged(Sender: TObject);
begin
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetCaption(const Value: string);
begin
  FCaption := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetHint(const Value: string);
begin
  FHint := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetPosition(const Value: TJvPosition);
begin
  FPosition := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetShowHint(const Value: Boolean);
begin
  FShowHint := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Changed;
end;

{*****************************************************}

procedure TJvCustomButtonOptions.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  Changed;
end;

end.

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCombobox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].
                André Snepvangers [asn@xs4all.nl]

Last Modified: 2003-09-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQCombobox;



interface

uses
  SysUtils, Classes, QGraphics, QControls, QForms, QStdCtrls,
  JvQExStdCtrls, JvQMaxPixel, JvQItemsSearchs;

type
  TJvCustomCombobox = class(TCustomComboBox)
  private
    FKey: Word;
    FSearching: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnRestored: TNotifyEvent;
    FOver: Boolean;
    FMaxPixel: TJvMaxPixel;
    FItemSearchs: TJvItemsSearchs;
    procedure MaxPixelChanged(Sender: TObject);
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseEnter(AControl : TControl); override ;
    procedure MouseLeave(AControl : TControl); override;
    procedure ParentColorChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
  protected
    property AutoComplete default True;
    property SelStart;
    property SelText;
    property SelLength;
    property ItemIndex;
    property DragMode;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property OnRestored: TNotifyEvent read FOnRestored write FOnRestored;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

  TJvComboBox = class(TJvCustomComboBox)
  published
    property SelStart;
    property SelText;
    property SelLength;
//    property HintColor;
    property InsertMode;
    property MaxPixel;
    property AutoComplete default True;
//    property AutoSave;
    property Style; {Must be published before Items}
    property Anchors;
    property CharCase;
    property Color;
    property Constraints;
    property DragMode;
    property DropDownCount;
    property Duplicates;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnHighLighted;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
    property OnRestored;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;


implementation

{**************************************************}

constructor TJvCustomCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSearching := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FOver := False;
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FItemSearchs := TJvItemsSearchs.Create;
end;

{**************************************************}

destructor TJvCustomCombobox.Destroy;
begin
  FMaxPixel.Free;
  FItemSearchs.Free;
  inherited;
end;

{**************************************************}

procedure TJvCustomCombobox.Loaded;
begin
  inherited;
end;

{**************************************************}

procedure TJvCustomCombobox.Change;
begin
  inherited;
end;

{**************************************************}

function TJvCustomCombobox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvCustomCombobox.SearchPrefix(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCustomCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FKey := Key;
end;

{**************************************************}


procedure TJvCustomCombobox.ParentColorChanged;
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvCustomCombobox.MouseEnter(AControl : TControl);
begin
  (*
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    FOver := True;
  end;
  *)
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvCustomCombobox.MouseLeave(AControl : TControl);
begin
  (*
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  *)
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);

end;

{**************************************************}

function TJvCustomCombobox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvCustomCombobox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCustomCombobox.MaxPixelChanged(Sender: TObject);
var
  st: string;
begin
  if Style <> csDropDownList then
  begin
    st := Text;
    FMaxPixel.Test(st, Font);
    if Text <> st then
      Text := st;
    SelStart := Length(Text);
  end;
end;

end.

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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvCombobox;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvMaxPixel, JvPropAutoSave, JvItemsSearchs, JVCLVer;

type
  TJvCombobox = class(TComboBox)
  private
    FKey: Word;
    FAutoComplete: Boolean;
    FSearching: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnRestored: TNotifyEvent;
    FOver: Boolean;
    FMaxPixel: TJvMaxPixel;
    FAutoSave: TJvAutoSave;
    FItemSearchs: TJvItemsSearchs;
    FAboutJVCL: TJVCLAboutInfo;
    procedure MaxPixelChanged(Sender: TObject);
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property SelStart;
    property SelText;
    property SelLength;
    property ItemIndex;

    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    property AutoSave: TJvAutoSave read FAutoSave write FAutoSave;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;

    property OnRestored: TNotifyEvent read FOnRestored write FOnRestored;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

{**************************************************}

constructor TJvCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FAutoComplete := True;
  FSearching := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FOver := False;
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FAutoSave := TJvAutoSave.Create(Self);
  FItemSearchs := TJvItemsSearchs.Create;
end;

{**************************************************}

destructor TJvCombobox.Destroy;
begin
  FMaxPixel.Free;
  FAutoSave.Free;
  FItemSearchs.Free;
  inherited;
end;

{**************************************************}

procedure TJvCombobox.Loaded;
var
  st: string;
  i: Integer;
begin
  inherited;
  if Style = csDropDownList then
  begin
    if FAutoSave.LoadValue(i) then
    begin
      ItemIndex := i;
      if Assigned(FOnRestored) then
        FOnRestored(Self);
    end;
  end
  else
  begin
    if FAutoSave.LoadValue(st) then
    begin
      Text := st;
      if Assigned(FOnRestored) then
        FOnRestored(Self);
    end;
  end;
end;

{**************************************************}

procedure TJvCombobox.Change;
var
  res: Integer;
  st: string;
  start, finish: Integer;
begin
  inherited;
  if not FSearching and FAutoComplete then
  begin
    st := Text;
    FMaxPixel.Test(st, Font);
    if Text <> st then
    begin
      Text := st;
      Exit;
    end;
    if (FKey <> 8) and (FKey <> 46) and (FKey <> 13) then
    begin
      FSearching := True;
      st := Text;
      res := SendMessage(Handle, CB_FINDSTRING, -1, Longint(PChar(st)));
      if res <> CB_ERR then
      try
        ItemIndex := res;
        start := Length(st);
        finish := Length(Items[res]) - start;
        Text := Items[res];
        SelStart := start;
        SelLength := finish;
      except
      end;
      FSearching := False;
    end;
  end;
  if Style = csDropDownList then
    FAutoSave.SaveValue(ItemIndex)
  else
    FAutoSave.SaveValue(Text);
end;

{**************************************************}

function TJvCombobox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvCombobox.SearchPrefix(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FKey := Key;
end;

{**************************************************}

procedure TJvCombobox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvCombobox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvCombobox.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvCombobox.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

function TJvCombobox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvCombobox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCombobox.MaxPixelChanged(Sender: TObject);
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

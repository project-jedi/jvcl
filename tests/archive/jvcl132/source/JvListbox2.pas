{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvListbox2.PAS, released on 2001-02-28.

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

unit JvListbox2;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, StdCtrls, Controls, Forms,
  JvItemsSearchs, JVCLVer;

type
  TListboxChange = procedure(Sender: TObject; Item: string) of object;

  TJvListbox2 = class(TListbox)
  private
    FEffect: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnSelectCancel: TNotifyEvent;
    FOver: Boolean;
    FMaxWidth: Integer;
    FScroll: Boolean;
    FOnDelete: TListboxChange;
    FOnAdd: TListboxChange;
    FOnChange: TNotifyEvent;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FItemSearchs: TJvItemsSearchs;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetHScroll(const Value: Boolean);
    procedure RefreshH;
    procedure SetEffect(const Value: Boolean);
  protected
    procedure LBAddString(var Msg: TMessage); message LB_ADDSTRING;
    procedure LBDeleteString(var Msg: TMessage); message LB_DELETEstring;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SelectCancel(var Msg: TMessage); message LBN_SELCANCEL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure Changed; virtual;
  public
    procedure WndProc(var Msg: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
    procedure SelectAll;override;
    procedure UnselectAll;
    procedure InvertSelection;
    procedure MoveSelectedUp; virtual;
    procedure MoveSelectedDown; virtual;
    procedure DeleteSelected;override;
    procedure DeleteAllButSelected;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HorScrollbar: Boolean read FScroll write SetHScroll default True;
    property HotTrack: Boolean read FEffect write SetEffect default False;
    property HintColor: TColor read FColor write FColor default clInfoBk;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnSelectCancel: TNotifyEvent read FOnSelectCancel write FOnSelectCancel;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDeleteString: TListboxChange read FOnDelete write FOnDelete;
    property OnAddString: TListboxChange read FOnAdd write FOnAdd;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
  end;

implementation

{**************************************************}

constructor TJvListBox2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxWidth := 0;
  FEffect := False;
  FColor := clInfoBk;
  FOver := False;
  FScroll := True;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FItemSearchs := TJvItemsSearchs.Create;
end;

{**************************************************}

destructor TJvListbox2.Destroy;
begin
  FItemSearchs.Free;
  inherited;
end;

{**************************************************}

procedure TJvListBox2.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    if FScroll then
      Style := Style or WS_HSCROLL
    else
      Style := Style xor WS_HSCROLL;
end;

{***********************************************}

procedure TJvListbox2.SetEffect(const Value: Boolean);
begin
  FEffect := Value;
  if FEffect then
    Ctl3d := False;
end;

{***********************************************}

procedure TJvListbox2.RefreshH;
var
  i: Integer;
  ItemWidth: Word;
begin
  FMaxWidth := 0;
  for i := 0 to Items.Count - 1 do
  begin
    ItemWidth := Canvas.TextWidth(Items[I] + ' ');
    if FMaxWidth < ItemWidth then
      FMaxWidth := ItemWidth;
  end;
  SetHScroll(FScroll);
end;

{***********************************************}

procedure TJvListbox2.LBAddString(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnAdd) then
    FOnAdd(Self, StrPas(PChar(Msg.lParam)));
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvListbox2.LBDeleteString(var Msg: TMessage);
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, Items.Strings[LongInt(Msg.wParam)]);
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvListbox2.WndProc(var Msg: TMessage);
var
  ItemWidth: Word;
begin
  case Msg.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        ItemWidth := Canvas.TextWidth(StrPas(PChar(Msg.lParam)) + ' ');
        if FMaxWidth < ItemWidth then
          FMaxWidth := ItemWidth;
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
      end;
    LB_DELETESTRING:
      begin
        ItemWidth := Canvas.TextWidth(Items[Msg.wParam] + ' ');
        if ItemWidth = FMaxWidth then
        begin
          inherited;
          RefreshH;
          Exit;
        end;
      end;
    LB_RESETCONTENT:
      SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    WM_SETFONT:
      begin
        inherited;
        Canvas.Font.Assign(Font);
        RefreshH;
        Exit;
      end;
  end;
  inherited;
end;

{***********************************************}

procedure TJvListbox2.SetHScroll(const Value: Boolean);
begin
  if FScroll <> Value then
  begin
    FScroll := Value;
    if FScroll then
      SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
    ReCreateWnd;
  end;
end;

{***********************************************}

procedure TJvListbox2.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvListbox2.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvListbox2.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FEffect then
      Ctl3d := True;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvListbox2.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FEffect then
      Ctl3d := False;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

function TJvListbox2.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvListbox2.SearchPrefix(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvListbox2.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

{**************************************************}

procedure TJvListbox2.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

{**************************************************}

procedure TJvListbox2.SelectCancel(var Msg: TMessage);
begin
  if Assigned(FOnSelectCancel) then
    FOnSelectCancel(Self);
end;

{**************************************************}

function TJvListbox2.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvListbox2.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
  Changed;
end;

{**************************************************}

procedure TJvListbox2.SelectAll;
var
  i: Integer;
begin
  if MultiSelect then
  begin
    Items.BeginUpdate;
    for i := 0 to Items.Count - 1 do
      Selected[i] := True;
    Items.EndUpdate;
  end;
end;
{**************************************************}

procedure TJvListbox2.UnselectAll;
var
  i: Integer;
begin
  if MultiSelect then
  begin
    Items.BeginUpdate;
    for i := 0 to Items.Count - 1 do
      Selected[i] := False;
    Items.EndUpdate;
  end;
end;
{**************************************************}

procedure TJvListbox2.InvertSelection;
var
  i: Integer;
begin
  if MultiSelect then
  begin
    Items.BeginUpdate;
    for i := 0 to Items.Count - 1 do
      Selected[i] := not (Selected[i]);
    Items.EndUpdate;
  end;
end;
{**************************************************}

procedure TJvListbox2.DeleteSelected;
var
  i: Integer;
begin
  if MultiSelect then
  begin
    for i := Items.Count - 1 downto 0 do
      if Selected[i] then
        Items.Delete(i);
  end
  else if ItemIndex <> -1 then
  begin
    i := ItemIndex;
    Items.Delete(i);
    if i > 0 then
      Dec(i);
    if Items.Count > 0 then
      ItemIndex := i;
  end;
  Changed;
end;

{**************************************************}

procedure TJvListbox2.MoveSelectedDown;
var
  i: Integer;
begin
  if not MultiSelect then
  begin
    if (ItemIndex <> -1) and (ItemIndex < Items.Count - 1) then
    begin
      Items.Exchange(ItemIndex, ItemIndex + 1);
      ItemIndex := ItemIndex + 1;
    end;
    Exit;
  end;
  if (Items.Count > 0) and (SelCount > 0) and (not (Selected[Items.Count - 1])) then
  begin
    i := Items.Count - 2;
    while i >= 0 do
    begin
      if Selected[i] then
      begin
        Items.Exchange(i, i + 1);
        Selected[i + 1] := True;
      end;
      Dec(i);
    end;
  end;
end;

{**************************************************}

procedure TJvListbox2.MoveSelectedUp;
var
  i: Integer;
begin
  if not MultiSelect then
  begin
    if ItemIndex > 1 then
    begin
      Items.Exchange(ItemIndex, ItemIndex - 1);
      ItemIndex := ItemIndex - 1;
    end;
    Exit;
  end;
  if (Items.Count > 0) and (SelCount > 0) and not Selected[0] then
  begin
    i := 1;
    while i < Items.Count do
    begin
      if Selected[i] then
      begin
        Items.Exchange(i, i - 1);
        Selected[i - 1] := True;
      end;
      Inc(i);
    end;
  end;
end;

{**************************************************}

procedure TJvListbox2.DeleteAllButSelected;
var
  i: Integer;
begin
  if not MultiSelect then
    Exit;
  i := 0;
  while i < Items.Count do
    if not (Selected[i]) then
      Items.Delete(i)
    else
      Inc(i);
  Changed;
end;

{**************************************************}

procedure TJvListbox2.Changed;
begin
  // (rom) TODO?
end;

end.

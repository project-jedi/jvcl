{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPlaylist.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPlaylist;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvCtrls;

type
  TJvPlaylist = class(TJvListbox)
  private
    FShow: Boolean;
    FItems: TStringList;
    FShowExt: Boolean;
    FRefresh: Boolean;
    procedure SetShow(const Value: Boolean);
    procedure SetItems(const Value: TStringList);
    procedure SetShowExt(const Value: Boolean);
  protected
    procedure LBDeleteString(var Msg: TMessage); message LB_DELETESTRING;
    procedure Changed; override;

    function GetPath(Value: string; Position: Integer): string;
    procedure Refresh;
    procedure ItemsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddItem(Item: String; AObject: TObject); {$IFDEF COMPILER6_UP}override;{$ENDIF}
    procedure AddItems(Value: TStrings);
    function GetItem(Index: Integer): string;
    procedure DeleteDeadFiles;
    procedure SortBySongName;
    procedure SortByPath;
    procedure SortByPathInverted;
    procedure SortBySongNameInverted;
    procedure ReverseOrder;
    procedure RandomOrder;
    procedure MoveSelectedUp; override;
    procedure MoveSelectedDown; override;
    procedure SavePlaylist(FileName: string);
    procedure LoadPlaylist(FileName: string);
  published
    property ShowNumbers: Boolean read FShow write SetShow default False;
    property ShowExtension: Boolean read FShowExt write SetShowExt default False;
    property Items: TStringList read FItems write SetItems;
  end;

implementation

{**************************************************}

constructor TJvPlaylist.Create(AOwner: TComponent);
begin
  inherited;
  FShow := False;
  FShowExt := False;
  FRefresh := False;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChanged;
end;

{**************************************************}

destructor TJvPlaylist.Destroy;
begin
  FItems.Free;
  inherited;
end;

{**************************************************}

function TJvPlayList.GetPath(Value: string; Position: Integer): string;
begin
  Result := ExtractFileName(Value);
  if not FShowExt then
    Result := ChangeFileExt(Result, '');
  if FShow then
    Result := IntToStr(Position + 1) + '. ' + Result;
end;

{**************************************************}

procedure TJvPlaylist.AddItem(Item: String; AObject: TObject); 
begin
  Items.AddObject(Item,AObject);
end;

{**************************************************}

procedure TJvPlaylist.AddItems(Value: TStrings);
begin
  Items.Text := Items.Text + Value.Text;
end;

{**************************************************}

function TJvPlaylist.GetItem(Index: Integer): string;
begin
  Result := Items[Index];
end;

{**************************************************}

procedure TJvPlaylist.DeleteDeadFiles;
var
  i: Integer;
begin
  for i := Items.Count - 1 downto 0 do
    if not FileExists(Items[i]) then
      Items.Delete(i);
end;

{**************************************************}

procedure TJvPlaylist.SortBySongName;
var
  a, b, c: Integer;
begin
  FRefresh := True;
  for a := 0 to Items.Count - 1 do
  begin
    c := a;
    for b := a to Items.Count - 1 do
      if ExtractFileName(Items[b]) < ExtractFileName(Items[c]) then
        c := b;
    Items.Exchange(a, c);
  end;
  FRefresh := False;
  Refresh;
end;

{**************************************************}

procedure TJvPlaylist.SortByPath;
begin
  FItems.Sort;
end;

{**************************************************}

procedure TJvPlaylist.SortByPathInverted;
begin
  FItems.Sort;
  ReverseOrder;
end;

{**************************************************}

procedure TJvPlaylist.SortBySongNameInverted;
begin
  SortBySongName;
  ReverseOrder;
end;

{**************************************************}

procedure TJvPlaylist.ReverseOrder;
var
  i, j: Integer;
begin
  j := FItems.Count - 1;
  for i := 0 to FItems.Count div 2 - 1 do
    FItems.Exchange(i, j - i);
end;

{**************************************************}

procedure TJvPlaylist.RandomOrder;
var
  i, j, k: Integer;
begin
  Randomize;
  for i := 0 to FItems.Count div 2 do
  begin
    j := Random(FItems.Count);
    k := Random(FItems.Count);
    FItems.Exchange(j, k);
  end;
end;

{**************************************************}

procedure TJvPlaylist.SavePlaylist(FileName: string);
begin
  FItems.SaveToFile(FileName);
end;

{**************************************************}

procedure TJvPlaylist.LoadPlaylist(FileName: string);
var
  st, st2: string;
  i: Integer;
begin
  FItems.Clear;
  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    for i := 0 to Count - 1 do
    begin
      st := Strings[i];
      if Length(st) > 0 then
        if st[1] <> '#' then
        begin
          st2 := ExtractFilePath(FileName);
          if st2[Length(st2)] <> '\' then
            st2 := st2 + '\';
          if ((not (FileExists(st))) or (Pos('\', st) = 0)) then
            if FileExists(st2 + st) then
              st := st2 + st;
          FItems.Add(st);
        end;
    end;
    Free;
  end;
end;

{**************************************************}

procedure TJvPlaylist.Refresh;
var
  i: Integer;
begin
  FRefresh := True;
  if Items.Count <> inherited Items.Count then
  begin
    inherited Items.Clear;
    for i := 0 to Items.Count - 1 do
      inherited Items.Add(GetPath(Items[i], i));
  end
  else
    for i := 0 to Items.Count - 1 do
      inherited Items[i] := GetPath(Items[i], i);
  FRefresh := False;
end;

{**************************************************}

procedure TJvPlaylist.SetShow(const Value: Boolean);
begin
  if Value <> FShow then
  begin
    FShow := Value;
    Refresh;
  end;
end;

{**************************************************}

procedure TJvPlaylist.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);
  Refresh;
end;

{**************************************************}

procedure TJvPlaylist.SetShowExt(const Value: Boolean);
begin
  if Value <> FShowExt then
  begin
    FShowExt := Value;
    Refresh;
  end;
end;

{**************************************************}

procedure TJvPlaylist.ItemsChanged(Sender: TObject);
begin
  Refresh;
end;

{**************************************************}

procedure TJvPlaylist.LBDeleteString(var Msg: TMessage);
begin
  inherited;
  if not FRefresh then
  begin
    Items.OnChange := nil;
    Items.Delete(LongInt(Msg.wParam));
    Items.OnChange := ItemsChanged;
  end;
end;

{**************************************************}

procedure TJvPlaylist.Changed;
begin
  Refresh;
end;

{**************************************************}

procedure TJvPlaylist.MoveSelectedDown;
var
  i: Integer;
begin
  if MultiSelect = False then
  begin
    if (ItemIndex <> -1) and (ItemIndex < Items.Count - 1) then
    begin
      Items.Exchange(ItemIndex, ItemIndex + 1);
      ItemIndex := ItemIndex + 1;
    end;
    Exit;
  end;
  Items.OnChange := nil;
  FRefresh := True;
  if (Items.Count > 0) and (SelCount > 0) and not Selected[Items.Count - 1] then
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
  FRefresh := False;
  Items.OnChange := ItemsChanged;
  Refresh;
end;

{**************************************************}

procedure TJvPlaylist.MoveSelectedUp;
var
  i: Integer;
begin
  if MultiSelect = False then
  begin
    if ItemIndex > 1 then
    begin
      Items.Exchange(ItemIndex, ItemIndex - 1);
      ItemIndex := ItemIndex - 1;
    end;
    Exit;
  end;
  Items.OnChange := nil;
  FRefresh := True;
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
  FRefresh := False;
  Items.OnChange := ItemsChanged;
  Refresh;
end;

end.

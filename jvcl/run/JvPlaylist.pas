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
  Messages, SysUtils, Classes, Controls, StdCtrls,
  JvCtrls;

type
  TJvPlaylist = class(TJvListBox)
  private
    FShowNumbers: Boolean;
    FItems: TStrings;
    FShowExtension: Boolean;
    FRefresh: Boolean;
    FShowDrive: boolean;
    procedure SetShowNumbers(const Value: Boolean);
    procedure SetItems(const Value: TStrings);
    procedure SetShowExtension(const Value: Boolean);
    procedure SetShowDrive(const Value: boolean);
  protected
    procedure LBDeleteString(var Msg: TMessage); message LB_DELETESTRING;
    procedure Changed; override;
    function GetPath(Value: string; Position: Integer): string;
    procedure Refresh;
    procedure ItemsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(Item: string; AObject: TObject); {$IFDEF COMPILER6_UP} override; {$ENDIF}
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
    property ShowDrive:boolean read FShowDrive write SetShowDrive default true;
    property ShowNumbers: Boolean read FShowNumbers write SetShowNumbers default False;
    property ShowExtension: Boolean read FShowExtension write SetShowExtension default False;
    property Items: TStrings read FItems write SetItems;
  end;

implementation

constructor TJvPlaylist.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowDrive := true;
  FShowNumbers := False;
  FShowExtension := False;
  FRefresh := False;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChanged;
end;

destructor TJvPlaylist.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJvPlayList.GetPath(Value: string; Position: Integer): string;
var i:integer;
begin
  Result := Value;
  if not FShowDrive then
  begin
    i := Pos(':',Result);
    if i > 0 then
      Result := Copy(Result,i+1,MaxInt)
  end;
  if not FShowExtension then
    Result := ChangeFileExt(Result, '');
  if FShowNumbers then
    Result := IntToStr(Position + 1) + '. ' + Result;
end;

procedure TJvPlaylist.AddItem(Item: string; AObject: TObject);
begin
  Items.AddObject(Item, AObject);
end;

procedure TJvPlaylist.AddItems(Value: TStrings);
begin
  Items.AddStrings(Value);
end;

function TJvPlaylist.GetItem(Index: Integer): string;
begin
  Result := Items[Index];
end;

procedure TJvPlaylist.DeleteDeadFiles;
var
  I: Integer;
begin
  for I := Items.Count - 1 downto 0 do
    if not FileExists(Items[I]) then
      Items.Delete(I);
end;

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

procedure TJvPlaylist.SortByPath;
begin
  TStringList(FItems).Sort;
end;

procedure TJvPlaylist.SortByPathInverted;
begin
  TStringList(FItems).Sort;
  ReverseOrder;
end;

procedure TJvPlaylist.SortBySongNameInverted;
begin
  SortBySongName;
  ReverseOrder;
end;

procedure TJvPlaylist.ReverseOrder;
var
  I, J: Integer;
begin
  J := FItems.Count - 1;
  for I := 0 to FItems.Count div 2 - 1 do
    FItems.Exchange(I, J - I);
end;

procedure TJvPlaylist.RandomOrder;
var
  I, J, K: Integer;
begin
  Randomize;
  for I := 0 to FItems.Count div 2 do
  begin
    J := Random(FItems.Count);
    K := Random(FItems.Count);
    FItems.Exchange(J, K);
  end;
end;

procedure TJvPlaylist.SavePlaylist(FileName: string);
begin
  FItems.SaveToFile(FileName);
end;

procedure TJvPlaylist.LoadPlaylist(FileName: string);
{var
  St, St2: string;
  I: Integer;}
begin
  FItems.LoadFromFile(FileName);
{
  FItems.Clear;
  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    for I := 0 to Count - 1 do
    begin
      St := Strings[I];
      if Length(St) > 0 then
        if St[1] <> '#' then
        begin
          St2 := ExtractFilePath(FileName);
          if St2[Length(St2)] <> '\' then
            St2 := St2 + '\';
          if (not FileExists(St)) or (Pos('\', St) = 0) then
            if FileExists(St2 + St) then
              St := St2 + St;
          FItems.Add(St);
        end;
    end;
    Free;
  end;
  }
end;

procedure TJvPlaylist.Refresh;
var
  I: Integer;
begin
  FRefresh := True;
  if Items.Count <> inherited Items.Count then
  begin
    inherited Items.Clear;
    for I := 0 to Items.Count - 1 do
      inherited Items.Add(GetPath(Items[I], I));
  end
  else
    for I := 0 to Items.Count - 1 do
      inherited Items[I] := GetPath(Items[I], I);
  FRefresh := False;
end;

procedure TJvPlaylist.SetShowNumbers(const Value: Boolean);
begin
  if Value <> FShowNumbers then
  begin
    FShowNumbers := Value;
    Refresh;
  end;
end;

procedure TJvPlaylist.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
  Refresh;
end;

procedure TJvPlaylist.SetShowExtension(const Value: Boolean);
begin
  if Value <> FShowExtension then
  begin
    FShowExtension := Value;
    Refresh;
  end;
end;

procedure TJvPlaylist.ItemsChanged(Sender: TObject);
begin
  Refresh;
end;

procedure TJvPlaylist.LBDeleteString(var Msg: TMessage);
begin
  inherited;
  if not FRefresh then
  begin
    TStringList(FItems).OnChange := nil;
    Items.Delete(LongInt(Msg.WParam));
    TStringList(FItems).OnChange := ItemsChanged;
  end;
end;

procedure TJvPlaylist.Changed;
begin
  Refresh;
end;

procedure TJvPlaylist.MoveSelectedDown;
var
  I: Integer;
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
  TStringList(FItems).OnChange := nil;
  FRefresh := True;
  if (Items.Count > 0) and (SelCount > 0) and not Selected[Items.Count - 1] then
  begin
    I := Items.Count - 2;
    while I >= 0 do
    begin
      if Selected[I] then
      begin
        Items.Exchange(I, I + 1);
        Selected[I + 1] := True;
      end;
      Dec(I);
    end;
  end;
  FRefresh := False;
  TStringList(FItems).OnChange := ItemsChanged;
  Refresh;
end;

procedure TJvPlaylist.MoveSelectedUp;
var
  I: Integer;
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
  TStringList(FItems).OnChange := nil;
  FRefresh := True;
  if (Items.Count > 0) and (SelCount > 0) and not Selected[0] then
  begin
    I := 1;
    while I < Items.Count do
    begin
      if Selected[I] then
      begin
        Items.Exchange(I, I - 1);
        Selected[I - 1] := True;
      end;
      Inc(I);
    end;
  end;
  FRefresh := False;
  TStringList(FItems).OnChange := ItemsChanged;
  Refresh;
end;

procedure TJvPlaylist.SetShowDrive(const Value: boolean);
begin
  if FShowDrive <> Value then
  begin
    FShowDrive := Value;
    Refresh;
  end;
end;

end.


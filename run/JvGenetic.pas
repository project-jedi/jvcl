{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGenetic.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGenetic;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  JvTypes, JvComponent;

type
  {$IFNDEF COMPILER6_UP}
  PByte = ^Byte;
  {$ENDIF COMPILER6_UP}
  TJvTestMember = function(Sender: TObject; Index: Integer; Member: PByte): Byte of object;

  TJvGenetic = class(TJvComponent)
  private
    FMembers: TStringList;
    FGeneration: Integer;
    FSize: Integer;
    FCount: Integer;
    FOnTestMember: TJvTestMember;
    FCrossover: Double;
    FMutationProbability: Double;
    procedure SetCount(const Value: Integer);
    procedure SetSize(const Value: Integer);
    procedure KillThemAll(Value: TStringList);
    function Generate(Father, Mother: PByte; Size: Integer): PByte;
    function Mutate(Value: Byte): Byte;
    function DoCrossover: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NewGeneration;
    procedure NextGeneration;
    function GetMember(Index: Integer): PByte;
    function GetAverage: Double;
    property Generation: Integer read FGeneration;
  published
    property MemberSize: Integer read FSize write SetSize default 4;
    property Count: Integer read FCount write SetCount default 10;
    property CrossoverProbability: Double read FCrossover write FCrossover;
    property MutationProbability: Double read FMutationProbability write FMutationProbability;
    property OnTestMember: TJvTestMember read FOnTestMember write FOnTestMember;
  end;

implementation

uses
  JvResources;

type
  TGeneticMember = class(TObject)
  public
    Points: Cardinal;
    Data: PByte;
  end;

constructor TJvGenetic.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMembers := TStringList.Create;
  Randomize;
  FGeneration := 0;
  FCount := 10;
  FSize := 4;
  FCrossover := 0.6;
  FMutationProbability := 0.003;
end;

destructor TJvGenetic.Destroy;
begin
  KillThemAll(FMembers);
  FMembers.Free;
  inherited Destroy;
end;

function TJvGenetic.DoCrossover: Boolean;
begin
  Result := Random < FCrossover;
end;

function TJvGenetic.Generate(Father, Mother: PByte; Size: Integer): PByte;
var
  I, Count: Integer;
  P, S: PByte;
begin
  if DoCrossover then
    Count := Random(Size - 1)
  else
    Count := Size;

  Result := AllocMem(Size);
  P := Result;
  S := Father;
  for I := 0 to Count - 1 do
  begin
    P^ := Mutate(S^);
    Inc(P);
    Inc(S);
  end;
  S := Mother;
  Inc(S, Count);
  for I := Count to Size - 1 do
  begin
    P^ := Mutate(S^);
    Inc(P);
    Inc(S);
  end;
end;

function TJvGenetic.GetAverage: Double;
var
  I: Integer;
begin
  Result := 0.0;
  if FMembers.Count <> 0 then
  begin
    for I := 0 to FMembers.Count - 1 do
      Result := Result + TGeneticMember(FMembers.Objects[I]).Points;
    Result := Result / FMembers.Count;
  end;
end;

function TJvGenetic.GetMember(Index: Integer): PByte;
begin
  Result := TGeneticMember(FMembers.Objects[Index]).Data;
end;

procedure TJvGenetic.KillThemAll(Value: TStringList);
var
  I: Integer;
begin
  for I := 0 to Value.Count-1 do
  begin
    FreeMem(TGeneticMember(Value.Objects[I]).Data);
    TGeneticMember(Value.Objects[I]).Free;
  end;
  Value.Clear;
end;

function TJvGenetic.Mutate(Value: Byte): Byte;
var
  B: Byte;
  I: Integer;
begin
  B := $80;
  Result := Value;
  for I := 0 to 7 do
  begin
    if Random < FMutationProbability then
    begin
      if (Result and B) = 0 then
        Result := Result or B
      else
        Result := Result and (not B);
    end;
    B := B shr 1;
  end;
end;

procedure TJvGenetic.NewGeneration;
var
  I, J: Integer;
  Member: TGeneticMember;
  P: PByte;
begin
  if (FCount > 0) and (FSize > 0) then
  begin
    KillThemAll(FMembers);
    FGeneration := 0;
    for I := 0 to FCount - 1 do
    begin
      Member := TGeneticMember.Create;
      Member.Data := AllocMem(FSize);
      P := Member.Data;
      for J := 0 to FSize - 1 do
      begin
        Byte(P^) := Random(256);
        Inc(P);
      end;
      if not Assigned(FOnTestMember) then
        raise EJVCLException.CreateRes(@RsENoTest);
      Member.Points := FOnTestMember(Self, I, Member.Data);
      FMembers.AddObject('', TObject(Member));
    end;
  end;
end;

procedure TJvGenetic.NextGeneration;
var
  A, B, Tot: Cardinal;
  I: Integer;
  Father, Mother: Integer;
  FGenerat: TStringList;
  Member: TGeneticMember;
begin
  if (FCount > 0) and (FSize > 0) then
  begin
    Inc(FGeneration);

    //Compute the sum of Points
    Tot := 0;
    for I := 0 to FCount - 1 do
      Inc(Tot, TGeneticMember(FMembers.Objects[I]).Points);

    //Create new Generation
    FGenerat := TStringList.Create;
    for I := 0 to FCount do
    begin
      A := Random(Tot);
      B := TGeneticMember(FMembers.Objects[0]).Points;
      Father := 0;
      while B < A do
      begin
        Inc(Father);
        Inc(B, TGeneticMember(FMembers.Objects[Father]).Points);
      end;

      A := Random(Tot);
      B := TGeneticMember(FMembers.Objects[0]).Points;
      Mother := 0;
      while B < A do
      begin
        Inc(Mother);
        Inc(B, TGeneticMember(FMembers.Objects[Mother]).Points);
      end;

      //Copy, Crossover and mutate
      Member := TGeneticMember.Create;
      Member.Data := Generate(TGeneticMember(FMembers.Objects[Mother]).Data,
        TGeneticMember(FMembers.Objects[Father]).Data, FSize);

      if Assigned(FOnTestMember) then
        Member.Points := FOnTestMember(Self, I, Member.Data)
      else
        raise EJVCLException.CreateRes(@RsENoTest);

      //Add new element to FGenerat
      FGenerat.AddObject('', TObject(Member));
    end;
    KillThemAll(FMembers);
    FMembers.Assign(FGenerat);
    FGenerat.Free;
  end;
end;

procedure TJvGenetic.SetCount(const Value: Integer);
begin
  if FCount <> Value then
  begin
    FCount := Value;
    KillThemAll(FMembers);
    FGeneration := 0;
  end;
end;

procedure TJvGenetic.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    KillThemAll(FMembers);
    FGeneration := 0;
  end;
end;

end.


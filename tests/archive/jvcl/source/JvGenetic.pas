{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGenetic.PAS, released on 2001-02-28.

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

unit JvGenetic;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, JvTypes, JvComponent;

type
  TOnTest = function(Sender: TObject; Index: Integer; Member: PByte): Byte of object;

  TJvGenetic = class(TJvComponent)
  private
    FMembers: TStringList;
    FGeneration: Integer;
    FSize: Integer;
    FNumbers: Integer;
    FOnTest: TOnTest;
    FCrossover: Real;
    FMutate: Real;
    procedure SetNumbers(const Value: Integer);
    procedure SetSize(const Value: Integer);
    procedure KillThemAll(Value: TStringList);
    function Generate(Father, Mother: PByte; Size: Integer): PByte;
    function Mutate(Value: Byte): Byte;
    function DoCrossover: Boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure NewGeneration;
    procedure NextGeneration;
    function GetMember(Index: Integer): PByte;
    function GetAverage: Real;
  published
    property Generation: Integer read FGeneration;
    property MemberSize: Integer read FSize write SetSize default 4;
    property Numbers: Integer read FNumbers write SetNumbers default 10;
    property OnTestMember: TOnTest read FOnTest write FOnTest;
    property CrossoverProbability: Real read FCrossover write FCrossover;
    property MutateProbability: Real read FMutate write FMutate;
  end;

implementation

type
  TGeneticMember = class
    Points: Cardinal;
    Data: PByte;
  end;

resourcestring
  RC_NoTest = 'TJvGenetic : OnTestMember must be Assigned !';

  {***************************************************}

constructor TJvGenetic.Create(AOwner: TComponent);
begin
  inherited;

  FMembers := TStringList.Create;
  Randomize;
  FGeneration := 0;
  FNumbers := 10;
  FSize := 4;
  FCrossover := 0.6;
  FMutate := 0.003;
end;

{***************************************************}

destructor TJvGenetic.Destroy;
begin
  KillThemAll(FMembers);
  FMembers.Free;
  inherited;
end;

{***************************************************}

function TJvGenetic.DoCrossover: Boolean;
begin
  Result := Random < FCrossover;
end;

{***************************************************}

function TJvGenetic.Generate(Father, Mother: PByte; Size: Integer): PByte;
var
  i, count: Integer;
  p, s: PByte;
begin
  if DoCrossover then
    count := Random(Size - 1)
  else
    count := Size;

  Result := AllocMem(Size);
  p := Result;
  s := Father;
  for i := 0 to count - 1 do
  begin
    p^ := Mutate(s^);
    Inc(p);
    Inc(s);
  end;
  s := Mother;
  Inc(s, count);
  for i := count to Size - 1 do
  begin
    p^ := Mutate(s^);
    Inc(p);
    Inc(s);
  end;
end;

{***************************************************}

function TJvGenetic.GetAverage: Real;
var
  i: Integer;
begin
  Result := 0.0;
  if FMembers.Count <> 0 then
  begin
    for i := 0 to FMembers.Count - 1 do
      Result := Result + TGeneticMember(FMembers.Objects[i]).Points;
    Result := Result / FMembers.Count;
  end;
end;

{***************************************************}

function TJvGenetic.GetMember(Index: Integer): PByte;
begin
  Result := TGeneticMember(FMembers.Objects[Index]).Data;
end;

{***************************************************}

procedure TJvGenetic.KillThemAll(Value: TStringList);
begin
  while Value.Count > 0 do
  begin
    FreeMem(TGeneticMember(Value.Objects[0]).Data, FSize);
    TGeneticMember(Value.Objects[0]).Free;
    Value.Delete(0);
  end;
end;

{***************************************************}

function TJvGenetic.Mutate(Value: Byte): Byte;
var
  b: Byte;
  i: Integer;
begin
  b := $80;
  Result := Value;
  for i := 0 to 7 do
  begin
    if Random < FMutate then
    begin
      if (Result and b) = 0 then
        Result := Result or b
      else
        Result := Result and (not b);
    end;
    b := b shr 1;
  end;
end;

{***************************************************}

procedure TJvGenetic.NewGeneration;
var
  i, j: Integer;
  Member: TGeneticMember;
  p: PByte;
begin
  if (FNumbers > 0) and (FSize > 0) then
  begin
    KillThemAll(FMembers);
    FGeneration := 0;
    for i := 0 to FNumbers - 1 do
    begin
      Member := TGeneticMember.Create;
      Member.Data := AllocMem(FSize);
      p := Member.Data;
      for j := 0 to FSize - 1 do
      begin
        Byte(p^) := Random(256);
        Inc(p);
      end;
      if not Assigned(FOnTest) then
        raise EJVCLException.Create(RC_NoTest);
      Member.Points := FOnTest(Self, i, Member.Data);
      FMembers.AddObject('', TObject(Member));
    end;
  end;
end;

{***************************************************}

procedure TJvGenetic.NextGeneration;
var
  a, b, tot: Cardinal;
  i: Integer;
  Father, Mother: Integer;
  FGenerat: TStringList;
  Member: TGeneticMember;
begin
  if (FNumbers > 0) and (FSize > 0) then
  begin
    Inc(FGeneration);

    //Compute the sum of Points
    tot := 0;
    for i := 0 to FNumbers - 1 do
      Inc(tot, TGeneticMember(FMembers.Objects[i]).Points);

    //Create new Generation
    FGenerat := TStringList.Create;
    for i := 0 to FNumbers do
    begin
      a := Random(tot);
      b := TGeneticMember(FMembers.Objects[0]).Points;
      Father := 0;
      while b < a do
      begin
        Inc(Father);
        Inc(b, TGeneticMember(FMembers.Objects[Father]).Points);
      end;

      a := Random(tot);
      b := TGeneticMember(FMembers.Objects[0]).Points;
      Mother := 0;
      while b < a do
      begin
        Inc(Mother);
        Inc(b, TGeneticMember(FMembers.Objects[Mother]).Points);
      end;

      //Copy, Crossover and mutate
      Member := TGeneticMember.Create;
      Member.Data := Generate(TGeneticMember(FMembers.Objects[Mother]).Data,
        TGeneticMember(FMembers.Objects[Father]).Data,
        FSize);

      if Assigned(FOnTest) then
        Member.Points := FOnTest(Self, i, Member.Data)
      else
        raise EJVCLException.Create(RC_NoTest);

      //Add new element to FGenerat
      FGenerat.AddObject('', TObject(Member));
    end;
    KillThemAll(FMembers);
    FMembers.Assign(FGenerat);
    FGenerat.Free;
  end;
end;

{***************************************************}

procedure TJvGenetic.SetNumbers(const Value: Integer);
begin
  if FNumbers <> Value then
  begin
    FNumbers := Value;
    KillThemAll(FMembers);
    FGeneration := 0;
  end;
end;

{***************************************************}

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

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgGridHeaderControl.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgGridHeaderControl;

{$I jvcl.inc}

interface

uses
  Windows, Classes, ComCtrls, Grids, SysUtils, Forms,
  {$IFNDEF DelphiPersonalEdition}
  DBGrids,
  {$ENDIF DelphiPersonalEdition}  // Defines added by JGB
  JVCLVer;

type
  TJvgGridHeaderControl = class(THeaderControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FGrid: TCustomGrid;
    FJoinColumns: TStringList;
    //    aColWidths: array[0..255] of word;
    FEqualSize: Boolean;
    // FSections: THeaderSections;
    function GetJoinColumns: TStrings;
    procedure SetJoinColumns(Value: TStrings);
    procedure SetEqualSize(Value: Boolean);
  protected
    procedure Loaded; override;
    procedure Resize; override;
    // procedure SetSections(Value: THeaderSections);
    procedure SectionResize(Section: THeaderSection); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResizeColumns;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Grid: TCustomGrid read FGrid write FGrid;
    property JoinColumns: TStrings read GetJoinColumns write SetJoinColumns;
    property EqualSize: Boolean read FEqualSize write SetEqualSize default True;
    // property Sections: THeaderSections read FSections write SetSections;
  end;

  TJvgPublicGrid = class(TCustomGrid)
  end;

implementation

uses
  Math;

constructor TJvgGridHeaderControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJoinColumns := TStringList.Create;
  // FSections := THeaderSections.Create(Self);
  FEqualSize := True;
end;

destructor TJvgGridHeaderControl.Destroy;
begin
  FJoinColumns.Free;
  inherited Destroy;
end;

procedure TJvgGridHeaderControl.Loaded;
begin
  inherited Loaded;
  // (rom) disabled  no effect
  // if not Assigned(Grid) then
  //  Exit;
end;

procedure TJvgGridHeaderControl.Resize;
begin
  inherited Resize;
  if not Assigned(Grid) then
  begin
    //  for I:=0 to TJvgPublicGrid(Grid).ColCount-1 do
    //    aColWidths[I] := TJvgPublicGrid(Grid).ColWidths[I];
    ResizeColumns;
  end;
end;

procedure TJvgGridHeaderControl.SectionResize(Section: THeaderSection);
begin
  inherited SectionResize(Section);
  ResizeColumns;
end;

procedure TJvgGridHeaderControl.ResizeColumns;
var
  //   ItemsCount,
  I, Col, Sect, ColsToJoin, ColsToJoinWidth: Integer;
  Indicator: Boolean;
  G: TJvgPublicGrid;
begin
  if not Assigned(Grid) then
    Exit;

  G := TJvgPublicGrid(Grid);
  //   ItemsCount := min(G.ColCount, Sections.Count);
  for I := 0 to Max(JoinColumns.Count - 1, Sections.Count - 1) do
  try
    if JoinColumns.Count <= I then
      JoinColumns.Add('1');
    JoinColumns.Objects[I] := Pointer(StrToInt(JoinColumns[I]));
  except
    JoinColumns.Objects[I] := Pointer(1);
  end;

  Col := 0;
  Sect := 0;
  // DEFINE ADDED BY JGB 6-10-2003
  {$IFDEF DelphiPersonalEdition}
  Indicator := False;
  {$ELSE}
  Indicator := (Grid is TDBGrid) and (dgIndicator in TDBGrid(G).Options);
  {$ENDIF DelphiPersonalEdition}
  if Indicator then
    Col := 1;

  while (Col < G.ColCount) and (Sect < Sections.Count) do
  begin
    ColsToJoin := Min(Integer(JoinColumns.Objects[Sect]), G.ColCount - Col);

    ColsToJoinWidth := 0;
    for I := 0 to ColsToJoin - 1 do
      Inc(ColsToJoinWidth, G.ColWidths[Col + I]);

    //      inc(ColsToJoinWidth ,-ColsToJoin);

    //    ColsToJoinWidth := 0;
    //    for I:=0 to ColsToJoin-1 do
    //      inc(ColsToJoinWidth, G.ColWidths[Col+I]);

    if ColsToJoinWidth <> 0 then
    begin
      for I := 0 to ColsToJoin - 1 do
      begin
        if EqualSize then
          G.ColWidths[Col + I] := Trunc((ColsToJoinWidth / ColsToJoin /
            ColsToJoinWidth) * Sections[Sect].Width) - 1
        else
          G.ColWidths[Col + I] := Trunc((G.ColWidths[Col + I] /
            ColsToJoinWidth) * Sections[Sect].Width) - 1;
      end;
      //G.ColWidths[Col + ColsToJoin-1] := G.ColWidths[Col + ColsToJoin-1] + Sections[Sect].Width - ColsToJoinWidth - ColsToJoin;
    end;

    Inc(Col, Integer(JoinColumns.Objects[Sect]));
    Inc(Sect);
  end;
  if G.BorderStyle <> bsNone then
    G.ColWidths[Ord(Indicator)] := G.ColWidths[Ord(Indicator)] - 3;
  if Indicator then
    G.ColWidths[1] := G.ColWidths[1] - 12;
end;

{ (rom) disabled  unused
procedure TJvgGridHeaderControl.SetSections(Value: THeaderSections);
begin
  FSections.Assign(Value);
end;
}

function TJvgGridHeaderControl.GetJoinColumns: TStrings;
begin
  Result := FJoinColumns;
end;

procedure TJvgGridHeaderControl.SetJoinColumns(Value: TStrings);
begin
  FJoinColumns.Assign(Value);
  ResizeColumns;
end;

procedure TJvgGridHeaderControl.SetEqualSize(Value: Boolean);
begin
  if FEqualSize <> Value then
  begin
    FEqualSize := Value;
    ResizeColumns;
  end;
end;

end.


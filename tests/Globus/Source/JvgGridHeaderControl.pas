{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgGridHeaderControl.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgGridHeaderControl;

interface

uses Windows,
   Classes,
   comctrls,
   grids,
   sysutils,
   Forms,
   dbgrids,
   JVCLVer,
   dialogs;

type
   TJvgGridHeaderControl = class(THeaderControl)
   private
      FGrid: TCustomGrid;
      FJoinColumns: TStringList;
      //    aColWidths: array[0..255] of word;
      FEqualSize: boolean;
      FSections: THeaderSections;
      FAboutJVCL: TJVCLAboutInfo;
   public
      FActiveSectionNo: integer;
      procedure ResizeColumns;
   protected
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Loaded; override;
      procedure Resize; override;
      procedure SetSections(Value: THeaderSections);
      procedure SetJoinColumns(Value: TStringList);
      procedure SetEqualSize(Value: boolean);
      procedure SectionResize(Section: THeaderSection); override;
   published
      property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
         False;
      property Grid: TCustomGrid read FGrid write FGrid;
      property JoinColumns: TStringList read FJoinColumns write SetJoinColumns;
      property EqualSize: boolean read FEqualSize write SetEqualSize default
         true;
      //    property Sections: THeaderSections read FSections write SetSections;
   end;

   TJvgPublicGrid = class(TCustomGrid)
   end;

implementation
uses JvgUtils;

constructor TJvgGridHeaderControl.Create(AOwner: TComponent);
begin
   inherited;
   FJoinColumns := TStringList.Create;
   FSections := THeaderSections.Create(self);
   FEqualSize := true;
end;

destructor TJvgGridHeaderControl.Destroy;
begin
   FJoinColumns.Free;
   inherited;
end;

procedure TJvgGridHeaderControl.Loaded;
var
   i                          : integer;
begin
   inherited;
   if not Assigned(Grid) then
      exit;
end;

procedure TJvgGridHeaderControl.Resize;
var
   i                          : integer;
begin
   inherited;
   if not Assigned(Grid) then
      exit;
   //  for i:=0 to TJvgPublicGrid(Grid).ColCount-1 do
   //    aColWidths[i] := TJvgPublicGrid(Grid).ColWidths[i];

   ResizeColumns;
end;

procedure TJvgGridHeaderControl.SectionResize(Section: THeaderSection);
begin
   inherited;
   ResizeColumns;
end;

procedure TJvgGridHeaderControl.ResizeColumns;
var
   i, ItemsCount, Col, Sect, ColsToJoin, ColsToJoinWidth: integer;
   fIndicator                 : boolean;
   G                          : TJvgPublicGrid;
begin
   if not Assigned(Grid) then
      exit;

   G := TJvgPublicGrid(Grid);
   ItemsCount := min(G.ColCount, Sections.Count);
   for i := 0 to max(FJoinColumns.Count - 1, Sections.Count - 1) do
   try
      if FJoinColumns.Count <= i then
         FJoinColumns.Add('1');
      FJoinColumns.Objects[i] := Pointer(StrToInt(FJoinColumns[i]));
   except
      FJoinColumns.Objects[i] := Pointer(1);
   end;

   Col := 0;
   Sect := 0;
   fIndicator := (Grid is TDBGrid) and (dgIndicator in TDBGrid(g).Options);
   if fIndicator then
      col := 1;

   while (Col < G.ColCount) and (Sect < Sections.Count) do
   begin
      ColsToJoin := min(integer(FJoinColumns.Objects[Sect]), G.ColCount - Col);

      ColsToJoinWidth := 0;
      for i := 0 to ColsToJoin - 1 do
         inc(ColsToJoinWidth, g.ColWidths[Col + i]);

      //      inc(ColsToJoinWidth ,-ColsToJoin);

      //    ColsToJoinWidth := 0;
      //    for i:=0 to ColsToJoin-1 do
      //      inc(ColsToJoinWidth, G.ColWidths[Col+i]);

      if ColsToJoinWidth <> 0 then
      begin
         for i := 0 to ColsToJoin - 1 do
         begin
            if EqualSize then
               G.ColWidths[Col + i] := trunc((ColsToJoinWidth / ColsToJoin /
                  ColsToJoinWidth) * Sections[Sect].Width) - 1
            else
               G.ColWidths[Col + i] := trunc((g.ColWidths[Col + i] /
                  ColsToJoinWidth) * Sections[Sect].Width) - 1;
         end;
         //G.ColWidths[Col + ColsToJoin-1] := G.ColWidths[Col + ColsToJoin-1] + Sections[Sect].Width - ColsToJoinWidth - ColsToJoin;
      end;

      inc(Col, integer(JoinColumns.Objects[Sect]));
      inc(Sect);
   end;
   if G.BorderStyle <> bsNone then
      G.ColWidths[integer(fIndicator)] := G.ColWidths[integer(fIndicator)] - 3;
   if fIndicator then
      G.ColWidths[1] := G.ColWidths[1] - 12;
end;
//------------------------------------------------------------------------------

procedure TJvgGridHeaderControl.SetSections(Value: THeaderSections);
begin
   FSections.Assign(Value);
end;

procedure TJvgGridHeaderControl.SetJoinColumns(Value: TStringList);
begin
   FJoinColumns.Assign(Value);
   ResizeColumns;
end;

procedure TJvgGridHeaderControl.SetEqualSize(Value: boolean);
begin
   FEqualSize := Value;
   ResizeColumns;
end;

end.

